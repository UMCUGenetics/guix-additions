;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is not officially part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.

(define-module (umcu packages boost)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages shells))

(define-public boost-1.58
  (package
    (name "boost")
    (version "1.58.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "1rfkqxns60171q62cppiyzj8pmsbwp1l8jd7p6crriryqd7j1z7x"))
              (patches (list (search-patch "boost-mips-avoid-m32.patch")))))
    (build-system gnu-build-system)
    (inputs `(("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)
       ("tcsh" ,tcsh)))
    (arguments
     (let ((build-flags
            `("threading=multi" "link=shared"

              ;; Set the RUNPATH to $libdir so that the libs find each other.
              (string-append "linkflags=-Wl,-rpath="
                             (assoc-ref outputs "out") "/lib")

              ;; Boost's 'context' library is not yet supported on mips64, so
              ;; we disable it.  The 'coroutine' library depends on 'context',
              ;; so we disable that too.
              ,@(if (string-prefix? "mips64" (or (%current-target-system)
                                                 (%current-system)))
                    '("--without-context" "--without-coroutine")
                    '()))))
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'bootstrap)
           (replace
            'configure
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (substitute* '("libs/config/configure"
                               "libs/spirit/classic/phoenix/test/runtest.sh"
                               "tools/build/doc/bjam.qbk"
                               "tools/build/src/engine/execunix.c"
                               "tools/build/src/engine/Jambase"
                               "tools/build/src/engine/jambase.c")
                  (("/bin/sh") (which "sh")))

                (setenv "SHELL" (which "sh"))
                (setenv "CONFIG_SHELL" (which "sh"))

                (unless (zero? (system* "./bootstrap.sh"
                                        (string-append "--prefix=" out)
                                        "--with-toolset=gcc"))
                  (throw 'configure-error)))))
           (replace
            'build
            (lambda* (#:key outputs #:allow-other-keys)
              (zero? (system* "./b2" ,@build-flags))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (zero? (system* "./b2" "install" ,@build-flags))))))))

    (home-page "http://boost.org")
    (synopsis "Peer-reviewed portable C++ source libraries")
    (description
     "A collection of libraries intended to be widely useful, and usable
across a broad spectrum of applications.")
    (license (license:x11-style "http://www.boost.org/LICENSE_1_0.txt"
                                "Some components have other similar licences."))))
