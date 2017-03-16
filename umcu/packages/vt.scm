;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages vt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl))

;; TODO: Vt bundles htslib (and a couple of other libraries), for which we
;; have a separate package already.  It would be great to unbundle this,
;; before upstreaming it.
(define-public vt
  (package
   (name "vt")
   (version "0.5772")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/atks/vt/archive/"
                  version ".tar.gz"))
            (sha256
             (base32 "1rwpvb5qqwqk3215jw9c5240wdnh6f0y84a5a8vcbxx2g0254ixi"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'check
          (lambda _
            (system* "make" "test")))
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin")))
              (mkdir-p bin)
              (install-file "vt" bin)))))))
   (native-inputs
    `(("perl" ,perl)))
   (inputs
    `(("zlib" ,zlib)))
   (home-page "http://genome.sph.umich.edu/wiki/Vt")
   (synopsis "Variant tool set that discovers short variants from NGS data")
   (description "This package provides a variant tool set that discovers
short variants from Next Generation Sequencing data.")
   (license license:expat)))
