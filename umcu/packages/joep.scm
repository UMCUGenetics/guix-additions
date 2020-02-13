;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages joep)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages maths))

(define-public metamaps
  (let ((commit "e23f8a8688159ff0d092557a40305dbc7acc2342"))
    (package
     (name "metamaps")
     (version (string-append "0.0-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DiltheyLab/MetaMaps.git")
                    (commit commit)))
              (sha256
               (base32
                "0h9ahkv7axw4qzgbvhsz4r699swiv64hlwjy6h8s11vjls2dslrp"))))
     (build-system gnu-build-system)
     (arguments
      `(#:configure-flags (list (string-append
                                 "--with-boost="
                                 (assoc-ref %build-inputs "boost")))
        #:tests? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'shared-boost
            (lambda _
              (substitute* "configure.ac"
               (("libboost_math_c99.a") "libboost_math_c99.so")))))))
     (native-inputs
      `(("autoconf" ,autoconf)))
     (inputs
      `(("boost" ,boost)
        ("zlib" ,zlib)
        ("gsl" ,gsl)))
     (home-page "https://github.com/DiltheyLab/MetaMaps")
     (synopsis "Long-read metagenomic analysis")
     (description "MetaMaps is tool specifically developed for the analysis
of long-read (PacBio/Oxford Nanopore) metagenomic datasets.")
     (license license:public-domain))))
