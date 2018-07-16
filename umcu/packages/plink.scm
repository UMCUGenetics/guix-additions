;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages plink)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths))

(define-public plink2-1.90b3
  (package
    (name "plink2")
    (version "1.90b3")
    (source
     (origin
      (method url-fetch)
      ;; https://github.com/chrchang/plink-ng/archive/v1.90b3.tar.gz
       (uri (string-append
             "https://github.com/chrchang/plink-ng/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "03fzib1al5qkr9vxv63wxmv6y2pfb1rmir0h8jpi72r87hczqjig"))
       (patches (list (search-patch "plink-ng-Makefile-zlib.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
                 (lambda _
                   (zero? (system* "make" "-f" "Makefile.std"))
                   ))                 
        (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (install-file "plink2" bin)
                      (symlink (string-append bin "/plink2")
                               (string-append bin "/plink"))
                      #t))))))
    (inputs
     `(("zlib" ,zlib)
       ("openblas" ,openblas)
       ("atlas" ,atlas)
       ("lapack" ,lapack)
       ("gfortran" ,gfortran)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://www.cog-genomics.org/plink2")
    (synopsis "Whole genome association analysis toolset")
    (description
     "PLINK is a whole genome association analysis toolset, designed to
perform a range of basic, large-scale analyses in a computationally efficient
manner.  The focus of PLINK is purely on analysis of genotype/phenotype data,
so there is no support for steps prior to this (e.g. study design and
planning, generating genotype or CNV calls from raw data).  Through
integration with gPLINK and Haploview, there is some support for the
subsequent visualization, annotation and storage of results.")
    ;; Code is released under GPLv2, except for fisher.h, which is under
    ;; LGPLv2.1+
    (license (list license:gpl2 license:lgpl2.1+))))
