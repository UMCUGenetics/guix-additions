;;; GNU Guix --- Functional package management for GNU
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

;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.

;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.

(define-module (umcu packages conifer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)

(define-public python-conifer
  (package
   (name "python-conifer")
   (version "0.2.2")
   (source (origin
            (method url-fetch)
            (uri "https://downloads.sourceforge.net/project/conifer/CoNIFER%200.2.2/conifer_v0.2.2.tar.gz")
          (sha256
           (base32 "03hij9gw8l9669q3ghhpw7spr00v6hscky7da7k0a7fnsyn0c4qn"))))

   (build-system python-build-system)
   (propagated-inputs
    `(("python-matplotlib" ,python-matplotlib)
      ("python-numpy" ,python-numpy)
      ("python-pysam" ,python-pysam)
      ("python-pytables" ,python-pytables)
      ("python-scipy" ,python-scipy)))

   (home-page "http://conifer.sourceforge.net/")
   (synopsis "Copy Number Inference From Exome Reads")
   (description "CoNIFER uses exome sequencing data to find copy number variants (CNVs) and genotype the copy-number of duplicated genes")

   (license license:expat)))
