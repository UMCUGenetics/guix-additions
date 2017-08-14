;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Joep de Ligt <j.deligt@umcutrecht.nl>
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

(define-module (umcu packages conifer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics))

(define-public python-conifer
  (package
   (name "python-conifer")
   (version "0.2.2")
   (source (origin
            (method url-fetch)
            (uri "https://downloads.sourceforge.net/project/conifer/CoNIFER%200.2.2/conifer_v0.2.2.tar.gz")
          (sha256
           (base32 "03hij9gw8l9669q3ghhpw7spr00v6hscky7da7k0a7fnsyn0c4qn"))))

   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (script-dir (string-append out "/lib/python2.7/site-packages/"))
               (tar  (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (PATH (string-append (assoc-ref %build-inputs "gzip") "/bin")))
          (mkdir-p script-dir)
          (setenv "PATH" PATH)
          (system* tar "xvf" (assoc-ref %build-inputs "source"))
          (install-file "conifer_v0.2.2/conifer_functions.py" script-dir)
          (install-file "conifer_v0.2.2/conifer.py" script-dir)
   ))))

   (native-inputs
    `(("gzip" ,gzip)
      ("tar" ,tar)))

   (propagated-inputs
    `(("python-matplotlib" ,python-matplotlib)
      ("python-numpy" ,python-numpy)
      ("python-pysam" ,python-pysam)
      ("python-tables" ,python-tables)
      ("python-scipy" ,python-scipy)))

   (home-page "http://conifer.sourceforge.net/")
   (synopsis "Copy Number Inference From Exome Reads")
   (description "CoNIFER uses exome sequencing data to find copy number variants (CNVs) and genotype the copy-number of duplicated genes")

   (license license:expat)))
