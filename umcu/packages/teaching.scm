;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages teaching)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics))

(define-public iq-tree
  (package
    (name "iq-tree")
    (version "1.6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Cibiv/IQ-TREE/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "11528sxv9hkmg20r2fy9zyq0rylbrhcjnh6cr61dwmx1wwnhj54n"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DIQTREE_FLAGS=omp")
       #:tests? #f))
    (inputs
     `(("eigen" ,eigen)
       ("zlib" ,zlib)))
    (home-page "http://www.iqtree.org/")
    (synopsis "Efficient software for phylogenomic inference")
    (description
     "This package provides software for phylogenomic inference.")
    (license license:gpl2)))
