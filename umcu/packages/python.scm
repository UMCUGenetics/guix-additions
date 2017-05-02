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

(define-module (umcu packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression))

(define-public python-py2bit
  (package
   (name "python-py2bit")
   (version "0.2.1")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "py2bit" version))
            (sha256
             (base32
              "1cdf4qlmgwsh1f4k0wdv2sr8x9qn4366p0k3614vbd0fpqiarxrl"))))
   (build-system python-build-system)
   (home-page "https://github.com/dpryan79/py2bit")
   (synopsis "A package for accessing 2bit files using lib2bit")
   (description "A package for accessing 2bit files using lib2bit")
   (license license:expat)))

(define-public python-deeptools
  (package
   (name "python-deeptools")
   (version "2.5.0.1")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "deepTools" version))
     (sha256
      (base32
       "1ybnbz4y1aql2fgdf0s18cf4arsd8myv18rd3nna261sppjfxlfs"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-matplotlib" ,python-matplotlib)
      ("python-numpy" ,python-numpy)
      ("python-numpydoc" ,python-numpydoc)
      ("python-py2bit" ,python-py2bit)
      ("python-pybigwig" ,python-pybigwig)
      ("python-pysam" ,python-pysam)
      ("python-scipy" ,python-scipy)))
   (home-page "http://pypi.python.org/pypi/deepTools/")
   (synopsis "Useful tools for exploring deep sequencing data")
   (description "Useful tools for exploring deep sequencing data")
   (license #f)))

(define-public python-macs2
  (package
    (name "python-macs2")
    (version "2.1.1.20160309")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MACS2" version))
       (sha256
        (base32
         "09ixspd1vcqmz1c81ih70xs4m7qml2iy5vyx1y74zww3iy1vl210"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)))
    (home-page "http://github.com/taoliu/MACS/")
    (synopsis "Model Based Analysis for ChIP-Seq data")
    (description "Model Based Analysis for ChIP-Seq data")
    (license #f)))

(define-public python-pytabix
  (package
    (name "python-pytabix")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             (pypi-uri "pytabix" version)))
       (sha256
        (base32
         "1ldp5r4ggskji6qx4bp2qxy2vrvb3fam03ksn0gq2hdxgrlg2x07"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/slowkow/pytabix")
    (synopsis "Python interface for tabix")
    (description "Python interface for tabix")
    (license license:expat)))

(define-public python2-pytabix
  (package-with-python2 python-pytabix))
