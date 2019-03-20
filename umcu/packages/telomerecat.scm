;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages telomerecat)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages cran))

(define-public python2-parabam
(package
  (name "python2-parabam")
  (version "2.2.5")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "parabam" version))
      (sha256
        (base32
          "1a4pq7lligzg636qixx67c3kxcrpsyvxhakrfkxvhww8084b9rrj"))))
  (build-system python-build-system)
  (arguments
    `(#:python ,python-2
      #:tests? #f ))
  (propagated-inputs
    `(("python2-numpy" ,python2-numpy)
      ("python2-pysam" ,python2-pysam)))
  (home-page "")
  (synopsis "Parallel BAM File Analysis")
  (description "Parallel BAM File Analysis")
  (license license:gpl3)))

(define-public telomecat
(package
  (name "telomerecat" )
  (version "3.2")
  (source (origin
    (method url-fetch)
      (uri "https://files.pythonhosted.org/packages/ae/9c/08288b2a8ccd7d8092a8bd8198d014a0ccbafa1e5e77e872347a6424725e/telomerecat-3.2.tar.gz")
        (sha256
          (base32
            "0m71w1s52rishfy9jbn76c7qh6jzga4xj1jxx7m5gq690q4m13fm"))))
  (build-system python-build-system)
  (arguments
    `(#:python ,python-2
      #:tests? #f ))
  (propagated-inputs
    `(("python2-pypdf2" ,python2-pypdf2)
     ("python2-numpy" ,python2-numpy)
     ("python2-pysam" ,python2-pysam)
     ("python2-pandas" ,python2-pandas)
     ("r-argparser" ,r-argparser)
     ("python2" ,python-2.7)
     ("python2-parabam" ,python2-parabam)))
  (inputs
    `(("unzip" ,unzip)))
  (home-page "http://pypi.python.org/pypi/telomerehunter/")
  (synopsis "Estimation of Telomere Content from WGS Data")
  (description "TelomereHunter extracts, sorts and analyses telomeric reads
                from WGS Data. It is designed to take BAM files from a tumor and/or a control
                sample as input. The tool was developed at the German Cancer Research Center (DKFZ).")
  (license #f)))
