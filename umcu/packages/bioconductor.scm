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

(define-module (umcu packages bioconductor)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages statistics))

(define-public r-biocinstaller
  (package
    (name "r-biocinstaller")
    (version "1.22.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocInstaller" version))
              (sha256
               (base32
                "02qkfq6f2b7v9klri6d1nv21r54bywv1zd5x47ka0jhhp946cqpr"))))
    (properties
     `((upstream-name . "BiocInstaller")))
    (build-system r-build-system)
    (home-page "http://bioconductor.org/packages/BiocInstaller")
    (synopsis "Bioconductor installer package.")
    (description "This package is used to install and update Bioconductor,
CRAN, and (some) github packages.")
    (license license:artistic2.0)))

(define-public r-biocviews
  (package
    (name "r-biocviews")
    (version "1.40.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biocViews" version))
              (sha256
               (base32
                "1d1g06zwx3xhc07mdhs5x31730xw08fg3x73xyfj0qdy2ykww3f9"))))
    (properties
     `((upstream-name . "biocViews")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-graph" ,r-graph)
       ("r-rbgl" ,r-rbgl)
       ("r-rcurl" ,r-rcurl)
       ("r-xml" ,r-xml)
       ("r-knitr" ,r-knitr)
       ("r-runit" ,r-runit)))
    (home-page "http://bioconductor.org/packages/Biocviews")
    (synopsis "Bioconductor structures for vocabularies and narratives of views")
    (description "This package provides structures for vocabularies and
narratives of views for Bioconductor packages.")
    (license license:artistic2.0)))

(define-public r-rbgl
  (package
    (name "r-rbgl")
    (version "1.49.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://bioconductor.org/packages/3.4/bioc/src/contrib/"
                    "RBGL_" version ".tar.gz"))
              (sha256
               (base32
                "0rpiqpalbxk82jkwv3l0fimq9y2hrdi0f7ca4v21318541vfsncs"))))
    (properties
     `((upstream-name . "RBGL")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-graph" ,r-graph)))
    (home-page "http://bioconductor.org/packages/RBGL")
    (synopsis "Interface to the graph algorithms from BOOST.")
    (description
     "This package provides a fairly extensive and comprehensive interface to
the graph algorithms contained in the BOOST library.")
    (license license:artistic2.0)))

;; FIXME: Upon installation, it tries to connect to bioconductor.org.
(define-public r-bioccheck
  (package
    (name "r-bioccheck")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocCheck" version))
              (sha256
               (base32
                "0bvkhxmr25nb0gkncifbc3x48i5zgls3737g3xais6jx16wd4q35"))))
    (properties
     `((upstream-name . "BiocCheck")))
    (build-system r-build-system)
    (inputs
     `(("which" ,which)))
    (propagated-inputs
     `(("r-graph" ,r-graph)
       ("r-knitr" ,r-knitr)
       ("r-httr" ,r-httr)
       ("r-optparse" ,r-optparse)
       ("r-devtools" ,r-devtools)
       ("r-biocinstaller" ,r-biocinstaller)
       ("r-biocviews" ,r-biocviews)))
    (home-page "http://bioconductor.org/packages/BiocCheck")
    (synopsis "Executes Bioconductor-specific package checks.")
    (description "This package executes Bioconductor-specific
package checks.")
    (license license:artistic2.0)))

(define-public r-optparse
  (package
    (name "r-optparse")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "optparse" version))
       (sha256
        (base32
         "1g8as89r91xxi5j5azsd6vrfrhg84mnfx2683j7pacdp8s33radw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-getopt" ,r-getopt)))
    (home-page
     "https://github.com/trevorld/optparse")
    (synopsis "Command Line Option Parser")
    (description
     "This package provides a command line parser inspired by Python's
'optparse' library to be used with Rscript to write \"#!\" shebang
scripts that accept short and long flag/options.")
    (license license:gpl2+)))

(define-public r-getopt
  (package
    (name "r-getopt")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "getopt" version))
       (sha256
        (base32
         "00f57vgnzmg7cz80rjmjz1556xqcmx8nhrlbbhaq4w7gl2ibl87r"))))
    (build-system r-build-system)
    (home-page "https://github.com/trevorld/getopt")
    (synopsis "C-like getopt behavior.")
    (description
     "Package designed to be used with Rscript to write ``#!'' shebang
scripts that accept short and long flags/options.  Many users will prefer
using instead the packages optparse or argparse which add extra features
like automatically generated help option and usage, support for default
values, positional argument support, etc.")                                                                     
    (license license:gpl2+)))
