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

(define-module (umcu packages mutationalpatterns)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages statistics))

;; ----------------------------------------------------------------------------
;; NEW PACKAGES
;; ----------------------------------------------------------------------------

(define-public r-lmtest
  (package
    (name "r-lmtest")
    (version "0.9-34")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lmtest" version))
       (sha256
        (base32 "0bhdfwrrwjkmlw0wwx7rh6lhdjp68p7db5zfzginnv3dxmksvvl6"))))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("r-zoo" ,r-zoo)))
    (home-page "http://cran.r-project.org/web/packages/lmtest")
    (synopsis "Test suite for linear regression models")
    (description "This package provides a collection of tests, data sets, and
examples for diagnostic checking in linear regression models.  Furthermore,
some generic tools for inference in parametric models are provided.")
    (license license:gpl2+)))

(define-public r-scatterplot3d
  (package
    (name "r-scatterplot3d")
    (version "0.3-37")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "scatterplot3d" version))
       (sha256
        (base32
         "04mbhgjcfrzzrqkn0rw9fd1pqhdimfl35pr6p0wvz5wvzs8klpws"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/scatterplot3d")
    (synopsis "Package for 3D scatter plotting of multivariate data")
    (description "This package can be used for the visualization of
multivariate data in a three dimensional space using a parallel
projection.  Higher dimensions (fourth, fifth, etc.) of the data can be
visualized to some extent using, e.g. different colors, symbol types or
symbol sizes.")
    (license license:gpl2+)))

(define-public r-sandwich
  (package
    (name "r-sandwich")
    (version "2.3-4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "sandwich" version))
              (sha256
               (base32
                "0kbdfkqc8h3jpnlkil0c89z1192q207lii92yirc61css7izfli0"))))
    (build-system r-build-system)
    ;; These inputs are needed to run all tests.
    (inputs
     `(("r-lmtest" ,r-lmtest)
       ("r-scatterplot3d" ,r-scatterplot3d)))
    (propagated-inputs
     `(("r-zoo" ,r-zoo)))
    (home-page "http://cran.r-project.org/web/packages/sandwich")
    (synopsis "Robust covariance matrix estimators")
    (description "This package provides model-robust standard error estimators
for cross-sectional, time series, and longitudinal data.")
    (license license:gpl2+)))

(define-public r-th-data
  (package
    (name "r-th-data")
    (version "1.0-7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "TH.data" version))
              (sha256
               (base32
                "03h7lr5nh8090w167y3pmpxa0na1mqq4g4k8vz3ypjxc9ls2dq99"))))
    (properties `((upstream-name . "TH.data")))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/TH.data")
    (synopsis "Package with data from Torsten Hothorn")
    (description "This package contains data sets used in other packages
Torsten Hothorn maintains.")
    (license license:gpl3+)))

(define-public r-multcomp
  (package
    (name "r-multcomp")
    (version "1.4-5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "multcomp" version))
              (sha256
               (base32
                "0f7q6402i8zki81ip7i076x2yd3ji0n3vnsrfbgp8vy8pqkpxvii"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-codetools" ,r-codetools)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-sandwich" ,r-sandwich)
       ("r-th-data" ,r-th-data)))
    (home-page "http://cran.r-project.org/web/packages/multcomp")
    (synopsis "This package does simultaneous inference in general parametric models")
    (description "This package provides methods for simultaneous tests and
confidence intervals for general linear hypotheses in parametric models,
including linear, generalized linear, linear mixed effects, and survival
models.  The package includes demos reproducing analyzes presented in the
book \"Multiple Comparisons Using R\" (Bretz, Hothorn, Westfall, 2010, CRC
Press).")
    (license license:gpl2+)))

(define-public r-lsmeans
  (package
    (name "r-lsmeans")
    (version "2.23")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lsmeans" version))
       (sha256
        (base32
         "0f3i2415nd6s80lcw0cbksz2g360ws6yvvc0c3rw47z95zi599fj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-coda" ,r-coda)
       ("r-estimability" ,r-estimability)
       ("r-multcomp" ,r-multcomp)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-plyr" ,r-plyr)
       ("r-xtable" ,r-xtable)))
    (home-page "http://cran.r-project.org/web/packages/lsmeans")
    (synopsis "This package can obtain least-squares means")
    (description "This package can obtain least-squares means for many linear,
generalized linear, and mixed models. It can also compute contrasts or linear
functions of least-squares means, and comparisons of slopes, and it can plot
and compact letter displays.")
    (license license:gpl2+)))

(define-public r-gdata
  (package
    (name "r-gdata")
    (version "2.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gdata" version))
       (sha256
        (base32
         "0kiy3jbcszlpmarg311spdsfi5pn89wgy742dxsbzxk8907fr5w0"))))
    (build-system r-build-system)
    (inputs
     `(("perl" ,perl)))
    (propagated-inputs
     `(("r-gtools" ,r-gtools)))
    (home-page "http://cran.r-project.org/web/packages/gdata")
    (synopsis "Various R programming tools for data manipulation")
    (description
     "") ; TODO: Add a meaningful description.
    (license license:gpl2+)))

(define-public r-kernsmooth
  (package
    (name "r-kernsmooth")
    (version "2.23-15")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "KernSmooth" version))
              (sha256
               (base32
                "1xhha8kw10jv8pv8b61hb5in9qiw3r2a9kdji3qlm991s4zd4wlb"))))
    (properties `((upstream-name . "KernSmooth")))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://cran.r-project.org/web/packages/KernSmooth")
    (synopsis "Functions for Kernel Smoothing Supporting Wand & Jones (1995)")
    (description
     "This package provides functions for kernel smoothing (and density
estimation) corresponding to the book: Wand, M.P.  and Jones, M.C. (1995)
\"Kernel Smoothing\".")
    (license license:unlicense)))

(define-public r-gtools
  (package
    (name "r-gtools")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gtools" version))
       (sha256
        (base32
         "1xknwk9xlsj027pg0nwiizigcrsc84hdrig0jn0cgcyxj8dabdl6"))))
    (build-system r-build-system)
    (home-page
     "http://cran.r-project.org/web/packages/gtools")
    (synopsis "Assisting functions for developing R packages")
    (description
     "") ; TODO: Add a meaningful description.
    (license license:gpl2+)))

(define-public r-gplots
  (package
  (name "r-gplots")
  (version "3.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "gplots" version))
      (sha256
        (base32
          "02nb8n3s7c1zxq2s7ycaq2ys72y7mzirxrwj954h6gdc4x1zhg9l"))))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-catools" ,r-catools)
      ("r-gdata" ,r-gdata)
      ("r-gtools" ,r-gtools)
      ("r-kernsmooth" ,r-kernsmooth)))
  (home-page "http://cran.r-project.org/web/packages/gplots")
  (synopsis "Various R tools for plotting data")
  (description
    "This package provides plotting tools for R.  One of its many features is
the ability to plot Venn diagrams for up to five sets.")
  (license license:gpl2+)))

(define-public r-mutationalpatterns
  (package
    (name "r-mutationalpatterns")
    (version "0.1-alpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/CuppenResearch/MutationalPatterns/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "0sg9c6f54pqsav7lixiqk8rkj8v464dl91i16s63zm3dm4z96ahy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-variantannotation" ,r-variantannotation)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-gplots" ,r-gplots)
       ("r-nmf" ,r-nmf)
       ("r-reshape2" ,r-reshape2)
       ("r-plyr" ,r-plyr)
       ("r-pracma" ,r-pracma)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-bsgenome" ,r-bsgenome)))
    (home-page "https://github.com/CuppenResearch/MutationalPatterns")
    (synopsis "Package for extracting and visualizing mutational patterns in
SNV data")
    (description "This R package provides a comprehensive set of flexible
functions for easy finding and plotting of mutational patterns in Single
Nucleotide Variant (SNV) data.")
    (license license:expat)))
