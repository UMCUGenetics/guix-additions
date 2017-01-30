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
  #:use-module (gnu packages web)
  #:use-module (gnu packages statistics))

(define-public r-rmisc
  (package
  (name "r-rmisc")
  (version "1.5")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "Rmisc" version))
      (sha256
        (base32
          "1ijjhfy3v91fspid77rrkc5dkcb2lav37wc3f4k5lwrn24wzy5y8"))))
  (properties `((upstream-name . "Rmisc")))
  (build-system r-build-system)
  (propagated-inputs
   `(("r-plyr" ,r-plyr)
     ("r-rcpp" ,r-rcpp)))
  (home-page
    "http://cran.r-project.org/web/packages/Rmisc")
  (synopsis "Rmisc: Ryan Miscellaneous")
  (description
    "The Rmisc library contains many functions useful for data analysis and utility operations.")
  (license license:gpl3)))

(define-public r-shiny
  (package
    (name "r-shiny")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "shiny" version))
              (sha256
               (base32
                "152v2z1cyg6893b0qd5rs12a2m9dbzk7ak04qvbcym0s9y4l0kf4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-htmltools" ,r-htmltools)
       ("r-httpuv" ,r-httpuv)
       ("r-jsonlite" ,r-jsonlite)
       ("r-mime" ,r-mime)
       ("r-r6" ,r-r6)
       ("r-sourcetools" ,r-sourcetools)
       ("r-xtable" ,r-xtable)))
    (home-page "http://shiny.rstudio.com")
    (synopsis "Web application framework for R")
    (description "Makes it incredibly easy to build interactive web
applications with R.  Automatic reactive binding between inputs and
outputs and extensive pre-built widgets make it possible to build
beautiful, responsive, and powerful applications with minimal effort.")
    (license license:gpl3+)))

(define-public r-interactivedisplaybase
  (package
    (name "r-interactivedisplaybase")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "interactiveDisplayBase" version))
              (sha256
               (base32
                "1gxa1sc2sk7xvxc4p74cwjkxdk3ns7igl51jg7a7086k729k3m8j"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-shiny" ,r-shiny)))
    (home-page "http://bioconductor.org/packages/interactiveDisplayBase")
    (synopsis "Base package for enabling web displays of Bioconductor objects")
    (description "This package contains the the basic methods needed to
generate interactive Shiny based display methods for Bioconductor objects.")
    (license license:artistic2.0)))
    
(define-public r-annotationhub
  (package
    (name "r-annotationhub")
    (version "2.6.4")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationHub" version))
              (sha256
               (base32
                "1kciqhdd0s447hqhlg1dy603fi1l26acyny1qk2iigpwhvdry9cr"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rsqlite" ,r-rsqlite)
       ("r-biocinstaller" ,r-biocinstaller)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-s4vectors" ,r-s4vectors)
       ("r-interactivedisplaybase" ,r-interactivedisplaybase)
       ("r-httr" ,r-httr)
       ("r-yaml" ,r-yaml)))
    (home-page "http://bioconductor.org/packages/AnnotationHub")
    (synopsis "Client to access AnnotationHub resources")
    (description "This package provides a client for the Bioconductor 
AnnotationHub web resource.  The AnnotationHub web resource provides a central
location where genomic files (e.g., VCF, bed, wig) and other resources from
standard locations (e.g., UCSC, Ensembl) can be discovered.  The resource
includes metadata about each resource, e.g., a textual description, tags, and
date of modification.  The client creates and manages a local cache of files
retrieved by the user, helping with quick and reproducible access.")
    (license license:lgpl3)))

(define-public r-ensembldb
  (package
    (name "r-ensembldb")
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ensembldb" version))
              (sha256
               (base32
                "1hxvjplkgkibjfzrbi6whqr7czf5digh96j7ww6szdyxy86gbf3r"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rsqlite" ,r-rsqlite)
       ("r-dbi" ,r-dbi)
       ("r-biobase" ,r-biobase)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-annotationhub" ,r-annotationhub)
       ("r-rsamtools" ,r-rsamtools)
       ("r-iranges" ,r-iranges)))
    (home-page "http://bioconductor.org/packages/ensembldb")
    (synopsis "Utilities to create and use Ensembl based annotation databases")
    (description "This package provides functions to create and use transcript
centric annotation databases/packages.  The annotation for the databases are
directly fetched from Ensembl using their Perl API.  The functionality and data
is similar to that of the TxDb packages from the GenomicFeatures package, but,
in addition to retrieve all gene/transcript models and annotations from the
database, the ensembldb package provides also a filter framework allowing to
retrieve annotations for specific entries like genes encoded on a chromosome
region or transcript models of lincRNA genes.")
    (license license:lgpl3)))

(define-public r-biovizbase
  (package
    (name "r-biovizbase")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biovizBase" version))
              (sha256
               (base32
                "0f3zwn0g0rc3ld8dd5sgpr14l9aff57j766h4grkiyazyyb46bnh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-scales" ,r-scales)
       ("r-hmisc" ,r-hmisc)
       ("r-dichromat" ,r-dichromat)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)
       ("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-biostrings" ,r-biostrings)
       ("r-rsamtools" ,r-rsamtools)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-ensembldb" ,r-ensembldb)))
    (home-page "http://bioconductor.org/packages/biovizBase")
    (synopsis "Basic graphic utilities for visualization of genomic data")
    (description "This package is designed to provide a set of utilities, color
schemes and conventions for genomic data.  It serves as the base for various
high-level packages for biological data visualization.  This saves development
effort and encourages consistency.")
    (license license:artistic2.0)))

(define-public r-gviz
  (package
    (name "r-gviz")
    (version "1.18.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Gviz" version))
              (sha256
               (base32
                "1dk5cww1w2zqxc7p98d4kgzvy3drsj60va5zlvr14r4wgs5dwqvj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-genomicranges" ,r-genomicranges)
       ("r-xvector" ,r-xvector)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-biomart" ,r-biomart)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-bsgenome" ,r-bsgenome)
       ("r-biostrings" ,r-biostrings)
       ("r-biovizbase" ,r-biovizbase)
       ("r-rsamtools" ,r-rsamtools)
       ("r-latticeextra" ,r-latticeextra)
       ("r-matrixstats" ,r-matrixstats)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-digest" ,r-digest)))
    (home-page "http://bioconductor.org/packages/Gviz")
    (synopsis "Plotting data and annotations along genomic coordinates")
    (description "This package uses the biomaRt and the rtracklayer packages to
perform live annotation queries to Ensembl and UCSC and translates this to e.g.
gene/transcript structures in viewports of the grid graphics package.  This
results in genomic information plotted together with your data.")
    (license license:artistic2.0)))

(define-public r-deseq
  (package
    (name "r-deseq")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq" version))
       (sha256
        (base32
         "18f0400pcmla88kc2prscw0skkf7bww0mnkrj6hhxyy79dhzdy86"))))
    (properties `((upstream-name . "DESeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-locfit" ,r-locfit)
       ("r-lattice" ,r-lattice)
       ("r-genefilter" ,r-genefilter)
       ("r-geneplotter" ,r-geneplotter)
       ("r-rcolorbrewer" , r-rcolorbrewer)))
    (home-page "http://bioconductor.org/packages/DESeq")
    (synopsis "Differential gene expression analysis")
    (description
     "This package provides functions to estimate variance-mean dependence in
count data from high-throughput nucleotide sequencing assays and test for
differential expression based on a model using the negative binomial
distribution.")
    (license license:lgpl3+)))
