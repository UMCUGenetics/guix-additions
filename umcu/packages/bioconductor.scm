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
  #:use-module (gnu packages cran)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
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

(define-public r-pastecs
  (package
    (name "r-pastecs")
    (version "1.3-18")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "pastecs" version))
              (sha256
               (base32
                "0ixlnc1psgqgm71bsf5z5j65lvr92ghpsk9f1ifm94dzjhi6d22i"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)))
    (home-page "http://www.sciviews.org/pastecs")
    (synopsis "Analysis of space-time ecological series")
    (description
     "Regulation, decomposition and analysis of space-time series.  The
pastecs library is a PNEC-Art4 and IFREMER initiative to bring PASSTEC 
2000 (http://www.obs-vlfr.fr/~enseigne/anado/passtec/passtec.htm)
functionalities to R.")
    (license license:gpl2+)))

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
    
(define-public r-ensembldb
  (package
    (name "r-ensembldb")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ensembldb" version))
              (sha256
               (base32
                "1np96nry1hba8lk4bg3grf8w3k6xz9lgd2jcl3vrj6wsl184c3fr"))))
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
       ("r-iranges" ,r-iranges)
       ("r-annotationfilter" ,r-annotationfilter)
       ("r-protgenerics" ,r-protgenerics)))
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
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biovizBase" version))
              (sha256
               (base32
                "1pfyhjwlxw9p2q5ip0irxpwndgakvn6z6ay5ahgz2gkkk8x8i29w"))))
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
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Gviz" version))
              (sha256
               (base32
                "161mf1lwqcgl8058xsypbcy48p8jhc93gbg9x375p721ccfdxrps"))))
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

(define-public r-amap
  (package
    (name "r-amap")
    (version "0.8-14")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "amap" version))
              (sha256
               (base32
                "1dz37z9v4zvyvqrs4xvpfv468jwvpxav60qn2w0049bw8llj6xdl"))))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://mulcyber.toulouse.inra.fr/projects/amap/")
    (synopsis "Another multidimensional analysis package")
    (description "Tools for clustering and principal component analysis (with
robust methods, and parallelized functions).")
    (license license:gpl2+)))

(define-public r-diffbind
  (package
    (name "r-diffbind")
    (version "2.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DiffBind" version))
       (sha256
        (base32
         "0w3dwhjkf0sc7bd3m13gwym03j3pyli3xy2y7dqsqn8mhm64bqcy"))))
    (properties `((upstream-name . "DiffBind")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-rcolorbrewer" , r-rcolorbrewer)
       ("r-amap" ,r-amap)
       ("r-edger" ,r-edger)
       ("r-gplots" ,r-gplots)
       ("r-limma" ,r-limma)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-locfit" ,r-locfit)
       ("r-iranges" ,r-iranges)
       ("r-zlibbioc" ,r-zlibbioc)
       ("r-lattice" ,r-lattice)
       ("r-systempiper" ,r-systempiper)
       ("r-rcpp" ,r-rcpp)
       ("r-dplyr" ,r-dplyr)
       ("r-biocparallel" ,r-biocparallel)
       ("r-s4vectors" ,r-s4vectors)
       ("r-rsamtools" ,r-rsamtools)
       ("r-deseq2" ,r-deseq2)))
    (home-page "http://bioconductor.org/packages/DiffBind")
    (synopsis "Differential binding analysis of ChIP-Seq peak data")
    (description
     "This package computes differentially bound sites from multiple
ChIP-seq experiments using affinity (quantitative) data.  Also enables
occupancy (overlap) analysis and plotting functions.")
    (license license:lgpl3+)))

(define-public r-hwriter
  (package
    (name "r-hwriter")
    (version "1.3.2")
    (source (origin
             (method url-fetch)
             (uri (cran-uri "hwriter" version))
             (sha256
              (base32
               "0arjsz854rfkfqhgvpqbm9lfni97dcjs66isdsfvwfd2wz932dbb"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/hwriter")
    (synopsis "HTML Writer - Outputs R objects in HTML format")
    (description "Easy-to-use and versatile functions to output R objects in
HTML format")
    (license license:lgpl2.1)))

(define-public r-dexseq
  (package
   (name "r-dexseq")
   (version "1.22.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "DEXSeq" version))
            (sha256
             (base32 "085aqk1wlzzqcqcqhvz74y099kr2ln5dwdxd3rl6zan806mgwahg"))))
   (properties `((upstream-name . "DEXSeq")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biocparallel" ,r-biocparallel)
      ("r-biobase" ,r-biobase)
      ("r-summarizedexperiment" ,r-summarizedexperiment)
      ("r-iranges" ,r-iranges)
      ("r-genomicranges" ,r-genomicranges)
      ("r-deseq2" ,r-deseq2)
      ("r-annotationdbi" ,r-annotationdbi)
      ("r-rcolorbrewer" ,r-rcolorbrewer)
      ("r-s4vectors" ,r-s4vectors)
      ("r-biocgenerics" ,r-biocgenerics)
      ("r-biomart" ,r-biomart)
      ("r-hwriter" ,r-hwriter)
      ("r-stringr" ,r-stringr)
      ("r-rsamtools" ,r-rsamtools)
      ("r-statmod" ,r-statmod)
      ("r-geneplotter" ,r-geneplotter)
      ("r-genefilter" ,r-genefilter)))
   (home-page "https://bioconductor.org/packages/DEXSeq/")
   (synopsis "Inference of differential exon usage in RNA-Seq")
   (description "The package is focused on finding differential exon usage
using RNA-seq exon counts between samples with different experimental designs.
It provides functions that allows the user to make the necessary statistical
tests based on a model that uses the negative binomial distribution to estimate
the variance between biological replicates and generalized linear models for
testing.  The package also provides functions for the visualization and
exploration of the results.")
   (license license:gpl3+)))

(define-public r-ripseeker
  (package
    (name "r-ripseeker")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RIPSeeker" version))
       (sha256
        (base32
         "1yvn9d4psifkipv1mp42qi5h09a5023cbf7mhw6nmyqpkcw4bwjd"))))
    (properties `((upstream-name . "RIPSeeker")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-genomicranges" ,r-genomicranges)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-rsamtools" ,r-rsamtools)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-rtracklayer" ,r-rtracklayer)))
    (home-page "http://bioconductor.org/packages/RIPSeeker")
    (synopsis "Identifying protein-associated transcripts from RIP-seq
experiments")
    (description
     "This package infers and discriminates RIP peaks from RIP-seq alignments
using two-state HMM with negative binomial emission probability.  While
RIPSeeker is specifically tailored for RIP-seq data analysis, it also provides
a suite of bioinformatics tools integrated within this self-contained software
package comprehensively addressing issues ranging from post-alignments
processing to visualization and annotation.")
    (license license:gpl2+)))

(define-public r-multtest
  (package
    (name "r-multtest")
    (version "2.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "multtest" version))
       (sha256
        (base32
         "0q302f3yf9v7mlq2kib7ynq015d5f94jrsk9drkp5vq0z5j0h3sw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-survival" ,r-survival)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biobase" ,r-biobase)
       ("r-mass" ,r-mass)))
    (home-page "http://bioconductor.org/packages/multtest")
    (synopsis "Resampling-based multiple hypothesis testing")
    (description
     "Non-parametric bootstrap and permutation resampling-based multiple
testing procedures (including empirical Bayes methods) for controlling the
family-wise error rate (FWER), generalized family-wise error rate (gFWER),
tail probability of the proportion of false positives (TPPFP), and false
discovery rate (FDR).  Several choices of bootstrap-based null distribution
are implemented (centered, centered and scaled, quantile-transformed).  
Single-step and step-wise methods are available.  Tests based on a variety of
t- and F-statistics (including t-statistics based on regression parameters
from linear and survival models as well as those based on correlation
parameters) are included.  When probing hypotheses with t-statistics, users
may also select a potentially faster null distribution which is multivariate
normal with mean zero and variance covariance matrix derived from the vector
influence function.  Results are reported in terms of adjusted p-values,
confidence regions and test statistic cutoffs.  The procedures are directly
applicable to identifying differentially expressed genes in DNA microarray
experiments.")
    (license license:lgpl3)))

(define-public r-regioner
  (package
    (name "r-regioner")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "regioneR" version))
       (sha256
        (base32
         "0mvwk2yjsdxda7w6f82dbj91i0zrr95ipglfyw9ndhl2ki8dka0i"))))
    (properties `((upstream-name . "regioneR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-memoise" ,r-memoise)
       ("r-genomicranges" ,r-genomicranges)
       ("r-bsgenome" ,r-bsgenome)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-iranges" ,r-iranges)))
    (home-page "http://bioconductor.org/packages/regioneR")
    (synopsis
     "Association analysis of genomic regions based on permutation tests")
    (description
     "This package offers a statistical framework based on customizable
permutation tests to assess the association between genomic region sets
and other genomic features.")
    (license license:artistic2.0)))

(define-public r-idr
  (package
    (name "r-idr")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "idr" version))
              (sha256
               (base32
                "05nvgw1xdg670bsjjrxkgd1mrdkciccpw4krn0zcgdf2r21dzgwb"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/idr")
    (synopsis "Irreproducible discovery rate")
    (description
     "This is a package for estimating the copula mixture model and plotting
correspondence curves in \"Measuring reproducibility of high-throughput
experiments\" (2011), Annals of Applied Statistics, Vol. 5, No. 3, 1752-1779,
by Li, Brown, Huang, and Bickel")
    (license license:gpl2+)))

(define-public r-venndiagram
  (package
    (name "r-venndiagram")
    (version "1.6.17")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "VennDiagram" version))
              (sha256
               (base32
                "14cahgxm5kq133j5wv6p7ivlmcmym5r39v4dpj69wnq6w9rjqki8"))))
    (properties `((upstream-name . "VennDiagram")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-futile-logger" ,r-futile-logger)))
    (home-page "http://cran.r-project.org/web/packages/VennDiagram")
    (synopsis "Generate High-Resolution Venn and Euler Plots")
    (description
     "This package provides a set of functions to generate high-resolution
Venn and Euler plots.  Includes handling for several special cases, including
two-case scaling, and extensive customization of plot shape and structure.")
    (license license:gpl2+)))

(define-public r-chippeakanno
  (package
    (name "r-chippeakanno")
    (version "3.8.9")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPpeakAnno" version))
       (sha256
        (base32
         "1wykx52xqnz9pcxgfzf5i0ckrw41jg0piwynx1v1ldjxajbdh904"))))
    (properties `((upstream-name . "ChIPpeakAnno")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-go-db" ,r-go-db)
       ("r-biomart" ,r-biomart)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-matrixstats" ,r-matrixstats)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-limma" ,r-limma)
       ("r-multtest" ,r-multtest)
       ("r-rbgl" ,r-rbgl)
       ("r-graph" ,r-graph)
       ("r-biocinstaller" ,r-biocinstaller)
       ("r-regioner" ,r-regioner)
       ("r-dbi" ,r-dbi)
       ("r-ensembldb" ,r-ensembldb)
       ("r-biobase" ,r-biobase)
       ("r-seqinr" ,r-seqinr)
       ("r-idr" ,r-idr)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-rsamtools" ,r-rsamtools)
       ("r-venndiagram" ,r-venndiagram)))
    (home-page "http://bioconductor.org/packages/RIPSeeker")
    (synopsis "Identifying protein-associated transcripts from RIP-seq
experiments")
    (description
     "This package infers and discriminates RIP peaks from RIP-seq alignments
using two-state HMM with negative binomial emission probability.  While
RIPSeeker is specifically tailored for RIP-seq data analysis, it also provides
a suite of bioinformatics tools integrated within this self-contained software
package comprehensively addressing issues ranging from post-alignments
processing to visualization and annotation.")
    (license license:gpl2+)))

(define-public r-marray
  (package
   (name "r-marray")
   (version "1.54.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "marray" version))
            (sha256
             (base32 "1mg7svy1cxv72fqsckqlsm6pv3fy4k1rril3v7g6d60wqfdnm68f"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-limma" ,r-limma)))
   (home-page "http://bioconductor.org/packages/marray")
   (synopsis "Exploratory analysis for two-color spotted microarray data")
   (description "This package contains class definitions for two-color spotted
microarray data.  Fuctions for data input, diagnostic plots, normalization and
quality checking.")
   (license license:lgpl2.0+)))

(define-public r-cghbase
  (package
   (name "r-cghbase")
   (version "1.36.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHbase" version))
            (sha256
             (base32 "1za4128wjhfb45qq9q01y2lpm23zl0pkppfxnw9qai8y3hca1sq7"))))
   (properties `((upstream-name . "CGHbase")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-marray" ,r-marray)))
   (home-page "http://bioconductor.org/packages/CGHbase")
   (synopsis "Base functions and classes for arrayCGH data analysis")
   (description "This package contains functions and classes that are needed by
arrayCGH packages.")
   (license license:gpl2+)))

(define-public r-snowfall
  (package
   (name "r-snowfall")
   (version "1.84-6.1")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "snowfall" version))
            (sha256
             (base32 "13941rlw1jsdjsndp1plzj1cq5aqravizkrqn6l25r9im7rnsi2w"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-snow" ,r-snow)))
   (home-page "http://cran.r-project.org/web/packages/snowfall")
   (synopsis "Easier cluster computing (based on snow).")
   (description "Usability wrapper around snow for easier development of
parallel R programs.  This package offers e.g. extended error checks, and
additional functions.  All functions work in sequential mode, too, if no
cluster is present or wished.  Package is also designed as connector to
the cluster management tool sfCluster, but can also used without it.")  
   (license license:gpl2+)))

(define-public r-cghcall
  (package
   (name "r-cghcall")
   (version "2.36.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHcall" version))
            (sha256
             (base32 "0wnyslj7yn0yc57q60w2y644lmvvvih6rzm55rrad34lz2h39bsz"))))
   (properties `((upstream-name . "CGHcall")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-cghbase" ,r-cghbase)
      ("r-impute" ,r-impute)
      ("r-dnacopy" ,r-dnacopy)
      ("r-snowfall" ,r-snowfall)))
   (home-page "http://bioconductor.org/packages/CGHcall")
   (synopsis "Base functions and classes for arrayCGH data analysis")
   (description "This package contains functions and classes that are needed by
arrayCGH packages.")
   (license license:gpl2+)))

(define-public r-qdnaseq
  (package
   (name "r-qdnaseq")
   (version "1.10.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "QDNAseq" version))
            (sha256
             (base32 "0pa9r9pndk2viv7xcy961c26vx2afpc4rr7dl5zihsgb4y2az8vc"))))
   (properties `((upstream-name . "QDNAseq")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-cghbase" ,r-cghbase)
      ("r-cghcall" ,r-cghcall)
      ("r-dnacopy" ,r-dnacopy)
      ("r-genomicranges" ,r-genomicranges)
      ("r-iranges" ,r-iranges)
      ("r-matrixstats" ,r-matrixstats)
      ("r-r-utils" ,r-r-utils)
      ("r-rsamtools" ,r-rsamtools)))
   (home-page "http://bioconductor.org/packages/QDNAseq")
   (synopsis "Quantitative DNA sequencing for chromosomal aberrations")
   (description "The genome is divided into non-overlapping fixed-sized bins,
number of sequence reads in each counted, adjusted with a simultaneous
two-dimensional loess correction for sequence mappability and GC content, and
filtered to remove spurious regions in the genome.  Downstream steps of
segmentation and calling are also implemented via packages DNAcopy and CGHcall,
respectively.")
   (license license:gpl2+)))

(define-public r-reshape
  (package
   (name "r-reshape")
   (version "0.8.6")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "reshape" version))
            (sha256
             (base32
              "1f1ngalc22knhdm9djv1m6abnjqpv1frdzxfkpakhph2l67bk7fq"))))
  (build-system r-build-system)
  (propagated-inputs
   `(("r-plyr" ,r-plyr)
     ("r-rcpp" ,r-rcpp)))
  (home-page "http://had.co.nz/reshape")
  (synopsis "Flexibly Reshape Data")
  (description "Flexibly restructure and aggregate data using just two
functions: melt and cast.")
  (license license:expat)))

(define-public r-snpstats
  (package
   (name "r-snpstats")
   (version "1.24.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "snpStats" version))
            (sha256
             (base32 "1hkgjd12cz6rfm7w51dgc7cxvrsrqgc2byfinbp0d42rnc3600n2"))))
   (properties `((upstream-name . "Snpstats")))
   (build-system r-build-system)
   (inputs
    `(("zlib" ,zlib)))
   (propagated-inputs
    `(("r-survival" ,r-survival)
      ("r-matrix" ,r-matrix)
      ("r-biocgenerics" ,r-biocgenerics)
      ("r-zlibbioc" ,r-zlibbioc)))
   (home-page "http://bioconductor.org/packages/snpStats")
   (synopsis "SnpMatrix and XSnpMatrix classes and methods")
   (description "This package provides classes and statistical methods for
large SNP association studies.  This extends the earlier snpMatrix package,
allowing for uncertainty in genotypes.")
   (license license:gpl3+)))

(define-public r-funcisnp
  (package
   (name "r-funcisnp")
   (version "1.18.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "FunciSNP" version))
            (sha256
             (base32 "03wzrrdhnp6svf4vy907h6bbnpfgrbqhqabsah37qzld15qm2xs2"))))
   (properties `((upstream-name . "FunciSNP")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-ggplot2" ,r-ggplot2)
      ("r-txdb-hsapiens-ucsc-hg19-knowngene" ,r-txdb-hsapiens-ucsc-hg19-knowngene)
      ("r-funcisnp-data" ,r-funcisnp-data)
      ("r-biocgenerics" ,r-biocgenerics)
      ("r-biobase" ,r-biobase)
      ("r-s4vectors" ,r-s4vectors)
      ("r-iranges" ,r-iranges)
      ("r-genomicranges" ,r-genomicranges)
      ("r-rsamtools" ,r-rsamtools)
      ("r-rtracklayer" ,r-rtracklayer)
      ("r-chippeakanno" ,r-chippeakanno)
      ("r-variantannotation" ,r-variantannotation)
      ("r-plyr" ,r-plyr)
      ("r-snpstats" ,r-snpstats)
      ("r-reshape" ,r-reshape)
      ("r-scales" ,r-scales)))
   (home-page "http://bioconductor.org/packages/FunciSNP")
   (synopsis "Identification of candidate regulatory SNPs")
   (description "This package integrates information from GWAS, 1000genomes
and chromatin feature to identify functional SNP in coding or non-coding
regions.")
   (license license:gpl3+)))

(define-public r-funcisnp-data
  (package
   (name "r-funcisnp-data")
   (version "1.10.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://bioconductor.org/packages/release/"
                                "data/experiment/src/contrib/FunciSNP.data_"
                                version ".tar.gz"))
            (sha256
             (base32 "0r0iv8hp25ld9vpyij9lrk0xphaxcz4j4panyc83i18d6s9jw51s"))))
   (properties `((upstream-name . "FunciSNP.data")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-iranges" ,r-iranges)
      ("r-rtracklayer" ,r-rtracklayer)))
   (home-page "http://bioconductor.org/packages/FunciSNP.data")
   (synopsis "Various data sets for use with the FunciSNP package")
   (description "This package provides data sets needed for FunciSNP to
integrate information from GWAS, 1000genomes and chromatin feature, in
order to identify functional SNP in coding or non-coding regions.")
   (license license:gpl3+)))

(define-public r-ggdendro
  (package
    (name "r-ggdendro")
    (version "0.1-20")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggdendro" version))
              (sha256
               (base32
                "1zzq1hxd0d1qa5hrzwfkdw6fzscpcafbwbpkrb62dm559y8awp0j"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-mass" ,r-mass)
       ("r-knitr" ,r-knitr)))
    (home-page "https://github.com/andrie/ggdendro")
    (synopsis "Create Dendrograms and Tree Diagrams Using 'ggplot2'")
    (description "This is a set of tools for dendrograms and tree plots using
'ggplot2'.  The 'ggplot2' philosophy is to clearly separate data from the
presentation.  Unfortunately the plot method for dendrograms plots directly
to a plot device with out exposing the data.  The 'ggdendro' package resolves
this by making available functions that extract the dendrogram plot data.
The package provides implementations for tree, rpart, as well as diana and
agnes cluster diagrams.")    
    (license license:gpl2+)))

(define-public r-pasilla
  (package
    (name "r-pasilla")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://bioconductor.org/packages/release/data/experiment"
                    "/src/contrib/pasilla_" version ".tar.gz"))
              (sha256
               (base32
                "0nz7s5sdd58bml8bb0c7c2vp8f0pxjl67kijaryncnqq3d2klc1l"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dexseq" ,r-dexseq)
       ("r-biocstyle" ,r-biocstyle)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-knitr" ,r-knitr)))
    (home-page "http://bioconductor.org/packages/pasilla/")
    (synopsis "Data package with per-exon and per-gene read counts")
    (description "This package provides per-exon and per-gene read counts
computed for selected genes from RNA-seq data that were presented in the
article 'Conservation of an RNA regulatory map between Drosophila and mammals'
by Brooks et al., Genome Research 2011.")
    (license license:lgpl2.1+)))

(define-public r-rlang
  (package
   (name "r-rlang")
   (version "0.1.1")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "rlang" version))
            (sha256
             (base32
              "0grwqy3zkvz96mvpwfbfyqid4jkfrqh3ldy2n6dpv2kjd1fzj0ar"))))
   (build-system r-build-system)
   (home-page "http://rlang.tidyverse.org")
   (synopsis "Functions for base types, core R and 'Tidyverse' features")
   (description "This package provides a toolbox for working with base types,
core R features like the condition system, and core 'Tidyverse' features like
tidy evaluation.")
   (license license:gpl3+)))

(define-public r-blob
  (package
   (name "r-blob")
   (version "1.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "blob" version))
     (sha256
      (base32
       "05pazzcyz3c3vd2l70zq9cf172cgjff4dnf419zigfnxycyn1mhn"))))
   (build-system r-build-system)
   (propagated-inputs `(("r-tibble" ,r-tibble)))
   (home-page "https://github.com/hadley/blob")
   (synopsis "Simple S3 class for representing vectors of binary data")
   (description "R's raw vector is useful for storing a single binary
object.  What if you want to put a vector of them in a data frame?  The
blob package provides the blob object, a list of raw vectors, suitable
for use as a column in data frame.")
   (license license:gpl3+)))

(define-public r-grimport
  (package
    (name "r-grimport")
    (version "0.9-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "grImport" version))
       (sha256
        (base32
         "1d8fd7502qj7cirjqdkr1qj51rylw2fz5hs06avfvc2dxs2xwfw1"))))
    (properties `((upstream-name . "grImport")))
    (build-system r-build-system)
    (inputs
     `(("ghostscript" ,ghostscript)))
    (propagated-inputs
     `(("r-xml" ,r-xml)))
    (home-page "http://cran.r-project.org/web/packages/grImport")
    (synopsis "Importing Vector Graphics")
    (description "Functions for converting, importing, and drawing PostScript
pictures in R plots.")
    (license license:gpl2+)))

(define-public r-tfmpvalue
  (package
    (name "r-tfmpvalue")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TFMPvalue" version))
       (sha256
        (base32
         "1892jmgqywm0jp5l5k88brp4h8szkbi9bxi0v1jni1929qnsmqyf"))))
    (properties
     `((upstream-name . "TFMPvalue")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/ge11232002/TFMPvalue")
    (synopsis
     "Efficient and Accurate P-Value Computation for Position Weight Matrices")
    (description "In putative Transcription Factor Binding Sites (TFBSs)
identification from sequence/alignments, we are interested in the significance
of certain match score.  TFMPvalue provides the accurate calculation of P-value
with score threshold for Position Weight Matrices, or the score with given
P-value.  This package is an interface to code originally made available by
Helene Touzet and Jean-Stephane Varre, 2007, Algorithms Mol Biol:2, 15.")
    (license license:gpl2)))

(define-public r-motifdb
  (package
   (name "r-motifdb")
   (version "1.18.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotifDb" version))
            (sha256
             (base32 "0vnigpb23vnc8jfg2smz9bqr0kkfvksyckwpg5glc4pmvpygjh4k"))))
   (properties `((upstream-name . "MotifDb")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biocgenerics" ,r-biocgenerics)
      ("r-s4vectors" ,r-s4vectors)
      ("r-biostrings" ,r-biostrings)
      ("r-iranges" ,r-iranges)
      ("r-rtracklayer" ,r-rtracklayer)))
   (home-page "http://bioconductor.org/packages/MotifDb/")
   (synopsis "")
   (description "")
   (license license:artistic2.0)))

(define-public r-motifbreakr
  (package
   (name "r-motifbreakr")
   (version "1.6.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifbreakR" version))
            (sha256
             (base32 "1q14kjrbfvsqdllfqxdgibdv33jzal4yn0dzvr38jz0xwd00yvqv"))))
   (properties `((upstream-name . "motifbreakR")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-grimport" ,r-grimport)
      ("r-stringr" ,r-stringr)
      ("r-biocgenerics" ,r-biocgenerics)
      ("r-s4vectors" ,r-s4vectors)
      ("r-iranges" ,r-iranges)
      ("r-genomeinfodb" ,r-genomeinfodb)
      ("r-genomicranges" ,r-genomicranges)
      ("r-biostrings" ,r-biostrings)
      ("r-bsgenome" ,r-bsgenome)
      ("r-rtracklayer" ,r-rtracklayer)
      ("r-variantannotation" ,r-variantannotation)
      ("r-biocparallel" ,r-biocparallel)
      ("r-motifstack" ,r-motifstack)
      ("r-gviz" ,r-gviz)
      ("r-matrixstats" ,r-matrixstats)
      ("r-tfmpvalue" ,r-tfmpvalue)
      ("r-motifdb" ,r-motifdb)))
   (home-page "http://bioconductor.org/packages/snpStats")
   (synopsis "SnpMatrix and XSnpMatrix classes and methods")
   (description "This package provides classes and statistical methods for
large SNP association studies.  This extends the earlier snpMatrix package,
allowing for uncertainty in genotypes.")
   (license license:gpl3+)))

(define-public r-rgadem
  (package
   (name "r-rgadem")
   (version "2.24.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "rGADEM" version))
            (sha256
             (base32 "0wxmmcmcj9jn90w4z1mxafwx9gj92akn47m79wqc1v8x4qcqkb2p"))))
   (properties `((upstream-name . "rGADEM")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biostrings" ,r-biostrings)
      ("r-iranges" ,r-iranges)
      ("r-bsgenome" ,r-bsgenome)
      ("r-seqlogo" ,r-seqlogo)))
   (home-page "http://bioconductor.org/packages/rGADEM/")
   (synopsis "")
   (description "")
   (license license:artistic2.0)))

(define-public r-motiv
  (package
   (name "r-motiv")
   (version "1.32.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotIV" version))
            (sha256
             (base32 "1j3ffvr1bgd6a3r7yiv57mfbyy8c7af037s4xicfmakf6f1f4bw9"))))
   (properties `((upstream-name . "MotIV")))
   (build-system r-build-system)
   (inputs
    `(("gsl" ,gsl)))
   (propagated-inputs
    `(("r-s4vectors" ,r-s4vectors)
      ("r-iranges" ,r-iranges)
      ("r-biostrings" ,r-biostrings)
      ("r-rgadem" ,r-rgadem)
      ("r-lattice" ,r-lattice)))
   (home-page "http://bioconductor.org/packages/MotIV/")
   (synopsis "")
   (description "")
   (license license:gpl2)))

(define-public r-motifstack
  (package
   (name "r-motifstack")
   (version "1.20.1")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifStack" version))
            (sha256
             (base32 "1kv9qbzqa0xiaal43xlfgab3c8v81czrr7m29nv6pmv34csllgs1"))))
   (properties `((upstream-name . "motifStack")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-grimport" ,r-grimport)
      ("r-motiv" ,r-motiv)
      ("r-ade4" ,r-ade4)
      ("r-scales" ,r-scales)
      ("r-htmlwidgets" ,r-htmlwidgets)
      ("r-xml" ,r-xml)
      ("r-biostrings" ,r-biostrings)))
   (home-page "http://bioconductor.org/packages/motifStack/")
   (synopsis "")
   (description "")
   (license license:gpl2+)))

(define-public r-snplocs.hsapiens.dbsnp144.grch37
  (package
    (name "r-snplocs.hsapiens.dbsnp144.grch37")
    (version "0.99.20")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "http://bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib"
                                  "/SNPlocs.Hsapiens.dbSNP144.GRCh37_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1z8kx43ki1jvj7ms7pcybakcdimfwr6zpjvspkjmma97bdz093iz"))))
    (properties
     `((upstream-name . "TxDb.Hsapiens.UCSC.hg19.knownGene")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-bsgenome" ,r-bsgenome)
       ("r-biostrings" ,r-biostrings)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:artistic2.0)))
