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
  #:use-module (gnu packages compression)
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
    (version "2.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DiffBind" version))
       (sha256
        (base32
         "1p0wfq847czn3san9i41wa0kmj759g2g73bfgzwfnjj165n9kc03"))))
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
       ("r-biobase" ,r-biobase)))
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
