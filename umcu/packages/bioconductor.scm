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
     ("r-rcpp" ,r-rcpp)
     ("r-lattice" ,r-lattice)))
  (home-page
    "http://cran.r-project.org/web/packages/Rmisc")
  (synopsis "Rmisc: Ryan Miscellaneous")
  (description "The Rmisc library contains functions for data analysis and
utility operations.")
  (license license:gpl3)))

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

(define-public r-ggrepel
  (package
    (name "r-ggrepel")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggrepel" version))
       (sha256
        (base32
         "0g0qfm6g71rv27423c1x846ipilnj213knyzrcr09vrpxc87l618"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-rcpp" ,r-rcpp)
       ("r-scales" ,r-scales)))
    (home-page "http://github.com/slowkow/ggrepel")
    (synopsis "Repulsive text and label geoms for ggplot2")
    (description "This package provides text and label geoms for ggplot2 that
help to avoid overlapping text labels.  Labels repel away from each other and
away from the data points.")
    (license license:gpl3)))

(define-public r-diffbind
  (package
    (name "r-diffbind")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DiffBind" version))
       (sha256
        (base32
         "1i6s1hxhcw0x7c2mhi297dcq89d0r4j30k83gxg27yaxjswni4b5"))))
    (properties `((upstream-name . "DiffBind")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-amap" ,r-amap)
       ("r-biocparallel" ,r-biocparallel)
       ("r-deseq2" ,r-deseq2)
       ("r-dplyr" ,r-dplyr)
       ("r-edger" ,r-edger)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-ggrepel" ,r-ggrepel)
       ("r-gplots" ,r-gplots)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-limma" ,r-limma)
       ("r-locfit" ,r-locfit)
       ("r-rcolorbrewer" , r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-systempiper" ,r-systempiper)
       ("r-zlibbioc" ,r-zlibbioc)))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RIPSeeker" version))
       (sha256
        (base32
         "0bqkzwrncww7il36273chkd3gfxmii7p566ycki9qij419pwr35y"))))
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
    (synopsis
     "Identifying protein-associated transcripts from RIP-seq experiments")
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
    (version "2.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "multtest" version))
       (sha256
        (base32
         "0n11rd49xl2vn3ldmfips7d3yb70l8npjcqsxyswr9ypjhgzkv9j"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "regioneR" version))
       (sha256
        (base32
         "1vprp3l929hwzmvgskbhawfgnrymwc9n2rxd16rgagnv1dxnjxfp"))))
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
    (version "1.6.18")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "VennDiagram" version))
              (sha256
               (base32
                "05vhsk5ylspa6b919gk9v4rbwm9sc4lsfq0wz308a8dilkg8cqpa"))))
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
    (version "3.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPpeakAnno" version))
       (sha256
        (base32
         "0c4ijvz049ns01g5r75as1r2w9pvymwh09qs1d7sslvm3jap05s0"))))
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
    (home-page "http://bioconductor.org/packages/ChIPpeakAnno")
    (synopsis
     "Batch annotation of peaks identified from ChIP-seq and ChIP-chip experiments")
    (description
     "The package includes functions to retrieve the sequences around the peak,
obtain enriched Gene Ontology (GO) terms, find the nearest gene, exon, miRNA or
custom features such as most conserved elements and other transcription factor
binding sites supplied by users.  Starting 2.0.5, new functions have been added
for finding the peaks with bi-directional promoters with summary statistics
(peaksNearBDP), for summarizing the occurrence of motifs in peaks 
(summarizePatternInPeaks) and for adding other IDs to annotated peaks or
enrichedGO (addGeneIDs). This package leverages the biomaRt, IRanges, Biostrings,
BSgenome, GO.db, multtest and stat packages.")
    (license license:gpl2+)))

(define-public r-marray
  (package
   (name "r-marray")
   (version "1.56.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "marray" version))
            (sha256
             (base32 "14c93i86yc7jn4ax8p4l0z6v9xisw1bv7gzb4a0gbxhxn7mddaic"))))
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
   (version "1.38.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHbase" version))
            (sha256
             (base32 "0fynvcsjdbgp69i0nxrc8ni58rhb1kx9k5r3nb91n9i8s43gjqlm"))))
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
   (version "2.40.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHcall" version))
            (sha256
             (base32 "11pi6awz3858yb4s0z3qf3kcmsdgp6d4aj41g4lfix1sv5amllch"))))
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
   (version "1.14.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "QDNAseq" version))
            (sha256
             (base32 "0lgbv4s0xqgrs7q6ynb3c273sf7pyrp51jnc8ravq1z5g0a2zshy"))))
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

(define-public r-snpstats
  (package
   (name "r-snpstats")
   (version "1.26.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "snpStats" version))
            (sha256
             (base32 "1f8i8pj741h8539lqj508ji27p5ljghyvmdrh3qcfx5jwn9jq8bj"))))
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

(define-public r-funcisnp-data
  (package
   (name "r-funcisnp-data")
   (version "1.12.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://bioconductor.org/packages/release/"
                                "data/experiment/src/contrib/FunciSNP.data_"
                                version ".tar.gz"))
            (sha256
             (base32 "1y37cgqnbzcddg3v2gl4832s0l7ncm7p7p4dyk0kz29bl62j8g2i"))))
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

(define-public r-funcisnp
  (package
   (name "r-funcisnp")
   (version "1.22.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "FunciSNP" version))
            (sha256
             (base32 "1lgvca8npk1092dyl5jhx46ir46b3ab5jgshj5b19dg0bdzi0fj1"))))
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
    (description "This package provides functions for converting, importing,
and drawing PostScript pictures in R plots.")
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
     "Efficient and accurate P-value computation for position weight matrices")
    (description "In putative Transcription Factor Binding Sites (TFBSs)
identification from sequence/alignments, we are interested in the significance
of certain match score.  TFMPvalue provides the accurate calculation of P-value
with score threshold for position weight matrices, or the score with given
P-value.  This package is an interface to code originally made available by
Helene Touzet and Jean-Stephane Varre, 2007, Algorithms Mol Biol:2, 15.")
    (license license:gpl2)))

(define-public r-motifdb
  (package
   (name "r-motifdb")
   (version "1.20.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotifDb" version))
            (sha256
             (base32 "16gk7sbrk188kv3mdsnvcnfzvd2dyxm7wmmwvcqz560x0xn0l0k9"))))
   (properties `((upstream-name . "MotifDb")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biocgenerics" ,r-biocgenerics)
      ("r-s4vectors" ,r-s4vectors)
      ("r-biostrings" ,r-biostrings)
      ("r-iranges" ,r-iranges)
      ("r-rtracklayer" ,r-rtracklayer)))
   (home-page "http://bioconductor.org/packages/MotifDb")
   (synopsis "Annotated collection of protein-DNA binding sequence motifs")
   (description "This package provides more than 2000 annotated position
frequency matrices from nine public sources, for multiple organisms.")
   (license license:artistic2.0)))

(define-public r-motifbreakr
  (package
   (name "r-motifbreakr")
   (version "1.8.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifbreakR" version))
            (sha256
             (base32 "1jfff6jgvrylhnpyc5zq1fah33xq8vf7hypv22vg7js6m8bmcx5j"))))
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
   (home-page "http://bioconductor.org/packages/motifbreakR")
   (synopsis "Predicting disruptiveness of single nucleotide polymorphisms on transcription factor binding sites")
   (description "This package allows biologists to judge in the first place
whether the sequence surrounding the polymorphism is a good match, and in
the second place how much information is gained or lost in one allele of
the polymorphism relative to another.  This package is both flexible and
extensible over previous offerings; giving a choice of algorithms for
interrogation of genomes with motifs from public sources that users can choose
from; these are 1) a weighted-sum probability matrix, 2) log-probabilities,
and 3) weighted by relative entropy.  This package can predict effects for
novel or previously described variants in public databases, making it suitable
for tasks beyond the scope of its original design.  Lastly, it can be used to
interrogate any genome curated within Bioconductor (currently there are 22).")
   (license license:gpl3+)))

(define-public r-rgadem
  (package
   (name "r-rgadem")
   (version "2.26.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "rGADEM" version))
            (sha256
             (base32 "1rbw8k20ri6jhqn9mgkjzyjp7s1z58bgxd5hb35zcpyd7fb2aifw"))))
   (properties `((upstream-name . "rGADEM")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biostrings" ,r-biostrings)
      ("r-iranges" ,r-iranges)
      ("r-bsgenome" ,r-bsgenome)
      ("r-seqlogo" ,r-seqlogo)))
   (home-page "http://bioconductor.org/packages/rGADEM")
   (synopsis "De novo motif discovery")
   (description "This package is an efficient de novo motif discovery tool for
large-scale genomic sequence data.")
   (license license:artistic2.0)))

(define-public r-motiv
  (package
   (name "r-motiv")
   (version "1.34.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotIV" version))
            (sha256
             (base32 "0lvkzbw328mxyp2pd95ymi1nbk78jx880h00jxwdf17a5rkikgh6"))))
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
   (synopsis "Motif Identification and validation")
   (description "This package makes use of STAMP for comparing a set of motifs
to a given database (e.g. JASPAR).  It can also be used to visualize motifs,
motif distributions, modules and filter motifs.")
   (license license:gpl2)))

(define-public r-motifstack
  (package
   (name "r-motifstack")
   (version "1.22.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifStack" version))
            (sha256
             (base32 "0jx2wny3pbfmlwn1ndd7hgka1gzvikad16ncynrxxz3rdfl859pf"))))
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
   (synopsis "Plot stacked logos for DNA, RNA, and amino acid sequence")
   (description "This package is designed for graphic representation of
multiple motifs with different similarity scores.  It works with both DNA/RNA
sequence motif and amino acid sequence motif.  In addition, it provides the
flexibility for users to customize the graphic parameters such as the font type
and symbol colors.")
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
     `((upstream-name . "SNPlocs.Hsapiens.dbSNP144.GRCh37")))
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
    (home-page
     "http://bioconductor.org/packages/SNPlocs.Hsapiens.dbSNP144.GRCh37")
    (synopsis "SNP locations for Homo sapiens (dbSNP Build 144)")
    (description "This package provides SNP locations and alleles for Homo
sapiens extracted from NCBI dbSNP Build 144.  The source data files used for
this package were created by NCBI on May 29-30, 2015, and contain SNPs mapped
to reference genome GRCh37.p13.  WARNING: Note that the GRCh37.p13 genome is a
patched version of GRCh37.  However the patch doesn't alter chromosomes 1-22,
X, Y, MT. GRCh37 itself is the same as the hg19 genome from UCSC *except* for
the mitochondrion chromosome.  Therefore, the SNPs in this package can be
injected in @code{BSgenome.Hsapiens.UCSC.hg19} and they will land at the
correct position but this injection will exclude chrM (i.e. nothing will be
injected in that sequence).")
    (license license:artistic2.0)))

(define-public r-flock
  (package
    (name "r-flock")
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "flock" version))
              (sha256
               (base32
                "1zg93p74icj4bhxnmnssj2xp6vw4yaksyavq03497v33xfpdxss7"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "http://cran.r-project.org/web/packages/flock")
    (synopsis "Process Synchronization Using File Locks")
    (description "Implements synchronization between R processes (spawned by
using the @code{parallel} package for instance) using file locks.  Supports both
exclusive and shared locking.")
    (license license:asl2.0)))
