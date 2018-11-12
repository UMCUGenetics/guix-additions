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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

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

(define-public r-gviz
  (package
    (name "r-gviz")
    (version "1.22.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Gviz" version))
              (sha256
               (base32
                "182f88s18vx7wkd6s4sy4bvrkn81733ha9pfiifya92c2ipb6jan"))))
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
    (version "3.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPpeakAnno" version))
       (sha256
        (base32
         "0mnz79kmkmjxx88rxranfri6y6q7zy823ljs1gljz6iz4gvgdj02"))))
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

(define-public r-funcisnp-data
  (package
   (name "r-funcisnp-data")
   (version "1.14.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://bioconductor.org/packages/release/"
                                "data/experiment/src/contrib/FunciSNP.data_"
                                version ".tar.gz"))
            (sha256
             (base32 "0zsffyhd9nxsg3a34sd2mycbvs8dsw96pqn1jj7zw5whldjihjwh"))))
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

(define-public r-pasilla
  (package
    (name "r-pasilla")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://bioconductor.org/packages/release/data/experiment"
                    "/src/contrib/pasilla_" version ".tar.gz"))
              (sha256
               (base32
                "0h6ll2csja5vnmylazb1qklz7ybk5hn0nxgkvxhhsmyqk9w7gk6m"))))
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

(define-public r-splitstackshape
  (package
   (name "r-splitstackshape")
   (version "1.4.2")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "splitstackshape" version))
     (sha256
      (base32
       "0m9karfh0pcy0jj3dzq87vybxv9gmcrq5m2k7byxpki95apbrsmg"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-data-table" ,r-data-table)))
   (home-page "http://github.com/mrdwab/splitstackshape")
   (synopsis "Stack and Reshape Datasets After Splitting Concatenated Values")
   (description "Online data collection tools like Google Forms often export
multiple-response questions with data concatenated in cells.  The concat.split
(cSplit) family of functions splits such data into separate cells.  The package
also includes functions to stack groups of columns and to reshape wide data,
even when the data are \"unbalanced\".")
   (license license:gpl3)))

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
      ("r-rtracklayer" ,r-rtracklayer)
      ("r-splitstackshape" ,r-splitstackshape)))
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

(define r-spp-custom
  (package
    (name "r-spp-custom")
    (version "1.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/kundajelab/phantompeakqualtools/raw/master/spp_"
                    version ".tar.gz"))
              (sha256
               (base32 "02sj0482ph0sn9lpmxcmldsrj3sph70r4jp5k0idgbl27qbfcfyh"))))
    (build-system r-build-system)
    (inputs
     `(("boost" ,boost)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-catools" ,r-catools)
       ("r-rsamtools" ,r-rsamtools)))
    (home-page "https://github.com/kundajelab/phantompeakqualtools")
    (synopsis "")
    (description "")
    (license #f)))

(define-public r-phantompeakqualtools
  (package
    (name "r-phantompeakqualtools")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/kundajelab/phantompeakqualtools/"
                    "archive/" version ".tar.gz"))
              (sha256
               (base32
                "0s0nk9y7jb0gm1kvbcxn5n6aiavwlkc779myrnqm1wb51flcrjw6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((script (string-append (assoc-ref outputs "out")
                                          "/share/scripts")))
               (install-file "run_spp.R" script)))))))
    (propagated-inputs
     `(("r-catools" ,r-catools)
       ("r-snow" ,r-snow)
       ("r-snowfall" ,r-snowfall)
       ("r-bitops" ,r-bitops)
       ("r-rsamtools" ,r-rsamtools)
       ("r-spp-custom" ,r-spp-custom)
       ("gawk" ,gawk)
       ("samtools" ,samtools)
       ("boost" ,boost)
       ("gzip" ,gzip)))
    (home-page "https://github.com/kundajelab/phantompeakqualtools")
    (synopsis "")
    (description "")
    (license #f)))

(define-public r-fda
  (package
   (name "r-fda")
   (version "2.4.7")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "fda" version))
            (sha256
             (base32
              "0371c32kxxvfdh18ldgc2p76dr2436ay0n9prdjwm9v0azp4rp9k"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-matrix" ,r-matrix)))
   (home-page "http://www.functionaldata.org")
   (synopsis "Functional Data Analysis")
   (description
    "These functions were developed to support functional data analysis as
described in Ramsay, J.  O.  and Silverman, B.  W. (2005) Functional Data
Analysis.  New York: Springer.  They were ported from earlier versions in
Matlab and S-PLUS.  An introduction appears in Ramsay, J.  O., Hooker,
Giles, and Graves, Spencer (2009) Functional Data Analysis with R and
Matlab (Springer).  The package includes data sets and script files working
many examples including all but one of the 76 figures in this latter book.
Matlab versions of the code and sample analyses are no longer distributed
through CRAN, as they were when the book was published.  For those, ftp
from <http://www.psych.mcgill.ca/misc/fda/downloads/FDAfuns/> There you find
a set of .zip files containing the functions and sample analyses, as well as
two .txt files giving instructions for installation and some additional
information.  The changes from Version 2.4.1 are fixes of bugs in density.fd
and removal of functions create.polynomial.basis, polynompen, and polynomial.
These were deleted because the monomial basis does the same thing and because
there were errors in the code.")
   (license license:gpl2+)))

(define-public r-lsd
  (package
   (name "r-lsd")
   (version "3.0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "LSD" version))
            (sha256
             (base32
              "069p33aw6iwikp82b7b8wa77wlyjqwr4hcwvrgaxgwqdgn6jjg3k"))))
   (properties `((upstream-name . "LSD")))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/LSD")
   (synopsis "Lots of Superior Depictions")
   (description "Create lots of colorful plots in a plethora of variations
(try the LSD demotour())")
   ;; License: "unlimited" -- whatever that means.
   (license #f)))

(define-public r-fourcseq
  (package
   (name "r-fourcseq")
   (version "1.12.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "FourCSeq" version))
            (sha256
             (base32 "08fq289m8g7fgmsrph1gq45q6zpfnrzpm0n4qyf2vc5hsdgnm4qh"))))
   (properties `((upstream-name . "FourCSeq")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-deseq2" ,r-deseq2)
      ("r-biobase" ,r-biobase)
      ("r-biostrings" ,r-biostrings)
      ("r-genomicranges" ,r-genomicranges)
      ("r-summarizedexperiment" ,r-summarizedexperiment)
      ("r-rsamtools" ,r-rsamtools)
      ("r-ggbio" ,r-ggbio)
      ("r-reshape2" ,r-reshape2)
      ("r-rtracklayer" ,r-rtracklayer)
      ("r-fda" ,r-fda)
      ("r-genomicalignments" ,r-genomicalignments)
      ("r-gtools" ,r-gtools)
      ("r-matrix" ,r-matrix)
      ("r-lsd" ,r-lsd)
      ("r-ggplot2" ,r-ggplot2)))
   (home-page "http://bioconductor.org/packages/FourCSeq/")
   (synopsis "Package analyse 4C sequencing data")
   (description "FourCSeq is an R package dedicated to the analysis of (multiplexed) 4C sequencing data. The package provides a pipeline to detect specific interactions between DNA elements and identify differential interactions between conditions. The statistical analysis in R starts with individual bam files for each sample as inputs. To obtain these files, the package contains a python script (extdata/python/demultiplex.py) to demultiplex libraries and trim off primer sequences. With a standard alignment software the required bam files can be then be generated.")
   (license license:gpl3+)))

(define-public r-txdb-dmelanogaster-ucsc-dm3-ensgene
  (package
    (name "r-txdb-dmelanogaster-ucsc-dm3-ensgene")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "http://bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib"
                                  "/TxDb.Dmelanogaster.UCSC.dm3.ensGene_"
                                  "3.2.2.tar.gz"))
              (sha256
               (base32
                "1337x23rdmiiza83ms225kri37h16q5hw1lw0m577abcgip3d7c7"))))
    (properties
     `((upstream-name . "TxDb.Hsapiens.UCSC.hg19.knownGene")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-genomicfeatures" ,r-genomicfeatures)))
    (home-page
     "http://bioconductor.org/packages/TxDb.Dmelanogaster.UCSC.dm3.ensGene/")
    (synopsis "Annotation package for Dmelanogaster in TxDb format")
    (description
     "This package provides an annotation databases generated from UCSC by
exposing these as @code{TxDb} objects.")
    (license license:artistic2.0)))

(define-public r-reordercluster
  (package
   (name "r-reordercluster")
   (version "1.0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "ReorderCluster" version))
            (sha256
             (base32
              "0ss750frzvj0bm1w7zblmcsjpszhnbffwlkaw31sm003lbx9hy58"))))
   (properties `((upstream-name . "ReorderCluster")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-gplots" ,r-gplots)
      ("r-rcpp" ,r-rcpp)))
   (home-page "http://cran.r-project.org/web/packages/ReorderCluster")
   (synopsis "Reordering the dendrogram according to the class labels")
   (description "Tools for performing the leaf reordering for the dendrogram
that preserves the hierarchical clustering result and at the same time tries
to group instances from the same class together.")
   (license license:gpl3+)))

(define-public r-aneufinderdata
  (package
   (name "r-aneufinderdata")
   (version "1.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://bioconductor.org/packages"
                                "/release/data/experiment/src/contrib/"
                                "AneuFinderData_" version ".tar.gz"))
            (sha256
             (base32
              "17snbhlvb64mqcq26kawd92wpckjzxbl78chp87hrwsfhf7my6hm"))))
   (build-system r-build-system)
   (home-page "http://bioconductor.org/packages/AneuFinderData")
   (synopsis "Data package for AneuFinder")
   (description "This package contains data used by AneuFinder.")
   (license license:artistic2.0)))

(define-public r-ecp
  (package
    (name "r-ecp")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ecp" version))
              (sha256
               (base32
                "0siq5acpy4wmqzm6zvpqj1m8a05hgk5jhb1clv06y40kqk5kjrap"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "http://cran.r-project.org/web/packages/ecp")
    (synopsis "Non-Parametric Multiple Change-Point Analysis of Multivariate Data")
    (description
     "Implements various procedures for finding multiple change-points.  Two
methods make use of dynamic programming and pruning, with no distributional
assumptions other than the existence of certain absolute moments in one method.
Hierarchical and exact search methods are included.  All methods return the set
of estimated change- points as well as other summary information.")
    (license license:gpl2+)))

(define-public r-aneufinder
  (package
    (name "r-aneufinder")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AneuFinder" version))
              (sha256
               (base32
                "07xdqal5i0qpcgr4hqdfq0hpzj7kg0wvjcngidqq9k0zci8na8gh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicranges" ,r-genomicranges)
       ("r-aneufinderdata" ,r-aneufinderdata)
       ("r-ecp" ,r-ecp)
       ("r-foreach" ,r-foreach)
       ("r-doparallel" ,r-doparallel)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-s4vectors" ,r-s4vectors)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-bamsignals" ,r-bamsignals)
       ("r-dnacopy" ,r-dnacopy)
       ("r-biostrings" ,r-biostrings)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-ggplot2" ,r-ggplot2)
       ("r-reshape2" ,r-reshape2)
       ("r-ggdendro" ,r-ggdendro)
       ("r-ggrepel" ,r-ggrepel)
       ("r-reordercluster" ,r-reordercluster)
       ("r-mclust" ,r-mclust)
       ("r-cowplot" ,r-cowplot)))
    (home-page "http://bioconductor.org/packages/AneuFinder")
    (synopsis "Copy number variation analysis in single-cell-sequencing data")
    (description "This package implements functions for copy number variant
calling, plotting, export and analysis from whole-genome single cell
sequencing data.")
    (license license:artistic2.0)))

(define-public r-bsgenome-btaurus-ucsc-bostau8
  (package
    (name "r-bsgenome-btaurus-ucsc-bostau8")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "http://bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib"
                                  "/BSgenome.Btaurus.UCSC.bosTau8_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "16wjy1aw9nvx03r7w8yh5w7sw3pn8i9nczd0n0728l6nnyqxlsz6"))))
    (properties
     `((upstream-name . "SNPlocs.Hsapiens.dbSNP144.GRCh37")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs `(("r-bsgenome" ,r-bsgenome)))
    (home-page "http://bioconductor.org/packages/BSgenome.Btaurus.UCSC.bosTau8")
    (synopsis "Full genome sequences for Bos taurus (UCSC version bosTau8)")
    (description "This package provides the full genome sequences for Bos
taurus (UCSC version bosTau8).")
    (license license:artistic2.0)))

(define-public r-ideoviz
  (package
    (name "r-ideoviz")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IdeoViz" version))
              (sha256
               (base32
                "1ihfxp4dngwk72nxw36w15d28cpz4y0qxagap4km76i51c9jm855"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-iranges" ,r-iranges)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-genomeinfodb" ,r-genomeinfodb)))
    (home-page "http://bioconductor.org/packages/IdeoViz")
    (synopsis "Plots data (continuous/discrete) along chromosomal ideogram")
    (description "This package provides functions to plot data associated with
arbitrary genomic intervals along chromosomal ideogram.")
    (license license:artistic2.0)))

(define-public r-readxl
  (package
   (name "r-readxl")
   (version "1.0.0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "readxl" version))
            (sha256
             (base32
              "1bf7gxw9r11m4llyymplxiaa4gzgyj4bwmwad5in756pzq3jzmpv"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-cellranger" ,r-cellranger)
      ("r-rcpp" ,r-rcpp)
      ("r-tibble" ,r-tibble)))
   (home-page "http://readxl.tidyverse.org")
   (synopsis "Read excel files")
   (description "Import excel files into R.  Supports '.xls' via the embedded
'libxls' C library <https://sourceforge.net/projects/libxls/> and '.xlsx' via
the embedded 'RapidXML' C++ library <https://rapidxml.sourceforge.net>.  Works
on Windows, Mac and Linux without external dependencies.")
   (license license:gpl3)))

(define-public r-polynom
  (package
   (name "r-polynom")
   (version "1.3-9")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "polynom" version))
     (sha256
      (base32
       "1s4xxv5rvpigawknvq27v9vzvs83phfsj5h8mim2lmf5bj950nnk"))))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/polynom")
   (synopsis "Functions for univariate polynomial manipulations")
   (description "This package provides a collection of functions to implement a
class for univariate polynomial manipulations.")
   (license license:gpl2)))

(define-public r-ggpmisc
  (package
   (name "r-ggpmisc")
   (version "0.2.16")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "ggpmisc" version))
     (sha256
      (base32
       "1hq5zpc3zf4hq49ykrzrgj92cmh6gjsq6kpzcvzvnm5wyh9yy2iq"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-broom" ,r-broom)
      ("r-dplyr" ,r-dplyr)
      ("r-ggplot2" ,r-ggplot2)
      ("r-lubridate" ,r-lubridate)
      ("r-mass" ,r-mass)
      ("r-plyr" ,r-plyr)
      ("r-polynom" ,r-polynom)
      ("r-splus2r" ,r-splus2r)
      ("r-tibble" ,r-tibble)
      ("r-xts" ,r-xts)
      ("r-zoo" ,r-zoo)))
   (home-page "http://www.r4photobiology.info")
   (synopsis "Miscellaneous Extensions to @code{ggplot2}")
   (description "This package provides extensions to @code{ggplot2} respecting
the grammar of graphics paradigm.  Provides new statistics to locate and tag
peaks and valleys in 2D plots, a statistics to add a label with the equation
of a polynomial fitted with lm(), or R^2 or adjusted R^2 or information
criteria for any model fitted with function lm().  Additional statistics give
access to functions in package 'broom'.  Provides a function for flexibly
converting time series to data frames suitable for plotting with 
@code{ggplot()}.  In addition provides statistics and ggplot geometries useful
for diagnosing what data are passed to compute_group() and 
@code{compute_panel()} functions and to geometries.")
   (license license:gpl2+)))

(define-public r-repr
  (package
   (name "r-repr")
   (version "0.12.0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "repr" version))
            (sha256
             (base32
              "1p6a2ryb5iaf4i6nn1iav26bh83wmvncwpk25hyrzd5rxich1bq3"))))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/repr")
   (synopsis "Serializable representations")
   (description "String and binary representations of objects for several
formats/mime types.")
   (license license:gpl3)))

(define-public r-irdisplay
  (package
  (name "r-irdisplay")
  (version "0.4.4")
  (source (origin
           (method url-fetch)
           (uri (cran-uri "IRdisplay" version))
           (sha256
            (base32
             "19l4flvik8zw2pany8dpjbrh0bji6bag6pmclgwqnq80532hnfp8"))))
  (properties
   `((upstream-name . "IRdisplay")))
  (build-system r-build-system)
  (propagated-inputs
   `(("r-repr" ,r-repr)))
  (home-page "http://cran.r-project.org/web/packages/IRdisplay")
  (synopsis "Jupyter display machinery")
  (description "An interface to the rich display capabilities of 'Jupyter'
front-ends.  Designed to be used from a running 'IRkernel'")
  (license license:expat)))

(define-public r-pbdzmq
  (package
   (name "r-pbdzmq")
   (version "0.3-2")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "pbdZMQ" version))
            (sha256
             (base32
              "0dzwwffinn9bbb73dmmh88c374f9057bl0a8dq97fbv63j4a5qpc"))))
   (properties `((upstream-name . "pbdZMQ")))
   (build-system r-build-system)
   (inputs
    `(("zlib" ,zlib)
      ("zeromq" ,zeromq)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
   (home-page "http://r-pbd.org/")
   (synopsis
    "Programming with Big Data -- Interface to 'ZeroMQ'")
   (description
    "@code{ZeroMQ} is a well-known library for high-performance asynchronous
messaging in scalable, distributed applications.  This package provides high
level R wrapper functions to easily utilize 'ZeroMQ'.  We mainly focus on
interactive client/server programming frameworks.")
   (license license:gpl3)))

(define-public irkernel
  (package
   (name "irkernel")
   (version "0.8.11")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/IRkernel/IRkernel/archive/"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "0qf4ra3r772xq7l52nch51d4alywbp946y3hmdzpzrysbr1prs8m"))))
   (build-system r-build-system)
   ;; TODO: Registering the kernel so that jupyter automatically detects it doesn't work.
   ;; (arguments
   ;;  `(#:phases
   ;;    (modify-phases %standard-phases
   ;;      (add-after 'install 'register-kernel
   ;;        (lambda _
   ;;          (with-output-to-file "register-kernel.R"
   ;;            (lambda _
   ;;              (format #t "library(\"IRkernel\")~%IRkernel::installspec(user = FALSE)~%")))
   ;;          (system "Rscript register-kernel.R"))))))
   (inputs
    `(("python-jupyter-client" ,python-jupyter-client)))
   (propagated-inputs
    `(("r-repr" ,r-repr)
      ("r-evaluate" ,r-evaluate)
      ("r-irdisplay" ,r-irdisplay)
      ("r-pbdzmq" ,r-pbdzmq)
      ("r-crayon" ,r-crayon)
      ("r-jsonlite" ,r-jsonlite)
      ("r-uuid" ,r-uuid)
      ("r-digest" ,r-digest)))
   (home-page "https://github.com/IRkernel/IRkernel")
   (synopsis "Native R kernel for Jupyter notebooks")
   (description "The R kernel for the Jupyter environment executes R code which
the front-end submits to the kernel via the network.")
   (license license:expat)))

(define-public r-ggsignif
  (package
  (name "r-ggsignif")
  (version "0.4.0")
  (source (origin
           (method url-fetch)
           (uri (cran-uri "ggsignif" version))
           (sha256
            (base32
             "1rn58d7pb3axk6chiihryykrzw76adaa2yiafq4d0j6qbhax78f7"))))
  (build-system r-build-system)
  (propagated-inputs `(("r-ggplot2" ,r-ggplot2)))
  (home-page "https://github.com/const-ae/ggsignif")
  (synopsis "Significance brackets for ggplot2")
  (description
    "Enrich your ggplots with group-wise comparisons.  This package provides an
easy way to indicate if two groups are significantly different.  Commonly this
is shown by a bracket on top connecting the groups of interest which itself is
annotated with the level of significance (NS, *, **, ***).  The package provides
a single layer (geom_signif()) that takes the groups for comparison and the test
(@code{t.test()}, @code{wilcox.text()} etc.) as arguments and adds the
annotation to the plot.")
  (license license:gpl3)))

(define-public r-coverageview
  (package
    (name "r-coverageview")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "CoverageView" version))
              (sha256
               (base32
                "1091mxiclb3zhr48bdj7z8gi3v69l4w25cfpfbxhhp70rffn9xfr"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-s4vectors" ,r-s4vectors)
       ("r-iranges" ,r-iranges)
       ("r-genomicranges" ,r-genomicranges)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rsamtools" ,r-rsamtools)))
    (home-page "http://bioconductor.org/packages/CoverageView")
    (synopsis "Coverage visualization package for R")
    (description "This package provides a framework for the visualization of
genome coverage profiles.  It can be used for ChIP-seq experiments, but it can
be also used for genome-wide nucleosome positioning experiments or other
experiment types where it is important to have a framework in order to inspect
how the coverage distributed across the genome.")
    (license license:artistic2.0)))

(define-public r-bezier
  (package
    (name "r-bezier")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "bezier" version))
              (sha256
               (base32
                "1bhqf1zbshkf1x8mgqp4mkgdxk9jxi51xj6i47kqkyn9gbdzch0c"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/bezier")
    (synopsis "Bezier Curve and Spline Toolkit")
    (description
     "This package is a toolkit for working with Bezier curves and splines.
The package provides functions for point generation, arc length estimation,
degree elevation and curve fitting.")
    (license license:gpl2)))

(define-public r-karyoploter
  (package
    (name "r-karyoploter")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "karyoploteR" version))
              (sha256
               (base32
                "18g6dm6bl5qyr50gydlvq948cmvg21iyb6l33cyh7p4haajf7xaa"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-regioner" ,r-regioner)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-memoise" ,r-memoise)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-s4vectors" ,r-s4vectors)
       ("r-biovizbase" ,r-biovizbase)
       ("r-digest" ,r-digest)
       ("r-bezier" ,r-bezier)))
    (home-page "http://bioconductor.org/packages/karyoploteR/")
    (synopsis "Plot customizable linear genomes displaying arbitrary data")
    (description "This package creates karyotype plots of arbitrary genomes and
offers a complete set of functions to plot arbitrary data on them.  It mimicks
many R base graphics functions coupling them with a coordinate change function
automatically mapping the chromosome and data coordinates into the plot
coordinates.  In addition to the provided data plotting functions, it is easy
to add new ones.")
    (license license:artistic2.0)))

(define-public r-moments
  (package
  (name "r-moments")
  (version "0.14")
  (source (origin
           (method url-fetch)
           (uri (cran-uri "moments" version))
           (sha256
            (base32
             "0f9y58w1hxcz4bqivirx25ywlmc80gbi6dfx5cnhkpdg1pk82fra"))))
  (build-system r-build-system)
  (home-page "http://www.r-project.org")
  (synopsis "Moments, cumulants, skewness, kurtosis and related tests")
  (description "Functions to calculate: moments, Pearson's kurtosis,
Geary's kurtosis and skewness; tests related to them (Anscombe-Glynn,
D'Agostino, Bonett-Seier).")
  (license license:gpl2+)))

(define-public r-waveslim
  (package
    (name "r-waveslim")
    (version "1.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "waveslim" version))
       (sha256
        (base32
         "0lqslkihgrd7rbihqhhk57m9vkbnfsznkvk8430cvbcsn7vridii"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "http://waveslim.blogspot.com")
    (synopsis "Basic wavelet routines for up to three-dimensional signal processing")
    (description
     "Basic wavelet routines for time series (1D), image (2D) and array (3D)
analysis.  The code provided here is based on wavelet methodology developed in
Percival and Walden (2000); Gencay, Selcuk and Whitcher (2001); the dual-tree
complex wavelet transform (DTCWT) from Kingsbury (1999, 2001) as implemented by
Selesnick; and Hilbert wavelet pairs (Selesnick 2001, 2002).  All figures in chapters
4-7 of GSW (2001) are reproducible using this package and R code available at the book
website(s) below.")
    (license license:bsd-3)))

(define-public r-massspecwavelet
  (package
    (name "r-massspecwavelet")
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "MassSpecWavelet" version))
              (sha256
               (base32
                "0phrh0w1vh6kgl4vph79f3vyd569pn9kl6n1s4hll0ri61j8l0hz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-waveslim" ,r-waveslim)))
    (home-page "http://bioconductor.org/packages/MassSpecWavelet")
    (synopsis "Mass spectrum processing by wavelet-based algorithms")
    (description "This package provides methods for processing mass
spectrometry spectrum by using wavelet based algorithm.")
    (license license:gpl2+)))

(define-public r-xcms
  (package
    (name "r-xcms")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "xcms" version))
              (sha256
               (base32
                "0xqsx6cppjn0kv2smjri6kd3z4d3djlgxyxmbfayvwk9h4wsv1mg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocparallel" ,r-biocparallel)
       ("r-msnbase" ,r-msnbase)
       ("r-mzr" ,r-mzr)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-protgenerics" ,r-protgenerics)
       ("r-lattice" ,r-lattice)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-plyr" ,r-plyr)
       ("r-rann" ,r-rann)
       ("r-multtest" ,r-multtest)
       ("r-massspecwavelet" ,r-massspecwavelet)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "http://bioconductor.org/packages/xcms")
    (synopsis "LC/MS and GC/MS Data Analysis")
    (description "This package provides a framework for processing and
visualization of chromatographically separated and single-spectra mass
spectral data.  It imports from AIA/ANDI NetCDF, mzXML, mzData and mzML
files.  It preprocesses data for high-throughput, untargeted analyte
profiling.")
    (license license:gpl2+)))

(define-public r-libcoin
  (package
   (name "r-libcoin")
   (version "1.0-1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "libcoin" version))
     (sha256
      (base32
       "1i38v0pq5snx9clhgfiymk6lq87h41pajhp2dvin30snnq0py6qq"))))
   (build-system r-build-system)
   (propagated-inputs `(("r-mvtnorm" ,r-mvtnorm)))
   (home-page
    "http://cran.r-project.org/web/packages/libcoin")
   (synopsis "Linear test statistics for permutation inference")
   (description
    "This package provides basic infrastructure for linear test statistics and
permutation inference in the framework of Strasser and Weber (1999).  This
package must not be used by end-users.  CRAN package @code{coin} implements all
user interfaces and is ready to be used by anyone.")
   (license license:gpl2)))

(define-public r-inum
  (package
   (name "r-inum")
   (version "1.0-0")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "inum" version))
     (sha256
      (base32
       "1b013ak4d0iiwk5a0s7vvsl34d0hs881iq7zrvah4skb89x82cpm"))))
   (build-system r-build-system)
   (propagated-inputs `(("r-libcoin" ,r-libcoin)))
   (home-page
    "http://cran.r-project.org/web/packages/inum")
   (synopsis "Interval and enum-type representation of vectors")
   (description
    "This package provides an enum-type representation of vectors and
representation of intervals, including a method of coercing variables
in data frames.")
   (license license:gpl2)))

(define-public r-partykit
  (package
   (name "r-partykit")
   (version "1.2-2")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "partykit" version))
     (sha256
      (base32
       "118zrf9015kh0gicma98ddm80qhicdlhri2h1nwz0wlng9hkyxbg"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-formula" ,r-formula)
      ("r-inum" ,r-inum)
      ("r-libcoin" ,r-libcoin)
      ("r-mvtnorm" ,r-mvtnorm)
      ("r-rpart" ,r-rpart)
      ("r-survival" ,r-survival)))
   (home-page
    "http://partykit.R-Forge.R-project.org/partykit")
   (synopsis "Toolkit for recursive partytioning")
   (description
    "This package provides a toolkit with infrastructure for representing,
summarizing, and visualizing tree-structured regression and classification
models.  This unified infrastructure can be used for reading/coercing tree
models from different sources ('rpart', 'RWeka', 'PMML') yielding objects
that share functionality for print()/plot()/predict() methods.  Furthermore,
new and improved reimplementations of conditional inference trees
(@code{ctree()}) and model-based recursive partitioning (@code{mob()}) from
the @code{party} package are provided based on the new infrastructure.")
   (license #f)))

(define-public r-nnls
  (package
   (name "r-nnls")
   (version "1.4")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "nnls" version))
            (sha256
             (base32
              "07vcrrxvswrvfiha6f3ikn640yg0m2b4yd9lkmim1g0jmsmpfp8f"))))
   (build-system r-build-system)
   (native-inputs
    `(("gfortran" ,gfortran)))
   (home-page
    "http://cran.r-project.org/web/packages/nnls")
   (synopsis "The Lawson-Hanson algorithm for non-negative least squares")
   (description "This package provides an R interface to the Lawson-Hanson
implementation of an algorithm for non-negative least squares (NNLS).  It also
allows the combination of non-negative and non-positive constraints.")
   (license license:gpl2+)))

(define-public r-stabs
  (package
  (name "r-stabs")
  (version "0.6-3")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "stabs" version))
      (sha256
        (base32
          "17sa0sjxf6h7gx1ga1pxhv17yrz3qisaivbf5cbc3asvshhswqg9"))))
  (build-system r-build-system)
  (home-page "https://github.com/hofnerb/stabs")
  (synopsis "Stability selection with error control")
  (description
    "This package provides resampling procedures to assess the stability of
selected variables with additional finite sample error control for
high-dimensional variable selection procedures such as Lasso or boosting.
Both, standard stability selection (Meinshausen & Buhlmann, 2010) and
complementary pairs stability selection with improved error bounds
(Shah & Samworth, 2013) are implemented.  The package can be combined with
arbitrary user specified variable selection approaches.")
  (license license:gpl2)))

(define-public r-mboost
  (package
   (name "r-mboost")
   (version "2.9-0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "mboost" version))
            (sha256
             (base32
              "0hfw69h3j4hwwrdinmazl4abhij2drki5jp9i9qz30gyy16wf0f2"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-lattice" ,r-lattice)
      ("r-matrix" ,r-matrix)
      ("r-nnls" ,r-nnls)
      ("r-partykit" ,r-partykit)
      ("r-quadprog" ,r-quadprog)
      ("r-stabs" ,r-stabs)
      ("r-survival" ,r-survival)))
   (home-page "https://github.com/boost-R/mboost")
   (synopsis "Model-Based Boosting")
   (description "Functional gradient descent algorithm (boosting) for optimizing
general risk functions utilizing component-wise (penalised) least squares
estimates or regression trees as base-learners for fitting generalized linear,
additive and interaction models to potentially high-dimensional data.")
   (license license:gpl2)))

(define-public r-glinternet
  (package
   (name "r-glinternet")
   (version "1.0.8")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "glinternet" version))
     (sha256
      (base32
       "06ak80j01cbrr6s8hmcc734z476rrqzib0xqjgv7c3jbw0api71y"))))
   (build-system r-build-system)
   (home-page "http://web.stanford.edu/~hastie/Papers/glinternet_jcgs.pdf")
   (synopsis "Learning interactions via hierarchical group-lasso regularization")
   (description "Group-Lasso INTERaction-NET.  Fits linear pairwise-interaction
models that satisfy strong hierarchy: if an interaction coefficient is estimated
to be nonzero, then its two associated main effects also have nonzero estimated
coefficients.  Accommodates categorical variables (factors) with arbitrary
numbers of levels, continuous variables, and combinations thereof.  Implements
the machinery described in the paper \"Learning interactions via hierarchical
group-lasso regularization\" (JCGS 2015, Volume 24, Issue 3).
Michael Lim & Trevor Hastie (2015)")
   (license license:gpl2)))

(define-public r-rms
(package
  (name "r-rms")
  (version "5.1-2")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "rms" version))
      (sha256
        (base32
          "01wjxlqfz6l1bdsvxqq0lsbps0k86hx3ayb6fl2n2hxccvsfxkzi"))))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-ggplot2" ,r-ggplot2)
      ("r-hmisc" ,r-hmisc)
      ("r-htmltable" ,r-htmltable)
      ("r-htmltools" ,r-htmltools)
      ("r-lattice" ,r-lattice)
      ("r-multcomp" ,r-multcomp)
      ("r-nlme" ,r-nlme)
      ("r-polspline" ,r-polspline)
      ("r-quantreg" ,r-quantreg)
      ("r-rpart" ,r-rpart)
      ("r-sparsem" ,r-sparsem)
      ("r-survival" ,r-survival)))
  (native-inputs `(("gfortran" ,gfortran)))
  (home-page
    "http://biostat.mc.vanderbilt.edu/rms")
  (synopsis "Regression Modeling Strategies")
  (description
    "Regression modeling, testing, estimation, validation, graphics, prediction, and typesetting by storing enhanced model design attributes in the fit.  'rms' is a collection of functions that assist with and streamline modeling.  It also contains functions for binary and ordinal logistic regression models, ordinal models for continuous Y with a variety of distribution families, and the Buckley-James multiple regression model for right-censored responses, and implements penalized maximum likelihood estimation for logistic and ordinary linear models.  'rms' works with almost any regression model, but it was especially written to work with binary or ordinal regression models, Cox regression, accelerated failure time models, ordinary linear models,\tthe Buckley-James model, generalized least squares for serially or spatially correlated observations, generalized linear models, and quantile regression.")
  (license license:gpl2+)))

(define-public r-multcomp
(package
  (name "r-multcomp")
  (version "1.4-8")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "multcomp" version))
      (sha256
        (base32
          "0fm78g4zjc6ank316qfw977864shmy890znn4fahwc8jjdhpc252"))))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-codetools" ,r-codetools)
      ("r-mvtnorm" ,r-mvtnorm)
      ("r-sandwich" ,r-sandwich)
      ("r-survival" ,r-survival)
      ("r-th-data" ,r-th-data)))
  (home-page
    "http://cran.r-project.org/web/packages/multcomp")
  (synopsis
    "Simultaneous Inference in General Parametric Models")
  (description
    "Simultaneous tests and confidence intervals for general linear hypotheses in parametric models, including linear, generalized linear, linear mixed effects, and survival models.  The package includes demos reproducing analyzes presented in the book \"Multiple Comparisons Using R\" (Bretz, Hothorn, Westfall, 2010, CRC Press).")
  (license license:gpl2)))

(define-public r-polspline
(package
  (name "r-polspline")
  (version "1.1.13")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "polspline" version))
      (sha256
        (base32
          "08hz6wlaipjss3cfk0dvr7yy6fc7cd4hqv9finj40kkm5n262xck"))))
  (build-system r-build-system)
  (native-inputs `(("gfortran" ,gfortran)))
  (home-page
    "http://cran.r-project.org/web/packages/polspline")
  (synopsis "Polynomial Spline Routines")
  (description
    "Routines for the polynomial spline fitting routines hazard regression, hazard estimation with flexible tails, logspline, lspec, polyclass, and polymars, by C.  Kooperberg and co-authors.")
  (license license:gpl2+)))

(define-public r-sandwich
(package
  (name "r-sandwich")
  (version "2.4-0")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "sandwich" version))
      (sha256
        (base32
          "1h6c12cfv2x42laxf6ifxfk9hqzagvvvimzak88fv8vnxnf5nc9l"))))
  (build-system r-build-system)
  (propagated-inputs `(("r-zoo" ,r-zoo)))
  (home-page
    "http://cran.r-project.org/web/packages/sandwich")
  (synopsis "Robust Covariance Matrix Estimators")
  (description
    "Model-robust standard error estimators for cross-sectional, time series, clustered, panel, and longitudinal data.")
  (license #f)))

(define-public r-th-data
(package
  (name "r-th-data")
  (version "1.0-9")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "TH.data" version))
      (sha256
        (base32
          "03xfvww0krw0fn76qmmvrj7dx4shin57qafwhkrggfg25hbqlcfq"))))
  (properties `((upstream-name . "TH.data")))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-mass" ,r-mass) ("r-survival" ,r-survival)))
  (home-page
    "http://cran.r-project.org/web/packages/TH.data")
  (synopsis "TH's Data Archive")
  (description
    "Contains data sets used in other packages Torsten Hothorn maintains.")
  (license license:gpl3)))

(define-public r-pvclust
  (package
   (name "r-pvclust")
   (version "2.0-0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "pvclust" version))
            (sha256
             (base32
              "0hfpf257k5f1w59m0zq6sk0gaamflc3ldkw6qzbpyc4j94hiaihs"))))
   (build-system r-build-system)
   (home-page "http://www.sigmath.es.osaka-u.ac.jp/shimo-lab/prog/pvclust/")
   (synopsis "Hierarchical clustering with P-values via multiscale bootstrap resampling")
   (description "An implementation of multiscale bootstrap resampling for
assessing the uncertainty in hierarchical cluster analysis.  It provides AU
(approximately unbiased) p-value as well as BP (bootstrap probability) value
for each cluster in a dendrogram.")
   (license license:gpl2+)))

(define-public r-model4you
(package
  (name "r-model4you")
  (version "0.9-2")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "model4you" version))
      (sha256
        (base32
          "0bi69s4bxdqfkjg7ldg41a72vsvqi6ipzqhfbk5jhj7avfa990b9"))))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-formula" ,r-formula)
      ("r-ggplot2" ,r-ggplot2)
      ("r-gridextra" ,r-gridextra)
      ("r-partykit" ,r-partykit)
      ("r-sandwich" ,r-sandwich)
      ("r-survival" ,r-survival)))
  (home-page
    "http://cran.r-project.org/web/packages/model4you")
  (synopsis
    "Stratified and Personalised Models Based on Model-Based Trees and Forests")
  (description
    "Model-based trees for subgroup analyses in clinical trials and model-based forests for the estimation and prediction of personalised treatment effects (personalised models).  Currently partitioning of linear models, lm(), generalised linear models, glm(), and Weibull models, survreg(), is supported.  Advanced plotting functionality is supported for the trees and a test for parameter heterogeneity is provided for the personalised models.  For details on model-based trees for subgroup analyses see Seibold, Zeileis and Hothorn (2016) <doi:10.1515/ijb-2015-0032>; for details on model-based forests for estimation of individual treatment effects see Seibold, Zeileis and Hothorn (2017) <doi:10.1177/0962280217693034>.")
  (license #f)))

(define-public r-ihw
  (package
    (name "r-ihw")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IHW" version))
       (sha256
        (base32
         "0knsyckqlg86di06a9jwqwhk1pl09kmmzi0m0hrzy8msd9d6h9si"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-slam",r-slam)
        ("r-lpsymphony" ,r-lpsymphony)
        ("r-fdrtool" ,r-fdrtool)
        ("r-biocgenerics" ,r-biocgenerics)))
    (home-page "https://bioconductor.org/packages/IHW/")
    (synopsis "Independent Hypothesis Weighting")
    (description "Independent hypothesis weighting (IHW) is a multiple testing procedure that increases power compared to the method of Benjamini and Hochberg by assigning data-driven weights to each hypothesis. The input to IHW is a two-column table of p-values and covariates. The covariate can be any continuous-valued or categorical variable that is thought to be informative on the statistical properties of each hypothesis test, while it is independent of the p-value under the null hypothesis.")
    (license license:artistic2.0)))

(define-public r-slam
  (package
    (name "r-slam")
    (version "0.1-43")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "slam" version))
        (sha256
          (base32
            "0hy4qzngcgafxxr6ld7n9a9wy979ji998gpcc32vidwyab66dj5h"))))
    (build-system r-build-system)
    (home-page
      "http://cran.r-project.org/web/packages/slam")
    (synopsis
      "Sparse Lightweight Arrays and Matrices")
    (description
      "Data structures and algorithms for sparse arrays and matrices, based on index arrays and simple triplet representations, respectively.")
    (license license:gpl2)))


(define-public r-lpsymphony
  (package
    (name "r-lpsymphony")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lpsymphony" version))
       (sha256
        (base32
         "1h0qql7dn8l1rivsa1912ab2wnf2xr6qq1wfpln5wbjz6va96jnr"))))
    (build-system r-build-system)
        (home-page "https://bioconductor.org/packages/lpsymphony/")
  (inputs
     `(("gfortran" ,gfortran)))
  (synopsis "Symphony integer linear programming solver in R")
    (description "This package was derived from Rsymphony_0.1-17 from CRAN. These packages provide an R interface to SYMPHONY, an open-source linear programming solver written in C++. The main difference between this package and Rsymphony is that it includes the solver source code (SYMPHONY version 5.6), while Rsymphony expects to find header and library files on the users' system. Thus the intention of lpsymphony is to provide an easy to install interface to SYMPHONY. For Windows, precompiled DLLs are included in this package.")
    (license #f)))

(define-public r-apeglm
  (package
    (name "r-apeglm")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "apeglm" version))
       (sha256
        (base32
         "0pmqj3vwqxasxg6v22whxm7hr6liwyzh8rxaaki7pm35a0y2w9i8"))))
    (build-system r-build-system)
        (home-page "https://bioconductor.org/packages/apeglm/")
    (propagated-inputs
     `(("r-emdbook",r-emdbook)
        ("r-summarizedexperiment" ,r-summarizedexperiment)
        ("r-genomicranges" ,r-genomicranges)
        ("r-rcppeigen" ,r-rcppeigen)
        ("r-rcppnumerical" ,r-rcppnumerical)
        ("r-rcpp" ,r-rcpp)))
  (synopsis "Approximate posterior estimation for GLM coefficients")
    (description "apeglm provides Bayesian shrinkage estimators for effect sizes for a variety of GLM models, using approximation of the posterior for individual coefficients.")
    (license license:gpl2)))

(define-public r-rcppnumerical
  (package
    (name "r-rcppnumerical")
    (version "0.3-2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "RcppNumerical" version))
        (sha256
          (base32
            "0j0hvwsbidahk2zx3zk7fhc6m9cca27iq3ivx7vdvggz8iqzszrz"))))
    (properties `((upstream-name . "RcppNumerical")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-rcpp" ,r-rcpp) ("r-rcppeigen" ,r-rcppeigen)))
    (home-page
      "https://github.com/yixuan/RcppNumerical")
    (synopsis
      "'Rcpp' Integration for Numerical Computing Libraries")
    (description
      "This package provides a collection of open source libraries for numerical computing (numerical integration, optimization, etc.) and their integration with 'Rcpp'.")
    (license license:gpl2+)))

(define-public r-proc
  (package
    (name "r-proc")
    (version "1.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "pROC" version))
        (sha256
          (base32
            "05ad69a6fxy9k903cw3h4q59ch2jv6qfg9yjdbw3cgiiazcafrlj"))))
    (properties `((upstream-name . "pROC")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-ggplot2" ,r-ggplot2)
        ("r-plyr" ,r-plyr)
        ("r-rcpp" ,r-rcpp)))
    (home-page "http://expasy.org/tools/pROC/")
    (synopsis "Display and Analyze ROC Curves")
    (description
      "Tools for visualizing, smoothing and comparing receiver operating characteristic (ROC curves). (Partial) area under the curve (AUC) can be compared with statistical tests based on U-statistics or bootstrap.  Confidence intervals can be computed for (p)AUC or ROC curves.")
    (license license:lgpl3+)))

(define-public r-decipher
  (package
    (name "r-decipher")
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DECIPHER" version))
              (sha256
               (base32
                "1rzi0bmnd9xl79b9zsj4csgky61xa4aylma5lj9l9p3qvqc5gz4b"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-rsqlite" ,r-rsqlite)))
    (home-page "http://bioconductor.org/packages/DECIPHER")
    (synopsis "Tools for deciphering and managing biological sequences")
    (description "This package provides a toolset for deciphering and managing
biological sequences.")
    (license license:gpl3)))

(define-public r-hash
  (package
   (name "r-hash")
   (version "2.2.6")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "hash" version))
            (sha256
             (base32
              "0mkx59bmni3b283znvbndnkbar85fzavzdfgmwrhskidsqcz34yz"))))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/hash")
   (synopsis "Implementation of hash/associated arrays/dictionaries")
   (description
    "This package implements a data structure similar to hashes in Perl and
dictionaries in Python but with a purposefully R flavor.  For objects of
appreciable size, access using hashes outperforms native named lists and
vectors.")
   (license license:gpl2+)))

(define-public r-boruta
  (package
  (name "r-boruta")
  (version "6.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "Boruta" version))
      (sha256
        (base32
          "1pp8zal1vhxlzdhl20phn39m3ffhw5glyqbcgwa0w14zw2mpm6hw"))))
  (properties `((upstream-name . "Boruta")))
  (build-system r-build-system)
  (propagated-inputs `(("r-ranger" ,r-ranger)))
  (home-page "https://notabug.org/mbq/Boruta/")
  (synopsis
    "Wrapper Algorithm for All Relevant Feature Selection")
  (description "An all relevant feature selection wrapper algorithm.
It finds relevant features by comparing original attributes' importance
with importance achievable at random, estimated using their permuted
copies (shadows).")
  (license license:gpl2+)))
