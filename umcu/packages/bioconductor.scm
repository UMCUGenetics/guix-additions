;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019 Roel Janssen <roel@gnu.org>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages xml)
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

(define-public r-funcisnp-data
  (package
   (name "r-funcisnp-data")
   (version "1.20.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://bioconductor.org/packages/release/"
                                "data/experiment/src/contrib/FunciSNP.data_"
                                version ".tar.gz"))
            (sha256
             (base32 "1qj8x39kb6wanlr3zs81qjc9byy2pgrs9g7xbb8lj0125srkvzig"))))
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
   (version "1.28.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "FunciSNP" version))
            (sha256
             (base32 "0hvmfwyrk3j0ilw1cgy7f72bs94b2yk3lv6yxb7iwz55am6kavmm"))))
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://bioconductor.org/packages/release/data/experiment"
                    "/src/contrib/pasilla_" version ".tar.gz"))
              (sha256
               (base32
                "0yp96vzqbi0kgjsml9d77wg76j0j89f960p457a645hy7dnpfwrg"))))
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

(define-public r-motifdb
  (package
   (name "r-motifdb")
   (version "1.26.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotifDb" version))
            (sha256
             (base32 "05rxcxdkdpg0qg5qlylf0in9qhi16gpl8c9sfm4j2z3rvfxl0g60"))))
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
   (version "1.14.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifbreakR" version))
            (sha256
             (base32 "1dwbhkjmdk9syxhymhdsp3bkzcvm70p3nasy1bai9vv86gx57l14"))))
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
    (version "1.2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/kundajelab/phantompeakqualtools/"
                    "archive/" version ".tar.gz"))
              (sha256
               (base32
                "0ag881jbhic5x9r87yk1p4687divyzggk7agqrnwh71npvy4a52b"))))
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
   (version "2.4.8")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "fda" version))
            (sha256
             (base32
              "0n39rzbhg1hipzn51rzmbchn2358qgapg08iv7lmiqj5y7i9qns2"))))
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
   (version "4.0-0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "LSD" version))
            (sha256
             (base32
              "0fsp3pwrnnic9mzkd6yxa4bnxbvg68712lb20vd42wf6jb39r2h3"))))
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
   (version "1.18.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "FourCSeq" version))
            (sha256
             (base32 "15fz9y4v0ddj6qax329g52fa5d7y810yjn3bisqqw8vn92qvjdw3"))))
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
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ecp" version))
              (sha256
               (base32
                "0s0286ky1imhhs89bp1ylx8wvii55v7wzg1g49l03az64971kayj"))))
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
    (version "1.12.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AneuFinder" version))
              (sha256
               (base32
                "0inj83p9jzdblhl9xlzz07iqs0b6xrz1mkm357h8krn5aj3ar0x4"))))
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
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IdeoViz" version))
              (sha256
               (base32
                "0lgvkahpfmwzvdicavyvsv76pwcadfrsg6i2k5zllpd41jcgfv9z"))))
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

(define-public r-ggpmisc
  (package
   (name "r-ggpmisc")
   (version "0.3.1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "ggpmisc" version))
     (sha256
      (base32
       "1kcjdpq7xz3609prbcf3ikj87wgcq9rk1pzhb62bh885plczsfz2"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-broom" ,r-broom)
      ("r-dplyr" ,r-dplyr)
      ("r-ggplot2" ,r-ggplot2)
      ("r-gridextra" ,r-gridextra)
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

(define-public r-pbdzmq
  (package
   (name "r-pbdzmq")
   (version "0.3-3")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "pbdZMQ" version))
            (sha256
             (base32
              "1jkfcfhspvqra7vbllrvkz3jx8j7d0ang6zzcdjgpb7200sc29mf"))))
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

(define-public r-coverageview
  (package
    (name "r-coverageview")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "CoverageView" version))
              (sha256
               (base32
                "19va5jyahk6wi3jsvryzqqrxmqxxlzs1vl5nhylsbg9yp4jfa3a1"))))
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
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "bezier" version))
              (sha256
               (base32
                "1vw5128v8h973xwa1fdm9cw2jvrldj87nd55lddlp3qsz3ag4br6"))))
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
    (version "1.10.5")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "karyoploteR" version))
              (sha256
               (base32
                "13nbc618fbbhmiqgih6hcmwqx98k6s8y512yc79x5z7w8f8w6bc0"))))
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
       ("r-bamsignals" ,r-bamsignals)
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

(define-public r-inum
  (package
   (name "r-inum")
   (version "1.0-1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "inum" version))
     (sha256
      (base32
       "16d09391l65w557dkzhhx1aqn1ljamcmjj3yh42pwq037k0r8brw"))))
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
   (version "1.2-5")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "partykit" version))
     (sha256
      (base32
       "17324y5v65i0va2mvm26gl89s01xwcffg34fwq1mvylk1xwk13pl"))))
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
   (version "2.9-1")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "mboost" version))
            (sha256
             (base32
              "02ia3y0fxfjl02fb1nnl93j640fyl18jm15cgxyybhf27w4jdvb7"))))
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
   (version "1.0.10")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "glinternet" version))
     (sha256
      (base32
       "15dikazmhs7md7j8p45f67h3947br18hsrjl74cjk10vazd0ihng"))))
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

(define-public r-apeglm
  (package
    (name "r-apeglm")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "apeglm" version))
       (sha256
        (base32
         "0jx1xf83rvnpnphdz3s15lvx8hchs5yg825jzn2bszm0brq6bsqv"))))
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
    (version "0.3-3")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "RcppNumerical" version))
        (sha256
          (base32
            "15qwjfwx6yrh9sl2gndqfxw0b3iwnkr2nrgrccb6phpj3pdp7vsq"))))
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

(define-public r-decipher
  (package
    (name "r-decipher")
    (version "2.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DECIPHER" version))
              (sha256
               (base32
                "01wdp8jb7ywha24vzgzpmqrkk1z6iyavfbf4bva3rcsj6bkch604"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-rsqlite" ,r-rsqlite)))
    (home-page "http://bioconductor.org/packages/DECIPHER")
    (synopsis "Tools for deciphering and managing biological sequences")
    (description "This package provides a toolset for deciphering and managing
biological sequences.")
    (license license:gpl3)))

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

(define-public r-geneoverlap
  (package
  (name "r-geneoverlap")
  (version "1.20.0")
  (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "GeneOverlap" version))
            (sha256
             (base32
              "0nhikxwdd0zn8iw6rlg3dmh2gkrw8v9qzfnjabjj5a5qrd9xrv7v"))))
  (build-system r-build-system)
  (propagated-inputs
   `(("r-rcolorbrewer" ,r-rcolorbrewer)
     ("r-gplots" ,r-gplots)))
  (home-page "http://bioconductor.org/packages/GeneOverlap/")
  (synopsis "Test and visualize gene overlaps")
  (description "This package can be used to test two sets of gene lists
and visualize the results.")
  (license license:gpl3)))

(define-public r-chipseeker
  (package
  (name "r-chipseeker")
  (version "1.20.0")
  (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "ChIPseeker" version))
            (sha256
             (base32
              "0141v87s9hmgfsnkh005ai5fyrxld0y88575xn3qzg38slpdwv0j"))))
  (build-system r-build-system)
  (propagated-inputs
   `(("r-annotationdbi" ,r-annotationdbi)
     ("r-biocgenerics" ,r-biocgenerics)
     ("r-boot" ,r-boot)
     ("r-enrichplot" ,r-enrichplot)
     ("r-iranges" ,r-iranges)
     ("r-genomeinfodb" ,r-genomeinfodb)
     ("r-genomicranges" ,r-genomicranges)
     ("r-genomicfeatures" ,r-genomicfeatures)
     ("r-ggplot2" ,r-ggplot2)
     ("r-gplots" ,r-gplots)
     ("r-gridbase" ,r-gridbase)
     ("r-gtools" ,r-gtools)
     ("r-dplyr" ,r-dplyr)
     ("r-plotrix" ,r-plotrix)
     ("r-dplyr" ,r-dplyr)
     ("r-magrittr" ,r-magrittr)
     ("r-rcolorbrewer" ,r-rcolorbrewer)
     ("r-rtracklayer" ,r-rtracklayer)
     ("r-s4vectors" ,r-s4vectors)
     ("r-txdb-hsapiens-ucsc-hg19-knowngene" ,r-txdb-hsapiens-ucsc-hg19-knowngene)
     ("r-upsetr" ,r-upsetr)))
  (home-page "http://bioconductor.org/packages/ChIPseeker/")
  (synopsis "ChIPseeker for ChIP peak Annotation, Comparison, and Visualization")
  (description "This package implements functions to retrieve the nearest genes
around the peak, annotate genomic region of the peak, statstical methods for
estimate the significance of overlap among ChIP peak data sets, and incorporate
GEO database for user to compare the own dataset with those deposited in database.
The comparison can be used to infer cooperative regulation and thus can be used to
generate hypotheses.  Several visualization functions are implemented to summarize
the coverage of the peak experiment, average profile and heatmap of peaks binding
to TSS regions, genomic annotation, distance to TSS, and overlap of peaks or
genes.")
  (license license:artistic2.0)))

(define-public r-reactomepa
  (package
    (name "r-reactomepa")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ReactomePA" version))
              (sha256
               (base32
                "0nm765z24lnhwkh88kf799nlid7xhqy3ab36vc0wbg6kpkcpbxkm"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-dose" ,r-dose)
       ("r-enrichplot" ,r-enrichplot)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggraph" ,r-ggraph)
       ("r-reactome-db" ,r-reactome-db)
       ("r-igraph" ,r-igraph)
       ("r-graphite" ,r-graphite)))
    (home-page "https://guangchuangyu.github.io/software/ReactomePA")
    (synopsis "Reactome Pathway Analysis")
    (description "This package provides functions for pathway analysis based on
REACTOME pathway database. It implements enrichment analysis, gene set
enrichment analysis and several functions for visualization.")
    (license license:gpl2)))

(define-public r-reactome-db
  (package
    (name "r-reactome-db")
    (version "1.66.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.bioconductor.org/packages/release/data/"
                    "annotation/src/contrib/reactome.db_" version ".tar.gz"))
              (sha256
               (base32
                "093bara4hk0590gxrz8ingbym79bs5hcf4plarrqdcldlljqfsyl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "https://guangchuangyu.github.io/software/ReactomePA")
    (synopsis "Set of annotation maps for reactome")
    (description "This package provides a set of annotation maps for reactome
assembled using data from reactome.")
    (license license:cc-by4.0)))

(define-public r-graphite
  (package
    (name "r-graphite")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "graphite" version))
              (sha256
               (base32
                "1sp2lplwwi70spzx8hfxyk1397zp1fx752wpnb1aasz5f2hpfv03"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-checkmate" ,r-checkmate)
       ("r-graph" ,r-graph)
       ("r-httr" ,r-httr)
       ("r-rappdirs" ,r-rappdirs)))
    (home-page "https://guangchuangyu.github.io/software/ReactomePA")
    (synopsis "GRAPH Interaction from pathway Topological Environment")
    (description "Graph objects from pathway topology derived from Biocarta,
HumanCyc, KEGG, NCI, Panther, PathBank, PharmGKB, Reactome and SMPDB
databases.")
    (license license:agpl3)))

(define-public r-qvalue-2.14.0
  (package (inherit r-qvalue)
    (name "r-qvalue")
    (version "2.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "qvalue" version))
              (sha256
               (base32
                "00mahhwb4n2s6nycwkdkjs2qgyyyi7hyrby3qr269krprr6q3lh5"))))))

(define-public r-spdata
  (package
   (name "r-spdata")
   (version "0.3.2")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "spData" version))
     (sha256
      (base32
       "190msrrpn226x27pcnck4ac34f9k4xcn26cyz2apdri2nzkr6zbw"))))
   (properties `((upstream-name . "spData")))
   (build-system r-build-system)
   (home-page "https://github.com/Nowosad/spData")
   (synopsis "Datasets for Spatial Analysis")
   (description
    "Diverse spatial datasets for demonstrating, benchmarking and teaching
spatial data analysis.  It includes R data of class sf (defined by the
package 'sf'), Spatial ('sp'), and nb ('spdep').  Unlike other spatial
data packages such as 'rnaturalearth' and 'maps', it also contains data
stored in a range of file formats including GeoJSON, ESRI Shapefile and
GeoPackage.  Some of the datasets are designed to illustrate specific
analysis techniques.  cycle_hire() and cycle_hire_osm(), for example, is
designed to illustrate point pattern analysis techniques.")
   (license #f)))

(define-public r-spdep
  (package
   (name "r-spdep")
   (version "1.1-3")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "spdep" version))
     (sha256
      (base32
       "1f8cjffqqc6rnb3n4qym70ca6nz2kvrsd3g587wrqdr79nnbwnrk"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-boot" ,r-boot)
      ("r-coda" ,r-coda)
      ("r-deldir" ,r-deldir)
      ("r-expm" ,r-expm)
      ("r-gmodels" ,r-gmodels)
      ("r-learnbayes" ,r-learnbayes)
      ("r-mass" ,r-mass)
      ("r-matrix" ,r-matrix)
      ("r-nlme" ,r-nlme)
      ("r-sp" ,r-sp)
      ("r-sf" ,r-sf)
      ("r-spdata" ,r-spdata)))
   (home-page "https://github.com/r-spatial/spdep/")
   (synopsis
    "Spatial Dependence: Weighting Schemes, Statistics and Models")
   (description
    "This package provides a collection of functions to create spatial weights
matrix objects from polygon 'contiguities', from point patterns by distance and
tessellations, for summarizing these objects, and for permitting their use in
spatial data analysis, including regional aggregation by minimum spanning tree;
a collection of tests for spatial 'autocorrelation', including global
'Morans I', 'APLE', 'Gearys C', 'Hubert/Mantel' general cross product
statistic, Empirical Bayes estimates and 'Assun????o/Reis' Index, 'Getis/Ord'
G and multicoloured join count statistics, local 'Moran's I' and 'Getis/Ord' G,
'saddlepoint' approximations, exact tests for global and local 'Moran's I' and
'LOSH' local indicators of spatial heteroscedasticity; and functions for
estimating spatial simultaneous 'autoregressive' ('SAR') lag and error models,
impact measures for lag models, weighted and 'unweighted' 'SAR' and 'CAR'
spatial regression models, semi-parametric and Moran 'eigenvector' spatial
filtering, 'GM SAR' error models, and generalized spatial two stage least
squares models.")
   (license license:gpl2+)))

(define-public r-diagram
  (package
   (name "r-diagram")
   (version "1.6.4")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "diagram" version))
            (sha256
             (base32
              "0f6ffprn5k0ir1s7m9s7izc64aa17r3gnygagz5bihrlsvawaavw"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-shape" ,r-shape)))
   (home-page "http://cran.r-project.org/web/packages/diagram")
   (synopsis "Functions for visualising simple graphs and flow diagrams")
   (description
    "This package can be used to Visualise simple graphs (networks) based on a
transition matrix, utilities to plot flow diagrams, visualising webs, and
electrical networks.")
   (license license:gpl2+)))

(define-public r-kegggraph
  (package
   (name "r-kegggraph")
   (version "1.44.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "KEGGgraph" version))
            (sha256
             (base32
              "1vrzblywl5vp3k4vvp9wx8r3479ahvxd4773bc9nrmlbriz8yz5w"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-xml", r-xml)
      ("r-rcurl" ,r-rcurl)
      ("r-graph", r-graph)))
   (home-page "http://bioconductor.org/packages/KEGGgraph/")
   (synopsis "KEGGgraph: A graph approach to KEGG PATHWAY")
   (description "KEGGGraph is an interface between KEGG pathway and graph object
as well as a collection of tools to analyze, dissect and visualize these graphs.
It parses the regularly updated KGML (KEGG XML) files into graph models
maintaining all essential pathway attributes. The package offers functionalities
including parsing, graph operation, visualization and etc.")
   (license license:gpl2)))

(define-public r-pathview
  (package
   (name "r-pathview")
   (version "1.24.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "pathview" version))
            (sha256
             (base32
              "12jswp402bwmw3hnia3y0lvklfjpldc0f2893ircqx76n9cm17d9"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-kegggraph", r-kegggraph)
      ("r-xml", r-xml)
      ("r-rgraphviz", r-rgraphviz)
      ("r-graph", r-graph)
      ("r-png", r-png)
      ("r-annotationdbi", r-annotationdbi)
      ("r-keggrest", r-keggrest)
      ("r-org-hs-eg-db", r-org-hs-eg-db)))
   (home-page "http://bioconductor.org/packages/pathview/")
   (synopsis "Toolset for pathway-based data integration and visualization")
   (description "Pathview is a tool set for pathway based data integration
and visualization.  It maps and renders a wide variety of biological data
on relevant pathway graphs. All users need is to supply their data and
specify the target pathway. Pathview automatically downloads the pathway
graph data, parses the data file, maps user data to the pathway, and render
pathway graph with the mapped data.  In addition, Pathview also seamlessly
integrates with pathway and gene set (enrichment) analysis tools for
 large-scale and fully automated analysis.")
   (license license:gpl3)))

(define-public r-deepsnv
  (package
   (name "r-deepsnv")
   (version "1.30.0")
   (source
    (origin
     (method url-fetch)
     (uri (bioconductor-uri "deepSNV" version))
     (sha256
      (base32
       "004kay4zbw73fizrx1gim933jy7z8dq1g2qmywvvf0l2q06ffw9r"))))
   (properties `((upstream-name . "deepSNV")))
   (build-system r-build-system)
   (inputs
    `(("zlib" ,zlib)))
   (propagated-inputs
    `(("r-biostrings" ,r-biostrings)
      ("r-genomicranges" ,r-genomicranges)
      ("r-iranges" ,r-iranges)
      ("r-rhtslib" ,r-rhtslib)
      ("r-summarizedexperiment" ,r-summarizedexperiment)
      ("r-variantannotation" ,r-variantannotation)
      ("r-vgam" ,r-vgam)))
   (home-page
    "http://github.com/gerstung-lab/deepSNV")
   (synopsis
    "Detection of subclonal SNVs in deep sequencing data.")
   (description
    "This package provides provides quantitative variant callers for detecting subclonal mutations in ultra-deep (>=100x coverage) sequencing experiments.  The deepSNV algorithm is used for a comparative setup with a control experiment of the same loci and uses a beta-binomial model and a likelihood ratio test to discriminate sequencing errors and subclonal SNVs.  The shearwater algorithm computes a Bayes classifier based on a beta-binomial model for variant calling with multiple samples for precisely estimating model parameters - such as local error rates and dispersion - and prior knowledge, e.g.  from variation data bases such as COSMIC.")
   (license license:gpl3)))

(define-public r-structuralvariantannotation
  (package
    (name "r-structuralvariantannotation")
    (version "d6173c3d9dd1fa314c91092b51920925b22268c6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PapenfussLab/StructuralVariantAnnotation.git")
             (commit version)))
       (sha256
        (base32
         "12s4l5f778mnncbikqva941hzr07c59gnkd633hd03q3h57hm5xg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-devtools" ,r-devtools)
       ("r-dplyr" ,r-dplyr)
       ("r-genomicranges" ,r-genomicranges)
       ("r-roxygen2" ,r-roxygen2)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-stringr" ,r-stringr)
       ("r-testthat" ,r-testthat)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://github.com/PapenfussLab/StructuralVariantAnnotation")
    (synopsis "R package designed to simplify structural variant analysis")
    (description
     "This package contains useful helper functions for dealing with structural
variants in VCF format.  The packages contains functions for parsing VCFs from
a number of popular callers as well as functions for dealing with breakpoints
involving two separate genomic loci encoded as GRanges objects.")
    (license license:gpl3)))

(define-public r-mutsigextractor
  (let ((commit "18deddb70ad29211d2b35b0ce564b829f2105a13"))
    (package
      (name "r-mutsigextractor")
      (version (string-append "0-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/luannnguyen/mutSigExtractor.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0pcxk5ay68mgf17zll4r46s44y06dj8i3070ifh5bqgpq91dgkzq"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-genomicranges" ,r-genomicranges)
         ("r-variantannotation" ,r-variantannotation)
         ("r-bsgenome" ,r-bsgenome)
         ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)))
      (home-page "https://github.com/luannnguyen/mutSigExtractor")
      (synopsis "Extracts SNV, indel, and SV signatures from VCF files")
      (description "More about what it does (maybe more than one line). Use four spaces when indenting paragraphs within the Description.")
      (license license:expat))))

(define-public r-hiddenmarkov
  (package
   (name "r-hiddenmarkov")
   (version "1.8-11")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "HiddenMarkov" version))
            (sha256
             (base32
              "1yh85pdb9r90qxcl5gxslyplxzrx8knrrsl2q65l57zfkqj185ja"))))
   (properties `((upstream-name . "HiddenMarkov")))
   (build-system r-build-system)
   (native-inputs
    `(("gfortran" ,gfortran)))
   (home-page "http://cran.r-project.org/web/packages/HiddenMarkov")
   (synopsis "Hidden Markov models")
   (description "This package contains functions for the analysis of Discrete
Time Hidden Markov Models, Markov Modulated GLMs and the Markov Modulated
Poisson Process.  It includes functions for simulation, parameter estimation,
and the Viterbi algorithm. See the topic 'HiddenMarkov' for an introduction to
the package, and 'Change Log' for a list of recent changes. The algorithms are
based of those of Walter Zucchini.")
   (license license:gpl2+)))

(define-public r-depmixs4
  (package
   (name "r-depmixs4")
   (version "1.4-0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "depmixS4" version))
            (sha256
             (base32
              "0v8hvkg7ia7c6a0x7rw5fddm06vv1n0kwjk7g0kj3g18chvpnvw2"))))
   (properties
    `((upstream-name . "depmixS4")))
   (propagated-inputs
    `(("r-nnet" ,r-nnet)
      ("r-nlme" ,r-nlme)
      ("r-mass" ,r-mass)
      ("r-rsolnp" ,r-rsolnp)))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/depmixS4")
   (synopsis "Dependent Mixture Models")
   (description "This package fits latent (hidden) Markov models on mixed
categorical and continuous (time series) data, otherwise known as dependent
mixture models.")
   (license license:gpl2+)))

(define-public r-rnmf
  (package
   (name "r-rnmf")
   (version "0.5.0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "rNMF" version))
            (sha256
             (base32
              "1nz6h0j5ywdh48m0swmhp34hbkycd7n13rclrxaw85qi9wc42597"))))
   (properties
    `((upstream-name . "rNMF")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-knitr" ,r-knitr)
      ("r-nnls" ,r-nnls)))
   (home-page "https://cran.r-project.org/web/packages/rNMF")
   (synopsis "Robust Nonnegative Matrix Factorization")
   (description
    "An implementation of robust nonnegative matrix factorization (rNMF).  The
rNMF algorithm decomposes a nonnegative high dimension data matrix into the
product of two low rank nonnegative matrices, while detecting and trimming
outliers.  The main function is rnmf().  The package also includes a
visualization tool, see(), that arranges and prints vectorized images.")
   (license license:gpl2+)))

(define-public r-protgenerics-1.17.4
  (let ((commit "8bc2a4193f416f62b35c2a346242d0e0152eaad0"))
    (package
      (name "r-protgenerics")
      (version (string-append "1.17.4-" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lgatto/ProtGenerics.git")
               (commit commit)))
         (sha256
          (base32
           "1qp7ryp6c0zc8ag469ffcsmcmkln8d99002qb282da5c1xqp9jzh"))))
      (properties `((upstream-name . "ProtGenerics")))
      (build-system r-build-system)
      (home-page "https://github.com/lgatto/ProtGenerics")
      (synopsis "S4 generic functions for proteomics infrastructure")
      (description
       "This package provides S4 generic functions needed by Bioconductor
proteomics packages.")
      (license license:artistic2.0))))

(define-public r-mzr-2.19.6
  (let ((commit "dddbebcea9ab8d1ce09b7bad5f3b9334831bd42b"))
    (package
      (name "r-mzr")
      (version (string-append "2.19.6-" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sneumann/mzR.git")
               (commit commit)))
         (sha256
          (base32
           "0ls50axlcyb3rlyn1j49wk07pk7x59hzddlcznr8mzkpwwh0519k"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (delete-file-recursively "src/boost")
             (delete-file "R/zzz.R")
             #t))))
      (properties `((upstream-name . "mzR")))
      (build-system r-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'use-system-boost
             (lambda _
               (substitute* "src/Makevars"
                 (("\\./boost/libs.*") "")
                 (("ARCH_OBJS=" line)
                  (string-append line
                                 "\nARCH_LIBS=-lboost_system -lboost_regex \
-lboost_iostreams -lboost_thread -lboost_filesystem -lboost_chrono\n")))
               #t)))))
      (inputs
       `(;; XXX Boost 1.69 will not work here.
         ("boost" ,boost-for-mysql) ; use this instead of the bundled boost sources
         ("zlib" ,zlib)))
      (propagated-inputs
       `(("r-biobase" ,r-biobase)
         ("r-biocgenerics" ,r-biocgenerics)
         ("r-ncdf4" ,r-ncdf4)
         ("r-protgenerics" ,r-protgenerics-1.17.4)
         ("r-rcpp" ,r-rcpp)
         ("r-rhdf5lib" ,r-rhdf5lib)
         ("r-zlibbioc" ,r-zlibbioc)))
      (home-page "https://github.com/sneumann/mzR/")
      (synopsis "Parser for mass spectrometry data files")
      (description
       "The mzR package provides a unified API to the common file formats and
parsers available for mass spectrometry data.  It comes with a wrapper for the
ISB random access parser for mass spectrometry mzXML, mzData and mzML files.
The package contains the original code written by the ISB, and a subset of the
proteowizard library for mzML and mzIdentML.  The netCDF reading code has
previously been used in XCMS.")
      (license license:artistic2.0))))

(define-public r-xcms
  (let ((commit "2180f6144a615911a53acf66525f50005d2beacc"))
    (package
      (name "r-xcms")
      (version (string-append "3.7.4-" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sneumann/xcms.git")
               (commit commit)))
         (sha256
          (base32
           "0scyjmyhgmrl3x95xpnl5641aidw8s1n7r54pv2q7ibgr42d7hx9"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-devtools" ,r-devtools)
         ("r-biobase" ,r-biobase)
         ("r-biocgenerics" ,r-biocgenerics)
         ("r-biocparallel" ,r-biocparallel)
         ("r-rtracklayer" ,r-rtracklayer)
         ("r-dplyr" ,r-dplyr)
         ("r-genomicranges" ,r-genomicranges)
         ("r-lattice" ,r-lattice)
         ("r-massspecwavelet" ,r-massspecwavelet)
         ("r-msnbase" ,r-msnbase)
         ("r-multtest" ,r-multtest)
         ("r-variantannotation" ,r-variantannotation)
         ("r-biostrings" ,r-biostrings)
         ("r-mzr" ,r-mzr-2.19.6)
         ("r-plyr" ,r-plyr)
         ("r-protgenerics" ,r-protgenerics-1.17.4)
         ("r-rann" ,r-rann)
         ("r-rcolorbrewer" ,r-rcolorbrewer)
         ("r-robustbase" ,r-robustbase)
         ("r-s4vectors" ,r-s4vectors)))
      (home-page "https://bioconductor.org/packages/xcms/")
      (synopsis "LC/MS and GC/MS mass spectrometry data analysis")
      (description
       "This package provides a framework for processing and visualization of
chromatographically separated and single-spectra mass spectral data.  It
imports from AIA/ANDI NetCDF, mzXML, mzData and mzML files.  It preprocesses
data for high-throughput, untargeted analyte profiling. FIX FOR JOANNA")
      (license license:gpl2+))))

(define-public r-useful
  (package
   (name "r-useful")
   (version "1.2.6")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "useful" version))
            (sha256
             (base32
              "0n50v1q75k518sq23id14jphwla35q4sasahrnrnllwrachl67v1"))))
   (properties `((upstream-name . "useful")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-assertthat" ,r-assertthat)
      ("r-dplyr" ,r-dplyr)
      ("r-ggplot2" ,r-ggplot2)
      ("r-magrittr" ,r-magrittr)
      ("r-matrix" ,r-matrix)
      ("r-plyr" ,r-plyr)
      ("r-purrr" ,r-purrr)
      ("r-scales" ,r-scales)))
   (home-page "https://github.com/jaredlander/useful")
   (synopsis "A Collection of Handy, Useful Functions")
   (description "This package provides a set of little functions that have been
found useful to do little odds and ends such as plotting the results of K-means
clustering, substituting special text characters, viewing parts of a
@code{data.frame}, constructing formulas from text and building design and
response matrices.")
   (license license:bsd-3)))

(define-public r-bisquerna
  (package
   (name "r-bisquerna")
   (version "1.0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "BisqueRNA" version))
            (sha256
             (base32
              "1ks91gjfcnajg540930drcnqcrn4fi3735hywyy3y6i1q0wnq9vb"))))
   (properties `((upstream-name . "BisqueRNA")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-biobase" ,r-biobase)
      ("r-lsei" ,r-lsei)))
   (home-page "https://www.biorxiv.org/content/10.1101/669911v1")
   (synopsis "Decomposition of Bulk Expression with Single-Cell Sequencing")
   (description "This package provides tools to accurately estimate cell type
abundances from heterogeneous bulk expression.  A reference-based method
utilizes single-cell information to generate a signature matrix and
transformation of bulk expression for accurate regression based estimates.
A marker-based method utilizes known cell-specific marker genes to measure
relative abundances across samples.")
   (license license:gpl3)))

