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

(define-public r-aneufinder-umcu
  (package
    (name "r-aneufinder")
    (version "1.17.99-umcu")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/UMCUGenetics/aneufinder.git")
                   (commit "67346eb8597a6328fdf186261f88815fe881271f")))
             (sha256
              (base32
               "0dxpqibmlm01jzjl8kif1077gr20k1ssrzj8b3la41x4s1p96mgl"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (home-page "https://bioconductor.org/packages/AneuFinder/")
    (synopsis "Copy number variation analysis in single-cell-sequencing data")
    (description "This package contains a modified version of Aneufinder that
includes the GCSC correction method.  When in doubt, use the unmodified version
instead.")
    (license license:artistic2.0)))


(define-public r-structuralvariantannotation-for-hmf-pipeline
  (package
    (name "r-structuralvariantannotation")
    (version "0.0.0-d6173c3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/PapenfussLab/StructuralVariantAnnotation.git")
	     (commit "d6173c3d9dd1fa314c91092b51920925b22268c6")))
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
       ("r-assertthat" ,r-assertthat)
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

(define-public r-princurve
  (package
    (name "r-princurve")
    (version "2.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "princurve" version))
        (sha256
          (base32
            "0ifjwdpvydhn60aya84f5j0ymsq427j89j9a3g8pbigk6qnxj0g5"))))
    (properties `((upstream-name . "princurve")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page
      "https://github.com/rcannood/princurve")
    (synopsis
      "Fit a Principal Curve in Arbitrary Dimension")
    (description
      "Fitting a principal curve to a data matrix in arbitrary dimensions.  Hastie and Stuetzle (1989) <doi:10.2307/2289936>.")
    (license license:gpl2)))
 
(define-public r-slingshot
  (package
    (name "r-slingshot")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "slingshot" version))
        (sha256
          (base32
            "0gh91fkikw7y22sb00yvbjjssr64f4swikcz2ydri84c4jv8nvqa"))))
    (properties `((upstream-name . "slingshot")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-ape" ,r-ape)
        ("r-igraph" ,r-igraph)
        ("r-matrixstats" ,r-matrixstats)
        ("r-princurve" ,r-princurve)
        ("r-singlecellexperiment"
         ,r-singlecellexperiment)
        ("r-summarizedexperiment"
         ,r-summarizedexperiment)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://bioconductor.org/packages/slingshot")
    (synopsis
      "Tools for ordering single-cell sequencing")
    (description
      "This package provides functions for inferring continuous, branching lineage structures in low-dimensional data.  Slingshot was designed to model developmental trajectories in single-cell RNA sequencing data and serve as a component in an analysis pipeline after dimensionality reduction and clustering.  It is flexible enough to handle arbitrarily many branching events and allows for the incorporation of prior knowledge through supervised graph construction.")
    (license license:artistic2.0)))
 
(define-public r-tradeseq
  (package
    (name "r-tradeseq")
    (version "1.2.01")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "tradeSeq" version))
        (sha256
          (base32
            "1jqy2xn58j89lfsah9gvkphq9a5a8s7h6g5025r13n7ksh3whfbp"))))
    (properties `((upstream-name . "tradeSeq")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-biobase" ,r-biobase)
        ("r-biocparallel" ,r-biocparallel)
        ("r-clusterexperiment" ,r-clusterexperiment)
        ("r-dplyr" ,r-dplyr)
        ("r-edger" ,r-edger)
        ("r-ggplot2" ,r-ggplot2)
        ("r-igraph" ,r-igraph)
        ("r-magrittr" ,r-magrittr)
        ("r-mgcv" ,r-mgcv)
        ("r-monocle" ,r-monocle)
        ("r-pbapply" ,r-pbapply)
        ("r-princurve" ,r-princurve)
        ("r-rcolorbrewer" ,r-rcolorbrewer)
        ("r-s4vectors" ,r-s4vectors)
        ("r-singlecellexperiment"
         ,r-singlecellexperiment)
        ("r-slingshot" ,r-slingshot)
        ("r-summarizedexperiment"
         ,r-summarizedexperiment)
        ("r-tibble" ,r-tibble)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://statomics.github.io/tradeSeq/index.html")
    (synopsis
      "trajectory-based differential expression analysis for sequencing data")
    (description
      "tradeSeq provides a flexible method for fitting regression models that can be used to find genes that are differentially expressed along one or multiple lineages in a trajectory.  Based on the fitted models, it uses a variety of tests suited to answer different questions of interest, e.g.  the discovery of genes for which expression is associated with pseudotime, or which are differentially expressed (in a specific region) along the trajectory.  It fits a negative binomial generalized additive model (GAM) for each gene, and performs inference on the parameters of the GAM.")
    (license expat)))

(define-public r-nnlm
  (package
  (name "r-nnlm")
  (version "0.4.3")
  (source
    (origin
      (method url-fetch)
      (uri "https://cran.r-project.org/src/contrib/Archive/NNLM/NNLM_0.4.3.tar.gz")
      (sha256
        (base32
          "0p7p26070w2j57z5i89pmpgd470yh2z0rrfzpalzdngc8mzp2rkj"))))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-rcpp",r-rcpp)
      ("r-rcpparmadillo",r-rcpparmadillo)
      ("r-rcppprogress",r-rcppprogress)))
  (home-page "https://github.com/linxihui/NNLM")
  (synopsis "Nonnegative Linear Models (NNLM)")
  (description "This is a package for Nonnegative Linear 
    Models (NNLM). It implements fast sequential 
    coordinate descent algorithms for nonnegative linear 
    regression and nonnegative matrix factorization (NMF 
    or NNMF). It supports mean square error and 
    Kullback-Leibler divergence loss. Many other features 
    are also implemented, including missing value 
    imputation, domain knowledge integration, designable 
    W and H matrices and multiple forms of regularizations.")
  (license license:bsd-2)))

(define-public r-rffc
  (package
  (name "r-rffc")
  (version "1.0")
  (source
    (origin
      (method url-fetch)
      (uri "http://download.r-forge.r-project.org/src/contrib/rfFC_1.0.tar.gz")
      (sha256
        (base32
          "05x9wgzsmx4vb12lmcspymgmpb2xw8bwryb8ysg7vzg2nkh0ma3g"))))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-randomforest",r-randomforest)))
  (home-page "https://github.com/rforge/rffc/")
  (synopsis "Random Forest Feature Contributions ")
  (description "Functions for extracting feature contributions 
    from a random forest model from package 'randomForest'. Feature 
    contributions provide detailed information about the relationship 
    between data variables and the predicted value returned by random 
    forest model.")
  (license license:gpl2)))

(define-public r-sccnvcharacterizationhelper
  (package
  (name "r-sccnvcharacterizationhelper")
  (version "0.0.0.9000")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/UMCUGenetics/scCnvCharacterizationHelper/"
            "archive/" version ".tar.gz"))
      (sha256
        (base32
          "0rzmr4z20yy016d2mx3lbjlylidgqwfc1pr657vwzgk1wvvyv1cf"))))
  (build-system r-build-system)
  (propagated-inputs
   `(("r-genomicranges",r-genomicranges)
     ("r-genomeinfodb",r-genomeinfodb)
     ("r-iranges",r-iranges)
     ("r-rcurl",r-rcurl)
     ("r-jsonlite",r-jsonlite)
     ("r-ggplot2",r-ggplot2)
     ("r-rhtslib",r-rhtslib)))
  (home-page "https://github.com/UMCUGenetics/scCnvCharacterizationHelper/")
  (synopsis "Single-cell copy-number quantification pipeline helper.")
  (description "This package provides the basic building blocks for a
single-cell copy-number quantification pipeline.")
  (license license:gpl3)))

(define-public r-pacman
  (package
    (name "r-pacman")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "pacman" version))
        (sha256
          (base32
            "0z7gngd6h83cpjhq1vg75wvzhdjbgjh7gj5d4zvvi9gd2lmagjcy"))))
    (properties `((upstream-name . "pacman")))
    (build-system r-build-system)
    (propagated-inputs `(("r-remotes" ,r-remotes)))
    (home-page "https://github.com/trinker/pacman")
    (synopsis "Package Management Tool")
    (description
      "Tools to more conveniently perform tasks associated with add-on packages.  pacman conveniently wraps library and package related functions and names them in an intuitive and consistent fashion.  It seeks to combine functionality from lower level functions which can speed up workflow.")
    (license license:gpl2)))

(define-public r-memuse
  (package
    (name "r-memuse")
    (version "4.1-0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "memuse" version))
        (sha256
          (base32
            "1bbjp8y0ji71956fbaxiil7ynq2nkmmgz7i9xps83m3bbp5d3mjq"))))
    (properties `((upstream-name . "memuse")))
    (build-system r-build-system)
    (home-page
      "https://github.com/shinra-dev/memuse")
    (synopsis "Memory Estimation Utilities")
    (description
      "How much ram do you need to store a 100,000 by 100,000 matrix? How much ram is your current R session using? How much ram do you even have? Learn the scintillating answer to these and many more such questions with the 'memuse' package.")
    (license #f)))

(define-public r-pinfsc50
  (package
    (name "r-pinfsc50")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "pinfsc50" version))
        (sha256
          (base32
            "1547xyxmfb7zi8h9bsm6k67dcw4hpp129xzvmgwfw7r6p4af47zd"))))
    (properties `((upstream-name . "pinfsc50")))
    (build-system r-build-system)
    (home-page
      "https://cran.r-project.org/web/packages/pinfsc50")
    (synopsis
      "Sequence ('FASTA'), Annotation ('GFF') and Variants ('VCF') for 17 Samples of 'P. Infestans\" and 1 'P. Mirabilis'")
    (description
      "Genomic data for the plant pathogen \"Phytophthora infestans.\" It includes a variant file ('VCF'), a sequence file ('FASTA') and an annotation file ('GFF').  This package is intended to be used as example data for packages that work with genomic data.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-vcfr
  (package
    (name "r-vcfr")
    (version "1.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "vcfR" version))
        (sha256
          (base32
            "0lhxb3ac4fafwik9q3cds46svzf0hyca8k54chw3dpk50c0zz1yx"))))
    (properties `((upstream-name . "vcfR")))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
      `(("r-ape" ,r-ape)
        ("r-dplyr" ,r-dplyr)
        ("r-magrittr" ,r-magrittr)
        ("r-memuse" ,r-memuse)
        ("r-pinfsc50" ,r-pinfsc50)
        ("r-rcpp" ,r-rcpp)
        ("r-stringr" ,r-stringr)
        ("r-tibble" ,r-tibble)
        ("r-vegan" ,r-vegan)
        ("r-viridislite" ,r-viridislite)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/knausb/vcfR")
    (synopsis "Manipulate and Visualize VCF Data")
    (description
      "Facilitates easy manipulation of variant call format (VCF) data.  Functions are provided to rapidly read from and write to VCF files.  Once VCF data is read into R a parser function extracts matrices of data.  This information can then be used for quality control or other purposes.  Additional functions provide visualization of genomic data.  Once processing is complete data may be written to a VCF file (*.vcf.gz).  It also may be converted into other popular R objects (e.g., genlight, DNAbin).  VcfR provides a link between VCF data and familiar R software.")
    (license license:gpl3)))
