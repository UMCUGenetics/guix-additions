;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pth)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (umcu packages grid-engine)
  #:use-module (umcu packages perl)
  #:use-module (umcu packages python))

(define-public python-theano
  (package
    (name "python-theano")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Theano" version))
              (sha256
               (base32
                "1pmb5754qwiy1x2irciwn4xzsvwapdpi5agwwq8p1898sc1y0s37"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "http://deeplearning.net/software/theano/")
    (synopsis "Optimizing compiler for evaluating mathematical expressions on CPUs and GPUs.")
    (description "Optimizing compiler for evaluating mathematical expressions on CPUs and GPUs.")
    (license license:bsd-3)))

(define-public python-pymc3
  (package
  (name "python-pymc3")
  (version "3.5")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "pymc3" version))
      (sha256
        (base32
          "1vi11z1cjhc1hxbjvxay9n7a599z13p583qa6lhvnc6pqs1yd230"))))
  (build-system python-build-system)
  (arguments `(#:tests? #f ))
  (propagated-inputs
    `(;("python-enum34" ,python-enum34)
      ("python-h5py" ,python-h5py)
      ("python-joblib" ,python-joblib)
      ("python-numpy" ,python-numpy)
      ("python-pandas" ,python-pandas)
      ("python-patsy" ,python-patsy)
      ("python-six" ,python-six)
      ("python-theano" ,python-theano)
      ("python-tqdm" ,python-tqdm)))
  (home-page "http://github.com/pymc-devs/pymc3")
  (synopsis
    "Probabilistic Programming in Python: Bayesian Modeling and Probabilistic Machine Learning with Theano")
  (description
    "Probabilistic Programming in Python: Bayesian Modeling and Probabilistic Machine Learning with Theano")
  (license license:asl2.0)))

(define-public gatk4
  (package
    (name "gatk4")
    (version "4.1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/broadinstitute/gatk/releases/download/"
                    version "/gatk-" version ".zip"))
              (sha256
               (base32 "0aw8v4fsy4cmaji2rl7p42l58w2glrjxipygqk3m9k70rb155hbg"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((unzip   (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
               (tarball (assoc-ref %build-inputs "source"))
               (out     (string-append %output "/share/java/user-classes"))
               (bin     (string-append %output "/bin")))
           (mkdir-p out)
           (mkdir-p bin)
           (system (string-append unzip " " tarball))
           (chdir (string-append "gatk-" ,version))
           (install-file "gatk-package-4.1.5.0-local.jar" out)
           (symlink (string-append out "/gatk-package-4.1.5.0-local.jar")
                    (string-append out "/gatk.jar"))
           (install-file "gatk-package-4.1.5.0-spark.jar" out)
           (symlink (string-append out "/gatk-package-4.1.5.0-spark.jar")
                    (string-append out "/gatk-spark.jar"))
           (substitute* "gatk"
             (("/usr/bin/env python") (string-append
                                       (assoc-ref %build-inputs "python2")
                                       "/bin/python"))
             (("return \\[\"java\"\\]")
              (string-append "return [\"" (assoc-ref %build-inputs "icedtea-8") "/bin/java\"]"))
             (("findJar\\(\"local.jar\", envVariableOverride=GATK_LOCAL_JAR_ENV_VARIABLE\\)")
              (string-append "\"" out "/gatk.jar\""))
             (("findJar\\(\"spark.jar\", envVariableOverride=GATK_SPARK_JAR_ENV_VARIABLE\\)")
              (string-append "\"" out "/gatk-spark.jar\"")))
           (install-file "gatk" bin)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("python2" ,python-2.7)
       ("icedtea-8" ,icedtea-8)))
    (propagated-inputs
     `(("r" ,r)
       ("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)
       ("python-certifi",python-certifi)
       ("openmpi" ,openmpi)
       ("openssl" ,openssl)
       ("python" ,python-3)
       ("readline" ,readline)
       ("python-setuptools" ,python-setuptools)
       ("sqlite" ,sqlite)
       ("tk" ,tk)
       ("python-wheel" ,python-wheel)
       ("xz" ,xz)
       ("zlib" ,zlib)
       ("python-bleach" ,python-bleach)
       ("python-cycler" ,python-cycler)
       ("python-h5py" ,python-h5py)
       ("python-html5lib" ,python-html5lib-0.9)
       ("python-joblib" ,python-joblib)
       ("python-keras" ,python-keras)
       ("python-markdown" ,python-markdown)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-patsy" ,python-patsy)
       ("python-protobuf" ,python-protobuf)
       ("python-pymc3" ,python-pymc3)
       ("python-pyparsing" ,python-pyparsing)
       ("python-dateutil" ,python-dateutil)
       ("python-pytz" ,python-pytz)
       ("python-pyyaml" ,python-pyyaml)
       ("python-scipy" ,python-scipy)
       ("python-six" ,python-six)
       ("python-theano" ,python-theano)
       ("python-tqdm" ,python-tqdm)
       ("python-werkzeug" ,python-werkzeug)
       ("python3" ,python-3)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public score-client
  (package
   (name "score-client")
   (version "5.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://artifacts.oicr.on.ca/artifactory/dcc-release/bio/"
                  "overture/score-client/" version "/score-client-" version
                  "-dist.tar.gz"))
            (sha256
             (base32 "05pvffd43aqdh92g1p37p9p00wciqxp45n5gyybxvpgs1cfdqsfm"))))
   ;; We use the GNU build system mainly for its patch-shebang phases.
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is just copying a binary, so no tests to perform.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; No configuration, just copying.
        (delete 'build)     ; No building, just copying.
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (etc (string-append out "/etc/score-client"))
                   (bin (string-append out "/bin"))
                   (lib (string-append out "/lib")))

              (for-each mkdir-p (list out etc bin lib))

              (substitute* "bin/score-client"
               (("`dirname \\$0`/..") out)
               (("\\$\\(cd \\$\\{BASE_DIR\\} && pwd -P\\)") out)
               (("exec java") (string-append
                               "exec " (assoc-ref inputs "openjdk")
                               "/bin/java"))
               (("-Dlogging.path=\\$\\{BASE_DIR\\}/logs")
                "-Dlogging.path=${HOME}")
               (("type -p java")
                (string-append "type -p "
                               (assoc-ref inputs "openjdk")
                               "/bin/java"))
               (("_java=java")
                (string-append "_java="
                               (assoc-ref inputs "openjdk")
                               "/bin/java"))
               (("\\$\\{CLIENT_DIR\\}/conf") etc))

              (copy-recursively "bin" bin)
              (copy-recursively "conf" etc)
              (copy-recursively "lib" lib)

              (wrap-program (string-append out "/bin/score-client")
                `("_JAVA_OPTIONS" ":" = (,(string-append
                                           "-Djavax.net.ssl.trustStore="
                                           (assoc-ref inputs "openjdk")
                                           "/lib/security/cacerts"))))))))))
   (inputs
    `(("openjdk" ,openjdk11)))
   (home-page "https://docs.icgc.org/software/download/#score-client")
   (synopsis "Tool to view ICGC data")
   (description "This package provides a tool to download or view data in
the cloud environments of ICGC.")
   (license license:gpl3)))

(define-public metamaps
  (let ((commit "e23f8a8688159ff0d092557a40305dbc7acc2342"))
    (package
     (name "metamaps")
     (version (string-append "0.0-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DiltheyLab/MetaMaps.git")
                    (commit commit)))
              (sha256
               (base32
                "0h9ahkv7axw4qzgbvhsz4r699swiv64hlwjy6h8s11vjls2dslrp"))))
     (build-system gnu-build-system)
     (arguments
      `(#:configure-flags (list (string-append
                                 "--with-boost="
                                 (assoc-ref %build-inputs "boost")))
        #:tests? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'shared-boost
            (lambda _
              (substitute* "configure.ac"
               (("libboost_math_c99.a") "libboost_math_c99.so")))))))
     (native-inputs
      `(("autoconf" ,autoconf)))
     (inputs
      `(("boost" ,boost)
        ("zlib" ,zlib)
        ("gsl" ,gsl)))
     (home-page "https://github.com/DiltheyLab/MetaMaps")
     (synopsis "Long-read metagenomic analysis")
     (description "MetaMaps is tool specifically developed for the analysis
of long-read (PacBio/Oxford Nanopore) metagenomic datasets.")
     (license license:public-domain))))

(define-public picard-bin-1.141
  (package
   (name "picard")
   (version "1.141")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/broadinstitute/picard/releases/download/"
                  version "/picard-tools-" version ".zip"))
            (sha256
             (base32 "1ari9j37a0v8bm03c77pw729bqwbqqn6h15rw028jhl1iz4rgd5g"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("icedtea" ,icedtea-8)))
   (native-inputs
    `(("unzip" ,unzip)))
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'unpack
          (lambda _
            (zero? (system* "unzip" (assoc-ref %build-inputs "source")))))
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/picard/")))
              (chdir (string-append "picard-tools-" ,version))
              (install-file (string-append "htsjdk-" ,version ".jar") out)
              (install-file "libIntelDeflater.so" out)
              (install-file "picard-lib.jar" out)
              (install-file "picard.jar" out)))))))
   (home-page "http://broadinstitute.github.io/picard/")
    (synopsis "A set of Java command line tools for manipulating high-throughput
sequencing data (HTS) data and formats")
    (description "Picard comprises Java-based command-line utilities that
manipulate SAM files, and a Java API (HTSJDK) for creating new programs that
read and write SAM files. Both SAM text format and SAM binary (BAM) format are
supported.")
    (license license:expat)))

(define-public iq-tree
  (package
    (name "iq-tree")
    (version "1.6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Cibiv/IQ-TREE/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "11528sxv9hkmg20r2fy9zyq0rylbrhcjnh6cr61dwmx1wwnhj54n"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DIQTREE_FLAGS=omp")
       #:tests? #f))
    (inputs
     `(("eigen" ,eigen)
       ("zlib" ,zlib)))
    (home-page "http://www.iqtree.org/")
    (synopsis "Efficient software for phylogenomic inference")
    (description
     "This package provides software for phylogenomic inference.")
    (license license:gpl2)))

(define-public cat
  (package
   (name "cat")
   (version "5.0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/dutilh/CAT/archive/v"
                  version".tar.gz"))
            (sha256
             (base32
              "1gbq4vj0i7srylvb56ipmzcnh8zk34kfmyx76fbxh2cljp0p4k7x"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                  (share (string-append (assoc-ref outputs "out") "/share/CAT")))
              (mkdir-p bin)
              (mkdir-p share)
              (with-directory-excursion "CAT_pack"
                (install-file "CAT" bin)

                ;; Don't pollute the "bin" directory with Python libraries.
                (map (lambda (file)
                       (when (string-suffix? ".py" file)
                         (install-file file share)))
                     (find-files "."))

                ;; Make sure CAT can find its Python libraries.
                (wrap-program (string-append bin "/CAT")
                 `("PYTHONPATH" ":" = (,share "$PYTHONPATH"))))))))))
   (inputs
    `(("diamond" ,diamond)
      ("prodigal" ,prodigal)
      ("python" ,python)))
   (home-page "https://github.com/dutilh/CAT")
   (synopsis "Tool for taxonomic classification of contigs and metagenome-assembled genomes")
   (description "Contig Annotation Tool (CAT) and Bin Annotation Tool (BAT)
are pipelines for the taxonomic classification of long DNA sequences and
metagenome assembled genomes (MAGs/bins) of both known and (highly) unknown
microorganisms, as generated by contemporary metagenomics studies.  The core
algorithm of both programs involves gene calling, mapping of predicted ORFs
against the nr protein database, and voting-based classification of the entire
contig / MAG based on classification of the individual ORFs.  CAT and BAT can
be run from intermediate steps if files are formated appropriately")
   (license license:expat)))

(define-public r-ascat
  (let ((commit "9fb25feaae2d7d25a17f5eff7b99666ad7afbba8"))
    (package
     (name "r-ascat")
     (version (string-append "2.5.1-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Crick-CancerGenomics/ascat.git")
                    (commit commit)))
              (sha256
               (base32
                "02fxhqv4yf9dby8mmjb39fyqd141k3z4nhj0p8m2h4n7a476bdsc"))))
     (build-system r-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
         (add-after 'unpack 'move-to-ascat-dir
           (lambda _
             (chdir "ASCAT"))))))
     (propagated-inputs
      `(("r-rcolorbrewer" ,r-rcolorbrewer)))
     (home-page "https://github.com/Crick-CancerGenomics/ascat")
     (synopsis "ASCAT copy number R package")
     (description "This package provides the ASCAT R package that can be used
to infer tumour purity, ploidy and allele-specific copy number profiles.")
     (license license:gpl3))))

(define-public cgp-battenberg-r
  (package
   (name "cgp-battenberg-r")
   (version "2.2.8")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Wedge-Oxford/battenberg/archive/v"
                  version ".tar.gz"))
            (sha256
             (base32 "0gi9zv8clr795mzplf1d3dm5agc78xz40kmwckcjqaji4dnbcik1"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-devtools" ,r-devtools)
      ("r-readr" ,r-readr)
      ("r-doparallel" ,r-doparallel)
      ("r-ggplot2" ,r-ggplot2)
      ("r-rcolorbrewer" ,r-rcolorbrewer)
      ("r-gridextra" ,r-gridextra)
      ("r-gtools" ,r-gtools)
      ("r-ascat" ,r-ascat)))
   (home-page "https://github.com/Wedge-Oxford/battenberg")
   (synopsis "Battenberg R package for subclonal copy number estimation")
   (description "This package contains the Battenberg R package for subclonal
copy number estimation.")
   (license license:gpl3)))

(define-public cgp-battenberg-3.3.0
  (package
    (name "cgp-battenberg")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/cgpBattenberg/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17rrciv8c8vdvcx4yljkkl8rlzlpasrnl0i2c0q72zxvzgh9c8z3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; The Perl in Guix does not support threads.
         ;; The forks module is a drop-in replacement for it, so it
         ;; is easier to use that instead of recompiling Perl.
         (add-after 'unpack 'enable-threads
           (lambda _
             (substitute* "perl/bin/battenberg.pl"
               (("use strict;") "use forks;\nuse strict;"))))
         (add-before 'build 'move-to-subdirectory
           (lambda _
             (chdir "perl")))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (system* "perl" "Makefile.PL"
                      (string-append "PREFIX=" (assoc-ref outputs "out")))
             (system* "make")))
         (add-before 'reset-gzip-timestamps 'fix-permissions
           (lambda* (#:key outputs #:allow-other-keys)
             (chmod (string-append
                     (assoc-ref outputs "out")
                     "/lib/perl5/site_perl/5.30.2/"
                     "auto/share/module/Sanger-CGP-Battenberg-Implement"
                     "/battenberg/probloci.txt.gz") #o644)))
         (add-after 'reset-gzip-timestamps 'fix-permissions-after
           (lambda* (#:key outputs #:allow-other-keys)
             (chmod (string-append
                     (assoc-ref outputs "out")
                     "/lib/perl5/site_perl/5.30.2/"
                     "auto/share/module/Sanger-CGP-Battenberg-Implement"
                     "/battenberg/probloci.txt.gz") #o444))))))
    (propagated-inputs
     `(("allelecount" ,allelecount)
       ("htslib" ,htslib)
       ("which" ,which)
       ("pcap-core" ,pcap-core)
       ("perl-bio-pipeline-comparison" ,perl-bio-pipeline-comparison)
       ("impute2-bin" ,impute2-bin)
       ("cgpvcf" ,cgpvcf)
       ("perl-const-fast" ,perl-const-fast)
       ("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)
       ("perl-bio-db-hts" ,perl-bio-db-hts)
       ("bioperl-minimal" ,bioperl-minimal)
       ("perl-ipc-system-simple" ,perl-ipc-system-simple)
       ("perl-file-which" ,perl-file-which)
       ("perl-log-message" ,perl-log-message)
       ("perl-term-ui" ,perl-term-ui)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-forks" ,perl-forks)
       ("perl-bsd-resource" ,perl-bsd-resource)
       ("perl-sub-identify" ,perl-sub-identify)
       ("perl-autodie" ,perl-autodie)
       ("perl-archive-extract" ,perl-archive-extract)
       ("perl" ,perl)
       ("cgp-battenberg-r" ,cgp-battenberg-r)
       ("lsof" ,lsof)))
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-module-build" ,perl-module-build)
       ("perl-file-sharedir-install" ,perl-file-sharedir-install)))
    (home-page "https://github.com/cancerit/cgpBattenberg")
    (synopsis "Battenberg algorithm and associated implementation script")
    (description "This package provides a perl wrapper and an R program for the
Battenberg algorithm that can detect subclonality and copy number in matched
NGS data.")
    (license license:gpl3+)))

(define-public cgp-battenberg
  (package
    (name "cgp-battenberg")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/cgpBattenberg/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11g1ryyfi4k5cbfp25dam9kl7wx1c3pqg2247ldhczk872mbcgz6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; The Perl in Guix does not support threads.
         ;; The forks module is a drop-in replacement for it, so it
         ;; is easier to use that instead of recompiling Perl.
         (add-after 'unpack 'enable-threads
           (lambda _
             (substitute* "perl/bin/battenberg.pl"
               (("use strict;") "use forks;\nuse strict;"))))
         (add-before 'build 'move-to-subdirectory
           (lambda _
             (chdir "perl")))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (system* "perl" "Makefile.PL"
                      (string-append "PREFIX=" (assoc-ref outputs "out")))
             (system* "make")))
         (add-before 'reset-gzip-timestamps 'fix-permissions
           (lambda* (#:key outputs #:allow-other-keys)
             (chmod (string-append
                     (assoc-ref outputs "out")
                     "/lib/perl5/site_perl/5.30.2/"
                     "auto/share/module/Sanger-CGP-Battenberg-Implement"
                     "/battenberg/probloci.txt.gz") #o644)))
         (add-after 'reset-gzip-timestamps 'fix-permissions-after
           (lambda* (#:key outputs #:allow-other-keys)
             (chmod (string-append
                     (assoc-ref outputs "out")
                     "/lib/perl5/site_perl/5.30.2/"
                     "auto/share/module/Sanger-CGP-Battenberg-Implement"
                     "/battenberg/probloci.txt.gz") #o444))))))
    (propagated-inputs
     `(("allelecount" ,allelecount)
       ("htslib" ,htslib)
       ("which" ,which)
       ("pcap-core" ,pcap-core)
       ("perl-bio-pipeline-comparison" ,perl-bio-pipeline-comparison)
       ("impute2-bin" ,impute2-bin)
       ("cgpvcf" ,cgpvcf)
       ("perl-const-fast" ,perl-const-fast)
       ("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)
       ("perl-bio-db-hts" ,perl-bio-db-hts)
       ("bioperl-minimal" ,bioperl-minimal)
       ("perl-ipc-system-simple" ,perl-ipc-system-simple)
       ("perl-file-which" ,perl-file-which)
       ("perl-log-message" ,perl-log-message)
       ("perl-term-ui" ,perl-term-ui)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-forks" ,perl-forks)
       ("perl-bsd-resource" ,perl-bsd-resource)
       ("perl-sub-identify" ,perl-sub-identify)
       ("perl-autodie" ,perl-autodie)
       ("perl-archive-extract" ,perl-archive-extract)
       ("perl" ,perl)))
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-module-build" ,perl-module-build)
       ("perl-file-sharedir-install" ,perl-file-sharedir-install)))
    (home-page "https://github.com/cancerit/cgpBattenberg")
    (synopsis "Battenberg algorithm and associated implementation script")
    (description "This package provides a perl wrapper and an R program for the
Battenberg algorithm that can detect subclonality and copy number in matched
NGS data.")
    (license license:gpl3+)))

(define-public cgp-pindel
  (package
    (name "cgp-pindel")
    (version "2.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/cgpPindel/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "1vadlyffabqj696k9nnzqprxn5avf0a5iykpqjxmw8n2180lppvw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'move-to-subdirectory
           (lambda _
             (chdir "perl")))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (system* "perl" "Makefile.PL"
                      (string-append "PREFIX=" (assoc-ref outputs "out")))
             (system* "make"))))))
    (propagated-inputs
     `(("perl" ,perl)
       ("pcap-core" ,pcap-core)
       ("cgpvcf" ,cgpvcf)
       ("bioperl-minimal" ,bioperl-minimal)
       ("perl-bio-db-hts" ,perl-bio-db-hts)
       ("perl-const-fast" ,perl-const-fast)
       ("perl-file-which" ,perl-file-which)
       ("perl-pod-coverage" ,perl-pod-coverage)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-term-ui" ,perl-term-ui)
       ("perl-log-message" ,perl-log-message)
       ("perl-ipc-system-simple" ,perl-ipc-system-simple)
       ("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)
       ("perl-devel-cover" ,perl-devel-cover)))
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-module-build" ,perl-module-build)
       ("perl-file-sharedir-install" ,perl-file-sharedir-install)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:agpl3+)))

(define-public last
  (package
   (name "last")
   (version "1080")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://last.cbrc.jp/last-" version ".zip"))
            (sha256
               (base32
                "0az6xiqkbdcq858m1dlwvf7f7pa5fjldckkawcj8a38a2fq9drds"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("unzip" ,unzip)
      ("sed" ,sed)))
   (inputs
    `(("zlib" ,zlib)))
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-after 'unpack 'set-c-compiler
         (lambda* (#:key outputs #:allow-other-keys)
           (substitute* "src/makefile"
            (("# -Wconversion") "CC=gcc"))
           (substitute* "makefile"
            (("prefix = /usr/local")
             (string-append
              "prefix = " (assoc-ref outputs "out")))))))))
   (home-page "http://last.cbrc.jp/")
   (synopsis "Genome-scale sequence comparison")
   (description "")
   (license license:gpl3)))

(define-public glibc-locales-2.27
  (package (inherit (make-glibc-locales glibc-2.27))
           (name "glibc-locales-2.27")))

(define-public glibc-locales-2.28
  (package (inherit (make-glibc-locales glibc-2.28))
           (name "glibc-locales-2.28")))

(define-public scan_for_matches
  (package
   (name "scan_for_matches")
   (version "0.0")
   (source (origin
            (method url-fetch)
            (uri "http://www.theseed.org/servers/downloads/scan_for_matches.tgz")
            (sha256
             (base32 "13ynw9i6j76884pdi249qhvgpvr6ii7hnfkwnllaryxxxwq7kcf6"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
          (lambda _
            (invoke "gcc" "-O2" "-o" "scan_for_matches"  "ggpunit.c" "scan_for_matches.c")))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "scan_for_matches" bin)))))))
   (home-page "https://blog.theseed.org/servers/2010/07/scan-for-matches.html")
   (synopsis "Utility for locating patterns in DNA")
   (description "This package provides a utility for locating patterns in DNA
 or protein FASTA files.")
   (license #f)))
