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

;;;
;;; WARNING: This is work in progress.
;;;

(define-module (umcu packages cgp-battenberg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages web)
  #:use-module (umcu packages samtools)
  #:use-module (umcu packages vcftools)
  #:use-module (umcu packages perl)
  #:use-module (srfi srfi-1))

(define-public libmaus
  (package
   (name "libmaus")
   (version "0.0.196")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/gt1/libmaus/archive/"
                  version "-release-20150326095654.tar.gz"))
            (sha256
             (base32
              "0g92bl37ci8pzkgi2xnn2bck7y655jwcb1bm3mg42mj5lf5x2i5b"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (inputs
    `(("zlib" ,zlib)))
   (home-page "https://github.com/gt1/libmaus")
   (synopsis "Collection of bioinformatics data structures and algorithms")
   (description "This package contains a collection of bioinformatics data
structures and algorithms.  It provides I/O classes, bitio classes, text
indexing classes and BAM sequence alignment functionality.")
   (license license:gpl3+)))

(define-public biobambam
  (package
   (name "biobambam")
   (version "0.0.191")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/gt1/biobambam/archive/" version
                  "-release-20150401083643.tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "065fcwdh5sb6dg3mf5qk9w2818jxm27pvbv976qc00y7np2y2nqz"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f))
   (inputs
    `(("libmaus" ,libmaus)
      ("zlib" ,zlib)))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (home-page "https://github.com/gt1/biobambam")
   (synopsis "Collection of tools to work with BAM files")
   (description "This package contains the following programs: bamcollate2,
bammarkduplicates, bammaskflags, bamrecompress, bamsort, bamtofastq.")
   (license license:gpl3+)))

(define-public pcap-core
  (package
   (name "pcap-core")
   (version "3.5.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/ICGC-TCGA-PanCancer/PCAP-core/archive/v"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "06im5lf00jyghwmqjzb3dpglgjx7pi5ysda75fw8ygmj1fi5q8kj"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (system* "perl" "Makefile.PL"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))))))))
   (propagated-inputs
    `(("bwa" ,bwa)
      ("samtools" ,samtools)
      ("biobambam" ,biobambam)))
   (native-inputs
    `(("perl-module-install" ,perl-module-install)
      ("perl-module-build" ,perl-module-build)
      ("perl-file-sharedir-install" ,perl-file-sharedir-install)
      ("perl" ,perl)
      ("perltidy" ,perltidy)))
   (home-page "https://github.com/ICGC-TCGA-PanCancer/PCAP-core")
   (synopsis "NGS reference implementations and helper code for the ICGC/TCGA
Pan-Cancer Analysis Project")
   (description "")
   (license license:gpl2+)))

(define-public allelecount
  (package
    (name "allelecount")
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/alleleCount/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yza03nma4y5f34x61pdi902fkv9hzkfbpry9qs3nphjf6q5wcwj"))))
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
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (system* "perl" "Makefile.PL"
                      (string-append "PREFIX=" (assoc-ref outputs "out")))
             (system* "make")
             ;; Build the C alleleCounter program.
             (chdir "../c")
             (mkdir-p "bin")
             (substitute* "src/bam_access.c"
               (("\\#include <cram\\/cram.h>") "#include <htslib/cram.h>"))
             (system* "make")
             ;; Don't interfere with the "make install" command for the Perl
             ;; version.
             (chdir "../perl")))
         (add-after 'install 'install-allelecounter
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "../c/bin/alleleCounter" out)))))))
    (propagated-inputs
     `(("perl-const-fast" ,perl-const-fast)
       ("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)
       ("perl-bio-db-hts" ,perl-bio-db-hts)
       ("bioperl-minimal" ,bioperl-minimal)))
    (inputs
     `(("zlib" ,zlib)
       ("htslib" ,htslib)
       ("perl-pod-coverage" ,perl-pod-coverage)
       ("perl-file-which" ,perl-file-which)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-try-tiny" ,perl-try-tiny)
       ("samtools" ,samtools)))
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl" ,perl)))
    (home-page "https://github.com/cancerit/alleleCount")
    (synopsis "Support code for NGS copy number algorithms")
    (description "This package primarily exists to prevent code duplication
between some other projects, specifically AscatNGS and Battenburg.")
    (license license:agpl3+)))

(define-public cgpvcf
  (package
   (name "cgpvcf")
   (version "2.0.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/cancerit/cgpVcf/archive/v"
                                version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "009vpq2l1pxqfsvckapzxav5xr6kcjvg3krrfdx40qammcr4q1ak"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (system* "perl" "Makefile.PL"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))))))))
   (propagated-inputs
    `(("perl-bio-pipeline-comparison" ,perl-bio-pipeline-comparison)
      ("perl-const-fast" ,perl-const-fast)
      ("perl-data-uuid" ,perl-data-uuid)
      ("perl-datetime" ,perl-datetime)))
   (native-inputs
    `(("perl-module-install" ,perl-module-install)
      ("perl-module-build" ,perl-module-build)
      ("perl" ,perl)
      ("perltidy" ,perltidy)))
   (home-page "http://cancerit.github.io/cgpVcf/")
   (synopsis "")
   (description "")
   (license license:agpl3+)))

;; FIXME: This software cannot be trusted.
;; There is no source code, nor an intent to support any other version than
;; the "latest", which can change any moment.
(define-public impute2-bin
  (package
    (name "impute2")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://mathgen.stats.ox.ac.uk/impute/impute_v"
                    version "_x86_64_static.tgz"))
              (sha256
               (base32 "0py4m0asp1459nn1xsv552n3azqcfhipa4si8bzxs1a58q05jqcm"))))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out") "/bin")))
               (install-file "impute2" out)))))))
    (home-page "https://mathgen.stats.ox.ac.uk/impute/impute_v2.html")
    (synopsis "")
    (description "")
    (license #f)))

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
                     "/lib/perl5/site_perl/5.26.0/"
                     "auto/share/module/Sanger-CGP-Battenberg-Implement"
                     "/battenberg/probloci.txt.gz") #o644)))
         (add-after 'reset-gzip-timestamps 'fix-permissions-after
           (lambda* (#:key outputs #:allow-other-keys)
             (chmod (string-append
                     (assoc-ref outputs "out")
                     "/lib/perl5/site_perl/5.26.0/"
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
