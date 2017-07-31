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
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages web)
  #:use-module (umcu packages samtools)
  #:use-module (umcu packages vcftools)
  #:use-module (srfi srfi-1))

(define-public perl-data-uuid
  (package
    (name "perl-data-uuid")
    (version "1.221")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                                  "Data-UUID-" version ".tar.gz"))
              (sha256
               (base32
                "0rw60wib0mj5z0v909mplh750y40hzyzf4z0b6h4ajxplyiv5irw"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Data-UUID")
    (synopsis "Universally Unique Identifiers generator")
    (description "Data::UUID provides a framework for generating Universally
Unique Identifiers (UUIDs), also known as Globally Unique Identifiers (GUIDs).
A UUID is 128 bits long, and is guaranteed to be different from all other
UUIDs/GUIDs generated until 3400 CE.")
    (license (package-license perl))))

(define-public perl-const-fast
  (package
    (name "perl-const-fast")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "Const-Fast-" version ".tar.gz"))
       (sha256
        (base32
         "1nwlldgrx86yn7y6a53cqgvzm2ircsvxg1addahlcy6510x9a1gq"))))
    (inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-fatal" ,perl-test-fatal)))
    ;; Needed for tests.
    (native-inputs
     `(("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Const-Fast")
    (synopsis "Facility for creating read-only scalars, arrays, and hashes")
    (description "")
    (license (package-license perl))))

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

(define-public samtabix
  (let ((commit "10fd107909c1ac4d679299908be4262a012965ba"))
  (package
   (name "samtabix")
   (version (string-append "0-" (string-take commit 7)))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "http://genome-source.cse.ucsc.edu/samtabix.git")
                  (commit commit)))
            (sha256
             (base32 "0c1nj64l42v395sa84n7az43xiap4i6f9n9dfz4058aqiwkhkmma"))
            (file-name (string-append name "-" version "-checkout"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There is no test suite.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (inputs
    `(("zlib" ,zlib)))
   (native-inputs
    `(("perl" ,perl)
      ("python" ,python)
      ("luajit" ,luajit)))
   (home-page "http://genome-source.cse.ucsc.edu")
   (synopsis "")
   (description "")
   (license #f))))

(define-public kentutils
  (package
   (name "kentutils")
   (version "302.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/ENCODE-DCC/kentUtils/archive/v"
                  version ".tar.gz"))
            (sha256
             (base32 "0g184apsva5nwldkn7fzi6rinicrnyzc2xi68dk4qkn8cs5rq0fp"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (native-inputs
    `(("tcsh" ,tcsh)
      ("perl" ,perl)
      ("python" ,python-2)
      ("coreutils" ,coreutils)
      ("tcl" ,tcl)))
   (home-page "https://github.com/ENCODE-DCC/kentUtils")
   (synopsis "Jim Kent command line bioinformatic utilities")
   (description "Jim Kent command line bioinformatic utilities")
   (license #f)))

(define-public pcap-core
  (package
   (name "pcap-core")
   (version "2.5.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/ICGC-TCGA-PanCancer/PCAP-core/archive/v"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "1iq79acml7grs3gxvin8izmfl2lh1yrvhspr9a7mhc3221420afc"))))
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

(define-public perl-bio-db-hts
  (package
    (name "perl-bio-db-hts")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cpan.metacpan.org/authors/id/R/RI/RISHIDEV/Bio-DB-HTS-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1pg6nrjqykkar8nl7imgnbys3nw2p5320gxyl9qvsn6673m1pid7"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bioperl-minimal" ,bioperl-minimal)
       ("htslib" ,htslib)
       ("zlib" ,zlib)))
    (home-page "http://search.cpan.org/dist/Bio-DB-HTS")
    (synopsis "Perl interface to HTS library for DNA sequencing")
    (description "")
    (license license:asl2.0)))

(define-public allelecount
  (package
    (name "allelecount")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/alleleCount/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ds3d6p8finxbamawwwd87cj5p25bkxqlia85mahsdz1z9mfac64"))))
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

(define-public perl-extutils-manifest
  (package
    (name "perl-extutils-manifest")
    (version "1.70")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cpan.metacpan.org/authors/id/E/ET/ETHER/"
                    "ExtUtils-Manifest-" version ".tar.gz"))
              (sha256
               (base32
                "159bypwl8xpq1yi39prr49hl7x2xww5aj97nv169c8xja0h0dzzf"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/ExtUtils-Manifest")
    (synopsis "Utilities to write and check a MANIFEST file")
    (description "This package contains functions to manipulate a MANIFEST
file.  The package exports no functions by default.  The following are exported
on request: mkmanifest, manifind, manicheck, filecheck, fullcheck, skipcheck,
maniread, maniskip, manicopy, maniadd.")
    (license (package-license perl))))

(define-public perl-env-path
  (package
    (name "perl-env-path")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DS/DSB/Env-Path-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1qhmj15a66h90pjl2dgnxsb9jj3b1r5mpvnr87cafcl8g69z0jr4"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Env-Path")
    (synopsis "Advanced operations on path variables")
    (description "")
    (license #f)))

(define-public perl-bio-pipeline-comparison
  (package
    (name "perl-bio-pipeline-comparison")
    (version "1.123050")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AJ/AJPAGE/"
                           "Bio-Pipeline-Comparison-" version ".tar.gz"))
       (sha256
        (base32
         "081kn3zyi7zcwkaxrk5w52nkx7jrp0pwjcr8sai25l45711xli49"))))
    (build-system perl-build-system)
    ;; Only one test fails.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("htslib" ,htslib)
       ("which" ,which)))
    (native-inputs
     `(("perl-env-path" ,perl-env-path)
       ("perl-test-most" ,perl-test-most)))
    (inputs
     `(("bioperl-minimal" ,bioperl-minimal)
       ("perl-exception-class" ,perl-exception-class)
       ("perl-file-which" ,perl-file-which)
       ("perl-moose" ,perl-moose)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "http://search.cpan.org/dist/Bio-Pipeline-Comparison")
    (synopsis "Comparative assesment of variant calling (CAVar)")
    (description "")
    (license #f)))

(define-public perl-ipc-system-simple
  (package
    (name "perl-ipc-system-simple")
    (version "1.25")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/P/PJ/PJF/IPC-System-Simple-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0fsdb81shjj4hifyyzvj7vpkhq5jrfhlcpw2xbjfi1mqz8fsmdpi"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/IPC-System-Simple")
    (synopsis "Run commands simply, with detailed diagnostics")
    (description "")
    (license (package-license perl))))

(define-public perl-log-message
  (package
  (name "perl-log-message")
  (version "0.08")
  (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://cpan/authors/id/B/BI/BINGOS/Log-Message-"
                  version ".tar.gz"))
            (sha256
             (base32
              "0ipyk7zbvz31kf3mj5ahwi2cbcfy54s8387hx4cd29mg5bb7ssdx"))))
  (build-system perl-build-system)
  (home-page "http://search.cpan.org/dist/Log-Message")
  (synopsis "Powerful and flexible message logging mechanism")
  (description "")
  (license (package-license perl))))

(define-public perl-log-message-simple
  (package
  (name "perl-log-message-simple")
  (version "0.10")
  (source
   (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/B/BI/BINGOS/Log-Message-Simple-"
           version ".tar.gz"))
     (sha256
      (base32
       "15nxi935nfrf8dkdrgvcrf2qlai4pbz03yj8sja0n9mcq2jd24ma"))))
  (build-system perl-build-system)
  (inputs
   `(("perl-log-message" ,perl-log-message)))
  (home-page "http://search.cpan.org/dist/Log-Message-Simple")
  (synopsis "Simplified interface to Log::Message")
  (description "")
  (license (package-license perl))))

(define-public perl-term-ui
  (package
    (name "perl-term-ui")
    (version "0.46")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cpan.metacpan.org/authors/id/B/BI/BINGOS/Term-UI-"
             version ".tar.gz"))
       (sha256
        (base32
         "19p92za5cx1v7g57pg993amprcvm1az3pp7y9g5b1aplsy06r54i"))))
    (build-system perl-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("perl-log-message-simple" ,perl-log-message-simple)))
    (home-page "http://search.cpan.org/dist/Term-UI")
    (synopsis "User interfaces via Term::ReadLine made easy")
    (description "")
    (license (package-license perl))))

(define-public perl-bsd-resource
  (package
   (name "perl-bsd-resource")
   (version "1.2910")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cpan.metacpan.org/authors/id/J/JH/JHI/BSD-Resource-"
           version ".tar.gz"))
     (sha256
      (base32 "1gvgsg558vz3b9d9fqjpl3qam6nwmpbxpn25j0vhlhwbliv3szr9"))))
   (build-system perl-build-system)
   (home-page "http://search.cpan.org/dist/BSD-Resource")
   (synopsis "BSD process resource limit and priority functions")
   (description "")
   (license #f)))

(define-public perl-acme-damn
  (package
  (name "perl-acme-damn")
  (version "0.06")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://cpan.metacpan.org/authors/id/I/IB/IBB/Acme-Damn-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0vzf77pdlyqlshmvrmgcwj9p17l0v67gc2k82fj2r7gqp4rcj9br"))))
  (build-system perl-build-system)
  (inputs
    `(("perl-test-exception" ,perl-test-exception)))
  (home-page
    "http://search.cpan.org/dist/Acme-Damn")
  (synopsis "'Unbless' Perl objects.")
  (description "")
  (license #f)))

(define-public perl-sys-sigaction
  (package
  (name "perl-sys-sigaction")
  (version "0.21")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/L/LB/LBAXTER/Sys-SigAction-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1nw0rzf5za8yd8s2sbgnw478g5bdlsz7ck2mk2gynqfjdxx20i71"))))
  (build-system perl-build-system)
  (home-page
    "http://search.cpan.org/dist/Sys-SigAction")
  (synopsis
    "Perl extension for Consistent Signal Handling")
  (description "")
  (license (package-license perl))))

(define-public perl-forks
  (package
  (name "perl-forks")
  (version "0.36")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/R/RY/RYBSKEJ/forks-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "14srnq51n98aizdlg6lhzpzdqyjvxf5nfm431qiylvsc9zj29gk1"))))
  (build-system perl-build-system)
  (propagated-inputs
    `(("perl-acme-damn" ,perl-acme-damn)
      ("perl-devel-symdump" ,perl-devel-symdump)
      ("perl-list-moreutils" ,perl-list-moreutils)
      ("perl-sys-sigaction" ,perl-sys-sigaction)))
  (home-page "http://search.cpan.org/dist/forks")
  (synopsis "forks - emulate threads with fork")
  (description "")
  (license (package-license perl))))

(define-public perl-autodie
  (package
   (name "perl-autodie")
   (version "2.29")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cpan.metacpan.org/authors/id/P/PJ/PJF/autodie-"
           version ".tar.gz"))
     (sha256
      (base32
       "1gr9ab292xxman5zfyb3vbrrv88y9j51qlgq7z4pjh80wwbpkdzm"))))
   (build-system perl-build-system)
   (native-inputs
    `(("perl-import-into" ,perl-import-into)))
   (propagated-inputs
    `(("perl-forks" ,perl-forks)
      ;("perl-bsd-resource" ,perl-bsd-resource)
      ;("perl-ipc-system-simple" ,perl-ipc-system-simple)
      ("perl-sub-identify" ,perl-sub-identify)))
   (home-page "http://search.cpan.org/dist/autodie")
   (synopsis "Replace functions with ones that succeed or die with lexical scope")
   (description "")
   (license (package-license perl))))

(define-public perl-archive-extract
  (package
  (name "perl-archive-extract")
  (version "0.76")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://cpan.metacpan.org/authors/id/B/BI/BINGOS/Archive-Extract-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1z2chz7a5q6024h9rmzpq0z53x0jw7983ia5k1yxsih3lw60irws"))))
  (build-system perl-build-system)
  (home-page
    "http://search.cpan.org/dist/Archive-Extract")
  (synopsis "Generic archive extracting mechanism")
  (description "")
  (license (package-license perl))))

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
                     "/lib/perl5/site_perl/5.24.0/"
                     "auto/share/module/Sanger-CGP-Battenberg-Implement"
                     "/battenberg/probloci.txt.gz") #o644)))
         (add-after 'reset-gzip-timestamps 'fix-permissions-after
           (lambda* (#:key outputs #:allow-other-keys)
             (chmod (string-append
                     (assoc-ref outputs "out")
                     "/lib/perl5/site_perl/5.24.0/"
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

(define-public perl-browser-open
  (package
    (name "perl-browser-open")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/C/CF/CFRANKS/Browser-Open-"
             version ".tar.gz"))
       (sha256
        (base32
         "0rv80n5ihy9vnrzsc3l7wlk8880cwabiljrydrdnxq1gg0lk3sxc"))))
    (build-system perl-build-system)
    (home-page
     "http://search.cpan.org/dist/Browser-Open")
    (synopsis "open a browser in a given URL")
    (description "")
    (license (package-license perl))))

(define-public perl-parallel-iterator
  (package
    (name "perl-parallel-iterator")
    (version "1.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AN/ANDYA/Parallel-Iterator-"
             version ".tar.gz"))
       (sha256
        (base32
         "1x252cqzcyxkmf8p5dw34ais47ci1ldv2ds02m7a2ijpryam0jg8"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-module-build" ,perl-module-build)
       ("perl-file-sharedir-install" ,perl-file-sharedir-install)))
    (home-page
     "http://search.cpan.org/dist/Parallel-Iterator")
    (synopsis "Simple parallel execution")
    (description "")
    (license (package-license perl))))

(define-public perl-ppi
  (package
    (name "perl-ppi")
    (version "1.220")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MI/MITHALDU/PPI-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "10rbc3kq6qb17vcqdrb5473s55b69w3zb2xga79kcnnrwx8bw58y"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("perl-class-inspector" ,perl-class-inspector)
       ("perl-file-remove" ,perl-file-remove)
       ("perl-test-nowarnings" ,perl-test-nowarnings)
       ("perl-test-object" ,perl-test-object)
       ("perl-test-subcalls" ,perl-test-subcalls)))
    (inputs
     `(("perl-clone" ,perl-clone)
       ("perl-io-string" ,perl-io-string)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-params-util" ,perl-params-util)
       ("perl-task-weaken" ,perl-task-weaken)))
    (home-page "http://search.cpan.org/dist/PPI")
    (synopsis "Parse, Analyze and Manipulate Perl (without perl)")
    (description "")
    (license (package-license perl))))

(define-public perl-test-subcalls
  (package
    (name "perl-test-subcalls")
    (version "1.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AD/ADAMK/Test-SubCalls-"
             version ".tar.gz"))
       (sha256
        (base32
         "0w3fppif2pplbw8l1y3xc3vr1z016x02vdnvwadxff53gm2v0d53"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-hook-lexwrap" ,perl-hook-lexwrap)))
    (home-page "http://search.cpan.org/dist/Test-SubCalls")
    (synopsis "Track the number of times subs are called")
    (description "")
    (license (package-license perl))))

(define-public perl-hook-lexwrap
  (package
    (name "perl-hook-lexwrap")
    (version "0.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/ET/ETHER/Hook-LexWrap-"
             version ".tar.gz"))
       (sha256
        (base32
         "0bgc6w8zs45n6ksgk0zisn9a2vcr3lmzipkan2a94kzrk1gxq2xn"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Hook-LexWrap")
    (synopsis "Lexically scoped subroutine wrappers")
    (description "")
    (license (package-license perl))))

(define-public perl-test-object
  (package
    (name "perl-test-object")
    (version "0.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AD/ADAMK/Test-Object-"
             version ".tar.gz"))
       (sha256
        (base32
         "0ah3c7nn4zpg4r7ca00zk5c6xc7zcaxxjyvcc5fxqa4r0cdsjhni"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-Object")
    (synopsis "Thoroughly testing objects via registered handlers")
    (description "")
    (license (package-license perl))))

(define-public perl-ppi-html
  (package
    (name "perl-ppi-html")
    (version "1.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AD/ADAMK/PPI-HTML-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "04f5sfrb6ckfdd3lnyipmky9mdgsxr5b724sp1xaszx86d09c9l4"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("perl-css-tiny" ,perl-css-tiny)
       ("perl-params-util" ,perl-params-util)
       ("perl-ppi" ,perl-ppi)))
    (home-page
     "http://search.cpan.org/dist/PPI-HTML")
    (synopsis
     "Generate syntax-hightlighted HTML for Perl using PPI")
    (description "")
    (license (package-license perl))))

(define-public perl-css-tiny
  (package
    (name "perl-css-tiny")
    (version "1.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/C/CH/CHORNY/CSS-Tiny-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1yjjn4li8v3d51l7jgrbbkhjdpfm9mmira2xfgf3s58wlkk9vx38"))))
    (build-system perl-build-system)
    (home-page
     "http://search.cpan.org/dist/CSS-Tiny")
    (synopsis
     "Read/Write .css files with as little code as possible")
    (description "")
    (license (package-license perl))))

(define-public perl-devel-cover
  (package
    (name "perl-devel-cover")
    (version "1.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PJ/PJCJ/Devel-Cover-"
             version ".tar.gz"))
       (sha256
        (base32
         "0m20120c0454b0kmf9zzc5h4wjbgrkdpj4h9ygvbgd3kpgyrwbgv"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("perl-test-differences" ,perl-test-differences)))
    (inputs
     `(("perl-browser-open" ,perl-browser-open)
       ("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-class-xsaccessor" ,perl-class-xsaccessor)
       ("perl-moo" ,perl-moo)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-parallel-iterator" ,perl-parallel-iterator)
       ("perltidy" ,perltidy)
       ("perl-pod-coverage" ,perl-pod-coverage)
       ("perl-ppi-html" ,perl-ppi-html)
       ("perl-template-toolkit" ,perl-template-toolkit)
       ("perl-test-differences" ,perl-test-differences)))
    (home-page
     "http://search.cpan.org/dist/Devel-Cover")
    (synopsis "Code coverage metrics for Perl")
    (description "")
    (license (package-license perl))))

(define-public cgp-pindel
  (package
    (name "cgp-pindel")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/cgpPindel/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0qykaqi0fr2i75bw8zdkpw1i7crqzfsah51329cnwmdl06zcfbi6"))))
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
