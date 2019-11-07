;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (umcu packages bioconductor)
  #:use-module (umcu packages hmf))

(define-public perl-storable
  (package
   (name "perl-storable")
   (version "2.51")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://cpan/authors/id/A/AM/AMS/Storable-"
           version ".tar.gz"))
     (sha256
      (base32 "1gphq8yhqzrwwlx2i5a8914ccw41ywmpl7gc648s5frb269bfrm5"))))
   (build-system perl-build-system)
   (home-page "http://search.cpan.org/dist/Storable")
   (synopsis "Persistence for Perl data structures")
   (description "")
   (license #f)))

(define-public perl-set-intervaltree
  (package
   (name "perl-set-intervaltree")
   (version "0.10")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/B/BE/BENBOOTH/Set-IntervalTree-"
           version ".tar.gz"))
     (sha256
      (base32 "1g1yam3fwl11wvy489yhhfzrfdlqaj1dh7pgks3myjq71p7rrgg3"))))
   (build-system perl-build-system)
   (home-page "http://search.cpan.org/dist/Set-IntervalTree")
   (synopsis "Perform range-based lookups on sets of ranges.")
   (description "")
   (license #f)))

(define-public perl-math-vecstat
  (package
    (name "perl-math-vecstat")
    (version "0.08")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/A/AS/ASPINELLI/Math-VecStat-"
                    version ".tar.gz"))
              (sha256
               (base32
                "03bdcl9pn2bc9b50c50nhnr7m9wafylnb3v21zlch98h9c78x6j0"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Math-VecStat")
    (synopsis "Some basic numeric stats on vectors")
    (description "")
    (license #f)))

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

(define-public perl-bio-db-hts
  (package
    (name "perl-bio-db-hts")
    (version "2.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cpan.metacpan.org/authors/id/R/RI/RISHIDEV/Bio-DB-HTS-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0z5qvs1xx38nwfr2fqk582apj1jxrbabjxcawp38swj8b5lnqrdb"))))
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
   (version "1.2911")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cpan.metacpan.org/authors/id/J/JH/JHI/BSD-Resource-"
           version ".tar.gz"))
     (sha256
      (base32 "0g8c7825ng2m0yz5sy6838rvfdl8j3vm29524wjgf66ccfhgn74x"))))
   (build-system perl-build-system)
   (home-page "http://search.cpan.org/dist/BSD-Resource")
   (synopsis "BSD process resource limit and priority functions")
   (description "")
   (license #f)))

(define-public perl-acme-damn
  (package
  (name "perl-acme-damn")
  (version "0.08")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://cpan.metacpan.org/authors/id/I/IB/IBB/Acme-Damn-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "03kykdsz3fk5ppb9g92pvnif67zlk501finrwi1csbcizw1js39i"))))
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
  (version "0.23")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/L/LB/LBAXTER/Sys-SigAction-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0lykjlq5dsf7z927lpllzixd953izi3w7bg2pgy32h2k8n9nrvy4"))))
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
    (version "1.236")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MI/MITHALDU/PPI-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1v4a622h19d2d6m070jcxn47siqma9g9ly4fcrnrlz8bkhs4nry6"))))
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
    (propagated-inputs
     `(("perl-module-install" ,perl-module-install)))
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
    (propagated-inputs
     `(("perl-module-install" ,perl-module-install)))
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
    (propagated-inputs
     `(("perl-module-install" ,perl-module-install)))
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
    (version "1.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PJ/PJCJ/Devel-Cover-"
             version ".tar.gz"))
       (sha256
        (base32
         "0afkabxbbbwbnfh6b957cwxx1vcz00r4q49jbhcip3w6ib4pv60n"))))
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

(define-public hiddendomains
  (package
   (name "hiddendomains")
   (version "3.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://sourceforge/hiddendomains/hiddenDomains."
                  version ".tar.gz"))
            (sha256
             (base32 "17d8svkns7iwvg826pb75c92i3n9n77hcvcf0ialm6kxsqnxb8wq"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin   (string-append (assoc-ref outputs "out") "/bin"))
                  (share (string-append (assoc-ref outputs "out")
                                        "/share/hiddenDomains")))
              (mkdir-p bin)
              (for-each (lambda (file) (install-file file bin))
               '("binReads.pl"     "centersToGEM.pl" "domainsMergeToBed.pl"
                 "domainsToBed.pl" "hiddenDomains"   "peakCenters"))
              (mkdir-p share)
              (install-file "hiddenDomains.R" share)))))))
   (inputs
    `(("perl" ,perl)))
   (propagated-inputs
    `(("r-depmixS4" ,r-depmixs4)
      ("r-hiddenmarkov" ,r-hiddenmarkov)))
   (home-page "http://hiddendomains.sourceforge.net")
   (synopsis "Programs used to identify enrichment of ChIP-seq reads")
   (description "hiddenDomains is a suite of programs used to identify
significant enrichment of ChIP-seq reads that span large domains, like
HK27me3.  The input data can be in BAM format, or in a tab-delimited
'reads per bin' format described below.  The output is a BED formatted
file the lists the enriched domains and their posterior probabilities.")
   (license #f)))

(define-public perl-prokka
  (package
   (name "perl-prokka")
   (version "1.14.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/tseemann/prokka/archive/v"
           version ".tar.gz"))
     (sha256
      (base32
       "1d8v52mxr61pf8pg4vw72wlq6xlfm1m0b1ds14lzr5n2xz1lgdkh"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* '("bin/prokka"
                           "bin/prokka-abricate_to_fasta_db"
                           "bin/prokka-biocyc_to_fasta_db"
                           "bin/prokka-build_kingdom_dbs"
                           "bin/prokka-cdd_to_hmm"
                           "bin/prokka-clusters_to_hmm"
                           "bin/prokka-genbank_to_fasta_db"
                           "bin/prokka-genpept_to_fasta_db"
                           "bin/prokka-hamap_to_hmm"
                           "bin/prokka-make_tarball"
                           "bin/prokka-tigrfams_to_hmm"
                           "bin/prokka-uniprot_to_fasta_db")
             (("minced ")      (string-append (assoc-ref inputs "minced")   "/bin/minced "))
             (("blastp ")      (string-append (assoc-ref inputs "blast+")   "/bin/blastp "))
             (("hmmscan ")     (string-append (assoc-ref inputs "hmmer")    "/bin/hmmscan "))
             (("hmmpress ")    (string-append (assoc-ref inputs "hmmer")    "/bin/hmmpress "))
             (("\"parallel ")    (string-append "\"" (assoc-ref inputs "parallel") "/bin/parallel "))
             (("aragorn ")     (string-append (assoc-ref inputs "aragorn")  "/bin/aragorn "))
             (("prodigal ")    (string-append (assoc-ref inputs "prodigal") "/bin/prodigal "))
             (("cmscan ")      (string-append (assoc-ref inputs "infernal") "/bin/cmscan "))
             (("cmpress ")     (string-append (assoc-ref inputs "infernal") "/bin/cmpress "))
             (("makeblastdb ") (string-append (assoc-ref inputs "blast+")   "/bin/makeblastdb ")))

            (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
            (copy-recursively "bin" (string-append (assoc-ref outputs "out") "/bin"))
            (mkdir-p (string-append (assoc-ref outputs "out") "/share/prokka/db"))
            (copy-recursively "db" (string-append (assoc-ref outputs "out") "/share/prokka/db"))

            (for-each (lambda (program)
                        (wrap-program program
                          `("PERL5LIB" ":" = (,(getenv "PERL5LIB") "$PERL5LIB"))))
                      (find-files (string-append (assoc-ref outputs "out") "/bin"))))))))
   (inputs
    `(("perl" ,perl)
      ("perl-time-piece" ,perl-time-piece)
      ("perl-xml-simple" ,perl-xml-simple)
      ("perl-digest-md5" ,perl-digest-md5)
      ("perl-module-build" ,perl-module-build)
      ("perl-data-dumper" ,perl-data-dumper)
      ("bioperl-minimal" ,bioperl-minimal)
      ("perl-findbin-libs" ,perl-findbin-libs)
      ("perl-scalar-list-utils" ,perl-scalar-list-utils)
      ("parallel" ,parallel)
      ("aragorn" ,aragorn)
      ("blast+" ,blast+)
      ("hmmer" ,hmmer)
      ("prodigal" ,prodigal)
      ("infernal" ,infernal)
      ("minced" ,minced)
      ("findutils" ,findutils)
      ("icedtea" ,icedtea)))
   (native-search-paths
    (list (search-path-specification
           (variable "PROKKA_DBDIR")
           (files '("share/prokka/db")))))
   (home-page "https://github.com/tseemann/prokka")
   (synopsis "Rapid prokaryotic genome annotation")
   (description "This package provides tools for rapid prokaryotic
genome annotation.")
   (license license:gpl3)))
