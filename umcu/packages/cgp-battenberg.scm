
(define-module (umcu packages cgp-battenberg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages statistics)
  #:use-module (umcu packages samtools)
  #:use-module (umcu packages vcftools))

(define-public perltidy
  (package
    (name "perltidy")
    (version "20160302")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/perltidy/Perl-Tidy-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "19yw63yh5s3pq7k3nkw6nsamg5b8vvwyhgbizslgxg0mqgc4xl3d"))))
    (build-system perl-build-system)
    (home-page "http://perltidy.sourceforge.net/")
    (synopsis "")
    (description "")
    (license (package-license perl))))

(define-public perl-data-uuid
  (package
    (name "perl-data-uuid")
    (version "1.221")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Data-UUID-" version ".tar.gz"))
       (sha256
        (base32
         "0rw60wib0mj5z0v909mplh750y40hzyzf4z0b6h4ajxplyiv5irw"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Data-UUID")
    (synopsis "Universally Unique Identifiers generator")
    (description "Data::UUID provides a framework for generating v3 UUIDs
(Universally Unique Identifiers, also known as GUIDs (Globally Unique
Identifiers).  A UUID is 128 bits long, and is guaranteed to be different from
all other UUIDs/GUIDs generated until 3400 CE.")
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
   (inputs
    `(("perl-data-uuid" ,perl-data-uuid)
      ("perl-datetime" ,perl-datetime)
      ("perl-const-fast" ,perl-const-fast)))
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
      ()))
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
           (lambda* (#:key outputs #:allow-other-keys)
             (system* "perl" "Makefile.PL"
                      (string-append "PREFIX=" (assoc-ref outputs "out")))
             (system* "make"))))))
    (inputs
     `(("htslib" ,htslib)
       ("perl-bio-db-hts" ,perl-bio-db-hts)
       ("perl-const-fast" ,perl-const-fast)
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
