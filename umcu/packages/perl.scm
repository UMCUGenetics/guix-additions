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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system r)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (umcu packages bioconductor)
  #:use-module (umcu packages bioinformatics)
  #:use-module (umcu packages hmf)
  #:use-module (umcu packages perl))


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
      ("icedtea" ,icedtea-8)))
   (native-search-paths
    (list (search-path-specification
           (variable "PROKKA_DBDIR")
           (files '("share/prokka/db")))))
   (home-page "https://github.com/tseemann/prokka")
   (synopsis "Rapid prokaryotic genome annotation")
   (description "This package provides tools for rapid prokaryotic
genome annotation.")
   (license license:gpl3)))



(define-public cgp-cavemanwrapper-1.15.2
  (package
    (name "cgp-cavemanwrapper")
    (version "1.15.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/cgpCaVEManWrapper/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (list (search-patch "cgp-cavemanwrapper-fix-script.patch")))
              (sha256
               (base32
                "017b03j1sm64v6barbxx69420d3s1gx5nlcqg7rzn6mczj47g264"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'install)
         ;; The Perl in Guix does not support threads.
         ;; The forks module is a drop-in replacement for it, so it
         ;; is easier to use that instead of recompiling Perl.
         (add-after 'unpack 'enable-threads
           (lambda _
             (substitute* "bin/caveman_merge_results.pl"
               (("use strict;") "use forks;\nuse strict;"))))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin-dir (string-append (assoc-ref outputs "out") "/bin"))
                   (lib-dir (string-append (assoc-ref outputs "out")
                            "/lib/perl5/site_perl/5.28.0")))
               (mkdir-p bin-dir)
               (mkdir-p lib-dir)
               (install-file "bin/caveman_merge_results.pl" bin-dir)
               (copy-recursively "lib/Sanger" lib-dir)
               #t))))))
    (propagated-inputs
     `(("perl-file-path" ,perl-file-path)
       ("perl-file-which", perl-file-which)
       ("perl-const-fast", perl-const-fast)
       ("perl-capture-tiny", perl-capture-tiny)
       ("perl-ipc-system-simple", perl-ipc-system-simple)
       ("perl-autodie", perl-autodie)
       ("perl-try-tiny", perl-try-tiny)
       ("perl-carp", perl-carp)
       ("perl-forks", perl-forks)
       ("pcap-core" ,pcap-core)
       ("perl" ,perl)))
    (home-page "https://github.com/cancerit/cgpCaVEManWrapper")
    (synopsis "Reference implementation of CGP workflow for CaVEMan")
    (description "This package provides the reference implementation of CGP
workflow for CaVEMan SNV analysis.")
    (license license:agpl3+)))

(define-public perl-if
  (package
    (name "perl-if")
    (version "0.0608")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/X/XS/XSAWYERX/if-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "001p9ddi9hr1cnbbhng8ck9lf2dzh4sql01060krjkcwj486w81p"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/if")
    (synopsis
      "Perl if statement allows you to control the execution of your code based on conditions.")
    (description 
      "Perl if statement allows you to control the execution of your code based on conditions.")
    (license (package-license perl))))

(define-public perl-text-soundex
  (package
    (name "perl-text-soundex")
    (version "3.05")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/R/RJ/RJBS/Text-Soundex-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1vb0vg1109gfzaak74ynw5s00ml28f33j612g2lxw98b52s5bpgn"))))
    (build-system perl-build-system)
    (propagated-inputs `(("perl-if" ,perl-if)))
    (home-page
      "https://metacpan.org/release/Text-Soundex")
    (synopsis
      "Implementation of the soundex algorithm.")
    (description 
      "Soundex is a phonetic algorithm for indexing names by sound, as pronounced in English. The goal is for names with the same pronunciation to be encoded to the same representation so that they can be matched despite minor differences in spelling. Soundex is the most widely known of all phonetic algorithms and is often used (incorrectly) as a synonym for 'phonetic algorithm'. Improvements to Soundex are the basis for many modern phonetic algorithms. (Wikipedia, 2007)")
    (license (package-license perl))))
