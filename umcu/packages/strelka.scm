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

;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.


;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.

(define-module (umcu packages strelka)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (umcu packages boost)
  #:use-module (umcu packages vcftools))

(define-public strelka-1.0.15
  (package
    (name "strelka")
    (version "1.0.15")
    (source (origin
      (method url-fetch)
      (uri (string-append
            ;;"ftp://strelka:''@ftp.illumina.com/v1-branch/v"
            ;;version "/strelka_workflow-" version ".tar.gz"))
            "https://sites.google.com/site/strelkasomaticvariantcaller/home/"
            "download/" name "_workflow-" version ".tar.gz"))
      (sha256
       (base32 "1cwad2wlhdk09702ivblfiyv921af0al7s1gm1dn2d3b0v31qrp2"))
      (patches (list (search-patch "strelka-disable-tests.patch")
                     (search-patch "strelka-disable-install.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'build-some-more
           (lambda _
             (with-directory-excursion "strelka"
               (zero? (system* "make" "-j" (number->string
                                            (parallel-job-count))
                               "install")))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perl-lib-dir (string-append out "/lib/perl5/site_perl/"
                                                 ,(package-version perl)))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (libexec (string-append out "/libexec")))

               ;; Substitute the binary directories for samtools and bgzip.
               (substitute* '("src/perl/bin/configureStrelkaWorkflow.pl"
                              "src/perl/libexec/callSomaticVariants.pl"
                              "src/perl/libexec/consolidateResults.pl")
                            (("my \\$samtoolsDir = File::Spec->catdir\\(\\$optDir,'samtools'\\);")
                             (string-append "my $samtoolsDir = \""
                                            (assoc-ref inputs "samtools") "/bin\";"))
                            (("my \\$samtoolsBin = File::Spec->catfile\\(\\$optDir,'samtools','samtools'\\);")
                             (string-append "my $samtoolsBin = \""
                                            (assoc-ref inputs "samtools")
                                            "/bin/samtools\";")))

               (substitute* "src/perl/libexec/consolidateResults.pl"
                 (("my \\$bgzipBin = File::Spec->catfile\\(\\$optDir,'tabix','bgzip'\\);")
                  (string-append "my $bgzipBin = \"" (assoc-ref inputs "htslib") "/bin/bgzip\";"))
                 (("my \\$getHeaderCmd = \"bash")
                  (string-append "my $getHeaderCmd = \"" (assoc-ref inputs "bash") "/bin/bash")))

               (mkdir-p perl-lib-dir)
               (mkdir-p lib)
               (mkdir-p libexec)

               ;; Instead of patching out $optDir throughout the code, we can create
               ;; an empty directory so that these checks pass.  We already patched the
               ;; path to samtools and bgzip, so this should be fine.
               (mkdir-p (string-append out "/opt/samtools"))

               (install-file "src/c++/libexec/countFastaBases" libexec)
               (install-file "src/perl/bin/configureStrelkaWorkflow.pl" bin)
               (install-file "src/perl/libexec/consolidateResults.pl" libexec)
               (install-file "src/perl/libexec/filterSomaticVariants.pl" libexec)
               (install-file "src/perl/libexec/callSomaticVariants.pl" libexec)
               (install-file "src/perl/lib/Utils.pm" perl-lib-dir)
               (install-file "strelka/src/bin/strelka2" bin)
               (install-file "strelka/src/bin/starling2" bin)
               (install-file "strelka/src/bin/strelkaSiteSimulator" bin)

               ;; Also add Utils.pm to the lib folder, because strelka manipulates
               ;; its own Perl path to search in this folder.
               (install-file "src/perl/lib/Utils.pm" lib)

               ;; The configureStrelkaWorkflow.pl script looks for the
               ;; strelka2 binary in the libexec directory.
               (system* "ln" "--symbolic"
                        (string-append bin "/strelka2")
                        (string-append libexec "/strelka2"))))))))
    (inputs
     `(("boost" ,boost)
       ("perl" ,perl)
       ("bash" ,bash)
       ("zlib" ,zlib)
       ("samtools" ,samtools)))
    (native-inputs
     `(("bash" ,bash)
       ("python" ,python-2)
       ("gcc" ,gcc-5)))
    (propagated-inputs
     `(("vcftools" ,vcftools)
       ("htslib" ,htslib)))
    (native-search-paths (package-native-search-paths perl))
    (home-page "https://sites.google.com/site/strelkasomaticvariantcaller/")
    (synopsis "Somatic variant calling workflow for matched tumor-normal samples")
    (description "Analysis package designed to detect somatic SNVs and small
indels from the aligned sequencing reads of matched tumor-normal samples")
    ;; WARNING: The license is "Illumina Open Source Software License 1".
    ;; This effectively makes it nonfree software.
    (license license:non-copyleft)))

(define-public strelka-1.0.14
  (package (inherit strelka-1.0.15)
    (name "strelka")
    (version "1.0.14")
    (source (origin
      (method url-fetch)
      (uri (string-append
        "ftp://strelka:''@ftp.illumina.com/v1-branch/v"
        version "/strelka_workflow-" version ".tar.gz"))
      (sha256
        (base32 "0f9g2pkr1f7s4r8sxl53jxr2cjpyx53zf3va0jj8fxzavxiwmbmk"))
      (patches (list (search-patch "strelka-disable-tests.patch")
                     (search-patch "strelka-disable-install.patch")))))
    (propagated-inputs
     `(("vcftools" ,vcftools-0.1.14)
       ("htslib" ,htslib)))))

(define-public codemin
  (package
   (name "codemin")
   (version "1.0.5")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Illumina/strelka/raw/"
                  "5a993884687f2d92f794109e171d0bdeb95e504d"
                  "/redist/CodeMin-1.0.5.tar.bz2"))
            (sha256
             (base32 "1y8wsli1q626i80p3dmrc65p77ch164hj2sbxv497i9y89kvk35s"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((include-dir (string-append
                                (assoc-ref outputs "out") "/include")))
              (mkdir-p include-dir)
              (copy-recursively "include" include-dir)))))))
   (home-page "https://github.com/Illumina/strelka/tree/master/redist")
   (synopsis "Set of lightweight minimization functions.")
   (description "The CodeMin minimization library provides a set of lightweight
minimization functions originally developed for the CodeAxe phylogenetic
analysis package.")
   ;; MIT license.
   (license license:expat)))

(define-public strelka-2.9.2
  (package
   (name "strelka")
   (version "2.9.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Illumina/strelka/releases/download/v"
                  version "/strelka-" version ".release_src.tar.bz2"))
            (sha256
             (base32 "19bq2wzlxmnv8rx112y8z0sfvgsajnd0m945njmfy9p170qjqr27"))
            (patches
             (list (search-patch "strelka2-unbundle-dependencies.patch")))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'unbundle-dependencies
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "redist/CMakeLists.txt"
              ;; HTSlib
              (("superset\\(HTSLIB_DIR \"\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/\\$\\{HTSLIB_PREFIX\\}\"\\)")
               (format #f "superset(HTSLIB_DIR \"~a/bin\")" (assoc-ref inputs "htslib")))
              (("superset\\(HTSLIB_LIBRARY \"\\$\\{HTSLIB_DIR\\}/libhts.a\"\\)")
               (format #f "superset(HTSLIB_LIBRARY \"~a/lib/libhts.so\")"
                       (assoc-ref inputs "htslib")))
              ;; SAMtools
              (("set\\(SAMTOOLS_DIR \"\\$\\{CMAKE_CURRENT_BINARY_DIR}/\\$\\{SAMTOOLS_PREFIX\\}\"\\)")
               (format #f "set(SAMTOOLS_DIR \"~a/bin\")"
                       (assoc-ref inputs "samtools")))
              (("set\\(SAMTOOLS_LIBRARY \"\\$\\{SAMTOOLS_DIR\\}/libbam.a\"\\)")
               (format #f "set(SAMTOOLS_LIBRARY \"~a/lib/libbam.a\")"
                       (assoc-ref inputs "samtools"))))))
        (add-after 'install 'install-shared-libraries
          (lambda* (#:key inputs outputs  #:allow-other-keys)
            (let ((libdir (string-append (assoc-ref outputs "out") "/lib")))
              (mkdir-p libdir)
              (map (lambda (file)
                     (copy-file file (string-append libdir "/" (basename file))))
                   (find-files "." "\\.so")))))
        (add-after 'install 'patch-python-bin
          (lambda* (#:key inputs outputs  #:allow-other-keys)
            (let ((patch-path (string-append (assoc-ref outputs "out") "/lib/python")))
              (substitute* (list (string-append patch-path "/makeRunScript.py")
                                 (string-append patch-path "/pyflow/pyflow.py"))
                (("/usr/bin/env python")
                 (string-append (assoc-ref inputs "python") "/bin/python")))))))))
   (inputs
    `(("boost" ,boost)
      ("perl" ,perl)
      ("bash" ,bash)
      ("zlib" ,zlib)
      ("samtools" ,samtools)
      ("rapidjson" ,rapidjson)
      ("codemin" ,codemin)
      ("curl" ,curl)
      ("xz" ,xz)
      ("openssl" ,openssl)
      ("samtools" ,samtools)
      ("zlib" ,zlib)
      ("python" ,python)))
   (native-inputs
    `(("bash" ,bash)
      ("python" ,python-2)
      ("doxygen" ,doxygen)
      ("graphviz" ,graphviz)))
   (propagated-inputs
    `(("vcftools" ,vcftools)
      ("htslib" ,htslib)))
   (native-search-paths (package-native-search-paths perl))
   (home-page "https://github.com/Illumina/strelka")
   (synopsis "Small variant caller")
   (description "Strelka2 is a fast and accurate small variant caller optimized
for analysis of germline variation in small cohorts and somatic variation in
tumor/normal sample pairs.  The germline caller employs an efficient tiered
haplotype model to improve accuracy and provide read-backed phasing, adaptively
selecting between assembly and a faster alignment-based haplotyping approach at
each variant locus.  The germline caller also analyzes input sequencing data
using a mixture-model indel error estimation method to improve robustness to
indel noise.  The somatic calling model improves on the original Strelka method
for liquid and late-stage tumor analysis by accounting for possible tumor cell
contamination in the normal sample.  A final empirical variant re-scoring step
using random forest models trained on various call quality features has been
added to both callers to further improve precision.")
   (license license:gpl3+)))

(define-public strelka strelka-2.9.2)
