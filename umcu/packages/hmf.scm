;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;;
;;; This file is not officially part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (umcu packages hmf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system perl)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (umcu packages bioconductor)
  #:use-module (umcu packages boost)
  #:use-module (umcu packages bwa)
  #:use-module (umcu packages contra)
  #:use-module (umcu packages circos)
  #:use-module (umcu packages delly)
  #:use-module (umcu packages fastqc)
  #:use-module (umcu packages freebayes)
  #:use-module (umcu packages freec)
  #:use-module (umcu packages gatk)
  #:use-module (umcu packages genenetwork)
  #:use-module (umcu packages igvtools)
  #:use-module (umcu packages king)
  #:use-module (umcu packages picard)
  #:use-module (umcu packages plink)
  #:use-module (umcu packages sambamba)
  #:use-module (umcu packages samtools)
  #:use-module (umcu packages snpeff)
  #:use-module (umcu packages strelka)
  #:use-module (umcu packages varscan)
  #:use-module (umcu packages vcflib)
  #:use-module (umcu packages vcftools))

(define-public maven-bin
  ;; XXX: This package is only a binary inclusion of Maven.  It is different
  ;; from any other Guix package and you should NOT use this package.
  (package
   (name "maven")
   (version "3.5.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://apache.cs.uu.nl/maven/maven-3/" version
                                "/binaries/apache-maven-" version "-bin.tar.gz"))
            (sha256
             (base32 "0d7hjnj77hc7qqnnfmqlwij8n6pcldfavvd6lilvv5ak4hci9fdy"))))
   ;; We use the GNU build system mainly for its patch-shebang phases.
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is just copying a binary, so no tests to perform.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; No configuration, just copying.
        (delete 'build)     ; No building, just copying.
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((outdir (assoc-ref outputs "out")))
              (mkdir-p (string-append outdir))
              (copy-recursively "." outdir)
              (delete-file (string-append outdir "/README.txt"))
              (delete-file (string-append outdir "/NOTICE"))
              (delete-file (string-append outdir "/LICENSE"))))))))
   (propagated-inputs
    `(("which" ,which)))
   (home-page "https://maven.apache.org/")
   (synopsis "Build and dependency management tool for Java")
   (description "Apache Maven is a software project management and comprehension tool.
Based on the concept of a project object model (POM), Maven can manage a project's
build, reporting and documentation from a central piece of information.")
   (license license:asl2.0)))

(define-public hmftools
  (let ((commit "5cdd9f04ba20339083fbd1e7a1a5b34ec2596456"))
    (package
     (name "hmftools")
     (version (string-take commit 7))
     (source (origin
              (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hartwigmedical/hmftools.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1qkm8pcg41j1nhkyz3m9fcdsv6pcxq6gwldbshd7g40kf4x01ps5"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests? #f ; Tests are run in the install phase.
        #:phases
        (modify-phases %standard-phases
          (delete 'configure) ; Nothing to configure
          (add-after 'unpack 'disable-database-modules
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "pom.xml"
                ;; Disable the patient-db module because it needs a running
                ;; MySQL database.
                (("<module>patient-db</module>")
                 "<!-- <module>patient-db</module> -->")
                ;; Disable purity-ploidy-estimator because it needs patient-db.
                (("<module>purity-ploidy-estimator</module>")
                 "<!-- <module>purity-ploidy-estimator</module> -->")
                ;; The following modules fail to build due to a dependency
                ;; on itself.
                (("<module>health-checker</module>")
                 "<!-- <module>health-checker</module> -->")
                (("<module>patient-reporter</module>")
                 "<!-- <module>patient-reporter</module> -->"))))
          (replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((build-dir (getcwd))
                     (settings-dir (string-append build-dir "/mvn"))
                     (settings (string-append settings-dir "/settings.xml"))
                     (m2-dir (string-append build-dir "/m2/repository")))

                ;; Set JAVA_HOME to help maven find the JDK.
                (setenv "JAVA_HOME" (string-append (assoc-ref inputs "icedtea")
                                                   "/jre"))

                (mkdir-p m2-dir)
                (mkdir-p settings-dir)

                ;; Unpack the dependencies downloaded using maven.
                (with-directory-excursion m2-dir
                  (zero? (system* "tar" "xvf" (assoc-ref inputs "maven-deps"))))

                ;; Because the build process does not have a home directory in
                ;; which the 'm2' directory can be created (the directory
                ;; that will contain all downloaded dependencies for maven),
                ;; we need to set that directory to some other path.  This is
                ;; done using an XML configuration file of which a minimal
                ;; variant can be found below.
                (with-output-to-file settings
                  (lambda _
                    (format #t "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<settings xmlns=\"http://maven.apache.org/SETTINGS/1.0.0\"
          xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
          xsi:schemaLocation=\"http://maven.apache.org/SETTINGS/1.0.0 http://maven.apache.org/xsd/settings-1.0.0.xsd\">
<localRepository>~a</localRepository>
</settings>" m2-dir)))

                ;; Compile using maven's compile command.
                (let ((command (format #f "mvn compile --offline --global-settings ~s" settings)))
                  (format #t "Running: ~a" command)
                  (zero? (system command))))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((build-dir (getcwd))
                     (settings (string-append build-dir "/mvn/settings.xml"))
                     (output-dir (string-append (assoc-ref outputs "out")
                                                "/share/java/user-classes")))
                (zero? (system (string-append "mvn package --offline "
                                              "-Dmaven.test.skip=true "
                                              "--global-settings \""
                                              settings "\"")))
                (mkdir-p output-dir)
                (map (lambda (file-pair)
                       (copy-file (car file-pair)
                                  (string-append output-dir "/" (cdr file-pair))))
                     '(("hmf-gene-panel/target/hmf-gene-panel-1-jar-with-dependencies.jar" .
                        "hmf-gene-panel-1.jar")
                       ("amber/target/amber-1.0-jar-with-dependencies.jar" .
                        "amber-1.0.jar")
                       ("count-bam-lines/target/count-bam-lines-1.0-jar-with-dependencies.jar" .
                        "count-bam-lines-1.0.jar")
                       ("patient-report-mailer/target/patient-report-mailer-1.0-jar-with-dependencies.jar" .
                        "patient-report-mailer-1.0.jar")
                       ("rups-checker/target/rups-checker-1-jar-with-dependencies.jar" .
                        "rups-checker-1.jar")
                       ("bachelor/target/bachelor-1-jar-with-dependencies.jar" .
                        "bachelor-1.jar")
                       ("fastq-stats/target/fastq-stats-1.0-jar-with-dependencies.jar" .
                        "fastq-stats-1.0.jar")
                       ("break-point-inspector/target/break-point-inspector-1.2-jar-with-dependencies.jar" .
                        "break-point-inspector-1.2.jar")
                       ("bam-slicer/target/bam-slicer-1.0-jar-with-dependencies.jar" .
                        "bam-slicer-1.0.jar")
                       ("strelka-post-process/target/strelka-post-process-1.0-jar-with-dependencies.jar" .
                        "strelka-post-process-1.0.jar")))))))))
     (inputs
      `(("icedtea" ,icedtea-8 "jdk")
        ("maven" ,maven-bin)))
     (native-inputs
      `(("maven-deps"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://raw.githubusercontent.com/"
                                 "UMCUGenetics/guix-additions/master/blobs/"
                                 "hmftools-mvn-dependencies.tar.gz"))
             (sha256
              (base32
               "1c4hqah8qs14h8mpvj5z7kq4q0d8s0fa3p3f6gvmsmizkrblidv5"))))))
     (native-search-paths
      (list (search-path-specification
             (variable "GUIX_JARPATH")
             (files (list "share/java/user-classes")))))
     (home-page "https://github.com/hartwigmedical/hmftools")
     (synopsis "Various utility tools for working with genomics data.")
     (description "This package provides various tools for working with
genomics data developed by the Hartwig Medical Foundation.")
     (license license:expat))))

(define-public perl-findbin-libs
  (package
    (name "perl-findbin-libs")
    (version "2.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/L/LE/LEMBARK/FindBin-libs-"
             version ".tar.gz"))
       (sha256
        (base32
         "0306g1lpxfpv0r6491y6njjc312jx01zh2qqqa4cwkc0ya4jpdpn"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/FindBin-libs")
    (synopsis "")
    (description "")
    (license #f)))

(define-public perl-strictures-2
  (package
    (name "perl-strictures")
    (version "2.000003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "strictures-" version ".tar.gz"))
       (sha256
        (base32
         "08mgvf1d2651gsg3jgjfs13878ndqa4ji8vfsda9f7jjd84ymy17"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/strictures")
    (synopsis "Turn on strict and make all warnings fatal")
    (description "Strictures turns on strict and make all warnings fatal when
run from within a source-controlled directory.")
    (license (package-license perl))))

(define-public perl-indirect
  (package
    (name "perl-indirect")
    (version "0.37")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/V/VP/VPIT/indirect-"
             version ".tar.gz"))
       (sha256
        (base32
         "197vk0kvbz17xwvqrixg35f3i7yw4hilynvbzf1dizq4vcz2madd"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/indirect")
    (synopsis "Lexically warn about using the indirect method call syntax.")
    (description "This package provides a pragma that warns about indirect
method calls that are present in your code.  The indirect syntax is now
considered harmful, since its parsing has many quirks and its use is error
prone: when the subroutine @code{foo} has not been declared in the current
package, @code{foo $x} actually compiles to @code{$x->foo}, and
@code{foo { key => 1 }} to @code{'key'->foo(1)}.")
    (license (package-license perl))))

(define-public perl-b-hooks-op-check
  (package
    (name "perl-b-hooks-op-check")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/ET/ETHER/B-Hooks-OP-Check-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1kfdv25gn6yik8jrwik4ajp99gi44s6idcvyyrzhiycyynzd3df7"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)))
    (home-page "http://search.cpan.org/dist/B-Hooks-OP-Check")
    (synopsis "Wrap OP check callbacks")
    (description "")
    (license (package-license perl))))

(define-public perl-lexical-sealrequirehints
  (package
    (name "perl-lexical-sealrequirehints")
    (version "0.011")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Lexical-SealRequireHints-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0fh1arpr0hsj7skbn97yfvbk22pfcrpcvcfs15p5ss7g338qx4cy"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page
     "http://search.cpan.org/dist/Lexical-SealRequireHints")
    (synopsis "Prevent leakage of lexical hints")
    (description "")
    (license (package-license perl))))

(define-public perl-multidimensional
  (package
    (name "perl-multidimensional")
    (version "0.013")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/I/IL/ILMARI/multidimensional-"
             version ".tar.gz"))
       (sha256
        (base32
         "02p5zv68i39hnkmzzxsk1fi7xy56pfcsslrd7yqwzhq74czcw81x"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-b-hooks-op-check" ,perl-b-hooks-op-check)
       ("perl-lexical-sealrequirehints" ,perl-lexical-sealrequirehints)))
    (home-page
     "http://search.cpan.org/dist/multidimensional")
    (synopsis "Perl package to disable multidimensional array emulation")
    (description "")
    (license (package-license perl))))

(define-public perl-bareword-filehandles
  (package
    (name "perl-bareword-filehandles")
    (version "0.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/I/IL/ILMARI/bareword-filehandles-"
             version ".tar.gz"))
       (sha256
        (base32
         "0fdirls2pg7d6ymvlzzz59q3dy6hgh08k0qpr2mw51w127s8rav6"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-b-hooks-op-check" ,perl-b-hooks-op-check)
       ("perl-lexical-sealrequirehints" ,perl-lexical-sealrequirehints)))
    (home-page "http://search.cpan.org/dist/bareword-filehandles")
    (synopsis "Disables bareword filehandles")
    (description "")
    (license (package-license perl))))

(define-public hmf-pipeline
  (package
    (name "hmf-pipeline")
    (version "3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hartwigmedical/pipeline/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "0yd00hkh774lh4gql77s17im3m2xzcmai4a46n0ww2hx5zgpxd4q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (PATH (string-append (assoc-ref %build-inputs "gzip") "/bin"))
               (tarball (assoc-ref %build-inputs "source"))
               (current-dir (getcwd))
               (bin-dir (string-append %output "/bin"))
               (qscripts-dir (string-append %output "/share/hmf-pipeline/QScripts"))
               (templates-dir (string-append %output "/share/hmf-pipeline/templates"))
               (scripts-dir (string-append %output "/share/hmf-pipeline/scripts"))
               (lib-dir (string-append %output "/lib/perl5/site_perl/"
                                       ,(package-version perl)))
               (perlbin (string-append (assoc-ref %build-inputs "perl")
                                       "/bin/perl")))
           (setenv "PATH" PATH)
           ;; Create the directory structure in the build output directory.
           (mkdir-p lib-dir)
           (mkdir-p scripts-dir)
           (mkdir-p qscripts-dir)

           ;; Extract the modules into the Perl path.
           (chdir lib-dir)
           (system* tar "xvf" tarball (string-append "pipeline-" ,version "/lib/")
                    "--strip-components=2")

           ;; Patch the use of external tools
           (substitute* (list (string-append lib-dir "/HMF/Pipeline/Config.pm")
                              (string-append lib-dir "/HMF/Pipeline/Config/Validate.pm"))
             ;; Patch 'git'.
             (("qx\\(git ")
              (string-append "qx(" (assoc-ref %build-inputs "git") "/bin/git "))
             ;; Patch 'samtools'
             (("qx\\(\\$samtools ")
              (string-append "qx(" (assoc-ref %build-inputs "samtools")
                             "/bin/samtools "))
             ;; Patch 'bash'
             (("qx\\(bash ")
              (string-append "qx(" (assoc-ref %build-inputs "bash") "/bin/bash "))
             ;; Patch 'cat'
             (("qx\\(cat ")
              (string-append "qx(" (assoc-ref %build-inputs "coreutils") "/bin/cat ")))

           ;; Extract scripts to their own custom directory.
           (chdir scripts-dir)
           (system* tar "xvf" tarball (string-append "pipeline-" ,version "/scripts")
                    "--strip-components=2")

           ;; Extract QScripts to their own custom directory.
           (chdir qscripts-dir)
           (system* tar "xvf" tarball (string-append "pipeline-" ,version "/QScripts")
                    "--strip-components=2")

           ;; Extract the template scripts to their own custom directory.
           (chdir templates-dir)
           (system* tar "xvf" tarball
                    (string-append "pipeline-" ,version "/templates")
                    "--strip-components=2")

           ;; Extract the main scripts into the bin directory.
           (chdir %output)
           (system* tar "xvf" tarball
                    (string-append "pipeline-" ,version "/bin/pipeline.pl")
                    (string-append "pipeline-" ,version "/bin/create_config.pl")
                    "--strip-components=1")

           ;; Patch the shebang of the main scripts.
           (chdir bin-dir)
           (substitute* '("pipeline.pl" "create_config.pl")
             (("/usr/bin/env perl") perlbin))))))
   (inputs
    `(("perl" ,perl)
      ("git" ,git)
      ("bash" ,bash)
      ("coreutils" ,coreutils)))
   (native-inputs
    `(("source" ,source)
      ("tar" ,tar)
      ("gzip" ,gzip)))
   (propagated-inputs
    `(("samtools" ,samtools)
      ("perl-findbin-libs" ,perl-findbin-libs)
      ("perl-strictures" ,perl-strictures-2)
      ("perl-indirect" ,perl-indirect)
      ("perl-file-find-rule" ,perl-file-find-rule)
      ("perl-json" ,perl-json)
      ("perl-sort-key" ,perl-sort-key)
      ("perl-bareword-filehandles" ,perl-bareword-filehandles)
      ("perl-autovivification" ,perl-autovivification)
      ("perl-file-copy-recursive" ,perl-file-copy-recursive)
      ("perl-list-moreutils" ,perl-list-moreutils)
      ("perl-template-toolkit" ,perl-template-toolkit)
      ("perl-time-hires" ,perl-time-hires)
      ("perl-multidimensional" ,perl-multidimensional)))
   (home-page "https://github.com/hartwigmedical/pipeline")
   (synopsis "Default Hartwig Medical Data processing pipeline")
   (description "Pipeline of tools to process raw fastq data and
produce meaningful genomic data from Hartwig Medical.")
   (license license:expat)))
