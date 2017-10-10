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
  #:use-module (ice-9 ftw)
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
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (umcu packages bioconductor)
  #:use-module (umcu packages boost)
  #:use-module (umcu packages bwa)
  #:use-module (umcu packages circos)
  #:use-module (umcu packages contra)
  #:use-module (umcu packages delly)
  #:use-module (umcu packages fastqc)
  #:use-module (umcu packages freebayes)
  #:use-module (umcu packages freec)
  #:use-module (umcu packages gatk)
  #:use-module (umcu packages genenetwork)
  #:use-module (umcu packages grid-engine)
  #:use-module (umcu packages igvtools)
  #:use-module (umcu packages king)
  #:use-module (umcu packages manta)
  #:use-module (umcu packages mysql)
  #:use-module (umcu packages pbgzip)
  #:use-module (umcu packages picard)
  #:use-module (umcu packages plink)
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
                ;; The following modules fail to build due to a dependency
                ;; on itself.
                 (("<module>health-checker</module>")
                  "<!-- <module>health-checker</module> -->")
                 (("<module>patient-reporter</module>")
                  "<!-- <module>patient-reporter</module> -->"))))

           ;; To build the purity-ploidy-estimator, we need to build patient-db
           ;; first.  This needs a running MySQL database.  So, we need to set
           ;; this up before attempting to build the Java archives.
           (add-before 'build 'start-mysql-server
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((mysqld (string-append (assoc-ref inputs "mysql") "/bin/mysqld"))
                    (mysql (string-append (assoc-ref inputs "mysql") "/bin/mysql"))
                    (mysql-run-dir (string-append (getcwd) "/mysql")))
                (mkdir-p "mysql/data")
                (with-directory-excursion "mysql"
                  ;; Initialize the MySQL data store.  The mysql_install_db
                  ;; script uses relative paths to find things, so we need to
                  ;; change to the right directory.
                  (with-directory-excursion (assoc-ref inputs "mysql")
                    (system* "bin/mysql_install_db"
                             (string-append "--datadir=" mysql-run-dir "/data")
                             "--user=root"))

                  ;; Run the MySQL server.
                  (system (string-append
                           mysqld
                           " --datadir=" mysql-run-dir "/data "
                           "--user=root "
                           "--socket=" mysql-run-dir "/socket "
                           "--port=3306 "
                           "--explicit_defaults_for_timestamp "
                           "&> " mysql-run-dir "/mysqld.log &"))

                  (format #t "Waiting for MySQL server to start.")
                  (sleep 5)

                  ;; Create 'build' user.
                  (system* mysql
                           "--host=127.0.0.1"
                           "--port=3306"
                           "--user=root"
                           "-e" "CREATE USER build@localhost IDENTIFIED BY 'build'")

                  ;; Grant permissions to 'build' user.
                  (system* mysql
                           "--host=127.0.0.1"
                           "--port=3306"
                           "--user=root"
                           "-e" "GRANT ALL ON *.* TO 'build'@'localhost'")

                  ;; Create a database.
                  (system* mysql
                           "--host=127.0.0.1"
                           "--port=3306"
                           "--user=build"
                           "--password=build"
                           "-e" "CREATE DATABASE hmfpatients")))))
          (replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((build-dir (getcwd))
                     (home-dir (string-append build-dir "/home"))
                     (settings-dir (string-append build-dir "/mvn"))
                     (settings (string-append settings-dir "/settings.xml"))
                     (m2-dir (string-append build-dir "/m2/repository")))

                ;; Set JAVA_HOME to help maven find the JDK.
                (setenv "JAVA_HOME" (string-append (assoc-ref inputs "icedtea")
                                                   "/jre"))
                (mkdir-p home-dir)
                (setenv "HOME" home-dir)

                (mkdir-p m2-dir)
                (mkdir-p settings-dir)

                ;; Create credentials file.
                (with-output-to-file (string-append home-dir "/mysql.login")
                  (lambda _
                    (format #t "[client]~%database=~a~%user=~a~%password=~a~%socket=~a/mysql/socket"
                            "hmfpatients" "build" "build" build-dir)))

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

                ;; Remove assumptious/breaking code
                (substitute* "patient-db/src/main/resources/setup_database.sh"
                  (("if \\[ \\$\\{SCRIPT_EPOCH\\} -gt \\$\\{DB_EPOCH\\} \\];")
                   "if true;"))

                ;; Compile using maven's compile command.
                (zero? (system (format #f "mvn compile --offline --global-settings ~s" settings))))))
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
                       ("patient-db/target/patient-db-1.0-jar-with-dependencies.jar" .
                        "patient-db-1.0.jar")
                       ("purity-ploidy-estimator/target/purity-ploidy-estimator-1.2-jar-with-dependencies.jar" .
                        "purity-ploidy-estimator-1.2.jar")
                       ("bam-slicer/target/bam-slicer-1.0-jar-with-dependencies.jar" .
                        "bam-slicer-1.0.jar")
                       ("strelka-post-process/target/strelka-post-process-1.0-jar-with-dependencies.jar" .
                        "strelka-post-process-1.0.jar")))

                ;; The HMF pipeline expects the following filenames to exist.
                (chdir output-dir)
                (symlink "amber-1.0.jar" "amber.jar")
                (symlink "count-bam-lines-1.0.jar" "cobalt.jar")
                (symlink "purity-ploidy-estimator-1.2.jar" "purple.jar")))))))
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
               "1iflrwff51ll8vzcpb1dmh3hs2qsbb9h0rbys4gdw584xpdvcz0z"))))
        ("mysql" ,mysql-5.6.25)))
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

(define-public exoncov
  (package
    (name "exoncov")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/UMCUGenetics/ExonCov/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "1d3w2yjvbhjxvyly5a0db1fm3nnasx0p4ijz9fgg2ai02gda9qpb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (delete 'build) ; There is nothing to build.
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bindir (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bindir)
               (install-file "ExonCov.py" bindir)))))))
    (inputs
     `(("python" ,python-2)))
    (propagated-inputs
     `(("sambamba" ,sambamba)))
    (home-page "https://github.com/UMCUGenetics/ExonCov")
    (synopsis "Exon coverage statistics from BAM files")
    (description "This package can generate exon coverage statistics from
BAM files using @code{sambamba}.")
    (license license:expat)))

(define-public bammetrics
  (package
    (name "bammetrics")
    (version "2.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/UMCUGenetics/bamMetrics/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "0nbm5ll91p3slbjz7a3wmk02k621mcyha5mlr75gkh1l51dwc69d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "bamMetrics.pl"
               ;; The following hardcoded paths must be patched.
               (("my \\$picard_path = \"/hpc/local/CentOS7/cog_bioinf/picard-tools-1.141\";")
                (string-append "my $picard_path = \"" (assoc-ref inputs "picard") "\";"))
               (("my \\$sambamba_path = \"/hpc/local/CentOS7/cog_bioinf/sambamba_v0.6.1\";")
                (string-append "my $sambamba_path = \"" (assoc-ref inputs "sambamba") "\";"))
               ;; The following programs should be patched.
               (("java -Xmx")
                (string-append (assoc-ref inputs "icedtea") "/bin/java -Xmx"))
               (("Rscript")
                (string-append (assoc-ref inputs "r") "/bin/Rscript"))
               (("qsub")
                (string-append (assoc-ref inputs "grid-engine-core") "/bin/qsub")))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bindir (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bindir)
               (map delete-file '("LICENSE" ".gitignore" "README.md"))
               ;; TODO: Only copy bamMetrics.pl to the bindir, and other stuff
               ;; to its appropriate location.
               (copy-recursively "." bindir)))))))
    (inputs
     `(("sambamba" ,sambamba)
       ("perl" ,perl)
       ("r" ,r)
       ("picard" ,picard-bin-1.141)
       ("icedtea" ,icedtea)
       ("grid-engine-core" ,grid-engine-core)))
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-knitr" ,r-knitr)
       ("r-markdown" ,r-markdown)
       ("r-reshape" ,r-reshape)
       ("r-xtable" ,r-xtable)
       ("r-brew" ,r-brew)))
    (home-page "https://github.com/UMCUGenetics/bamMetrics")
    (synopsis "Generate BAM statistics and PDF/HTML reports")
    (description "This package provides a tool to generate BAM statistics and
PDF/HTML reports.  It has been developed to run on the Utrecht HPC.")
    (license license:expat)))

(define-public bamutils
  (package
    (name "bamutils")
    (version "1.0.13")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://genome.sph.umich.edu/w/images/7/70/BamUtilLibStatGen."
                version ".tgz"))
              (sha256
               (base32
                "0asr1kmjbr3cyf4hkg865y8c2s30v87xvws4q6c8pyfi6wfd1h8n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:make-flags `("USER_WARNINGS=-Wall"
                      ,(string-append "INSTALLDIR="
                                      (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://genome.sph.umich.edu/wiki/BamUtil")
    (synopsis "Programs for working on SAM/BAM files")
    (description "This package provides several programs that perform
operations on SAM/BAM files.  All of these programs are built into a
single executable called @code{bam}.")
    (license license:gpl3+)))

(define-public damage-estimator
  (let ((commit "5dc25d51509ee0349c31756903bd6a373a57c299"))
    (package
     (name "damage-estimator")
     (version "1.0")
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Ettwiller/Damage-estimator.git")
                    (commit commit)))
              (file-name (string-append name "-" version))
              (sha256
               (base32 "05mkcd1cbvg7rf92a310dixv5f38l6bz0hnilhp9i87cmfxl2632"))))
     (build-system trivial-build-system)
     (arguments
      `(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let ((source-dir (assoc-ref %build-inputs "source"))
                (output-dir (string-append %output "/share/damage-estimator")))
            (mkdir-p output-dir)
            (map (lambda (file)
                   (install-file (string-append source-dir "/" file)
                                 output-dir))
                 '("estimate_damage.pl"
                   "estimate_damage_location.pl"
                   "estimate_damage_location_context.pl"
                   "plot_damage.R"
                   "plot_damage_location.R"
                   "plot_damage_location_context.R"
                   "plot_random_sampling_damage.R"
                   "random_sampling_and_estimate_damage.pl"
                   "randomized2"
                   "split_mapped_reads.pl"))))))
     (native-inputs
      `(("source" ,source)))
     (propagated-inputs
      `(("samtools" ,samtools)
        ("r-ggplot2" ,r-ggplot2)
        ("r-reshape2" ,r-reshape2)))
     (home-page "https://github.com/Ettwiller/Damage-estimator")
     (synopsis "")
     (description "")
     (license license:agpl3))))

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
     `(#:modules ((guix build utils)
                  (ice-9 ftw))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw))
         (let ((tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (PATH (string-append (assoc-ref %build-inputs "gzip") "/bin"))
               (tarball (assoc-ref %build-inputs "source"))
               (current-dir (getcwd))
               (bin-dir (string-append %output "/bin"))
               (pipeline-dir (string-append %output "/share/hmf-pipeline"))
               (settings-dir (string-append %output "/share/hmf-pipeline/settings"))
               (qscripts-dir (string-append %output "/share/hmf-pipeline/QScripts"))
               (templates-dir (string-append %output "/share/hmf-pipeline/templates"))
               (scripts-dir (string-append %output "/share/hmf-pipeline/scripts"))
               (lib-dir (string-append %output "/lib/perl5/site_perl/"
                                       ,(package-version perl)))
               (perlbin (string-append (assoc-ref %build-inputs "perl")
                                       "/bin/perl")))
           (setenv "PATH" PATH)

           ;; Create the directory structure in the build output directory.
           (map mkdir-p (list lib-dir
                              scripts-dir
                              qscripts-dir
                              settings-dir
                              templates-dir))

           ;; Extract the modules into the Perl path.
           (chdir lib-dir)
           (system* tar "xvf" tarball (string-append "pipeline-" ,version "/lib/")
                    "--strip-components=2")

           ;; Patch the use of external tools
           (substitute* (list (string-append lib-dir "/HMF/Pipeline/Config.pm")
                              (string-append lib-dir "/HMF/Pipeline/Config/Validate.pm"))
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
           (with-directory-excursion scripts-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/scripts")
                      "--strip-components=2"))

           ;; Extract QScripts to their own custom directory.
           (with-directory-excursion qscripts-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/QScripts")
                      "--strip-components=2"))

           ;; Extract the template scripts to their own custom directory.
           (with-directory-excursion templates-dir
             (system* tar "xvf" tarball
                      (string-append "pipeline-" ,version "/templates")
                      "--strip-components=2")

             ;; Replace the 'java' command with the full path to the input 'java'
             ;; in each template file.  Some need Java 8, and some need Java 7,
             ;; so we need to be specific in these cases.

             ;; Java 8
             (substitute* '("Amber.sh.tt" "Cobalt.sh.tt" "Purple.sh.tt"
                            "HealthCheck.sh.tt" "BreakpointInspector.sh.tt")
               (("java -Xmx")
                (string-append (assoc-ref %build-inputs "icedtea-8")
                               "/bin/java -Xmx")))

             ;; Java 7
             (substitute* '("Realignment.sh.tt" "BAF.sh.tt" "CallableLoci.sh.tt"
                            "GermlineCalling.sh.tt" "GermlineAnnotation.sh.tt"
                            "GermlineFiltering.sh.tt" "PostStats.sh.tt" "Strelka.sh.tt")
               (("java -Xmx")
                (string-append (assoc-ref %build-inputs "icedtea-7")
                               "/bin/java -Xmx")))

             ;; Mixed Java 7 and Java 8
             (substitute* '("StrelkaPostProcess.sh.tt")
               (("java -jar \"\\[% opt.STRELKA_POST_PROCESS_PATH %\\]/strelka-post-process.jar\"")
                (string-append (assoc-ref %build-inputs "icedtea-8")
                               "/bin/java -jar "
                               "\"[% opt.STRELKA_POST_PROCESS_PATH %]"
                               "/strelka-post-process.jar\""))
               (("java -Xmx\\[% opt.STRELKAPOSTPROCESS_MEM %\\]G")
                (string-append (assoc-ref %build-inputs "icedtea-7")
                               "/bin/java -Xmx[% opt.STRELKAPOSTPROCESS_MEM %]G")))

             ;; Patch the 'make' command.
             (substitute* "Strelka.sh.tt"
               (("make -j") (string-append (assoc-ref %build-inputs "make")
                                           "/bin/make -j"))))

           ;; Extract the settings files to their own custom directory.
           (with-directory-excursion settings-dir
             (system* tar "xvf" tarball
                      (string-append "pipeline-" ,version "/settings")
                      "--strip-components=2")

             ;; Add a prefix to the 'INIFILE' directory specification.
             (substitute*
                 (scandir "."
                          (lambda (item)
                            (and (> (string-length item) 3)
                                 (string= (string-take-right item 3) "ini"))))
               (("INIFILE	settings")
                (string-append "INIFILE	" settings-dir)))

             (with-directory-excursion "include"
               (substitute*
                   (scandir "."
                            (lambda (item)
                              (and (> (string-length item) 3)
                                   (string= (string-take-right item 3) "ini"))))
                 (("INIFILE	settings")
                  (string-append "INIFILE	" settings-dir))))

             ;; We are going to roll our own tools.ini.
             (delete-file "include/tools.ini")
             (with-output-to-file "include/tools.ini"
               (lambda _
                 (format #t "# Generated by GNU Guix
BWA_PATH	~a
SAMBAMBA_PATH	~a

FASTQC_PATH	~a
PICARD_PATH	~a
BAMMETRICS_PATH	~a
EXONCALLCOV_PATH	~a
DAMAGE_ESTIMATOR_PATH	~a

QUEUE_PATH	~a
QUEUE_LOW_GZIP_COMPRESSION_PATH	~a
GATK_PATH	~a

STRELKA_PATH	~a
STRELKA_POST_PROCESS_PATH	~a

AMBER_PATH	~a
COBALT_PATH	~a
PURPLE_PATH	~a
CIRCOS_PATH	~a

FREEC_PATH	~a
QDNASEQ_PATH	~a

DELLY_PATH	~a
MANTA_PATH	~a
BPI_PATH	~a

IGVTOOLS_PATH	~a
SAMTOOLS_PATH	~a
TABIX_PATH	~a
PLINK_PATH	~a
KING_PATH	~a
BIOVCF_PATH	~a
BAMUTIL_PATH	~a
PBGZIP_PATH	~a
SNPEFF_PATH	~a
VCFTOOLS_PATH	~a
BCFTOOLS_PATH	~a
HEALTH_CHECKER_PATH	MISSING

REALIGNMENT_SCALA	IndelRealignment.scala
BASERECALIBRATION_SCALA	BaseRecalibration.scala
CALLING_SCALA	GermlineCaller.scala
FILTER_SCALA	GermlineFilter.scala

REPORT_STATUS	~a"
                         (string-append (assoc-ref %build-inputs "bwa") "/bin")
                         (string-append (assoc-ref %build-inputs "sambamba") "/bin")
                         (string-append (assoc-ref %build-inputs "fastqc") "/bin")
                         (string-append (assoc-ref %build-inputs "picard") "/share/java/picard")
                         (string-append (assoc-ref %build-inputs "bammetrics") "/bin")
                         (string-append (assoc-ref %build-inputs "exoncov") "/bin")
                         (string-append (assoc-ref %build-inputs "damage-estimator") "/share/damage-estimator")
                         (string-append (assoc-ref %build-inputs "gatk-queue") "/share/java/gatk")
                         (string-append (assoc-ref %build-inputs "gatk-queue") "/share/java/gatk")
                         (string-append (assoc-ref %build-inputs "gatk") "/share/java/gatk")
                         (string-append (assoc-ref %build-inputs "strelka") "/bin")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "circos") "/bin")
                         (string-append (assoc-ref %build-inputs "freec") "/bin")
                         (string-append (assoc-ref %build-inputs "r-qdnaseq") "/site-library/QDNAseq")
                         (string-append (assoc-ref %build-inputs "manta") "/bin")
                         (string-append (assoc-ref %build-inputs "delly") "/bin")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "igvtools") "/share/java/igvtools")
                         (string-append (assoc-ref %build-inputs "samtools") "/bin")
                         (string-append (assoc-ref %build-inputs "htslib") "/bin")
                         (string-append (assoc-ref %build-inputs "plink2") "/bin")
                         (string-append (assoc-ref %build-inputs "king") "/bin")
                         (string-append (assoc-ref %build-inputs "bio-vcf") "/bin")
                         (string-append (assoc-ref %build-inputs "bamutils") "/bin")
                         (string-append (assoc-ref %build-inputs "pbgzip") "/bin")
                         (string-append (assoc-ref %build-inputs "snpeff") "/share/java/snpeff")
                         (string-append (assoc-ref %build-inputs "vcftools") "/bin")
                         (string-append (assoc-ref %build-inputs "bcftools") "/bin")
                         ;; HEALTH-CHECKER
                         (string-append (assoc-ref %build-inputs "coreutils") "/bin/true")))))

           ;; Extract the main scripts into the bin directory.
           (with-directory-excursion %output
             (system* tar "xvf" tarball
                      (string-append "pipeline-" ,version "/bin/pipeline.pl")
                      (string-append "pipeline-" ,version "/bin/create_config.pl")
                      "--strip-components=1"))

           ;; Patch the shebang of the main scripts.
           (with-directory-excursion bin-dir
             (substitute* '("pipeline.pl" "create_config.pl")
               (("/usr/bin/env perl") perlbin))
             (substitute* "create_config.pl"
               (("my \\$settingsDir = catfile\\(dirname\\(abs_path\\(\\$0\\)\\), updir\\(\\), \"settings\"\\);")
                (string-append "my $settingsDir = \"" settings-dir "\";"))))

           ;; Make sure the templates can be found.
           (with-directory-excursion lib-dir
             (substitute* "HMF/Pipeline/Template.pm"
               (("my \\$source_template_dir = catfile\\(HMF::Pipeline::Config::pipelinePath\\(\\), \"templates\"\\);")
                (string-append "my $source_template_dir = \"" templates-dir "\";")))

             ;; Make sure the other subdirectories can be found.
             (substitute* "HMF/Pipeline/Config.pm"
               (("my \\$pipeline_path = pipelinePath\\(\\);")
                (string-append "my $pipeline_path = \"" pipeline-dir "\";"))
               (("my \\$output_fh = IO::Pipe->new\\(\\)->writer\\(\"tee")
                (string-append "my $output_fh = IO::Pipe->new()->writer(\""
                               (assoc-ref %build-inputs "coreutils") "/bin/tee"))
               (("my \\$error_fh = IO::Pipe->new\\(\\)->writer\\(\"tee")
                (string-append "my $error_fh = IO::Pipe->new()->writer(\""
                               (assoc-ref %build-inputs "coreutils") "/bin/tee"))
               (("\\$opt->\\{VERSION\\} = qx\\(git --git-dir \\$git_dir describe --tags\\);")
                (string-append "$opt->{VERSION} = \"" ,version "\";"))
               (("my \\$pipeline_path = pipelinePath\\(\\);")
                (string-append "my $pipeline_path = \"" pipeline-dir "\";"))
               (("rcopy \\$slice_dir") "$File::Copy::Recursive::KeepMode = 0; rcopy $slice_dir")))))))
    (inputs
     `(("bammetrics" ,bammetrics)
       ("bamutils" ,bamutils)
       ("bash" ,bash)
       ("bcftools" ,bcftools)
       ("bio-vcf" ,bio-vcf)
       ("bwa" ,bwa)
       ("circos" ,circos)
       ("damage-estimator" ,damage-estimator)
       ("delly" ,delly-0.7.7)
       ("exoncov" ,exoncov)
       ("fastqc" ,fastqc-bin-0.11.4)
       ("freec" ,freec-10.4)
       ("gatk" ,gatk-bin-3.4-46)
       ("gatk-queue" ,gatk-queue-bin-3.4-46)
       ("hmftools" ,hmftools)
       ("htslib" ,htslib)
       ("icedtea-8" ,icedtea-8)
       ("icedtea-7" ,icedtea-7)
       ("igvtools" ,igvtools-bin-2.3.60)
       ("king" ,king-bin-1.4)
       ("manta" ,manta)
       ("pbgzip" ,pbgzip)
       ("perl" ,perl)
       ("picard" ,picard-bin-1.141)
       ("plink2" ,plink2-1.90b3)
       ("make" ,gnu-make)
       ("r" ,r)))
    (native-inputs
     `(("gzip" ,gzip)
       ("source" ,source)
       ("tar" ,tar)))
    (propagated-inputs
     `(("perl-autovivification" ,perl-autovivification)
       ("perl-bareword-filehandles" ,perl-bareword-filehandles)
       ("perl-file-copy-recursive" ,perl-file-copy-recursive)
       ("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-findbin-libs" ,perl-findbin-libs)
       ("perl-indirect" ,perl-indirect)
       ("perl-json" ,perl-json)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-multidimensional" ,perl-multidimensional)
       ("perl-sort-key" ,perl-sort-key)
       ("perl-strictures" ,perl-strictures-2)
       ("perl-template-toolkit" ,perl-template-toolkit)
       ("perl-time-hires" ,perl-time-hires)
       ("r-qdnaseq" ,r-qdnaseq)
       ("sambamba" ,sambamba)
       ("samtools" ,samtools)
       ("snpeff" ,snpeff-bin-4.1)
       ("strelka" ,strelka-1.0.14)
       ("vcftools" ,vcftools)
       ("coreutils" ,coreutils)
       ("grid-engine" ,grid-engine-core)))
    ;; Bash, Perl and R are not propagated into the profile.  The programs are
    ;; invoked using their absolute link from the 'tools.ini' file.  We must
    ;; make sure that the environment variables for these interpreters are
    ;; set correctly.
    (native-search-paths
     (append (package-native-search-paths bash)
             (package-native-search-paths grid-engine-core)
             (package-native-search-paths perl)
             (package-native-search-paths r)))
    (search-paths native-search-paths)
    (home-page "https://github.com/hartwigmedical/pipeline")
    (synopsis "Default Hartwig Medical Data processing pipeline")
    (description "Pipeline of tools to process raw fastq data and
produce meaningful genomic data from Hartwig Medical.")
    (license license:expat)))
