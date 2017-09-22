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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages)
  #:use-module (umcu packages bioconductor)
  #:use-module (umcu packages boost)
  #:use-module (umcu packages bwa)
  #:use-module (umcu packages contra)
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
