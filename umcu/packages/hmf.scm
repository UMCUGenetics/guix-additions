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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (umcu packages bioconductor)
  #:use-module (umcu packages bwa)
  #:use-module (umcu packages circos)
  #:use-module (umcu packages contra)
  #:use-module (umcu packages fastqc)
  #:use-module (umcu packages freec)
  #:use-module (umcu packages gatk)
  #:use-module (umcu packages genenetwork)
  #:use-module (umcu packages sambamba)
  #:use-module (umcu packages grid-engine)
  #:use-module (umcu packages igvtools)
  #:use-module (umcu packages king)
  #:use-module (umcu packages manta)
  #:use-module (umcu packages mysql)
  #:use-module (umcu packages pbgzip)
  #:use-module (umcu packages perl)
  #:use-module (umcu packages picard)
  #:use-module (umcu packages samtools)
  #:use-module (umcu packages snpeff)
  #:use-module (umcu packages strelka)
  #:use-module (umcu packages varscan)
  #:use-module (umcu packages vcflib)
  #:use-module (umcu packages vcftools))

(define (hmftools-component name version url-suffix hash)
  (package
   (name name)
   (version version)
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/hartwigmedical/hmftools/"
                  "releases/download/" url-suffix))
            (file-name (basename url-suffix))
            (sha256
             (base32 hash))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((output-dir  (string-append (assoc-ref %outputs "out")
                                           "/share/java/user-classes"))
               (input-file  (assoc-ref %build-inputs "source"))
               (destination (string-append
                             output-dir "/" (basename input-file))))
          (mkdir-p output-dir)
          (copy-file input-file
                     (string-append output-dir "/" (basename ,url-suffix)))
          (symlink (basename ,url-suffix)
                   (string-append output-dir "/" ,name ".jar"))))))
   (native-search-paths
    (list (search-path-specification
           (variable "GUIX_JARPATH")
           (files (list "share/java/user-classes")))))
   (home-page "https://github.com/hartwigmedical/hmftools")
   (synopsis (string-append
              "Binary package for Hartwig Medical Foundations's " name))
   (description (string-append
                 "This package provides the binary for Hartwig Medical "
                 "Foundation's" name "."))
   (license license:expat)))

(define-public hmftools-amber-2.5
  (hmftools-component "amber" "2.5" "amber-v2-5/amber-2.5.jar"
                      "1ym2525vn354clw2x6xk3ps0xrxhz9485sj8jw8iqb6vybzmkizp"))

(define-public hmftools-amber-1.6
  (hmftools-component "amber" "1.6" "amber-v1-6/amber-1.6.jar"
                      "1xn29lv5cqydb0isnmhm6vs8g07c3y1k0ar03iyjha3zbcdgg7ry"))

(define-public hmftools-cobalt-1.4
  (hmftools-component "cobalt" "1.4" "cobalt-v1-4/cobalt-1.4.jar"
                      "0z8y80vfnks7wl1qa2811067c99nn6ckgvpnnssy1h7n6gxzrphi"))

(define-public hmftools-purple-2.17
  (hmftools-component "purple" "2.17" "purple-v2-17/purple-2.17.jar"
                      "0w54yp6h116s819j609w78pvz1p1a5vpzk6r3kzr44qkpjjr6pv7"))

(define-public hmftools-strelka-post-process-v1.6
  (hmftools-component "strelka-post-process" "1.6"
                      "strelka-post-process-v1-6/strelka-post-process-1.6.jar"
                      "0sy2xz8kvhzaal203assshhpp35kmydrz4qg16hawlpbxxmgx74d"))

(define-public gridss-bin
  (package
    (name "gridss")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/PapenfussLab/gridss/releases/download/v"
                    version "/gridss-" version "-gridss-jar-with-dependencies.jar"))
              (file-name (string-append name "-" version ".jar"))
              (sha256
               (base32 "0cicgv4yxr1qx63x75g9m14d4811axlj1lg8hs0lj2bzj801vd0b"))))
    (build-system gnu-build-system)
    (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'unpack) ; Don't unpack the jar.
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((jar (assoc-ref %build-inputs "source"))
                  (out (string-append (assoc-ref %outputs "out")
                                      "/share/java/" ,name "/")))
              (mkdir-p out)
              (copy-file jar (string-append out ,name "-" ,version ".jar"))
              (symlink (string-append out ,name "-" ,version ".jar")
                       (string-append out ,name ".jar"))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_JARPATH")
            (files (list "share/java/gridss")))))
    (propagated-inputs
     `(("r" ,r)
       ("r-variantannotation" ,r-variantannotation)
       ("r-ggplot2" ,r-ggplot2)
       ("r-scales" ,r-scales)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-data-table" ,r-data-table)
       ("r-stringr" ,r-stringr)))
    (home-page "https://github.com/PapenfussLab/gridss")
    (synopsis "Genomic Rearrangement IDentification Software Suite")
    (description "GRIDSS is a module software suite containing tools useful for
the detection of genomic rearrangements.  GRIDSS includes a genome-wide
break-end assembler, as well as a structural variation caller for Illumina
sequencing data.  GRIDSS calls variants based on alignment-guided positional
de Bruijn graph genome-wide break-end assembly, split read, and read pair
evidence.")
    (license license:gpl3+)))

(define-public grep-with-pcre
  (package (inherit grep)
    (name "grep-with-pcre")
    (inputs `(("pcre" ,pcre)))))

(define-public r-qdnaseq-hmf
  (package (inherit r-qdnaseq)
   (name "r-qdnaseq-hmf")
   (version "1.9.2-HMF.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/ccagc/QDNAseq/archive/v"
                  version ".tar.gz"))
            (sha256
             (base32 "1mzwcxcwr00kbf75xrxg0f6z9y5f87x1sq6kw5v16bvxv9ppn62h"))))))

(define-public hmftools-2017-09-21
  (let ((commit "5cdd9f04ba20339083fbd1e7a1a5b34ec2596456"))
    (package
     (name "hmftools")
     (version (string-append "20170921-" (string-take commit 7)))
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
           (add-before 'build 'patch-circos-configuration
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("purity-ploidy-estimator/src/main/resources/circos/circos.template"
                              "purity-ploidy-estimator/src/main/resources/circos/input.template")
                 (("<<include etc/")
                  (string-append "<<include " (assoc-ref inputs "circos")
                                 "/share/Circos/etc/"))
                 (("karyotype = data/")
                  (string-append "karyotype = "
                                 (assoc-ref inputs "circos")
                                 "/share/Circos/data/")))))
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
                     (map (lambda (file)
                            `(,file . ,(basename (string-append (string-drop-right file 26) ".jar"))))
                          (find-files "." "-jar-with-dependencies.jar")))
                #t))))))
     (inputs
      `(("icedtea" ,icedtea-8 "jdk")
        ("maven" ,maven-bin)
        ("circos" ,circos)))
     ;; Amber uses an R script for BAF segmentation.
     (propagated-inputs
      `(("r" ,r-minimal)
        ("r-copynumber" ,r-copynumber)))
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

(define-public hmftools-2018-01-11
  (let ((commit "8d30505dfab219e367a6e5d7d3f2e6ec74877e75"))
    (package (inherit hmftools-2017-09-21)
     (name "hmftools")
     (version (string-append "20180111-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hartwigmedical/hmftools.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0wk9jk7lg81cf29z6jng6v7qb9xflwn4h87s701i1j80vx24zr9y"))))
     (build-system gnu-build-system)
     (native-inputs
      `(("maven-deps"
         ,(origin
           (method url-fetch)
           (uri (string-append "https://github.com/UMCUGenetics/guix-additions"
                               "/raw/c0faeec521d6fbd21e120fbd7af4cc0b9eccbf5b"
                               "/blobs/hmftools-mvn-dependencies.tar.gz"))
           (sha256
            (base32
             "0mbz5q8zrwin1rgjk2bb5ax95210zyndm3bx5lszpql2pmwdbjgk"))))
        ("mysql" ,mysql-5.6.25))))))

(define-public hmftools-2018-03-06
  (let ((commit "367fde7e04e8dd931a0253fa9e2374a7a66b50be"))
    (package (inherit hmftools-2018-01-11)
     (name "hmftools")
     (version (string-append "20180306-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hartwigmedical/hmftools.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "02k8w7vymib68g499kbl4q91dyz71bb2p72i9l4dg80pa797ssnk")))))))


(define-public hmftools-2018-06-19
  (let ((commit "ce89d46addafda37b64e665ee32f43b814a261a6"))
    (package
     (name "hmftools")
     (version (string-append "20180619-" (string-take commit 7)))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/hartwigmedical/hmftools.git")
                     (commit commit)))
               (file-name (string-append name "-" version "-checkout"))
               (sha256
                (base32
                  "1h7i2xvnk1jws6hzbbi5pg0dnb6gwp49lwzlgbyq707r321qcbpc"))))
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
           (add-before 'build 'patch-circos-configuration
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("purity-ploidy-estimator/src/main/resources/circos/circos.template"
                              "purity-ploidy-estimator/src/main/resources/circos/input.template")
                 (("<<include etc/")
                  (string-append "<<include " (assoc-ref inputs "circos")
                                 "/share/Circos/etc/"))
                 (("karyotype = data/")
                  (string-append "karyotype = "
                                 (assoc-ref inputs "circos")
                                 "/share/Circos/data/")))))
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
                  (and (zero? (system (string-append
                                       "cat " (assoc-ref inputs "maven-deps-1") " "
                                              (assoc-ref inputs "maven-deps-2")
                                              " > " " ../deps.tar.gz")))
                       (zero? (system* "tar" "xvf" "../deps.tar.gz"))))

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
                     (map (lambda (file)
                            `(,file . ,(basename (string-append (string-drop-right file 26) ".jar"))))
                          (find-files "." "-jar-with-dependencies.jar")))
                #t))))))
     (inputs
      `(("icedtea" ,icedtea-8 "jdk")
        ("maven" ,maven-bin)
        ("circos" ,circos)))
     ;; Amber uses an R script for BAF segmentation.
     (propagated-inputs
      `(("r" ,r-minimal)
        ("r-copynumber" ,r-copynumber)))
     (native-inputs
      ;; Split files into two pieces because some very specific Git hosting
      ;; provider limit filesizes to 100MB.
      `(("maven-deps-1"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/UMCUGenetics/guix-additions/raw/"
                   "d616b7a6a4076737cd0a0f43f7ec448b4ea2d112/blobs/"
                   "hmftools-mvn-dependencies.tar.gz.partaa"))
             (sha256
              (base32
               "0qk33jh536jla3dmk97cj6xs960ydskjwbw87gh0wk3gri3w8yd2"))))
        ("maven-deps-2"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/UMCUGenetics/guix-additions/raw/"
                   "d616b7a6a4076737cd0a0f43f7ec448b4ea2d112/blobs/"
                   "hmftools-mvn-dependencies.tar.gz.partab"))
             (sha256
              (base32
               "0m8amb2ld99w6gyhsy74651d5c3208dj93127p3zbdh0ln1azarb"))))
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

(define-public hmftools-2018-09-03
  (let ((commit "a9956d906c48e18ae4e20557c4c10ec1041de4e7"))
    (package
     (name "hmftools")
     (version (string-append "20180903-" (string-take commit 7)))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/hartwigmedical/hmftools.git")
                     (commit commit)))
               (file-name (string-append name "-" version "-checkout"))
               (sha256
                (base32
                 "1lg2n1rwgx6snh48si7hv8dcdddrfji9j5zign56whsmfklrmpni"))))
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
           (add-before 'build 'patch-circos-configuration
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("purity-ploidy-estimator/src/main/resources/circos/circos.template"
                              "purity-ploidy-estimator/src/main/resources/circos/input.template")
                 (("<<include etc/")
                  (string-append "<<include " (assoc-ref inputs "circos")
                                 "/share/Circos/etc/"))
                 (("karyotype = data/")
                  (string-append "karyotype = "
                                 (assoc-ref inputs "circos")
                                 "/share/Circos/data/")))))
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
                  (zero? (system (string-append
                                  "tar xvf " (assoc-ref inputs "maven-deps")))))

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
                (unless (zero? (system (format #f "mvn compile --offline --global-settings ~s" settings)))
                  (throw 'compilation-failed "Compilation failed.")))))
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
                     (map (lambda (file)
                            `(,file . ,(basename (string-append (string-drop-right file 26) ".jar"))))
                          (find-files "." "-jar-with-dependencies.jar")))
                #t))))))
     (inputs
      `(("icedtea" ,icedtea-8 "jdk")
        ("maven" ,maven-bin)
        ("circos" ,circos)))
     ;; Amber uses an R script for BAF segmentation.
     (propagated-inputs
      `(("r" ,r-minimal)
        ("r-copynumber" ,r-copynumber)))
     (native-inputs
      `(("maven-deps"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://www.roelj.com/hmftools-20180903-a9956d9-"
                   "maven-dependencies.tar.gz"))
             (sha256
              (base32
               "1lacaxz9szvifi5bh2rj06zdnl5yq2vqvxqd5z4yv1023zbllzci"))))
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

(define-public hmftools-2018-11-02
  (let ((commit "cc96075219c297bb516930b9e18561b298286457"))
    (package
     (name "hmftools")
     (version (string-append "20181102-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hartwigmedical/hmftools.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1ca50apsf4zydpc2w9mradw59vm56p1r6cqmy9afbhz25vllp6yj"))))
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
                  "<!-- <module>patient-reporter</module> -->")
                 (("<module>actionability-vs-soc</module>")
                  "<!-- <module>actionability-vs-soc</module> -->"))))

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
                           "-e" "CREATE DATABASE hmfpatients_test")))))

           (add-before 'build 'patch-circos-configuration
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("purity-ploidy-estimator/src/main/resources/circos/circos.template"
                              "purity-ploidy-estimator/src/main/resources/circos/input.template")
                 (("<<include etc/")
                  (string-append "<<include " (assoc-ref inputs "circos")
                                 "/share/Circos/etc/"))
                 (("karyotype = data/")
                  (string-append "karyotype = "
                                 (assoc-ref inputs "circos")
                                 "/share/Circos/data/")))))
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
                            "hmfpatients_test" "build" "build" build-dir)))

                ;; Unpack the dependencies downloaded using maven.
                (with-directory-excursion m2-dir
                  (zero? (system (string-append
                                  "tar xvf " (assoc-ref inputs "maven-deps")))))

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
                (unless (zero? (system (format #f "mvn compile --offline --global-settings ~s" settings)))
                  (throw 'compilation-failed "Compilation failed.")))))
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
                     (map (lambda (file)
                            `(,file . ,(basename (string-append (string-drop-right file 26) ".jar"))))
                          (find-files "." "-jar-with-dependencies.jar")))

                ;; To make the package easier to integrate with the accompanying pipeline 4.8,
                ;; we provide symbolic links to the JAR files without version numbers.
                (with-directory-excursion output-dir
                  (symlink "amber-1.7.jar"                    "amber.jar")
                  (symlink "api-clients-local-SNAPSHOT.jar"   "api-clients.jar")
                  (symlink "bachelor-1.2.jar"                 "bachelor.jar")
                  (symlink "bachelor-pp-1.2.jar"              "bachelor-pp.jar")
                  (symlink "bam-slicer-1.3.jar"               "bam-slicer.jar")
                  (symlink "break-point-inspector-1.7.jar"    "break-point-inspector.jar")
                  (symlink "cgi-treatment-extractor-1.2.jar"  "cgi-treatment-extractor.jar")
                  (symlink "count-bam-lines-1.5.jar"          "cobalt.jar")
                  (symlink "data_analyser-1.0.jar"            "data_analyser.jar")
                  (symlink "fastq-stats-1.0.jar"              "fastq-stats.jar")
                  (symlink "hmf-gene-panel-builder-local-SNAPSHOT.jar" "hmf-gene-panel.jar")
                  (symlink "hmf-id-generator-1.4.jar"         "hmf-id-generator.jar")
                  (symlink "knowledgebase-importer-1.2.jar"   "knowledgebase-importer.jar")
                  (symlink "mnv-detector-1.4.jar"             "mnv-detector.jar")
                  (symlink "mnv-validator-1.4.jar"            "mnv-validator.jar")
                  (symlink "patient-db-3.12.jar"              "patient-db.jar")
                  (symlink "portal-data-converter-1.0.jar"    "portal-data-converter.jar")
                  (symlink "purity-ploidy-estimator-2.17.jar" "purple.jar")
                  (symlink "strelka-post-process-1.4.jar"     "strelka-post-process.jar")
                  (symlink "sv-analyser-1.0.jar"              "sv-analyser.jar")
                  (symlink "variant-annotator-1.6.jar"        "variant-annotator.jar"))
                #t))))))
     (inputs
      `(("icedtea" ,icedtea-8 "jdk")
        ("maven" ,maven-bin)
        ("circos" ,circos)))
     ;; Amber uses an R script for BAF segmentation.
     (propagated-inputs
      `(("r" ,r-minimal)
        ("r-copynumber" ,r-copynumber)))
     (native-inputs
      `(("maven-deps"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://www.roelj.com/hmftools-20181102-cc96075-"
                   "maven-dependencies.tar.gz"))
             (sha256
              (base32
               "16ffn537lcmambgl71vil3fp8wb7kqxn2yl384mi3ycqdcq6affx"))))
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

(define-public hmftools-2019-02-28
  (let ((commit "f5e098fda2051bf41dcb541aafcebc21a5f1da4f"))
    (package
     (name "hmftools")
     (version (string-append "20190228-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hartwigmedical/hmftools.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "049qlaydi865526y4sjl3s9hspap4c1z9k0q5a1977lwyvzs42cb"))))
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
                  "<!-- <module>patient-reporter</module> -->")
                 (("<module>actionability-vs-soc</module>")
                  "<!-- <module>actionability-vs-soc</module> -->"))))

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
                           "-e" "CREATE DATABASE hmfpatients_test")))))

           (add-before 'build 'patch-circos-configuration
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("purity-ploidy-estimator/src/main/resources/circos/circos.template"
                              "purity-ploidy-estimator/src/main/resources/circos/input.template")
                 (("<<include etc/")
                  (string-append "<<include " (assoc-ref inputs "circos")
                                 "/share/Circos/etc/"))
                 (("karyotype = data/")
                  (string-append "karyotype = "
                                 (assoc-ref inputs "circos")
                                 "/share/Circos/data/")))))
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
                            "hmfpatients_test" "build" "build" build-dir)))

                ;; Unpack the dependencies downloaded using maven.
                (with-directory-excursion m2-dir
                  (zero? (system (string-append
                                  "tar xvf " (assoc-ref inputs "maven-deps")))))

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
                (unless (zero? (system (format #f "mvn compile --offline --global-settings ~s" settings)))
                  (throw 'compilation-failed "Compilation failed.")))))
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
                     (map (lambda (file)
                            `(,file . ,(basename (string-append (string-drop-right file 26) ".jar"))))
                          (find-files "." "-jar-with-dependencies.jar")))

                ;; To make the package easier to integrate with the accompanying pipeline 4.8,
                ;; we provide symbolic links to the JAR files without version numbers.
                (with-directory-excursion output-dir
                  (symlink "amber-2.0.jar"                    "amber.jar")
                  (symlink "api-clients-local-SNAPSHOT.jar"   "api-clients.jar")
                  (symlink "bachelor-1.4.jar"                 "bachelor.jar")
                  (symlink "bachelor-pp-1.3.jar"              "bachelor-pp.jar")
                  (symlink "bam-slicer-1.3.jar"               "bam-slicer.jar")
                  (symlink "cgi-treatment-extractor-1.3.jar"  "cgi-treatment-extractor.jar")
                  (symlink "count-bam-lines-1.5.jar"          "cobalt.jar")
                  (symlink "fastq-stats-1.2.jar"              "fastq-stats.jar")
                  (symlink "hmf-gene-panel-builder-local-SNAPSHOT.jar" "hmf-gene-panel.jar")
                  (symlink "hmf-id-generator-2.0.jar"         "hmf-id-generator.jar")
                  (symlink "knowledgebase-importer-1.3.jar"   "knowledgebase-importer.jar")
                  (symlink "mnv-detector-1.4.jar"             "mnv-detector.jar")
                  (symlink "mnv-validator-1.4.jar"            "mnv-validator.jar")
                  (symlink "patient-db-3.15.jar"              "patient-db.jar")
                  (symlink "portal-data-converter-1.0.jar"    "portal-data-converter.jar")
                  (symlink "purity-ploidy-estimator-2.19.jar" "purple.jar")
                  (symlink "sage-1.0.jar"                     "sage.jar")
                  (symlink "sig_analyser-1.0.jar"             "sig_analyser.jar")
                  (symlink "strelka-post-process-1.4.jar"     "strelka-post-process.jar")
                  (symlink "sv-analyser-1.0.jar"              "sv-analyser.jar")
                  (symlink "sv-graphs-1.0.jar"                "sv-graphs.jar")
                  (symlink "sv-visualiser-1.0.jar"            "sv-visualiser.jar")
                  (symlink "variant-annotator-2.11.jar"        "variant-annotator.jar"))
             #t))))))
     (native-inputs
      `(("maven-deps"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://www.roelj.com/hmftools-20190228-f5e098f-"
                   "maven-dependencies.tar.gz"))
             (sha256
              (base32
               "06hn8arn9vbyws8ls3k4pz2zgappjhl6rbr4286ygyh9jw9l76pp"))))
        ("mysql" ,mysql-5.6.25)))
     (inputs
      `(("icedtea" ,icedtea-8 "jdk")
        ("maven" ,maven-bin)
        ("circos" ,circos)))
     ;; Amber uses an R script for BAF segmentation.
     (propagated-inputs
      `(("r" ,r-minimal)
        ("r-copynumber" ,r-copynumber)))
     (native-search-paths
      (list (search-path-specification
             (variable "GUIX_JARPATH")
             (files (list "share/java/user-classes")))))
     (home-page "https://github.com/hartwigmedical/hmftools")
     (synopsis "Various utility tools for working with genomics data.")
     (description "This package provides various tools for working with
genomics data developed by the Hartwig Medical Foundation.  This
specific version is compatible with the 4.8 pipeline release.")
     (license license:expat))))

(define-public hmftools-2018-08-08
  (let ((commit "ec08255d06fccd5c3ba65ac25c41ecab99f7cd29"))
    (package
     (name "hmftools")
     (version (string-append "20180808-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hartwigmedical/hmftools.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0rx39x590bycps29vmhrrjsgrpbl2zgik94qvdf3rl8dw7k0lhdr"))))
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
                  "<!-- <module>patient-reporter</module> -->")
                 (("<module>actionability-vs-soc</module>")
                  "<!-- <module>actionability-vs-soc</module> -->"))))

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

           (add-before 'build 'patch-circos-configuration
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("purity-ploidy-estimator/src/main/resources/circos/circos.template"
                              "purity-ploidy-estimator/src/main/resources/circos/input.template")
                 (("<<include etc/")
                  (string-append "<<include " (assoc-ref inputs "circos")
                                 "/share/Circos/etc/"))
                 (("karyotype = data/")
                  (string-append "karyotype = "
                                 (assoc-ref inputs "circos")
                                 "/share/Circos/data/")))))
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
                  (zero? (system (string-append
                                  "tar xvf " (assoc-ref inputs "maven-deps")))))

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
                (unless (zero? (system (format #f "mvn compile --offline --global-settings ~s" settings)))
                  (throw 'compilation-failed "Compilation failed.")))))
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
                     (map (lambda (file)
                            `(,file . ,(basename (string-append (string-drop-right file 26) ".jar"))))
                          (find-files "." "-jar-with-dependencies.jar")))

                ;; To make the package easier to integrate with the accompanying pipeline 4.8,
                ;; we provide symbolic links to the JAR files without version numbers.
                (with-directory-excursion output-dir
                  (symlink "actionability-analyzer-1.0.jar"   "actionability-analyzer.jar")
                  (symlink "amber-1.6.jar"                    "amber.jar")
                  (symlink "api-clients-1.0.jar"              "api-clients.jar")
                  (symlink "bachelor-1.2.jar"                 "bachelor.jar")
                  (symlink "bachelor-pp-1.0.jar"              "bachelor-pp.jar")
                  (symlink "bam-slicer-1.3.jar"               "bam-slicer.jar")
                  (symlink "cgi-treatment-extractor-1.0.jar"  "cgi-treatment-extractor.jar")
                  (symlink "count-bam-lines-1.4.jar"          "cobalt.jar")
                  (symlink "data_analyser-1.0.jar"            "data_analyser.jar")
                  (symlink "fastq-stats-1.0.jar"              "fastq-stats.jar")
                  (symlink "hmf-gene-panel-1.jar"             "hmf-gene-panel.jar")
                  (symlink "hmf-id-generator-1.1.jar"         "hmf-id-generator.jar")
                  (symlink "knowledgebase-importer-1.0.jar"   "knowledgebase-importer.jar")
                  (symlink "mnv-detector-1.4.jar"             "mnv-detector.jar")
                  (symlink "mnv-validator-1.4.jar"            "mnv-validator.jar")
                  (symlink "patient-db-3.11.jar"              "patient-db.jar")
                  (symlink "portal-data-converter-1.0.jar"    "portal-data-converter.jar")
                  (symlink "purity-pathology-1.0.jar"         "purity-pathology.jar")
                  (symlink "purity-ploidy-estimator-2.14.jar" "purple.jar")
                  (symlink "strelka-post-process-1.4.jar"     "strelka-post-process.jar")
                  (symlink "sv-analyser-1.0.jar"              "sv-analyser.jar")
                  (symlink "variant-annotator-1.5.jar"        "variant-annotator.jar"))
             #t))))))
     (native-inputs
      `(("maven-deps"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://www.roelj.com/hmftools-20180808-ec08255"
                   "-maven-dependencies.tar.gz"))
             (sha256
              (base32
               "1c212lkwjf12l4kylm9qgh6isla11ksgs9nlqhjaqp68x0b0gbwn"))))
        ("mysql" ,mysql-5.6.25)))
     (inputs
      `(("icedtea" ,icedtea-8 "jdk")
        ("maven" ,maven-bin)
        ("circos" ,circos)))
     ;; Amber uses an R script for BAF segmentation.
     (propagated-inputs
      `(("r" ,r-minimal)
        ("r-copynumber" ,r-copynumber)))
     (native-search-paths
      (list (search-path-specification
             (variable "GUIX_JARPATH")
             (files (list "share/java/user-classes")))))
     (home-page "https://github.com/hartwigmedical/hmftools")
     (synopsis "Various utility tools for working with genomics data.")
     (description "This package provides various tools for working with
genomics data developed by the Hartwig Medical Foundation.")
     (license license:expat))))

(define-public hmftools-for-pipeline-v3
  (package
   (name "hmftools")
   (version "pipeline-v3-compat")
   (source #f)
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is a meta-package.  No tests need to be executed here.
      #:phases
      (modify-phases %standard-phases
       (delete 'unpack)
       (delete 'configure)
       (delete 'build)
       (replace 'install
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((output-dir (lambda (path)
                               (string-append
                                (assoc-ref outputs "out")
                                "/share/java/user-classes/" path)))
                 (hmftools-2018 (lambda (path)
                                  (string-append
                                   (assoc-ref inputs "hmftools-2018-01-11")
                                   "/share/java/user-classes/" path))))
             (mkdir-p (output-dir ""))
             (chdir (output-dir ""))

             (copy-file (hmftools-2018 "amber-1.5.jar")
                        (output-dir "amber-1.5.jar"))
             (symlink "amber-1.5.jar" "amber.jar")

             (copy-file (hmftools-2018 "bachelor-1.jar")
                        (output-dir "bachelor-1.jar"))
             (symlink "bachelor-1.jar" "bachelor.jar")

             (copy-file (hmftools-2018 "bam-slicer-1.0.jar")
                        (output-dir "bam-slicer-1.0.jar"))
             (symlink "bam-slicer-1.0.jar" "bam-slicer.jar")

             (copy-file (hmftools-2018 "break-point-inspector-1.5.jar")
                        (output-dir "break-point-inspector-1.5.jar"))
             (symlink "break-point-inspector-1.5.jar" "break-point-inspector.jar")

             (copy-file (hmftools-2018 "count-bam-lines-1.2.jar")
                        (output-dir "count-bam-lines-1.2.jar"))
             (symlink "count-bam-lines-1.2.jar" "cobalt.jar")

             (copy-file (hmftools-2018 "fastq-stats-1.0.jar")
                        (output-dir "fastq-stats-1.0.jar"))
             (symlink "fastq-stats-1.0.jar" "fastq-stats.jar")

             (copy-file (hmftools-2018 "hmf-gene-panel-1.jar")
                        (output-dir "hmf-gene-panel-1.jar"))
             (symlink "hmf-gene-panel-1.jar" "hmf-gene-panel.jar")

             (copy-file (hmftools-2018 "patient-db-1.5.jar")
                        (output-dir "patient-db-1.5.jar"))
             (symlink "patient-db-1.5.jar" "patient-db.jar")

             (copy-file (hmftools-2018 "purity-ploidy-estimator-2.5.jar")
                        (output-dir "purity-ploidy-estimator-2.5.jar"))
             (symlink "purity-ploidy-estimator-2.5.jar" "purple.jar")

             ;; strelka-post-process has no version in its filename in the
             ;; 2018 release.
             (copy-file (hmftools-2018 "strelka-post-process.jar")
                        (output-dir "strelka-post-process.jar"))))))))
   (inputs
    `(("hmftools-2018-01-11" ,hmftools-2018-01-11)))
   (native-search-paths
    (list (search-path-specification
           (variable "GUIX_JARPATH")
           (files (list "share/java/user-classes")))))
   ;; Amber uses an R script for BAF segmentation.
   (propagated-inputs
    `(("r" ,r-minimal)
      ("r-copynumber" ,r-copynumber)))
   (home-page "https://github.com/hartwigmedical/hmftools")
   (synopsis "Various utility tools for working with genomics data.")
   (description "This package provides various tools for working with
genomics data developed by the Hartwig Medical Foundation.")
(license license:expat)))

(define-public hmftools-for-pipeline-v4.8
  (package
   (name "hmftools")
   (version "pipeline-v4.8-compat")
   (source #f)
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is a meta-package.  No tests need to be executed here.
      #:phases
      (modify-phases %standard-phases
       (delete 'unpack)
       (delete 'configure)
       (delete 'build)
       (replace 'install
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((output-dir (lambda (path)
                               (string-append
                                (assoc-ref outputs "out")
                                "/share/java/user-classes/" path)))
                 (hmftools-20180808 (lambda (path)
                                      (string-append
                                       (assoc-ref inputs "hmftools-2018-08-08")
                                       "/share/java/user-classes/" path)))
                 (hmftools-20181102 (lambda (path)
                                      (string-append
                                       (assoc-ref inputs "hmftools-2018-11-02")
                                       "/share/java/user-classes/" path))))
             (mkdir-p (output-dir ""))
             (chdir (output-dir ""))
             (symlink (hmftools-20180808 "amber-1.6.jar") "amber.jar")
             (symlink (hmftools-20180808 "count-bam-lines-1.4.jar") "cobalt.jar")
             (symlink (hmftools-20180808 "strelka-post-process-1.4.jar") "strelka-post-process.jar")
             (symlink (hmftools-20181102 "purity-ploidy-estimator-2.17.jar") "purple.jar")))))))
   (inputs
    `(("hmftools-2018-08-08" ,hmftools-2018-08-08)
      ("hmftools-2018-11-02" ,hmftools-2018-11-02)))
   (native-search-paths
    (list (search-path-specification
           (variable "GUIX_JARPATH")
           (files (list "share/java/user-classes")))))
   ;; Amber uses an R script for BAF segmentation.
   (propagated-inputs
    `(("r" ,r-minimal)
      ("r-copynumber" ,r-copynumber)))
   (home-page "https://github.com/hartwigmedical/hmftools")
   (synopsis "Various utility tools for working with genomics data.")
   (description "This package provides tools developed by Hartwig Medical
Foundation that are used in the accompanying HMF pipeline (version 4.8).")
(license license:expat)))

(define-public hmftools
  (package
   (name "hmftools")
   (version "pipeline-v4-compat")
   (source #f)
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is a meta-package.  No tests need to be executed here.
      #:phases
      (modify-phases %standard-phases
       (delete 'unpack)
       (delete 'configure)
       (delete 'build)
       (replace 'install
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((output-dir (lambda (path)
                               (string-append
                                (assoc-ref outputs "out")
                                "/share/java/user-classes/" path)))
                 (hmftools-2018 (lambda (path)
                                  (string-append
                                   (assoc-ref inputs "hmftools-2018-06-19")
                                   "/share/java/user-classes/" path))))
             (mkdir-p (output-dir ""))
             (chdir (output-dir ""))
             (symlink (hmftools-2018 "actionability-analyzer-1.0.jar")   "actionability-analyzer.jar")
             (symlink (hmftools-2018 "api-clients-1.0.jar")              "api-clients.jar")
             (symlink (hmftools-2018 "amber-1.5.jar")                    "amber.jar")
             (symlink (hmftools-2018 "bachelor-1.2.jar")                 "bachelor.jar")
             (symlink (hmftools-2018 "bachelor-pp-1.0.jar")              "bachelor-pp.jar")
             (symlink (hmftools-2018 "bam-slicer-1.3.jar")               "bam-slicer.jar")
             (symlink (hmftools-2018 "break-point-inspector-1.6.jar")    "break-point-inspector.jar")
             (symlink (hmftools-2018 "count-bam-lines-1.4.jar")          "cobalt.jar")
             (symlink (hmftools-2018 "fastq-stats-1.0.jar")              "fastq-stats.jar")
             (symlink (hmftools-2018 "hmf-gene-panel-1.jar")             "hmf-gene-panel.jar")
             (symlink (hmftools-2018 "hmf-id-generator-1.0.jar")         "hmf-id-generator.jar")
             (symlink (hmftools-2018 "knowledgebase-importer-1.0.jar")   "knowledgebase-importer.jar")
             (symlink (hmftools-2018 "mnv-detector-1.4.jar")             "mnv-detector.jar")
             (symlink (hmftools-2018 "mnv-validator-1.4.jar")            "mnv-validator.jar")
             (symlink (hmftools-2018 "patient-db-3.8.jar")               "patient-db.jar")
             (symlink (hmftools-2018 "portal-data-converter-1.0.jar")    "portal-data-converter.jar")
             (symlink (hmftools-2018 "purity-pathology-1.0.jar")         "purity-pathology.jar")
             (symlink (hmftools-2018 "purity-ploidy-estimator-2.14.jar") "purple.jar")
             (symlink (hmftools-2018 "strelka-post-process-1.4.jar")     "strelka-post-process.jar")
             (symlink (hmftools-2018 "sv-analyser-1.0.jar")              "sv-analyser.jar")
             (symlink (hmftools-2018 "variant-annotator-1.4.jar")        "variant-annotator.jar")))))))
   (inputs
    `(("hmftools-2018-06-19" ,hmftools-2018-06-19)))
   (native-search-paths
    (list (search-path-specification
           (variable "GUIX_JARPATH")
           (files (list "share/java/user-classes")))))
   ;; Amber uses an R script for BAF segmentation.
   (propagated-inputs
    `(("r" ,r-minimal)
      ("r-copynumber" ,r-copynumber)))
   (home-page "https://github.com/hartwigmedical/hmftools")
   (synopsis "Various utility tools for working with genomics data.")
   (description "This package provides various tools for working with
genomics data developed by the Hartwig Medical Foundation.")
   (license license:expat)))

(define-public hmftools-next
  (package
   (name "hmftools")
   (version "pipeline-v4-4-compat")
   (source #f)
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is a meta-package.  No tests need to be executed here.
      #:phases
      (modify-phases %standard-phases
       (delete 'unpack)
       (delete 'configure)
       (delete 'build)
       (replace 'install
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((output-dir (lambda (path)
                               (string-append
                                (assoc-ref outputs "out")
                                "/share/java/user-classes/" path)))
                 (hmftools-2018 (lambda (path)
                                  (string-append
                                   (assoc-ref inputs "hmftools-2018-11-02")
                                   "/share/java/user-classes/" path))))
             (mkdir-p (output-dir ""))
             (chdir (output-dir ""))
             (symlink (hmftools-2018 "api-clients-local-SNAPSHOT.jar")   "api-clients.jar")
             (symlink (hmftools-2018 "amber-1.7.jar")                    "amber.jar")
             (symlink (hmftools-2018 "bachelor-1.2.jar")                 "bachelor.jar")
             (symlink (hmftools-2018 "bachelor-pp-1.2.jar")              "bachelor-pp.jar")
             (symlink (hmftools-2018 "bam-slicer-1.3.jar")               "bam-slicer.jar")
             (symlink (hmftools-2018 "break-point-inspector-1.7.jar")    "break-point-inspector.jar")
             (symlink (hmftools-2018 "count-bam-lines-1.5.jar")          "cobalt.jar")
             (symlink (hmftools-2018 "fastq-stats-1.0.jar")              "fastq-stats.jar")
             (symlink (hmftools-2018 "hmf-gene-panel-builder-local-SNAPSHOT.jar") "hmf-gene-panel.jar")
             (symlink (hmftools-2018 "hmf-id-generator-1.4.jar")         "hmf-id-generator.jar")
             (symlink (hmftools-2018 "knowledgebase-importer-1.2.jar")   "knowledgebase-importer.jar")
             (symlink (hmftools-2018 "mnv-detector-1.4.jar")             "mnv-detector.jar")
             (symlink (hmftools-2018 "mnv-validator-1.4.jar")            "mnv-validator.jar")
             (symlink (hmftools-2018 "patient-db-3.12.jar")               "patient-db.jar")
             (symlink (hmftools-2018 "portal-data-converter-1.0.jar")    "portal-data-converter.jar")
             (symlink (hmftools-2018 "purity-ploidy-estimator-2.17.jar") "purple.jar")
             (symlink (hmftools-2018 "strelka-post-process-1.4.jar")     "strelka-post-process.jar")
             (symlink (hmftools-2018 "sv-analyser-1.0.jar")              "sv-analyser.jar")
             (symlink (hmftools-2018 "variant-annotator-1.6.jar")        "variant-annotator.jar")))))))
   (inputs
    `(("hmftools-2018-11-02" ,hmftools-2018-11-02)))
   (native-search-paths
    (list (search-path-specification
           (variable "GUIX_JARPATH")
           (files (list "share/java/user-classes")))))
   ;; Amber uses an R script for BAF segmentation.
   (propagated-inputs
    `(("r" ,r-minimal)
      ("r-copynumber" ,r-copynumber)))
   (home-page "https://github.com/hartwigmedical/hmftools")
   (synopsis "Various utility tools for working with genomics data.")
   (description "This package provides various tools for working with
genomics data developed by the Hartwig Medical Foundation.")
   (license license:expat)))

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
     `(("sambamba" ,sambamba-0.6.8)))
    (home-page "https://github.com/UMCUGenetics/ExonCov")
    (synopsis "Exon coverage statistics from BAM files")
    (description "This package can generate exon coverage statistics from
BAM files using @code{sambamba}.")
    (license license:expat)))

(define-public exoncov-2.1.1
  (package (inherit exoncov)
    (version "2.1.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/UMCUGenetics/ExonCov/archive/v"
                   version ".tar.gz"))
             (sha256
              (base32 "1a22mjmfgmjs9jx2wpx87zd4vzig97mdw8m2gr99cdlmssi3rdka"))))))

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
                (string-append (assoc-ref inputs "r-minimal") "/bin/Rscript"))
               (("my \\$command = \"perl")
                (string-append "my $command = \"" (assoc-ref inputs "perl") "/bin/perl"))
               (("qsub")
                (string-append (assoc-ref inputs "grid-engine-core") "/bin/qsub -V"))
               (("use POSIX qw\\(tmpnam\\);") "use File::Temp qw/ :POSIX /;")
               (("use File::Basename qw\\( dirname \\);")
                "use File::Basename qw( dirname fileparse );")
               (("\\$id =~ s/\\\\/tmp\\\\/file//;") "$id = fileparse($id);"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bindir (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bindir)
               (map delete-file '("LICENSE" ".gitignore" "README.md"))
               ;; TODO: Only copy bamMetrics.pl to the bindir, and other stuff
               ;; to its appropriate location.
               (copy-recursively "." bindir)
               #t))))))
    (inputs
     `(("sambamba" ,sambamba-0.6.8)
       ("perl" ,perl)
       ("r-minimal" ,r-minimal)
       ("picard" ,picard-bin-1.141)
       ("icedtea" ,icedtea-8)
       ("grid-engine-core" ,grid-engine-core)))
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-knitr" ,r-knitr)
       ("r-markdown" ,r-markdown)
       ("r-reshape" ,r-reshape)
       ("r-xtable" ,r-xtable)
       ("r-getoptlong" ,r-getoptlong)
       ("r-brew" ,r-brew)
       ("r" ,r)
       ("texlive" ,texlive)
       ("texinfo" ,texinfo)
       ("tar" ,tar)))
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
                (output-dir (string-append %output "/share/damage-estimator"))
                (files '("estimate_damage.pl"
                         "estimate_damage_location.pl"
                         "estimate_damage_location_context.pl"
                         "plot_damage.R"
                         "plot_damage_location.R"
                         "plot_damage_location_context.R"
                         "plot_random_sampling_damage.R"
                         "random_sampling_and_estimate_damage.pl"
                         "randomized2"
                         "split_mapped_reads.pl")))
            (mkdir-p output-dir)
            (map (lambda (file)
                   (install-file (string-append source-dir "/" file)
                                 output-dir))
                 files)
            ;; Patch samtools for Guix's samtools.
            (substitute* (string-append output-dir "/split_mapped_reads.pl")
              ((" = \"samtools")
               (string-append " = \"" (assoc-ref %build-inputs "samtools")
                              "/bin/samtools")))
            (substitute* (map (lambda (file)
                                (string-append output-dir "/" file)) files)
              (("#!/usr/bin/perl")
               (string-append "#!" (assoc-ref %build-inputs "perl")
                              "/bin/perl"))
              (("#!/usr/bin/env Rscript")
               (string-append "#!" (assoc-ref %build-inputs "r")
                              "/bin/Rscript"))
              (("#!/usr/bin/env ruby")
               (string-append "#!" (assoc-ref %build-inputs "ruby")
                              "/bin/ruby")))))))
     (native-inputs
      `(("source" ,source)))
     (inputs
      `(("perl" ,perl)
        ("r" ,r)
        ("ruby" ,ruby)))
     (propagated-inputs
      `(("samtools" ,samtools)
        ("r-ggplot2" ,r-ggplot2)
        ("r-reshape2" ,r-reshape2)))
     (home-page "https://github.com/Ettwiller/Damage-estimator")
     (synopsis "")
     (description "")
     (license license:agpl3))))

(define-public hmf-damage-estimator
  (package
   (name "hmf-damage-estimator")
   (version "1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://www.roelj.com/damage_estimator-"
                  version "-hmf.tar.gz"))
            (sha256
             (base32 "1fbyrmb2kzfbsw92agy715wqpkci2nkqwxlz7pb4qh5psk6crslg"))))
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
               (source-dir (string-append (getcwd) "/source"))
               (output-dir (string-append %output "/share/damage-estimator"))
               (files '("estimate_damage.pl"
                       "estimate_damage_location.pl"
                       "estimate_damage_location_context.pl"
                       "plot_damage.R"
                       "plot_damage_location.R"
                       "plot_damage_location_context.R"
                       "plot_random_sampling_damage.R"
                       "random_sampling_and_estimate_damage.pl"
                       "randomized2"
                       "split_mapped_reads.pl")))
          (setenv "PATH" PATH)
          (mkdir source-dir)
          (chdir source-dir)
          (system* tar "xvf" tarball)

          (mkdir-p output-dir)
          (map (lambda (file)
                 (install-file (string-append source-dir "/" file)
                               output-dir))
               files)

          ;; Patch samtools for Guix's samtools.
          (substitute* (string-append output-dir "/split_mapped_reads.pl")
                       (("`samtools")
                        (string-append "`" (assoc-ref %build-inputs "samtools")
                                       "/bin/samtools"))
                       ((" = \"samtools")
                        (string-append " = \"" (assoc-ref %build-inputs "samtools")
                                       "/bin/samtools"))
                       ((" = \"sambamba")
                        (string-append " = \"" (assoc-ref %build-inputs "sambamba")
                                       "/bin/sambamba")))
          (substitute* (string-append output-dir
                                      "/random_sampling_and_estimate_damage.pl")
                       (("\"perl") (string-append
                                    "\"" (assoc-ref %build-inputs "perl")
                                    "/bin/perl"))
                       (("new File::Temp\\( UNLINK => 1 \\)")
                        "tempfile(\".bam-tempXXXXXX\", DIR => dirname($out), UNLINK => 1)"))
          (substitute* (map (lambda (file)
                              (string-append output-dir "/" file)) files)
                       (("#!/usr/bin/perl")
                        (string-append "#!" (assoc-ref %build-inputs "perl")
                                       "/bin/perl"))
                       (("#!/usr/bin/env Rscript")
                        (string-append "#!" (assoc-ref %build-inputs "r")
                                       "/bin/Rscript"))
                       (("#!/usr/bin/env ruby")
                        (string-append "#!" (assoc-ref %build-inputs "ruby")
                                       "/bin/ruby")))))))
   (native-inputs
    `(("source" ,source)
      ("tar" ,tar)
      ("gzip" ,gzip)))
   (inputs
    `(("perl" ,perl)
      ("r" ,r)
      ("ruby" ,ruby)))
   (propagated-inputs
    `(("samtools" ,samtools)
      ("sambamba" ,sambamba)
      ("r-ggplot2" ,r-ggplot2)
      ("r-reshape2" ,r-reshape2)))
   (home-page "https://github.com/Ettwiller/Damage-estimator")
   (synopsis "")
   (description "")
   (license license:agpl3)))

(define-public r-matrixstats-0.50.2
  (package
    (inherit r-matrixstats)
    (version "0.50.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cran.rstudio.com/src/contrib/Archive/"
                    "matrixStats/matrixStats_" version ".tar.gz"))
              (sha256
               (base32 "0zj27xxx9cyrq16rn4g3l0krqg68p8f2qp18w1w4i767j87amlbj"))))))

(define-public r-qdnaseq-1.9.2
  (let ((commit "cd622dbc67f22160b821cd9044589954024549ae"))
    (package (inherit r-qdnaseq)
      (name "r-qdnaseq")
      (version "1.9.2")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Bioconductor-mirror/QDNAseq")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1n0vxyvqy47lamr7y3lmpvp0w3z3c8fs7rnfqcyzddpjblm63fkq"))))
      (propagated-inputs
       `(("r-matrixstats" ,r-matrixstats-0.50.2)
         ("r-biobase" ,r-biobase)
         ("r-cghbase" ,r-cghbase)
         ("r-cghcall" ,r-cghcall)
         ("r-dnacopy" ,r-dnacopy)
         ("r-genomicranges" ,r-genomicranges)
         ("r-iranges" ,r-iranges)
         ("r-r-utils" ,r-r-utils)
         ("r-rsamtools" ,r-rsamtools))))))

(define-public hmf-pipeline
  (package
    (name "hmf-pipeline")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hartwigmedical/pipeline/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "04fh3bs0pspjp2ih7hnv1dsbd26l2j7mmg57bmm18js390hkj8qh"))))
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
               (patch-bin (string-append (assoc-ref %build-inputs "patch") "/bin/patch"))
               (pipeline-dir (string-append %output "/share/hmf-pipeline"))
               (settings-dir (string-append %output "/share/hmf-pipeline/settings"))
               (qscripts-dir (string-append %output "/share/hmf-pipeline/QScripts"))
               (templates-dir (string-append %output "/share/hmf-pipeline/templates"))
               (scripts-dir (string-append %output "/share/hmf-pipeline/scripts"))
               (lib-dir (string-append %output "/lib/perl5/site_perl/" ,(package-version perl)))
               (perlbin (string-append (assoc-ref %build-inputs "perl") "/bin/perl"))
               (shbin (string-append (assoc-ref %build-inputs "bash") "/bin/sh"))
               (pythonbin (string-append (assoc-ref %build-inputs "python") "/bin/python")))
           (setenv "PATH" PATH)

           ;; Create the directory structure in the build output directory.
           (map mkdir-p (list lib-dir
                              scripts-dir
                              qscripts-dir
                              settings-dir
                              templates-dir))

           ;; Extract the modules into the Perl path.
           (with-directory-excursion lib-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/lib/")
                      "--strip-components=2"))

           ;; Extract the template scripts to their own custom directory.
           (with-directory-excursion templates-dir
             (system* tar "xvf" tarball
                      (string-append "pipeline-" ,version "/templates")
                      "--strip-components=2"))

           ;; Extract the settings files to their own custom directory.
           (with-directory-excursion settings-dir
             (system* tar "xvf" tarball
                      (string-append "pipeline-" ,version "/settings")
                      "--strip-components=2"))

           ;; Apply the following patches to make the pipeline compatible with
           ;; the latest versions of Cobalt and StrelkaPostProcess.
           (with-directory-excursion %output
             (format #t "Applying patches... ")
             (let ((patch1 (assoc-ref %build-inputs "patch1"))
                   (patch2 (assoc-ref %build-inputs "patch2"))
                   (patch3 (assoc-ref %build-inputs "patch3"))
                   (patch4 (assoc-ref %build-inputs "patch4"))
                   (patch5 (assoc-ref %build-inputs "patch5"))
                   (patch6 (assoc-ref %build-inputs "patch6"))
                   (patch7 (assoc-ref %build-inputs "patch7")))
               (format
                #t
                (if (and (zero? (system (string-append patch-bin " -p1 < " patch1)))
                         (zero? (system (string-append patch-bin " -p1 < " patch2)))
                         (zero? (system (string-append patch-bin " -p1 < " patch3)))
                         (zero? (system (string-append patch-bin " -p1 < " patch4)))
                         (zero? (system (string-append patch-bin " -p1 < " patch5)))
                         (zero? (system (string-append patch-bin " -p1 < " patch6)))
                         (zero? (system (string-append patch-bin " -p1 < " patch7))))
                    " Succeeded.~%"
                    " Failed.~%"))))

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
                      "--strip-components=2")

             ;; Patch the shebangs of the scripts.
             (substitute* "annotatePON.py"
               (("#!/usr/bin/env python") (string-append "#!" pythonbin)))
             (substitute* "convert_delly_TRA.pl"
               (("#!/usr/bin/env perl") (string-append "#!" perlbin)))
             (substitute* "run_QDNAseq.R"
               (("load_all\\(qdnaseq_path\\)")
                "library(\"QDNAseq\")")))

           ;; Extract QScripts to their own custom directory.
           (with-directory-excursion qscripts-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/QScripts")
                      "--strip-components=2"))

           (with-directory-excursion templates-dir
             ;; Replace the 'java' command with the full path to the input 'java'
             ;; in each template file.
             (substitute* '("Amber.sh.tt" "BAF.sh.tt" "BaseRecalibration.sh.tt"
                            "BreakpointInspector.sh.tt" "CallableLoci.sh.tt"
                            "Cobalt.sh.tt" "GermlineCalling.sh.tt"
                            "GermlineFiltering.sh.tt" "HealthCheck.sh.tt"
                            "PostStats.sh.tt" "Purple.sh.tt" "Realignment.sh.tt"
                            "Strelka.sh.tt")
               (("java -Xmx")
                (string-append (assoc-ref %build-inputs "icedtea-8")
                               "/bin/java -Xmx")))

             (substitute* '("StrelkaPostProcess.sh.tt")
               (("java -")
                (string-append (assoc-ref %build-inputs "icedtea-8") "/bin/java -"))
               ;; Work-around for:
               ;; https://github.com/hartwigmedical/pipeline/issues/18
               (("\\[% opt.OUTPUT_DIR %\\]/scripts/annotatePON.py")
                (string-append pythonbin " [% opt.OUTPUT_DIR %]/scripts/annotatePON.py")))

             ;; Work-around for:
             ;; https://github.com/hartwigmedical/pipeline/issues/18
             (substitute* "Delly.sh.tt"
               (("\\[% opt.OUTPUT_DIR %\\]/scripts/convert_delly_TRA.pl")
                (string-append perlbin " [% opt.OUTPUT_DIR %]/scripts/convert_delly_TRA.pl")))

             ;; Fix a path mistake in BAF.sh.tt and CallableLoci.sh.tt
             (substitute* '("BAF.sh.tt" "CallableLoci.sh.tt")
               (("-jar \"\\[% opt.QUEUE_PATH %\\]/GenomeAnalysisTK.jar\"")
                "-jar \"[% opt.GATK_PATH %]/GenomeAnalysisTK.jar\""))

             ;; Patch the Perl command
             (substitute* "PostStats.sh.tt"
               (("perl \\[% opt.BAMMETRICS_PATH %\\]/bamMetrics.pl")
                (string-append perlbin " [% opt.BAMMETRICS_PATH %]/bamMetrics.pl")))

             ;; Patch the path to the FREEC scripts directory
             (substitute* "Freec.sh.tt"
               (("< \\[% opt.FREEC_PATH %\\]")
                (string-append "< " (assoc-ref %build-inputs "freec") "/share/freec"))
               (("< \"\\[% opt.FREEC_PATH %\\]")
                (string-append "< \"" (assoc-ref %build-inputs "freec") "/share/freec")))

             ;; Patch the 'make' command.
             (substitute* "Strelka.sh.tt"
               (("make -j") (string-append (assoc-ref %build-inputs "make")
                                           "/bin/make -j")))

             (substitute* (scandir "." (lambda (item)
                                         (not (eq? (string-ref item 0) #\.))))
               (("rm ")
                (string-append (assoc-ref %build-inputs "coreutils")
                               "/bin/rm "))
               (("mv ")
                (string-append (assoc-ref %build-inputs "coreutils")
                               "/bin/mv "))
               (("grep ")
                (string-append (assoc-ref %build-inputs "grep") "/bin/grep "))
               (("find ")
                (string-append (assoc-ref %build-inputs "findutils") "/bin/find "))
               (("awk ")
                (string-append (assoc-ref %build-inputs "gawk") "/bin/awk "))
               (("diff -u")
                (string-append (assoc-ref %build-inputs "diffutils") "/bin/diff -u"))
               (("touch \"")
                (string-append (assoc-ref %build-inputs "coreutils")
                               "/bin/touch \""))
               (("mkdir ")
                (string-append (assoc-ref %build-inputs "coreutils")
                               "/bin/mkdir "))
               (("mkfifo ")
                (string-append (assoc-ref %build-inputs "coreutils")
                               "/bin/mkfifo "))
               (("wc ")
                (string-append (assoc-ref %build-inputs "coreutils")
                               "/bin/wc "))
               (("Rscript ")
                (string-append (assoc-ref %build-inputs "r-minimal") "/bin/Rscript "))
               (("/usr/bin/env perl") perlbin)
               ;; Use "sh" instead of "bash" to prevent loading bash
               ;; configuration files that modify the program's environment.
               (("/usr/bin/env bash") shbin))

             (substitute* "Kinship.sh.tt"
               (("cp ")
                (string-append (assoc-ref %build-inputs "coreutils") "/bin/cp "))))

           (with-directory-excursion settings-dir
             ;; Add a prefix to the 'INIFILE' directory specification.
             (substitute*
              (scandir "." (lambda (item) (string-suffix? "ini" item)))
              (("INIFILE	settings")
               (string-append "INIFILE	" settings-dir)))

             (with-directory-excursion "include"
               (substitute*
                (scandir "." (lambda (item) (string-suffix? "ini" item)))
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
                         (string-append (assoc-ref %build-inputs "gatk") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "gatk") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "gatk") "/share/java/user-classes")
                         (assoc-ref %build-inputs "strelka")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "circos") "/bin")
                         (string-append (assoc-ref %build-inputs "freec") "/bin")
                         (string-append (assoc-ref %build-inputs "r-qdnaseq") "/site-library/QDNAseq")
                         (string-append (assoc-ref %build-inputs "delly") "/bin")
                         (string-append (assoc-ref %build-inputs "manta") "/bin")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "igvtools") "/share/java/igvtools")
                         (string-append (assoc-ref %build-inputs "samtools") "/bin")
                         (string-append (assoc-ref %build-inputs "htslib") "/bin")
                         (string-append (assoc-ref %build-inputs "plink") "/bin")
                         (string-append (assoc-ref %build-inputs "king") "/bin")
                         (string-append (assoc-ref %build-inputs "bio-vcf") "/bin")
                         (string-append (assoc-ref %build-inputs "bamutils") "/bin")
                         (string-append (assoc-ref %build-inputs "pbgzip") "/bin")
                         (string-append (assoc-ref %build-inputs "snpeff") "/share/java/snpeff")
                         (string-append (assoc-ref %build-inputs "vcftools") "/bin")
                         (string-append (assoc-ref %build-inputs "bcftools") "/bin")
                         ;; HEALTH-CHECKER
                         (string-append (assoc-ref %build-inputs "coreutils") "/bin/true")))))

           (with-directory-excursion %output
             ;; Extract the main scripts into the bin directory.
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
                (string-append "my $settingsDir = \"" settings-dir  "\";"))))

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
               (("rcopy \\$slice_dir") "$File::Copy::Recursive::KeepMode = 0; rcopy $slice_dir"))

             (substitute* "HMF/Pipeline/Sge.pm"
               ;; Over-allocate by 4G for each job, because some SGE
               ;; implementations have memory overhead on each job.
               (("my \\$qsub = generic\\(\\$opt, \\$function\\) . \" -m a")
                "my $h_vmem = (4 + $opt->{$function.\"_MEM\"}).\"G\"; my $qsub = generic($opt, $function) . \" -m as -M $opt->{MAIL} -V -l h_vmem=$h_vmem")
               ;; Make sure that environment variables are passed along
               ;; to the jobs correctly.
               (("qsub -P") "qsub -m as -M $opt->{MAIL} -V -P")
               ;; Also apply the 4GB over-allocation to GATK-Queue-spawned jobs.
               (("my \\$qsub = generic\\(\\$opt, \\$function\\);")
                "my $h_vmem = (4 + $opt->{$function.\"_MEM\"}).\"G\"; my $qsub = generic($opt, $function) . \" -m as -M $opt->{MAIL} -l h_vmem=$h_vmem\";")
               ))))))
    (inputs
     `(("bammetrics" ,bammetrics)
       ("bamutils" ,bamutils)
       ("bash" ,bash)
       ("bwa" ,bwa-0.7.5a)
       ("damage-estimator" ,hmf-damage-estimator)
       ("delly" ,delly)
       ("exoncov" ,exoncov)
       ("fastqc" ,fastqc-bin-0.11.4)
       ("freec" ,freec-10.4)
       ("gatk" ,gatk-full-3.5-patched-bin)
       ("hmftools" ,hmftools-for-pipeline-v3)
       ("htslib" ,htslib)
       ("icedtea-8" ,icedtea-8)
       ("igvtools" ,igvtools-bin-2.3.60)
       ("king" ,king-bin-2.1.2)
       ("manta" ,manta)
       ("pbgzip" ,pbgzip)
       ("perl" ,perl)
       ("picard" ,picard-bin-1.141)
       ("plink" ,plink)
       ("python" ,python-2)
       ("make" ,gnu-make)
       ("findutils" ,findutils)
       ("diffutils" ,diffutils)
       ("r-minimal" ,r-minimal)))
    (native-inputs
     `(("gzip" ,gzip)
       ("source" ,source)
       ("tar" ,tar)
       ("patch" ,patch)
       ("patch1" ,(origin
                    (method url-fetch)
                    (uri (search-patch "0001-Adapt-command-line-options-for-StrelkaPostProcess.patch"))
                    (sha256 (base32 "0b7a0pmg5q0j1qqsbhl5y2phr5ndjyc0mlrsc2gbzv0d6c0ddmp3"))))
       ("patch2" ,(origin
                    (method url-fetch)
                    (uri (search-patch "0002-Adapt-Cobalt-command-line-options.patch"))
                    (sha256 (base32 "142z53nf9z4657h4yidia2va1gq83wdh7nzx008648vf9rlk2nl6"))))
       ("patch3" ,(origin
                   (method url-fetch)
                   (uri (search-patch "0003-Use-escaped-tabs-for-the-read-group.patch"))
                   (sha256 (base32 "0jac1fl1f17l29s3qqz6g6qq8hkhr8gbsr5kkhqylww62a6ybyh5"))))
       ("patch4" ,(origin
                   (method url-fetch)
                   (uri (search-patch "0004-Use-bcftools-to-annotate-PON-data.patch"))
                   (sha256 (base32 "05a01p5l8pigwan5zlzplanbg1pgvv8rbx3llir1hnqs7vvs3b1q"))))
       ("patch5" ,(origin
                   (method url-fetch)
                   (uri (search-patch "0005-Add-somatic-PON-filtering.patch"))
                   (sha256 (base32 "1bphww76pwj1cc968c50sdar37z5mf6yf6bnz5cw6xayvm6591sb"))))
       ("patch6" ,(origin
                   (method url-fetch)
                   (uri (search-patch "0006-Remove-FREEC_SNPFILE-option.patch"))
                   (sha256 (base32 "1gfb32l4hisgacx7ld40ib1fhc231m6sj7rr98am0mys3zy9lydm"))))
       ("patch7" ,(origin
                   (method url-fetch)
                   (uri (search-patch "0007-Properly-detect-and-run-fragment-mode.patch"))
                   (sha256 (base32 "04dqzk8i6khm7wnry5c8ad9cbj8w1l96ljwribjsdb5rxj36dg0a"))))))
    (propagated-inputs
     `(("bash" ,bash)
       ("bcftools" ,bcftools)
       ("bio-vcf" ,bio-vcf)
       ("circos" ,circos)
       ("perl-autovivification" ,perl-autovivification)
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
       ("icedtea-8" ,icedtea-8)
       ("r-biobase" ,r-biobase)
       ("r-biocstyle" ,r-biocstyle)
       ("r-bsgenome" ,r-bsgenome)
       ("r-copynumber" ,r-copynumber)
       ("r-cghbase" ,r-cghbase)
       ("r-cghcall" ,r-cghcall)
       ("r-devtools" ,r-devtools)
       ("r-digest" ,r-digest)
       ("r-dnacopy" ,r-dnacopy)
       ("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gtools" ,r-gtools)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
       ("r-pastecs" ,r-pastecs)
       ("r-qdnaseq" ,r-qdnaseq-hmf)
       ("r-r-utils" ,r-r-utils)
       ("r-roxygen2" ,r-roxygen2)
       ("r-rsamtools" ,r-rsamtools)
       ("r" ,r)
       ("sambamba" ,sambamba-0.6.8)
       ("samtools" ,samtools)
       ("snpeff" ,snpeff-bin-4.3t)
       ("strelka" ,strelka-1.0.14)
       ("vcftools" ,vcftools-0.1.14)
       ("coreutils" ,coreutils)
       ("grep" ,grep-with-pcre)
       ("sed" ,sed)
       ("gawk" ,gawk)
       ("perl" ,perl)
       ("inetutils" ,inetutils)
       ("util-linux" ,util-linux)
       ("grid-engine" ,grid-engine-core)
       ,@(package-propagated-inputs bammetrics)
       ,@(package-propagated-inputs gatk-full-3.5-patched-bin)))
    ;; Bash, Perl and R are not propagated into the profile.  The programs are
    ;; invoked using their absolute link from the 'tools.ini' file.  We must
    ;; make sure that the environment variables for these interpreters are
    ;; set correctly.
    (native-search-paths
     (append (package-native-search-paths bash)
             (package-native-search-paths grid-engine-core)
             (package-native-search-paths perl)
             (package-native-search-paths r)
             (package-native-search-paths ruby)))
    (search-paths native-search-paths)
    (home-page "https://github.com/hartwigmedical/pipeline")
    (synopsis "Default Hartwig Medical Data processing pipeline")
    (description "Pipeline of tools to process raw fastq data and
produce meaningful genomic data from Hartwig Medical.")
    (license license:expat)))

(define-public hmf-pipeline-data-resources
  (package
    (name "hmf-pipeline-data-resources")
    (version "4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://nc.hartwigmedicalfoundation.nl/index.php/s/"
                    "a8lgLsUrZI5gndd/download?path=/HMF-Pipeline-Resources"))
              (sha256
               (base32
                "0rlbzxqilapgljpf8b1w4xv4ph8jlaaqldyl2yjhlgm2i06wcmza"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar     (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (unzip   (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
               (tarball (assoc-ref %build-inputs "source"))
               (out     (string-append %output "/share/hmf-pipeline")))
           (mkdir-p out)
           (with-directory-excursion out
             (system* unzip tarball)
             (rename-file (string-append out "/HMF-Pipeline-Resources")
                          (string-append out "/resources")))
           (with-directory-excursion (string-append out "/resources")
             (system* unzip "Mappability.zip"))
           (with-directory-excursion (string-append out "/resources")
             (system* tar "xvf" (assoc-ref %build-inputs "germline-pon")))))))
    (native-inputs
     `(("tar" ,tar)
       ("unzip" ,unzip)
       ("germline-pon"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://nc.hartwigmedicalfoundation.nl/index.php/s/"
                 "a8lgLsUrZI5gndd/download?path="
                 "/HMF_PON_v2.0.tar"))
           (sha256
            (base32 "0m2ka2v95mbda5vfqylr7nnwq0k9yr9yrmwzlhfcj91cl3ihf70f"))))))
    (home-page "https://github.com/hartwigmedical/pipeline")
    (synopsis "Data resources for the Hartwig Medical pipeline")
    (description "This package contains files that are needed by the Hartwig
Medical pipeline.  Please see the README.pdf file for usage restrictions.")
    ;; See the README.pdf for restrictions.
    (license #f)))

(define-public hmf-pipeline-v4.1
  (package
    (name "hmf-pipeline")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hartwigmedical/pipeline/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "0254gljj654kbwsspvavyk7895s2jyk2i37nvkjziawfwidrb626"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 ftw))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw))
         (let ((tar           (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (PATH          (string-append (assoc-ref %build-inputs "gzip") "/bin"))
               (tarball       (assoc-ref %build-inputs "source"))
               (current-dir   (getcwd))
               (bin-dir       (string-append %output "/bin"))
               (pipeline-dir  (string-append %output "/share/hmf-pipeline"))
               (settings-dir  (string-append %output "/share/hmf-pipeline/settings"))
               (qscripts-dir  (string-append %output "/share/hmf-pipeline/QScripts"))
               (templates-dir (string-append %output "/share/hmf-pipeline/templates"))
               (scripts-dir   (string-append %output "/share/hmf-pipeline/scripts"))
               (lib-dir       (string-append %output "/lib/perl5/site_perl/" ,(package-version perl)))
               (perlbin       (string-append (assoc-ref %build-inputs "perl") "/bin/perl"))
               (shbin         (string-append (assoc-ref %build-inputs "bash") "/bin/sh"))
               (pythonbin     (string-append (assoc-ref %build-inputs "python") "/bin/python")))

           (setenv "PATH" PATH)

           ;; Create the directory structure in the build output directory.
           (map mkdir-p (list lib-dir scripts-dir qscripts-dir settings-dir templates-dir))

           ;; Extract the modules into the Perl path.
           (with-directory-excursion lib-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/lib/")
                      "--strip-components=2"))

           ;; Extract the template scripts to their own custom directory.
           (with-directory-excursion templates-dir
             (system* tar "xvf" tarball
                      (string-append "pipeline-" ,version "/templates")
                      "--strip-components=2"))

           ;; Extract the settings files to their own custom directory.
           (with-directory-excursion settings-dir
             (system* tar "xvf" tarball
                      (string-append "pipeline-" ,version "/settings")
                      "--strip-components=2"))

           ;; Patch the use of external tools
           (substitute* (list (string-append lib-dir "/HMF/Pipeline/Functions/Config.pm")
                              (string-append lib-dir "/HMF/Pipeline/Functions/Validate.pm"))
             (("qx\\(\\$samtools ") (string-append "qx(" (assoc-ref %build-inputs "samtools") "/bin/samtools "))
             (("qx\\(bash ")        (string-append "qx(" (assoc-ref %build-inputs "bash") "/bin/bash "))
             (("qx\\(cat ")         (string-append "qx(" (assoc-ref %build-inputs "coreutils") "/bin/cat ")))

           ;; Extract scripts to their own custom directory.
           (with-directory-excursion scripts-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/scripts")
                      "--strip-components=2"))

           ;; Extract QScripts to their own custom directory.
           (with-directory-excursion qscripts-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/QScripts")
                      "--strip-components=2"))

           (with-directory-excursion templates-dir
             (substitute* (scandir "." (lambda (item)
                                         (not (eq? (string-ref item 0) #\.))))
               (("rm ")      (string-append (assoc-ref %build-inputs "coreutils")  "/bin/rm "))
               (("mv ")      (string-append (assoc-ref %build-inputs "coreutils")  "/bin/mv "))
               (("grep ")    (string-append (assoc-ref %build-inputs "grep") "/bin/grep "))
               (("find ")    (string-append (assoc-ref %build-inputs "findutils") "/bin/find "))
               (("awk ")     (string-append (assoc-ref %build-inputs "gawk") "/bin/awk "))
               (("diff -u")  (string-append (assoc-ref %build-inputs "diffutils") "/bin/diff -u"))
               (("touch \"") (string-append (assoc-ref %build-inputs "coreutils") "/bin/touch \""))
               (("mkdir ")   (string-append (assoc-ref %build-inputs "coreutils") "/bin/mkdir "))
               (("mkfifo ")  (string-append (assoc-ref %build-inputs "coreutils") "/bin/mkfifo "))
               (("wc ")      (string-append (assoc-ref %build-inputs "coreutils") "/bin/wc "))
               (("Rscript ") (string-append (assoc-ref %build-inputs "r-minimal") "/bin/Rscript "))
               (("java ")    (string-append (assoc-ref %build-inputs "icedtea-8") "/bin/java "))
               (("qsub ")    (string-append (assoc-ref %build-inputs "grid-engine") "/bin/qsub -V "))
               (("/usr/bin/env perl") perlbin)
               ;; Use "sh" instead of "bash" to prevent loading bash
               ;; configuration files that modify the program's environment.
               (("/usr/bin/env bash") shbin)))

           (with-directory-excursion settings-dir
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

GATK_QUEUE_PATH	~a
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
GRIDSS_PATH	~a
GRIDSS_BWA_PATH	~a

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
HEALTH_CHECKER_PATH	/tmp

REALIGNMENT_SCALA	IndelRealignment.scala
BASERECALIBRATION_SCALA	BaseRecalibration.scala
GERMLINE_CALLING_SCALA	GermlineCaller.scala
GERMLINE_FILTER_SCALA	GermlineFilter.scala

REPORT_STATUS	~a"
                         (string-append (assoc-ref %build-inputs "bwa") "/bin")
                         (string-append (assoc-ref %build-inputs "sambamba") "/bin")
                         (string-append (assoc-ref %build-inputs "fastqc") "/bin")
                         (string-append (assoc-ref %build-inputs "picard") "/share/java/picard")
                         (string-append (assoc-ref %build-inputs "bammetrics") "/bin")
                         (string-append (assoc-ref %build-inputs "exoncov") "/bin")
                         (string-append (assoc-ref %build-inputs "damage-estimator") "/share/damage-estimator")
                         (string-append (assoc-ref %build-inputs "gatk-queue") "/share/java/gatk")
                         (string-append (assoc-ref %build-inputs "gatk") "/share/java/gatk")
                         (assoc-ref %build-inputs "strelka")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "circos") "/bin")
                         (string-append (assoc-ref %build-inputs "freec") "/bin")
                         (string-append (assoc-ref %build-inputs "r-qdnaseq") "/site-library/QDNAseq")
                         (string-append (assoc-ref %build-inputs "delly") "/bin")
                         (string-append (assoc-ref %build-inputs "manta") "/bin")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "gridss") "/share/java/gridss")
                         (string-append (assoc-ref %build-inputs "bwa-0.7.17") "/bin")
                         (string-append (assoc-ref %build-inputs "igvtools") "/share/java/igvtools")
                         (string-append (assoc-ref %build-inputs "samtools") "/bin")
                         (string-append (assoc-ref %build-inputs "htslib") "/bin")
                         (string-append (assoc-ref %build-inputs "plink") "/bin")
                         (string-append (assoc-ref %build-inputs "king") "/bin")
                         (string-append (assoc-ref %build-inputs "bio-vcf") "/bin")
                         (string-append (assoc-ref %build-inputs "bamutils") "/bin")
                         (string-append (assoc-ref %build-inputs "pbgzip") "/bin")
                         (string-append (assoc-ref %build-inputs "snpeff") "/share/java/snpeff")
                         (string-append (assoc-ref %build-inputs "vcftools") "/bin")
                         (string-append (assoc-ref %build-inputs "bcftools") "/bin")
                         ;; HEALTH-CHECKER
                         (string-append (assoc-ref %build-inputs "coreutils") "/bin/true")))))

           (with-directory-excursion %output
             ;; Extract the main scripts into the bin directory.
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
             (substitute* "HMF/Pipeline/Functions/Template.pm"
               (("my \\$source_template_dir = catfile\\(HMF::Pipeline::Functions::Config::pipelinePath\\(\\), \"templates\"\\);")
                (string-append "my $source_template_dir = \"" templates-dir "\";")))

             ;; Make sure the other subdirectories can be found.
             (substitute* "HMF/Pipeline/Functions/Config.pm"
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
               (("rcopy \\$slice_dir") "$File::Copy::Recursive::KeepMode = 0; rcopy $slice_dir"))

             (substitute* "HMF/Pipeline/Functions/Sge.pm"
               ;; Over-allocate by 5G for each job, because some SGE
               ;; implementations have memory overhead on each job.
               (("my \\$qsub = generic\\(\\$opt, \\$function\\) . \" -m a")
                "my $h_vmem = (5 + $opt->{$function.\"_MEM\"}).\"G\"; my $qsub = generic($opt, $function) . \" -m as -M $opt->{MAIL} -V -l h_vmem=$h_vmem")
               ;; Make sure that environment variables are passed along
               ;; to the jobs correctly.
               (("qsub -P") "qsub -m as -M $opt->{MAIL} -V -P")
               ;; Also apply the 5GB over-allocation to GATK-Queue-spawned jobs.
               (("my \\$qsub = generic\\(\\$opt, \\$function\\);")
                "my $h_vmem = (5 + $opt->{$function.\"_MEM\"}).\"G\"; my $qsub = generic($opt, $function) . \" -m as -M $opt->{MAIL} -l h_vmem=$h_vmem\";")
               ))))))
    (inputs
     `(("bammetrics" ,bammetrics)
       ("bamutils" ,bamutils)
       ("bash" ,bash)
       ("bwa" ,bwa-0.7.5a)
       ("bwa-0.7.17" ,bwa)
       ("damage-estimator" ,hmf-damage-estimator)
       ("delly" ,delly)
       ("exoncov" ,exoncov)
       ("fastqc" ,fastqc-bin-0.11.4)
       ("freec" ,freec-10.4)
       ("gatk" ,gatk-bin-3.8-0)
       ("gatk-queue" ,gatk-queue-bin-3.8-0)
       ("gridss" ,gridss-bin)
       ("hmftools" ,hmftools)
       ("htslib" ,htslib)
       ("icedtea-8" ,icedtea-8)
       ("igvtools" ,igvtools-bin-2.3.60)
       ("king" ,king-bin-2.1.2)
       ("manta" ,manta)
       ("pbgzip" ,pbgzip)
       ("perl" ,perl)
       ("picard" ,picard-bin-1.141)
       ("plink" ,plink)
       ("python" ,python-2)
       ("make" ,gnu-make)
       ("findutils" ,findutils)
       ("diffutils" ,diffutils)
       ("r-minimal" ,r-minimal)))
    (native-inputs
     `(("gzip" ,gzip)
       ("source" ,source)
       ("tar" ,tar)))
    (propagated-inputs
     `(("bash" ,bash)
       ("bcftools" ,bcftools)
       ("bio-vcf" ,bio-vcf)
       ("circos" ,circos)
       ("perl-autovivification" ,perl-autovivification)
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
       ("icedtea-8" ,icedtea-8)
       ("r-biobase" ,r-biobase)
       ("r-biocstyle" ,r-biocstyle)
       ("r-bsgenome" ,r-bsgenome)
       ("r-copynumber" ,r-copynumber)
       ("r-cghbase" ,r-cghbase)
       ("r-cghcall" ,r-cghcall)
       ("r-devtools" ,r-devtools)
       ("r-digest" ,r-digest)
       ("r-dnacopy" ,r-dnacopy)
       ("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gtools" ,r-gtools)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
       ("r-pastecs" ,r-pastecs)
       ("r-qdnaseq" ,r-qdnaseq-hmf)
       ("r-r-utils" ,r-r-utils)
       ("r-roxygen2" ,r-roxygen2)
       ("r-rsamtools" ,r-rsamtools)
       ("r" ,r)
       ("sambamba" ,sambamba-0.6.8)
       ("samtools" ,samtools)
       ("snpeff" ,snpeff-bin-4.3t)
       ("strelka" ,strelka-1.0.14)
       ("vcftools" ,vcftools-0.1.14)
       ("coreutils" ,coreutils)
       ("grep" ,grep-with-pcre)
       ("sed" ,sed)
       ("gawk" ,gawk)
       ("perl" ,perl)
       ("inetutils" ,inetutils)
       ("util-linux" ,util-linux)
       ("grid-engine" ,grid-engine-core)
       ,@(package-propagated-inputs bammetrics)
       ,@(package-propagated-inputs gatk-bin-3.8-0)))
    ;; Bash, Perl and R are not propagated into the profile.  The programs are
    ;; invoked using their absolute link from the 'tools.ini' file.  We must
    ;; make sure that the environment variables for these interpreters are
    ;; set correctly.
    (native-search-paths
     (append (package-native-search-paths bash)
             (package-native-search-paths grid-engine-core)
             (package-native-search-paths perl)
             (package-native-search-paths r)
             (package-native-search-paths ruby)))
    (search-paths native-search-paths)
    (home-page "https://github.com/hartwigmedical/pipeline")
    (synopsis "Default Hartwig Medical Data processing pipeline")
    (description "Pipeline of tools to process raw fastq data and
produce meaningful genomic data from Hartwig Medical.")
    (license license:expat)))

(define-public hmf-pipeline-v4.1-novalidate
  (package
    (name "hmf-pipeline-novalidate")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hartwigmedical/pipeline/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "0254gljj654kbwsspvavyk7895s2jyk2i37nvkjziawfwidrb626"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 ftw))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw))
         (let ((tar           (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (PATH          (string-append (assoc-ref %build-inputs "gzip") "/bin"))
               (tarball       (assoc-ref %build-inputs "source"))
               (current-dir   (getcwd))
               (bin-dir       (string-append %output "/bin"))
               (patch-bin     (string-append (assoc-ref %build-inputs "patch") "/bin/patch"))
               (pipeline-dir  (string-append %output "/share/hmf-pipeline"))
               (settings-dir  (string-append %output "/share/hmf-pipeline/settings"))
               (qscripts-dir  (string-append %output "/share/hmf-pipeline/QScripts"))
               (templates-dir (string-append %output "/share/hmf-pipeline/templates"))
               (scripts-dir   (string-append %output "/share/hmf-pipeline/scripts"))
               (lib-dir       (string-append %output "/lib/perl5/site_perl/" ,(package-version perl)))
               (perlbin       (string-append (assoc-ref %build-inputs "perl") "/bin/perl"))
               (shbin         (string-append (assoc-ref %build-inputs "bash") "/bin/sh"))
               (pythonbin     (string-append (assoc-ref %build-inputs "python") "/bin/python")))

           (setenv "PATH" PATH)

           ;; Create the directory structure in the build output directory.
           (map mkdir-p (list lib-dir scripts-dir qscripts-dir settings-dir templates-dir))

           ;; Extract the modules into the Perl path.
           (with-directory-excursion lib-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/lib/")
                      "--strip-components=2"))

           ;; Extract the template scripts to their own custom directory.
           (with-directory-excursion templates-dir
             (system* tar "xvf" tarball
                      (string-append "pipeline-" ,version "/templates")
                      "--strip-components=2"))

           ;; Extract the settings files to their own custom directory.
           (with-directory-excursion settings-dir
             (system* tar "xvf" tarball
                      (string-append "pipeline-" ,version "/settings")
                      "--strip-components=2"))

           ;; Apply the following patches to skip the read group validation.
           (with-directory-excursion %output
             (format #t "Applying patches... ")
             (let ((patch1 (assoc-ref %build-inputs "patch1")))
               (format
                #t
                (if (and (zero? (system (string-append patch-bin " -p1 < " patch1))))
                    " Succeeded.~%"
                    " Failed.~%"))))

           ;; Patch the use of external tools
           (substitute* (list (string-append lib-dir "/HMF/Pipeline/Functions/Config.pm")
                              (string-append lib-dir "/HMF/Pipeline/Functions/Validate.pm"))
             (("qx\\(\\$samtools ") (string-append "qx(" (assoc-ref %build-inputs "samtools") "/bin/samtools "))
             (("qx\\(bash ")        (string-append "qx(" (assoc-ref %build-inputs "bash") "/bin/bash "))
             (("qx\\(cat ")         (string-append "qx(" (assoc-ref %build-inputs "coreutils") "/bin/cat ")))

           ;; Extract scripts to their own custom directory.
           (with-directory-excursion scripts-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/scripts")
                      "--strip-components=2"))

           ;; Extract QScripts to their own custom directory.
           (with-directory-excursion qscripts-dir
             (system* tar "xvf" tarball (string-append "pipeline-" ,version "/QScripts")
                      "--strip-components=2"))

           (with-directory-excursion templates-dir
             (substitute* (scandir "." (lambda (item)
                                         (not (eq? (string-ref item 0) #\.))))
               (("rm ")      (string-append (assoc-ref %build-inputs "coreutils")  "/bin/rm "))
               (("mv ")      (string-append (assoc-ref %build-inputs "coreutils")  "/bin/mv "))
               (("grep ")    (string-append (assoc-ref %build-inputs "grep") "/bin/grep "))
               (("find ")    (string-append (assoc-ref %build-inputs "findutils") "/bin/find "))
               (("awk ")     (string-append (assoc-ref %build-inputs "gawk") "/bin/awk "))
               (("diff -u")  (string-append (assoc-ref %build-inputs "diffutils") "/bin/diff -u"))
               (("touch \"") (string-append (assoc-ref %build-inputs "coreutils") "/bin/touch \""))
               (("mkdir ")   (string-append (assoc-ref %build-inputs "coreutils") "/bin/mkdir "))
               (("mkfifo ")  (string-append (assoc-ref %build-inputs "coreutils") "/bin/mkfifo "))
               (("wc ")      (string-append (assoc-ref %build-inputs "coreutils") "/bin/wc "))
               (("Rscript ") (string-append (assoc-ref %build-inputs "r-minimal") "/bin/Rscript "))
               (("java ")    (string-append (assoc-ref %build-inputs "icedtea-8") "/bin/java "))
               (("qsub ")    (string-append (assoc-ref %build-inputs "grid-engine") "/bin/qsub -V "))
               (("/usr/bin/env perl") perlbin)
               ;; Use "sh" instead of "bash" to prevent loading bash
               ;; configuration files that modify the program's environment.
               (("/usr/bin/env bash") shbin)))

           (with-directory-excursion settings-dir
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

GATK_QUEUE_PATH	~a
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
GRIDSS_PATH	~a
GRIDSS_BWA_PATH	~a

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
HEALTH_CHECKER_PATH	/tmp

REALIGNMENT_SCALA	IndelRealignment.scala
BASERECALIBRATION_SCALA	BaseRecalibration.scala
GERMLINE_CALLING_SCALA	GermlineCaller.scala
GERMLINE_FILTER_SCALA	GermlineFilter.scala

REPORT_STATUS	~a"
                         (string-append (assoc-ref %build-inputs "bwa") "/bin")
                         (string-append (assoc-ref %build-inputs "sambamba") "/bin")
                         (string-append (assoc-ref %build-inputs "fastqc") "/bin")
                         (string-append (assoc-ref %build-inputs "picard") "/share/java/picard")
                         (string-append (assoc-ref %build-inputs "bammetrics") "/bin")
                         (string-append (assoc-ref %build-inputs "exoncov") "/bin")
                         (string-append (assoc-ref %build-inputs "damage-estimator") "/share/damage-estimator")
                         (string-append (assoc-ref %build-inputs "gatk-queue") "/share/java/gatk")
                         (string-append (assoc-ref %build-inputs "gatk") "/share/java/gatk")
                         (assoc-ref %build-inputs "strelka")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "circos") "/bin")
                         (string-append (assoc-ref %build-inputs "freec") "/bin")
                         (string-append (assoc-ref %build-inputs "r-qdnaseq") "/site-library/QDNAseq")
                         (string-append (assoc-ref %build-inputs "delly") "/bin")
                         (string-append (assoc-ref %build-inputs "manta") "/bin")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "gridss") "/share/java/gridss")
                         (string-append (assoc-ref %build-inputs "bwa-0.7.17") "/bin")
                         (string-append (assoc-ref %build-inputs "igvtools") "/share/java/igvtools")
                         (string-append (assoc-ref %build-inputs "samtools") "/bin")
                         (string-append (assoc-ref %build-inputs "htslib") "/bin")
                         (string-append (assoc-ref %build-inputs "plink") "/bin")
                         (string-append (assoc-ref %build-inputs "king") "/bin")
                         (string-append (assoc-ref %build-inputs "bio-vcf") "/bin")
                         (string-append (assoc-ref %build-inputs "bamutils") "/bin")
                         (string-append (assoc-ref %build-inputs "pbgzip") "/bin")
                         (string-append (assoc-ref %build-inputs "snpeff") "/share/java/snpeff")
                         (string-append (assoc-ref %build-inputs "vcftools") "/bin")
                         (string-append (assoc-ref %build-inputs "bcftools") "/bin")
                         ;; HEALTH-CHECKER
                         (string-append (assoc-ref %build-inputs "coreutils") "/bin/true")))))

           (with-directory-excursion %output
             ;; Extract the main scripts into the bin directory.
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
             (substitute* "HMF/Pipeline/Functions/Template.pm"
               (("my \\$source_template_dir = catfile\\(HMF::Pipeline::Functions::Config::pipelinePath\\(\\), \"templates\"\\);")
                (string-append "my $source_template_dir = \"" templates-dir "\";")))

             ;; Make sure the other subdirectories can be found.
             (substitute* "HMF/Pipeline/Functions/Config.pm"
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
               (("rcopy \\$slice_dir") "$File::Copy::Recursive::KeepMode = 0; rcopy $slice_dir"))

             (substitute* "HMF/Pipeline/Functions/Sge.pm"
               ;; Over-allocate by 5G for each job, because some SGE
               ;; implementations have memory overhead on each job.
               (("my \\$qsub = generic\\(\\$opt, \\$function\\) . \" -m a")
                "my $h_vmem = (5 + $opt->{$function.\"_MEM\"}).\"G\"; my $qsub = generic($opt, $function) . \" -m as -M $opt->{MAIL} -V -l h_vmem=$h_vmem")
               ;; Make sure that environment variables are passed along
               ;; to the jobs correctly.
               (("qsub -P") "qsub -m as -M $opt->{MAIL} -V -P")
               ;; Also apply the 5GB over-allocation to GATK-Queue-spawned jobs.
               (("my \\$qsub = generic\\(\\$opt, \\$function\\);")
                "my $h_vmem = (5 + $opt->{$function.\"_MEM\"}).\"G\"; my $qsub = generic($opt, $function) . \" -m as -M $opt->{MAIL} -l h_vmem=$h_vmem\";")
               ))))))
    (inputs
     `(("bammetrics" ,bammetrics)
       ("bamutils" ,bamutils)
       ("bash" ,bash)
       ("bwa" ,bwa-0.7.5a)
       ("bwa-0.7.17" ,bwa)
       ("damage-estimator" ,hmf-damage-estimator)
       ("delly" ,delly)
       ("exoncov" ,exoncov)
       ("fastqc" ,fastqc-bin-0.11.4)
       ("freec" ,freec-10.4)
       ("gatk" ,gatk-bin-3.8-0)
       ("gatk-queue" ,gatk-queue-bin-3.8-0)
       ("gridss" ,gridss-bin)
       ("hmftools" ,hmftools)
       ("htslib" ,htslib)
       ("icedtea-8" ,icedtea-8)
       ("igvtools" ,igvtools-bin-2.3.60)
       ("king" ,king-bin-2.1.2)
       ("manta" ,manta)
       ("pbgzip" ,pbgzip)
       ("perl" ,perl)
       ("picard" ,picard-bin-1.141)
       ("plink" ,plink)
       ("python" ,python-2)
       ("make" ,gnu-make)
       ("findutils" ,findutils)
       ("diffutils" ,diffutils)
       ("r-minimal" ,r-minimal)))
    (native-inputs
     `(("gzip" ,gzip)
       ("source" ,source)
       ("tar" ,tar)
       ("patch" ,patch)
       ("patch1" ,(origin
                    (method url-fetch)
                    (uri (search-patch "hmf-pipeline-skip-readgroup-validation.patch"))
                    (sha256 (base32 "0l9lax3pvbmm7a0gbbn1hvb9l0h4pid8xp3sbpgps5xxy1vnf7f1"))))))
    (propagated-inputs
     `(("bash" ,bash)
       ("bcftools" ,bcftools)
       ("bio-vcf" ,bio-vcf)
       ("circos" ,circos)
       ("perl-autovivification" ,perl-autovivification)
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
       ("icedtea-8" ,icedtea-8)
       ("r-biobase" ,r-biobase)
       ("r-biocstyle" ,r-biocstyle)
       ("r-bsgenome" ,r-bsgenome)
       ("r-copynumber" ,r-copynumber)
       ("r-cghbase" ,r-cghbase)
       ("r-cghcall" ,r-cghcall)
       ("r-devtools" ,r-devtools)
       ("r-digest" ,r-digest)
       ("r-dnacopy" ,r-dnacopy)
       ("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gtools" ,r-gtools)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
       ("r-pastecs" ,r-pastecs)
       ("r-qdnaseq" ,r-qdnaseq-hmf)
       ("r-r-utils" ,r-r-utils)
       ("r-roxygen2" ,r-roxygen2)
       ("r-rsamtools" ,r-rsamtools)
       ("r" ,r)
       ("sambamba" ,sambamba-0.6.8)
       ("samtools" ,samtools)
       ("snpeff" ,snpeff-bin-4.3t)
       ("strelka" ,strelka-1.0.14)
       ("vcftools" ,vcftools-0.1.14)
       ("coreutils" ,coreutils)
       ("grep" ,grep-with-pcre)
       ("sed" ,sed)
       ("gawk" ,gawk)
       ("perl" ,perl)
       ("inetutils" ,inetutils)
       ("util-linux" ,util-linux)
       ("grid-engine" ,grid-engine-core)
       ,@(package-propagated-inputs bammetrics)
       ,@(package-propagated-inputs gatk-bin-3.8-0)))
    ;; Bash, Perl and R are not propagated into the profile.  The programs are
    ;; invoked using their absolute link from the 'tools.ini' file.  We must
    ;; make sure that the environment variables for these interpreters are
    ;; set correctly.
    (native-search-paths
     (append (package-native-search-paths bash)
             (package-native-search-paths grid-engine-core)
             (package-native-search-paths perl)
             (package-native-search-paths r)
             (package-native-search-paths ruby)))
    (search-paths native-search-paths)
    (home-page "https://github.com/hartwigmedical/pipeline")
    (synopsis "Default Hartwig Medical Data processing pipeline")
    (description "Pipeline of tools to process raw fastq data and
produce meaningful genomic data from Hartwig Medical.")
    (license license:expat)))

;; ----------------------------------------------------------------------------
;; HMF-PIPELINE 4.8
;; ----------------------------------------------------------------------------

(define-public hmf-pipeline-v4.8
  (package
    (name "hmf-pipeline")
    (version "4.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hartwigmedical/pipeline/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0ih55dhc564g9a3w6cw8v8dcw6rr5mjrh04x4g4axl5rqcpc5arm"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 ftw))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw))
         (let* ((tar           (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
                (PATH          (string-append (assoc-ref %build-inputs "gzip") "/bin"))
                (tarball       (assoc-ref %build-inputs "source"))
                (current-dir   (getcwd))
                (bin-dir       (string-append %output "/bin"))
                (patch-bin     (string-append (assoc-ref %build-inputs "patch") "/bin/patch"))
                (pipeline-dir  (string-append %output "/share/hmf-pipeline"))
                (settings-dir  (string-append %output "/share/hmf-pipeline/settings"))
                (qscripts-dir  (string-append %output "/share/hmf-pipeline/QScripts"))
                (templates-dir (string-append %output "/share/hmf-pipeline/templates"))
                (scripts-dir   (string-append %output "/share/hmf-pipeline/scripts"))
                (lib-dir       (string-append %output "/lib/perl5/site_perl/" ,(package-version perl)))
                (perlbin       (string-append (assoc-ref %build-inputs "perl") "/bin/perl"))
                (shbin         (string-append (assoc-ref %build-inputs "bash") "/bin/sh"))
                (pythonbin     (string-append (assoc-ref %build-inputs "python") "/bin/python"))
                (extract-files (lambda (output-dir input-dir)
                                 (with-directory-excursion output-dir
                                   (system* tar "xvf" tarball
                                            (string-append "pipeline-" ,version "/" input-dir)
                                            "--strip-components=2")))))

           (setenv "PATH" PATH)

           ;; Create the directory structure in the build output directory.
           (map mkdir-p (list lib-dir scripts-dir qscripts-dir settings-dir templates-dir))

           ;; Extract the modules into the Perl path.
           (extract-files lib-dir "lib")

           ;; Extract the template scripts to their own custom directory.
           (extract-files templates-dir "templates")

           ;; Extract the settings files to their own custom directory.
           (extract-files settings-dir "settings")

           ;; Extract scripts to their own custom directory.
           (extract-files scripts-dir "scripts")

           ;; Apply the following patches to skip the read group validation.
           (with-directory-excursion %output
             (format #t "Applying patches... ")
             (let ((patch1 (assoc-ref %build-inputs "patch1"))
                   (patch2 (assoc-ref %build-inputs "patch2")))
               (if (and (zero? (system (string-append patch-bin " -p1 < " patch1)))
                        (zero? (system (string-append patch-bin " -p1 < " patch2))))
                   (format #t " Succeeded.~%")
                   (begin
                     (format #t " Failed.~%")
                     (throw 'applying-patch-failure)))))

           ;; Patch the use of external tools
           (substitute* (list (string-append lib-dir "/HMF/Pipeline/Functions/Config.pm")
                              (string-append lib-dir "/HMF/Pipeline/Functions/Validate.pm"))
             (("qx\\(\\$samtools ") (string-append "qx(" (assoc-ref %build-inputs "samtools") "/bin/samtools "))
             (("qx\\(bash ")        (string-append "qx(" (assoc-ref %build-inputs "bash") "/bin/bash "))
             (("qx\\(cat ")         (string-append "qx(" (assoc-ref %build-inputs "coreutils") "/bin/cat ")))

           ;; Extract QScripts to their own custom directory.
           (extract-files qscripts-dir "QScripts")

           (with-directory-excursion templates-dir
             (substitute* (scandir "." (lambda (item)
                                         (not (eq? (string-ref item 0) #\.))))
               (("rm ")      (string-append (assoc-ref %build-inputs "coreutils")  "/bin/rm "))
               (("mv ")      (string-append (assoc-ref %build-inputs "coreutils")  "/bin/mv "))
               (("grep ")    (string-append (assoc-ref %build-inputs "grep") "/bin/grep "))
               (("find ")    (string-append (assoc-ref %build-inputs "findutils") "/bin/find "))
               (("awk ")     (string-append (assoc-ref %build-inputs "gawk") "/bin/awk "))
               (("diff -u")  (string-append (assoc-ref %build-inputs "diffutils") "/bin/diff -u"))
               (("touch \"") (string-append (assoc-ref %build-inputs "coreutils") "/bin/touch \""))
               (("mkdir ")   (string-append (assoc-ref %build-inputs "coreutils") "/bin/mkdir "))
               (("mkfifo ")  (string-append (assoc-ref %build-inputs "coreutils") "/bin/mkfifo "))
               (("wc ")      (string-append (assoc-ref %build-inputs "coreutils") "/bin/wc "))
               (("Rscript ") (string-append (assoc-ref %build-inputs "r-minimal") "/bin/Rscript "))
               (("java ")    (string-append (assoc-ref %build-inputs "icedtea-8") "/bin/java "))
               (("qsub ")    (string-append (assoc-ref %build-inputs "grid-engine") "/bin/qsub -V "))
               (("/usr/bin/env perl") perlbin)
               ;; Use "sh" instead of "bash" to prevent loading bash
               ;; configuration files that modify the program's environment.
               (("/usr/bin/env bash") shbin)))

           (with-directory-excursion settings-dir
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
SAMTOOLS_PATH	~a

BAMMETRICS_PATH	~a
PICARD_PATH	~a
DAMAGE_ESTIMATOR_PATH	~a
BAMUTIL_PATH	~a

GATK_PATH	~a
GATK_QUEUE_PATH	~a

STRELKA_PATH	~a
STRELKA_POST_PROCESS_PATH	~a

AMBER_PATH	~a
COBALT_PATH	~a
PURPLE_PATH	~a
CIRCOS_PATH	~a

GRIDSS_PATH	~a
GRIDSS_BWA_PATH	~a

SNPEFF_PATH	~a
BCFTOOLS_PATH	~a
TABIX_PATH	~a

HEALTH_CHECKER_PATH	/tmp

REALIGNMENT_SCALA	IndelRealignment.scala
BASERECALIBRATION_SCALA	BaseRecalibration.scala
GERMLINE_CALLING_SCALA	GermlineCaller.scala
GERMLINE_FILTER_SCALA	GermlineFilter.scala

REPORT_STATUS	~a"
                         (string-append (assoc-ref %build-inputs "bwa") "/bin")
                         (string-append (assoc-ref %build-inputs "sambamba") "/bin")
                         (string-append (assoc-ref %build-inputs "samtools") "/bin")
                         (string-append (assoc-ref %build-inputs "bammetrics") "/bin")
                         (string-append (assoc-ref %build-inputs "picard") "/share/java/picard")
                         (string-append (assoc-ref %build-inputs "damage-estimator") "/share/damage-estimator")
                         (string-append (assoc-ref %build-inputs "bamutils") "/bin")
                         (string-append (assoc-ref %build-inputs "gatk") "/share/java/gatk")
                         (string-append (assoc-ref %build-inputs "gatk-queue") "/share/java/gatk")
                         (assoc-ref %build-inputs "strelka")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "hmftools") "/share/java/user-classes")
                         (string-append (assoc-ref %build-inputs "circos") "/bin")
                         (string-append (assoc-ref %build-inputs "gridss") "/share/java/gridss")
                         (string-append (assoc-ref %build-inputs "bwa-0.7.17") "/bin")
                         (string-append (assoc-ref %build-inputs "snpeff") "/share/java/snpeff")
                         (string-append (assoc-ref %build-inputs "bcftools") "/bin")
                         (string-append (assoc-ref %build-inputs "htslib") "/bin")
                         ;; HEALTH-CHECKER
                         (string-append (assoc-ref %build-inputs "coreutils") "/bin/true")))))

           (with-directory-excursion %output
             ;; Extract the main scripts into the bin directory.
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
             (substitute* "HMF/Pipeline/Functions/Template.pm"
               (("my \\$source_template_dir = catfile\\(HMF::Pipeline::Functions::Config::pipelinePath\\(\\), \"templates\"\\);")
                (string-append "my $source_template_dir = \"" templates-dir "\";")))

             ;; Make sure the other subdirectories can be found.
             (substitute* "HMF/Pipeline/Functions/Config.pm"
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
               (("rcopy \\$slice_dir") "$File::Copy::Recursive::KeepMode = 0; rcopy $slice_dir"))

             (substitute* "HMF/Pipeline/Functions/Sge.pm"
               ;; Over-allocate by 5G for each job, because some SGE
               ;; implementations have memory overhead on each job.
               (("my \\$qsub = generic\\(\\$opt, \\$function\\) . \" -m a")
                "my $h_vmem = (5 + $opt->{$function.\"_MEM\"}).\"G\"; my $qsub = generic($opt, $function) . \" -m as -M $opt->{MAIL} -V -l h_vmem=$h_vmem")
               ;; Make sure that environment variables are passed along
               ;; to the jobs correctly.
               (("qsub -P") "qsub -m as -M $opt->{MAIL} -V -P")
               ;; Also apply the 5GB over-allocation to GATK-Queue-spawned jobs.
               (("my \\$qsub = generic\\(\\$opt, \\$function\\);")
                "my $h_vmem = (5 + $opt->{$function.\"_MEM\"}).\"G\"; my $qsub = generic($opt, $function) . \" -m as -M $opt->{MAIL} -l h_vmem=$h_vmem\";")
               ))))))
    (inputs
     `(("bammetrics" ,bammetrics)
       ("bamutils" ,bamutils)
       ("bash" ,bash)
       ("bwa" ,bwa-0.7.5a)
       ("bwa-0.7.17" ,bwa)
       ("damage-estimator" ,hmf-damage-estimator)
       ("gatk" ,gatk-bin-3.8-0)
       ("gatk-queue" ,gatk-queue-bin-3.8-0)
       ("gridss" ,gridss-bin)
       ("hmftools" ,hmftools-for-pipeline-v4.8)
       ("htslib" ,htslib)
       ("icedtea-8" ,icedtea-8)
       ("igvtools" ,igvtools-bin-2.3.60)
       ("king" ,king-bin-2.1.2)
       ("perl" ,perl)
       ("picard" ,picard-bin-1.141)
       ("python" ,python-2)
       ("make" ,gnu-make)
       ("findutils" ,findutils)
       ("diffutils" ,diffutils)
       ("r-minimal" ,r-minimal)))
    (native-inputs
     `(("gzip" ,gzip)
       ("source" ,source)
       ("tar" ,tar)
       ("patch" ,patch)
       ("patch1" ,(origin
                    (method url-fetch)
                    (uri (search-patch "hmf-pipeline-skip-readgroup-validation.patch"))
                    (sha256 (base32 "1pdivvkbqiv80cjqnj0dgsq8yd2s62ch464cylad5n5ian8n1q5f"))))
       ("patch2" ,(origin
                    (method url-fetch)
                    (uri (search-patch "hmf-pipeline-disable-required-pon.patch"))
                    (sha256 (base32 "15znidznrvy00j10ig3caqp6dqm399y702sdgpsz15bmc9yzlq10"))))))
    (propagated-inputs
     `(("bash" ,bash)
       ("bcftools" ,bcftools)
       ("circos" ,circos)
       ("perl-autovivification" ,perl-autovivification)
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
       ("icedtea-8" ,icedtea-8)
       ("r-argparser" ,r-argparser)
       ("r-biobase" ,r-biobase)
       ("r-biocstyle" ,r-biocstyle)
       ("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-copynumber" ,r-copynumber)
       ("r-cghbase" ,r-cghbase)
       ("r-cghcall" ,r-cghcall)
       ("r-devtools" ,r-devtools)
       ("r-digest" ,r-digest)
       ("r-dnacopy" ,r-dnacopy)
       ("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gtools" ,r-gtools)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
       ("r-pastecs" ,r-pastecs)
       ("r-qdnaseq" ,r-qdnaseq-hmf)
       ("r-readr" ,r-readr)
       ("r-r-utils" ,r-r-utils)
       ("r-roxygen2" ,r-roxygen2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-stringr" ,r-stringr)
       ("r-stringdist" ,r-stringdist)
       ("r-structuralvariantannotation" ,r-structuralvariantannotation)
       ("r-testthat" ,r-testthat)
       ("r-tidyverse" ,r-tidyverse)
       ("r-variantannotation" ,r-variantannotation)
       ("r" ,r)
       ("sambamba" ,sambamba-0.6.8)
       ("samtools" ,samtools)
       ("snpeff" ,snpeff-bin-4.3t)
       ("strelka" ,strelka-1.0.14)
       ("coreutils" ,coreutils)
       ("grep" ,grep-with-pcre)
       ("sed" ,sed)
       ("gawk" ,gawk)
       ("perl" ,perl)
       ("inetutils" ,inetutils)
       ("util-linux" ,util-linux)
       ("grid-engine" ,grid-engine-core)
       ,@(package-propagated-inputs bammetrics)
       ,@(package-propagated-inputs gatk-bin-3.8-0)))
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

(define-public iap
  (package
    (name "iap")
    (version "hotfix_fedor13")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/UMCUGenetics/IAP/archive/"
                   version ".tar.gz"))
             (sha256
              (base32 "0g77wbh3pjc7840rllcxgczip983i6vlnzwc5m1qyyvsvmfjjmjj"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 ftw))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw))
         (format #t "Output dir: ~a~%" %output)
         (let ((tar           (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (PATH          (string-append (assoc-ref %build-inputs "gzip") "/bin"))
               (tarball (assoc-ref %build-inputs "source"))
               (current-dir (getcwd))
               (bin-dir       (string-append %output "/bin"))
               (patch-bin     (string-append (assoc-ref %build-inputs "patch") "/bin/patch"))
               (pipeline-dir  (string-append %output "/share/iap"))
               (settings-dir  (string-append %output "/share/iap/settings"))
               (qscripts-dir  (string-append %output "/share/iap/QScripts"))
               (templates-dir (string-append %output "/share/iap/templates"))
               (scripts-dir   (string-append %output "/share/iap/scripts"))
               (lib-dir       (string-append %output "/lib/perl5/site_perl/" ,(package-version perl) "/IAP"))
               (perlbin       (string-append (assoc-ref %build-inputs "perl") "/bin/perl"))
               (shbin         (string-append (assoc-ref %build-inputs "bash") "/bin/sh"))
               (pythonbin     (string-append (assoc-ref %build-inputs "python") "/bin/python")))
           (setenv "PATH" PATH)

           ;; Create the directory structure in the build output directory.
           (for-each mkdir-p (list bin-dir
                                   lib-dir
                                   scripts-dir
                                   qscripts-dir
                                   settings-dir
                                   templates-dir))

           (format #t "Extract the modules into the Perl path...~%")
           ;; Extract the modules into the Perl path.
           (with-directory-excursion lib-dir
             (system* tar "xvf" tarball (string-append "IAP-" ,version "/IAP")
                      "--strip-components=2"))

           (format #t "Extract the settings files to their own custom directory...~%")
           ;; Extract the settings files to their own custom directory.
           (with-directory-excursion settings-dir
             (system* tar "xvf" tarball
                      (string-append "IAP-" ,version "/settings")
                      "--strip-components=2"))

           (format #t "Extract scripts to their own custom directory...~%")
           ;; Extract scripts to their own custom directory.
           (with-directory-excursion scripts-dir
             (system* tar "xvf" tarball (string-append "IAP-" ,version "/scripts")
                      "--strip-components=2")

             ;; Patch the shebangs of the scripts.
             (substitute* '("filterFreebayes.py" "filterStrelka.py" "melt_somatic_vcf.py")
               (("#!/usr/bin/env python") (string-append "#!" pythonbin)))
             (substitute* '("delly_TRA_convert.pl" "get_stats_from_flagstat.pl"
                            "parse_MedViol_file.pl" "run_kinship_analyses_on_vcf.pl"
                            "run_SAp42ann_on_vcf.pl")
               (("#!/usr/bin/perl") (string-append "#!" perlbin)))
             (substitute* "run_QDNAseq.R"
               (("load_all\\(qdnaseq_path\\)")
                "library(\"QDNAseq\")")))

           (format #t "Extract QScripts to their own custom directory...~%")
           ;; Extract QScripts to their own custom directory.
           (with-directory-excursion qscripts-dir
             (system* tar "xvf" tarball (string-append "IAP-" ,version "/QScripts")
                      "--strip-components=2"))

           (with-directory-excursion settings-dir
             ;; We are going to roll our own tools.ini.
             (with-output-to-file "guix-tools.ini"
               (lambda _
                 (format #t "# Generated by GNU Guix
BWA_PATH	~a
SAMBAMBA_PATH	~a

FASTQC_PATH	~a
PICARD_PATH	~a
BAMMETRICS_PATH	~a
EXONCALLCOV_PATH	~a

QUEUE_PATH	~a
GATK_PATH	~a

STRELKA_PATH	~a

CIRCOS_PATH	~a

FREEC_PATH	~a
QDNASEQ_PATH	~a

DELLY_PATH	~a
MANTA_PATH	~a

IGVTOOLS_PATH	~a
SAMTOOLS_PATH	~a
TABIX_PATH	~a
PLINK_PATH	~a
KING_PATH	~a
BIOVCF_PATH	~a
BAMUTIL_PATH	~a
PBGZIP_PATH	~a
VCFTOOLS_PATH	~a
BCFTOOLS_PATH	~a
HEALTH_CHECKER_PATH	MISSING

REALIGNMENT_SCALA	IndelRealignment.scala
BASERECALIBRATION_SCALA	BaseRecalibration.scala
CALLING_SCALA	GermlineCaller.scala
FILTER_SCALA	GermlineFilter.scala
"
                         (string-append (assoc-ref %build-inputs "bwa") "/bin")
                         (string-append (assoc-ref %build-inputs "sambamba") "/bin")
                         (string-append (assoc-ref %build-inputs "fastqc") "/bin")
                         (string-append (assoc-ref %build-inputs "picard") "/share/java/picard")
                         (string-append (assoc-ref %build-inputs "bammetrics") "/bin")
                         (string-append (assoc-ref %build-inputs "exoncov") "/bin")
                         (string-append (assoc-ref %build-inputs "gatk-queue") "/share/java/gatk")
                         (string-append (assoc-ref %build-inputs "gatk") "/share/java/gatk")
                         (assoc-ref %build-inputs "strelka")
                         (string-append (assoc-ref %build-inputs "circos") "/bin")
                         (string-append (assoc-ref %build-inputs "freec") "/bin")
                         (string-append (assoc-ref %build-inputs "r-qdnaseq") "/site-library/QDNAseq")
                         (string-append (assoc-ref %build-inputs "delly") "/bin")
                         (string-append (assoc-ref %build-inputs "manta") "/bin")
                         (string-append (assoc-ref %build-inputs "igvtools") "/share/java/igvtools")
                         (string-append (assoc-ref %build-inputs "samtools") "/bin")
                         (string-append (assoc-ref %build-inputs "htslib") "/bin")
                         (string-append (assoc-ref %build-inputs "plink") "/bin")
                         (string-append (assoc-ref %build-inputs "king") "/bin")
                         (string-append (assoc-ref %build-inputs "bio-vcf") "/bin")
                         (string-append (assoc-ref %build-inputs "bamutils") "/bin")
                         (string-append (assoc-ref %build-inputs "pbgzip") "/bin")
                         (string-append (assoc-ref %build-inputs "vcftools") "/bin")
                         (string-append (assoc-ref %build-inputs "bcftools") "/bin")))))

           (with-directory-excursion bin-dir
             ;; Extract the main scripts into the bin directory.
             (system* tar "xvf" tarball
                      (string-append "IAP-" ,version "/illumina_pipeline.pl")
                      (string-append "IAP-" ,version "/illumina_createConfig.pl")
                      "--strip-components=1"))

           ;; Patch the shebang of the main scripts.
           (with-directory-excursion bin-dir
             (substitute* '("illumina_pipeline.pl" "illumina_createConfig.pl")
               (("/usr/bin/perl") perlbin))
             (substitute* "illumina_createConfig.pl"
               (("my \\$settingsDir = dirname\\(abs_path\\(\\$0\\)\\).\"settings\";")
                (string-append "my $settingsDir = \"" settings-dir "\";"))))

           (with-directory-excursion %output
             ;; Because the command-line tools are installed in the bin/
             ;; directory, we must adjust the location where IAP looks for
             ;; its scripts.
             (substitute* "bin/illumina_pipeline.pl"
               (("dirname\\(abs_path\\(\\$0\\)\\)")
                (string-append "\"" %output "/share/iap/\""))))))))
    (inputs
     `(("bammetrics" ,bammetrics)
       ("bamutils" ,bamutils)
       ("bash" ,bash)
       ("bwa" ,bwa)
       ("delly" ,delly)
       ("exoncov" ,exoncov-2.1.1)
       ("fastqc" ,fastqc-bin-0.11.4)
       ("freec" ,freec-10.4)
       ("gatk" ,gatk-bin-3.4-46)
       ("gatk-queue" ,gatk-queue-bin-3.4-46)
       ("htslib" ,htslib)
       ("icedtea-8" ,icedtea-8)
       ("igvtools" ,igvtools-bin-2.3.60)
       ("king" ,king-bin-2.1.2)
       ("manta" ,manta)
       ("pbgzip" ,pbgzip)
       ("perl" ,perl)
       ("picard" ,picard-bin-1.141)
       ("sambamba" ,sambamba-0.6.8)
       ("plink" ,plink)
       ("python" ,python-2)
       ("make" ,gnu-make)
       ("findutils" ,findutils)
       ("diffutils" ,diffutils)
       ("r-minimal" ,r-minimal)))
    (native-inputs
     `(("gzip" ,gzip)
       ("source" ,source)
       ("tar" ,tar)
       ("patch" ,patch)
       ("patch1" ,(origin
                    (method url-fetch)
                    (uri (search-patch "iap-fix-deprecated-perl-functions.patch"))
                    (sha256
                     (base32 "1vzrjs3mfzgfg7viaqxcas41rrzmr66m135p8sr8ld22iqjpmp33"))))))
    (propagated-inputs
     `(("bash" ,bash)
       ("bcftools" ,bcftools)
       ("bio-vcf" ,bio-vcf)
       ("circos" ,circos)
       ("perl-autovivification" ,perl-autovivification)
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
       ("icedtea-8" ,icedtea-8)
       ("r-biobase" ,r-biobase)
       ("r-biocstyle" ,r-biocstyle)
       ("r-bsgenome" ,r-bsgenome)
       ("r-copynumber" ,r-copynumber)
       ("r-cghbase" ,r-cghbase)
       ("r-cghcall" ,r-cghcall)
       ("r-devtools" ,r-devtools)
       ("r-digest" ,r-digest)
       ("r-dnacopy" ,r-dnacopy)
       ("r-genomicranges" ,r-genomicranges)
       ("r-getoptlong" ,r-getoptlong)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gtools" ,r-gtools)
       ("r-iranges" ,r-iranges)
       ("r-matrixstats" ,r-matrixstats)
       ("r-pastecs" ,r-pastecs)
       ("r-qdnaseq" ,r-qdnaseq-hmf)
       ("r-r-utils" ,r-r-utils)
       ("r-roxygen2" ,r-roxygen2)
       ("r-rsamtools" ,r-rsamtools)
       ("r" ,r)
       ;("sambamba" ,sambamba-0.6.8)
       ("samtools" ,samtools-1.2)
       ("strelka" ,strelka-1.0.14)
       ("vcftools" ,vcftools-0.1.14)
       ("coreutils" ,coreutils)
       ("grep" ,grep-with-pcre)
       ("sed" ,sed)
       ("gawk" ,gawk)
       ("perl" ,perl)
       ("inetutils" ,inetutils)
       ("util-linux" ,util-linux)
       ,@(package-propagated-inputs bammetrics)
       ,@(package-propagated-inputs gatk-full-3.5-patched-bin)))
    ;; Bash, Perl and R are not propagated into the profile.  The programs are
    ;; invoked using their absolute link from the 'tools.ini' file.  We must
    ;; make sure that the environment variables for these interpreters are
    ;; set correctly.
    (native-search-paths
     (append (package-native-search-paths bash)
             (package-native-search-paths grid-engine-core)
             (package-native-search-paths perl)
             (package-native-search-paths r)
             (package-native-search-paths ruby)))
    (search-paths native-search-paths)
    (home-page "https://github.com/UMCUGenetics/IAP")
    (synopsis "Illumina analysis pipeline used at UMC Utrecht.")
    (description "This package contains the Illumina analysis pipeline used at
UMC Utrecht.")
    (license license:expat)))
