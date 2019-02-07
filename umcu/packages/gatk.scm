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

(define-module (umcu packages gatk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages java)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages python)
  #:use-module (gnu packages java)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages time)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages perl))

(define-public maven-bin
  ;; XXX: This package is only a binary inclusion of Maven.  It is different
  ;; from any other Guix package and you should NOT use this package.
  (package
   (name "maven")
   (version "3.5.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://apache.cs.uu.nl/maven/maven-3/" version
                                "/binaries/apache-maven-" version "-bin.tar.gz"))
            (sha256
             (base32 "0kd1jzlz3b2kglppi85h7286vdwjdmm7avvpwgppgjv42g4v2l6f"))))
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

(define-public r-gsalib
  (package
   (name "r-gsalib")
   (version "2.1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "gsalib" version))
     (sha256
      (base32
       "1k3zjdydzb0dfh1ihih08d4cw6rdamgb97cdqna9mf0qdjc3pcp1"))))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/gsalib")
   (synopsis "Utility Functions For GATK")
   (description "This package contains utility functions used by the Genome
Analysis Toolkit (GATK) to load tables and plot data.  The GATK is a toolkit
for variant discovery in high-throughput sequencing data.")
   (license license:expat)))

(define-public r-naturalsort
  (package
   (name "r-naturalsort")
   (version "0.1.3")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "naturalsort" version))
            (sha256
             (base32
              "0mz801y9mzld9ypp3xmsjw2d8l9q97sdnv09wrci9xi3yg2sjf6d"))))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/naturalsort")
   (synopsis "Natural Ordering")
   (description "This package provides functions related to human natural
ordering.  It handles adjacent digits in a character sequence as a number
so that natural sort function arranges a character vector by their numbers,
not digit characters.  It is typically seen when operating systems lists
file names.  For example, a sequence a-1.png, a-2.png, a-10.png looks
naturally ordered because 1 < 2 < 10 and natural sort algorithm arranges
so whereas general sort algorithms arrange it into a-1.png, a-10.png,
a-2.png owing to their third and fourth characters.")
   (license license:bsd-3)))

(define-public r-hmm
  (package
   (name "r-hmm")
   (version "1.0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "HMM" version))
            (sha256
             (base32
              "0z0hcqfixx1l2a6d3lpy5hmh0n4gjgs0jnck441akpp3vh37glzw"))))
   (properties `((upstream-name . "HMM")))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/HMM")
   (synopsis "Hidden Markov Models (HMM)")
   (description "Easy to use library to setup, apply and make inference with
discrete time and discrete space Hidden Markov Models")
   (license license:gpl2+)))

(define-public gatk-bin-3.4-0
  (package
   (name "gatk")
   (version "3.4")
   (source (origin
             (method url-fetch)
             ;; FIXME: You need to be logged in on a web page to download
             ;; this release.  Please download the file manually and change
             ;; the path below accordingly.
            (uri (string-append
                  "file:///hpc/local/CentOS7/cog_bioinf/GenomeAnalysisTK_GuixSource/GenomeAnalysisTK-"
                  version ".tar.bz2"))
            (sha256
             (base32 "022wi4d64myp8nb4chpypb3pi8vnx1gsjhkncpjyd8pdks0p72sv"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("r-gsalib" ,r-gsalib)
      ("r-ggplot2" ,r-ggplot2)
      ("r-gplots" ,r-gplots)
      ("r-reshape" ,r-reshape)
      ("r-optparse" ,r-optparse)
      ("r-dnacopy" ,r-dnacopy)
      ("r-naturalsort" ,r-naturalsort)
      ("r-dplyr" ,r-dplyr)
      ("r-data-table" ,r-data-table)
      ("r-hmm" ,r-hmm)))
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/" ,name "/")))
              (mkdir-p out)
              (chdir "..")
              (install-file "GenomeAnalysisTK.jar" out)))))))
   (home-page "https://www.broadinstitute.org/gatk/")
   (synopsis "Package for analysis of high-throughput sequencing")
   (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
   ;; There are additional restrictions, so it's nonfree.
   (license license:expat)))

(define-public gatk-bin-3.8-0
  (package (inherit gatk-bin-3.4-0)
    (name "gatk")
    (version "3.8")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://software.broadinstitute.org/gatk/download/"
                   "auth?package=GATK"))
             (file-name (string-append name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1wdr0cwaww8053mkh70xxyiky82qir1xv25cflml9ihc3y2pn0fi"))))
    (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/" ,name "/")))
              (mkdir-p out)
              (install-file "GenomeAnalysisTK.jar" out)))))))
    (propagated-inputs
     `(("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)
       ("gatk-queue-bin-3.8-0" ,gatk-queue-bin-3.8-0)))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_JARPATH")
            (files (list "share/java/user-classes")))))))

(define-public gatk-bin-3.8-1
  (package (inherit gatk-bin-3.8-0)
    (name "gatk")
    (version "3.8-1-0-gf15c1c3ef")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://software.broadinstitute.org/gatk/download/"
                   "auth?package=GATK-archive&version=" version))
             (file-name (string-append name "-" version ".tar.bz2"))
             (sha256
              (base32
               "0p5yikcl54j7krp0sh6vw0wg4zs2a2dlllivpnzkxjnhs8s9b0m0"))))
    (propagated-inputs
     `(("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)
       ("gatk-queue-bin-3.8-1" ,gatk-queue-bin-3.8-1)))))

(define-public gatk-bin-3.4-46
  (package (inherit gatk-bin-3.4-0)
   (name "gatk")
   (version "3.4-46-gbc02625")
   (source (origin
             (method url-fetch)
             ;; FIXME: You need to be logged in on a web page to download
             ;; this release.  Please download the file manually and change
             ;; the path below accordingly.
             (uri (string-append
                   "https://software.broadinstitute.org/gatk/download/"
                   "auth?package=GATK-archive&version=" version))
             (file-name (string-append name "-" version ".tar.bz2"))
            (sha256
             (base32 "16g3dc75m31qc97dh3wrqh1rjjrlvk8jdx404ji8jpms6wlz6n76"))))
   (propagated-inputs
    `(("r-gsalib" ,r-gsalib)
      ("r-ggplot2" ,r-ggplot2)
      ("r-gplots" ,r-gplots)
      ("r-reshape" ,r-reshape)
      ("r-optparse" ,r-optparse)
      ("r-dnacopy" ,r-dnacopy)
      ("r-naturalsort" ,r-naturalsort)
      ("r-dplyr" ,r-dplyr)
      ("r-data-table" ,r-data-table)
      ("r-hmm" ,r-hmm)
      ("gatk-queue-bin-3.4-46" ,gatk-queue-bin-3.4-46)))))

(define-public gatk-queue-bin-3.4-0
  (package
   (name "gatk-queue")
   (version "3.4")
   (source (origin
             (method url-fetch)
             ;; FIXME: You need to be logged in on a web page to download
             ;; this release.  Please download the file manually and change
             ;; the path below accordingly.
            (uri (string-append
                  "file:///hpc/local/CentOS7/cog_bioinf/GenomeAnalysisTK_GuixSource/Queue-"
                  version ".tar.bz2"))
            (sha256
             (base32 "0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda _
            (chdir "..") ; The build system moves into the "resources" folder.
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/gatk/")))
              (mkdir-p out)
              (install-file "Queue.jar" out)))))))
   (home-page "https://www.broadinstitute.org/gatk/")
   (synopsis "Package for analysis of high-throughput sequencing")
   (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
   ;; There are additional restrictions, so it's nonfree.
   (license license:expat)))

(define-public gatk-queue-bin-3.8-0
  (package (inherit gatk-queue-bin-3.4-0)
    (name "gatk-queue")
    (version "3.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://software.broadinstitute.org/gatk/"
                                 "download/auth?package=Queue"))
             (file-name (string-append name "-" version ".tar.bz2"))
             (sha256
              (base32 "0ka2l77583rqad4s96spbk3npv42mgpzba9bqj8r50xn30yfpa8k"))))
    (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/gatk/")))
              (mkdir-p out)
              (install-file "../Queue.jar" out)))))))))

(define-public gatk-queue-bin-3.8-1
  (package (inherit gatk-queue-bin-3.8-0)
    (name "gatk-queue")
    (version "3.8-1-0-gf15c1c3ef")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://software.broadinstitute.org/gatk/download/"
                   "auth?package=Queue-archive&version=" version))
             (file-name (string-append name "-" version ".tar.bz2"))
             (sha256
              (base32 "0435lf2751w3l2m86m3h6girwr09kpiqahq3pj49gibqnyylx4sq"))))
    (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/gatk/")))
              (mkdir-p out)
              (install-file "Queue.jar" out)))))))))

(define-public gatk-queue-bin-3.4-46
  (package (inherit gatk-queue-bin-3.4-0)
   (name "gatk-queue")
   (version "3.4-46-gbc02625")
   (source (origin
             (method url-fetch)
             ;; FIXME: You need to be logged in on a web page to download
             ;; this release.  Please download the file manually and change
             ;; the path below accordingly.
             (uri (string-append
                   "https://software.broadinstitute.org/gatk/download/"
                   "auth?package=Queue-archive&version=" version))
             (file-name (string-append name "-" version ".tar.bz2"))
            (sha256
             (base32 "1d396y7jgiphvcbcy1r981m5lm5sb116a00h42drw103g63g6gr5"))))
   ))

;;
;; FIXME: This package builds fine, but it doesn't include the R scripts needed by
;; various GATK and Queue processes.  It will therefore crash at the end of your
;; computation.  To solve this problem, we need to remove
;; -Dresource.bundle.skip=true from the compile options, but in that case the
;; build fails in the build environment because Javadoc seems to hardcode /bin/sh
;; somewhere, which is not available in the build environment.
(define-public gatk-full-3.5
  (package
    (name "gatk")
    (version "3.5-e91472d")
    (source (origin
              (method url-fetch)
              (uri "https://github.com/broadgsa/gatk-protected/archive/3.5.tar.gz")
              (sha256
               (base32 "0g07h5a7ajsyapgzh7nxz0yjp3d2v4fwhfnkcs0sfnq7s2rpsh9z"))
              (patches (list (search-patch "gatk-disable-vectorloglesscaching.patch")
                             (search-patch "gatk-apply-area-51-restrictions.patch")))))
    (build-system gnu-build-system)
    (arguments
      `(#:tests? #f ; Tests are run in the install phase.
        #:phases
        (modify-phases %standard-phases
          (delete 'configure) ; Nothing to configure
          (replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((build-dir (getcwd))
                     (home-dir (string-append build-dir "/home"))
                     (settings-dir (string-append build-dir "/mvn"))
                     (settings (string-append settings-dir "/settings.xml"))
                     (m2-dir (string-append build-dir "/m2/repository"))
                     (fakebin (string-append build-dir "/fakebin")))

                ;; Turns out that there's an unused import that breaks the build.
                ;; Fortunately, we can easily remove it.
                (substitute* (string-append "public/gatk-tools-public/src/main"
                                            "/java/org/broadinstitute/gatk"
                                            "/tools/walkers/varianteval"
                                            "/VariantEval.java")
                  (("import oracle.jrockit.jfr.StringConstantPool;")
                   "//import oracle.jrockit.jfr.StringConstantPool;"))

                ;; Patch hardcoded /bin/sh entries.
                (substitute* (string-append "public/gatk-queue/src/test/scala"
                                            "/org/broadinstitute/gatk/queue"
                                            "/util/ShellUtilsUnitTest.scala")
                  (("/bin/sh")
                   (string-append (assoc-ref %build-inputs "bash") "/bin/sh")))

                (mkdir-p settings-dir)
                (mkdir-p m2-dir)

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

                ;; Set JAVA_HOME to help maven find the JDK.
                (setenv "JAVA_HOME" (string-append (assoc-ref inputs "icedtea") "/jre"))
                (mkdir-p home-dir)
                (setenv "HOME" home-dir)

                (mkdir-p m2-dir)
                (mkdir-p settings-dir)

                ;; Compile using maven's compile command.
                (let ((compile-options (string-append
                                        "-fn " ; Javadoc targets fail.
                                        ;"-Dresource.bundle.skip=true "
                                        "-Dmaven.tests.skip=true "
                                        "--offline")))
                  (system (format #f "mvn compile ~a --global-settings ~s"
                                  compile-options settings))
                  (system (format #f "mvn verify ~a --global-settings ~s"
                                  compile-options settings))
                  (system (format #f "mvn package ~a --global-settings ~s"
                                  compile-options settings))))))
          (replace 'install
            (lambda _
              (let ((out (string-append (assoc-ref %outputs "out")
                                        "/share/java/user-classes/")))
                (mkdir-p out)
                (install-file "target/GenomeAnalysisTK.jar" out)
                (install-file "target/Queue.jar" out)))))))
    (native-inputs
     `(("maven-deps"
        ,(origin
          (method url-fetch)
          (uri (string-append "https://raw.githubusercontent.com/"
                              "UMCUGenetics/guix-additions/master/blobs/"
                              "gatk-mvn-dependencies.tar.gz"))
          (sha256
           (base32
            "1rrc7clad01mw83zyfgc4bnfn0nqvfc0mabd8wnj61p64xrigny9"))))))
    (inputs
     `(("icedtea" ,icedtea-7 "jdk")
       ("maven" ,maven-bin)
       ("bash" ,bash)
       ("perl" ,perl)
       ("r" ,r)))
    (propagated-inputs
     `(("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_JARPATH")
            (files (list "share/java/user-classes")))))
    (home-page "https://github.com/broadgsa/gatk-protected")
    (synopsis "Package for analysis of high-throughput sequencing")
   (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
   ;; There are additional restrictions, so it's nonfree.
   (license license:expat)))

(define-public gatk-full-3.5-patched-bin
  (package
    (name "gatk")
    (version "3.5-e91472d-patched")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/"
                                  "UMCUGenetics/guix-additions/master/blobs/"
                                  "gatk-patched-prebuilt.tar.gz"))
              (sha256
               (base32
                "14j9k3jscm278r5scydn9afb5d11yd2iij5km0yddidpxfnpn0r7"))))
    (build-system gnu-build-system)
    (arguments
      `(#:tests? #f ; Tests are run in the install phase.
        #:phases
        (modify-phases %standard-phases
          (delete 'configure) ; Nothing to configure
          (delete 'build) ; Nothing to build
          (replace 'install
            (lambda _
              (let ((out (string-append (assoc-ref %outputs "out")
                                        "/share/java/user-classes/")))
                (mkdir-p out)
                (install-file "GenomeAnalysisTK.jar" out)
                (install-file "Queue.jar" out)))))))
    (propagated-inputs
     `(("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_JARPATH")
            (files (list "share/java/user-classes")))))
    (home-page "https://github.com/broadgsa/gatk-protected")
    (synopsis "Package for analysis of high-throughput sequencing")
   (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
   ;; There are additional restrictions, so it's nonfree.
   (license license:expat)))

(define-public python-theano
  (package
    (name "python-theano")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Theano" version))
              (sha256
               (base32
                "1pmb5754qwiy1x2irciwn4xzsvwapdpi5agwwq8p1898sc1y0s37"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "http://deeplearning.net/software/theano/")
    (synopsis "Optimizing compiler for evaluating mathematical expressions on CPUs and GPUs.")
    (description "Optimizing compiler for evaluating mathematical expressions on CPUs and GPUs.")
    (license license:bsd-3)))

(define-public python-pymc3
  (package
  (name "python-pymc3")
  (version "3.5")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "pymc3" version))
      (sha256
        (base32
          "1vi11z1cjhc1hxbjvxay9n7a599z13p583qa6lhvnc6pqs1yd230"))))
  (build-system python-build-system)
  (arguments `(#:tests? #f ))
  (propagated-inputs
    `(;("python-enum34" ,python-enum34)
      ("python-h5py" ,python-h5py)
      ("python-joblib" ,python-joblib)
      ("python-numpy" ,python-numpy)
      ("python-pandas" ,python-pandas)
      ("python-patsy" ,python-patsy)
      ("python-six" ,python-six)
      ("python-theano" ,python-theano)
      ("python-tqdm" ,python-tqdm)))
  (home-page "http://github.com/pymc-devs/pymc3")
  (synopsis
    "Probabilistic Programming in Python: Bayesian Modeling and Probabilistic Machine Learning with Theano")
  (description
    "Probabilistic Programming in Python: Bayesian Modeling and Probabilistic Machine Learning with Theano")
  (license license:asl2.0)))

(define-public python-keras-preprocessing
  (package
    (name "python-keras-preprocessing")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras_Preprocessing" version))
       (sha256
        (base32
         "152i7k01xd3r7kin2s329ddi23b0ym6rb2ha1shnxh7cfxivljc6"))))
    (build-system python-build-system)
    (inputs
     `(("python-six" ,python-six)
       ("python-scipy" ,python-scipy)))
    (home-page
     "https://github.com/keras-team/keras-preprocessing")
    (synopsis
     "Easy data preprocessing and data augmentation for deep learning models")
    (description
     "Easy data preprocessing and data augmentation for deep learning models")
    (license license:expat)))

(define-public python-keras
  (package
    (name "python-keras")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras" version))
       (sha256
        (base32
         "1grl2znv1yssrci3r0vc4qzbqzhjfkkqjdg3bqd7y8dgaz8rk12v"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    ;(propagated-inputs
    ; `(("python-keras-preprocessing" ,python-keras-preprocessing)))
    (home-page "https://github.com/keras-team/keras")
    (synopsis "Deep Learning for humans")
    (description "Deep Learning for humans")
    (license license:expat)))

(define-public gatk4
  (package
    (name "gatk4")
    (version "4.0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/broadinstitute/gatk/releases/download/"
                    version "/gatk-" version ".zip"))
              (sha256
               (base32 "0yv4hdz8x2q4ycsnxpfwpzlhm1g7rz7idjh0kl1c4kagqj7c0ryn"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((unzip   (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
               (tarball (assoc-ref %build-inputs "source"))
               (out     (string-append %output "/share/java/user-classes"))
               (bin     (string-append %output "/bin")))
           (mkdir-p out)
           (mkdir-p bin)
           (system (string-append unzip " " tarball))
           (chdir (string-append "gatk-" ,version))
           (install-file "gatk-package-4.0.1.2-local.jar" out)
           (symlink (string-append out "/gatk-package-4.0.1.2-local.jar")
                    (string-append out "/gatk.jar"))
           (install-file "gatk-package-4.0.1.2-spark.jar" out)
           (symlink (string-append out "/gatk-package-4.0.1.2-spark.jar")
                    (string-append out "/gatk-spark.jar"))
           (substitute* "gatk"
             (("/usr/bin/env python") (string-append
                                       (assoc-ref %build-inputs "python2")
                                       "/bin/python"))
             (("return \\[\"java\"\\]")
              (string-append "return [\"" (assoc-ref %build-inputs "icedtea-8") "/bin/java\"]"))
             (("findJar\\(\"local.jar\", envVariableOverride=GATK_LOCAL_JAR_ENV_VARIABLE\\)")
              (string-append "\"" out "/gatk.jar\""))
             (("findJar\\(\"spark.jar\", envVariableOverride=GATK_SPARK_JAR_ENV_VARIABLE\\)")
              (string-append "\"" out "/gatk-spark.jar\"")))
           (install-file "gatk" bin)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("python2" ,python-2.7)
       ("icedtea-8" ,icedtea-8)))
    (propagated-inputs
     `(("r" ,r)
       ("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)
       ("python-certifi",python-certifi)
       ("openmpi" ,openmpi)
       ("openssl" ,openssl)
       ("python" ,python-3.6)
       ("readline" ,readline)
       ("python-setuptools" ,python-setuptools)
       ("sqlite" ,sqlite)
       ("tk" ,tk)
       ("python-wheel" ,python-wheel)
       ("xz" ,xz)
       ("zlib" ,zlib)
       ("python-bleach" ,python-bleach)
       ("python-cycler" ,python-cycler)
       ("python-h5py" ,python-h5py)
       ("python-html5lib" ,python-html5lib-0.9)
       ("python-joblib" ,python-joblib)
       ("python-keras" ,python-keras)
       ("python-markdown" ,python-markdown)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-patsy" ,python-patsy)
       ("python-protobuf" ,python-protobuf)
       ("python-pymc3" ,python-pymc3)
       ("python-pyparsing" ,python-pyparsing)
       ("python-dateutil" ,python-dateutil)
       ("python-pytz" ,python-pytz)
       ("python-pyyaml" ,python-pyyaml)
       ("python-scipy" ,python-scipy)
       ("python-six" ,python-six)
       ("python-theano" ,python-theano)
       ("python-tqdm" ,python-tqdm)
       ("python-werkzeug" ,python-werkzeug)
       ("python3" ,python-3.6)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

