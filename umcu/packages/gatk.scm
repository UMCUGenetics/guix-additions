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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages java)
  #:use-module (gnu packages statistics))

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
              (install-file "GenomeAnalysisTK.jar" out)))))))))

(define-public gatk-bin-3.4-46
  (package (inherit gatk-bin-3.4-0)
   (name "gatk")
   (version "3.4-46")
   (source (origin
             (method url-fetch)
             ;; FIXME: You need to be logged in on a web page to download
             ;; this release.  Please download the file manually and change
             ;; the path below accordingly.
            (uri (string-append
                  "file:///hpc/local/CentOS7/cog_bioinf/GenomeAnalysisTK_GuixSource/GenomeAnalysisTK-"
                  version ".tar.bz2"))
            (sha256
             (base32 "16g3dc75m31qc97dh3wrqh1rjjrlvk8jdx404ji8jpms6wlz6n76"))))))

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

(define-public gatk-queue-bin-3.4-46
  (package (inherit gatk-queue-bin-3.4-0)
   (name "gatk-queue")
   (version "3.4-46")
   (source (origin
             (method url-fetch)
             ;; FIXME: You need to be logged in on a web page to download
             ;; this release.  Please download the file manually and change
             ;; the path below accordingly.
            (uri (string-append
                  "file:///hpc/local/CentOS7/cog_bioinf/GenomeAnalysisTK_GuixSource/Queue-"
                  version ".tar.bz2"))
            (sha256
             (base32 "1d396y7jgiphvcbcy1r981m5lm5sb116a00h42drw103g63g6gr5"))))))
