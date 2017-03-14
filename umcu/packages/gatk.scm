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
  #:use-module (gnu packages)
  #:use-module (gnu packages java))

(define-public gatk-bin-3.4
  (package
   (name "gatk")
   (version "3.4")
   (source (origin
             (method url-fetch)
             ;; FIXME: You need to be logged in on a web page to download
             ;; this release.  Please download the file manually and change
             ;; the path below accordingly.
            (uri (string-append
                  "file:///hpc/local/CentOS7/cog_bioinf/GenomeAnalysisTK-"
                  version ".tar.bz2"))
            (sha256
             (base32 "022wi4d64myp8nb4chpypb3pi8vnx1gsjhkncpjyd8pdks0p72sv"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("icedtea" ,icedtea-7)
      ("gatk-queue" ,gatk-queue-bin-3.4)))
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (add-before 'install 'debug-current-working-directory
          (lambda _
            (chdir "..")
            (display (format #f "~s~%" (getcwd)))))
        (replace 'install
          (lambda _
            (display (format #f "~s~%" (getcwd)))
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/" ,name "/")))
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

(define-public gatk-bin-3.4-46
  (package (inherit gatk-bin-3.4)
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
             (base32 "16g3dc75m31qc97dh3wrqh1rjjrlvk8jdx404ji8jpms6wlz6n76"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("icedtea" ,icedtea-7)
      ("gatk-queue" ,gatk-queue-bin-3.4-46)))
    )))

(define-public gatk-queue-bin-3.4
  (package
   (name "gatk-queue")
   (version "3.4")
   (source (origin
             (method url-fetch)
             ;; FIXME: You need to be logged in on a web page to download
             ;; this release.  Please download the file manually and change
             ;; the path below accordingly.
            (uri (string-append
                  "file:///hpc/local/CentOS7/cog_bioinf/Queue-"
                  version ".tar.bz2"))
            (sha256
             (base32 "103gns8f427h0b944d7v6aci7lzq1c8g3pngjn271m3p0j5cd7mj"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("icedtea" ,icedtea-7)))
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

(define-public gatk-queue-bin-3.4-46
  (package (inherit gatk-queue-bin-3.4)
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
             (base32 "1d396y7jgiphvcbcy1r981m5lm5sb116a00h42drw103g63g6gr5"))))
  ))
