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

;; PROGRAM             IAP        GUIX UPSTREAM  !EQ   FIXED/READY
;; BWA                 0.7.5a     0.7.12         X     X
;; Sambamba            0.5.9      0.5.9                  (crashes, has to do with LLVM)
;; FastQ               0.11.4     0.11.5         X     X
;; Picard              1.141      2.1.0          X     X
;; bamMetrics          -          -              X     
;; GATK                3.4-46     3.5            X     X
;; snpEff              4.1        4.2            X     X
;; vcftools            0.1.14     0.1.12b        X     X
;; strelka             1.0.14     1.0.14               X
;; VarScan             2.4.0      2.4.1          X     X
;; FreeBayes           0.1.14     1.0.2          X
;; mutect              1.1.7      -              X       (crashes, has to do with GATK)
;; freec               7.2        8.7            X       (version not available, contacted author)
;; delly               0.7.2      0.7.2                X
;; IGVTools            2.3.60     2.3.60               X
;; Samtools            1.2        1.3            X     X
;; Tabix               0.2.6      -              X     ?
;; PLink               1.9b3      1.90b3         X     X (same)
;; King                1.4        1.9            X     X
;; BioVCF              0.9.2      ?
;; VCFLib              ?          1.0.2          ?

(define-module (umcu packages iap)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages statistics)
  #:use-module (umcu packages boost)
  #:use-module (umcu packages bioconductor)
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

(define-public cuppenresearch-iap
  (let ((commit "a7c5973f1302bb0b7286263deeca0ec74cf5170f"))
    (package
     (name "cuppenresearch-iap")
     (version "2.2.1")
     (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/roelj/IAP/archive/"
                                  commit ".tar.gz"))
              (sha256
               (base32
                "1s5sk8kxjxb3avrm7rha0ah5z6jxfflhz127iyaw1sapb3v0qq06"))))
     (build-system trivial-build-system)
     (arguments
      `(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let ((tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
                (PATH (string-append (assoc-ref %build-inputs "gzip") "/bin"))
                (tarball (assoc-ref %build-inputs "source"))
                (bin-dir (string-append %output "/bin"))
                (qscripts-dir (string-append %output "/share/iap/QScripts"))
                (scripts-dir (string-append %output "/share/iap/scripts"))
                (lib-dir (string-append %output "/lib/perl5/site_perl/"
                                        ,(package-version perl) "/IAP"))
                (perlbin (string-append (assoc-ref %build-inputs "perl")
                                        "/bin/perl")))
            (setenv "PATH" PATH)
            ;; Create the directory structure in the build output directory.
            (mkdir-p lib-dir)
            (mkdir-p bin-dir)
            (mkdir-p scripts-dir)
            (mkdir-p qscripts-dir)
            ;; Extract the modules into the Perl path.
            (chdir lib-dir)
            (system* tar "xvf" tarball (string-append "IAP-" ,commit "/IAP")
                                       "--strip-components=2")
            ;; Extract scripts to their own custom directory.
            (chdir scripts-dir)
            (system* tar "xvf" tarball (string-append "IAP-" ,commit "/scripts")
                                       "--strip-components=2")
            ;; Extract QScripts to their own custom directory.
            (chdir qscripts-dir)
            (system* tar "xvf" tarball (string-append "IAP-" ,commit "/QScripts")
                                       "--strip-components=2")
            ;; Extract the main scripts into the bin directory.
            (chdir bin-dir)
            (system* tar "xvf" tarball
                     (string-append "IAP-" ,commit "/illumina_pipeline.pl")
                     (string-append "IAP-" ,commit "/illumina_createConfig.pl")
                     "--strip-components=1")
            ;; Patch the scripts path.
            (substitute* "illumina_pipeline.pl"
              (("dirname\\(abs_path\\(\\$0\\)\\)")
               (string-append "'" scripts-dir "'")))
            ;; Patch the shebangs
            (substitute* '("illumina_pipeline.pl" "illumina_createConfig.pl")
              (("/usr/bin/perl") perlbin))))))
     (native-inputs
      `(("source" ,source)
        ("tar" ,tar)
        ("gzip" ,gzip)))
     (propagated-inputs
      `(("fastqc" ,fastqc-bin-0.11.4)
        ("bwa" ,bwa-0.7.5a)
        ("sambamba" ,sambamba-0.5.9)
        ;; GATK requires a manual download.
        ;("gatk" ,gatk-bin-3.4)
        ;("gatk-queue" ,gatk-queue-bin-3.4)
        ("picard" ,picard-bin-1.141)
        ("snpeff" ,snpeff-bin-4.1)
        ("samtools" ,samtools-1.2)
        ("vcftools" ,vcftools-0.1.14)
        ("igvtools" ,igvtools-bin-2.3.71)
        ("bio-vcf" ,bio-vcf)
        ("contra" ,contra-2.0.6)
        ("control-freec" ,freec-8.7)
        ("varscan" ,varscan-2.4.0)
        ("strelka" ,strelka-1.0.15)
        ("freebayes" ,freebayes-1.0.2)
        ("vcflib" ,vcflib-1.0.2)
        ("delly" ,delly-0.7.2)
        ("plink2" ,plink2-1.90b3)
        ("king" ,king-bin-2.1.2)
        ("perl" ,perl)
        ("perl-file-path" ,perl-file-path)
        ("r" ,r)
        ("r-ggplot2" ,r-ggplot2)
        ("r-knitr" ,r-knitr)
        ("r-markdown" ,r-markdown)
        ("r-reshape2" ,r-reshape2)
        ("r-xtable" ,r-xtable)
        ("r-brew" ,r-brew)
        ("r-pastecs" ,r-pastecs)))
     (home-page "https://github.com/CuppenResearch/IAP")
     (synopsis "Illumina analysis pipeline modules")
     (description "This package contains a set of Perl modules for the Illumina
Analysis Pipeline used by the Cuppen research group at the UMC in Utrecht.")
     (license license:expat))))
