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
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
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
   (home-page "https://maven.apache.org/")
   (synopsis "Build and dependency management tool for Java")
   (description "Apache Maven is a software project management and comprehension tool.
Based on the concept of a project object model (POM), Maven can manage a project's
build, reporting and documentation from a central piece of information.")
   (license license:asl2.0)))
