;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages pathseq)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system perl)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl))

(define-public rmblast
  (package (inherit blast+)
    (name "rmblast")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/"
                    version "/ncbi-blast-" version "+-src.tar.gz"))
              (sha256
               (base32
                "15n937pw5aqmyfjb6l387d18grqbb96l63d5xj4l7yyh0zbf2405"))
              (patches (search-patches "rmblast-isb-2.6.0+-changes-vers2.patch"
                                       "blast+-fix-makefile.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled bzip2, zlib and pcre.
                  (delete-file-recursively "c++/src/util/compress/bzip2")
                  (delete-file-recursively "c++/src/util/compress/zlib")
                  (delete-file-recursively "c++/src/util/regexp")
                  (substitute* "c++/src/util/compress/Makefile.in"
                    (("bzip2 zlib api") "api"))
                  ;; Remove useless msbuild directory
                  (delete-file-recursively
                   "c++/src/build-system/project_tree_builder/msbuild")
                  #t))))))

(define-public pathseq-pipeline-tools
  (let ((commit "2a4f15d5dec1b2fbf707cab4a8517eedff070a33"))
    (package
      (name "pathseq-pipeline-tools")
      (version "1.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ChandraPedamallu/PathSeq.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "141fwk1knknvmrddpgpqiqcdcz7iarqrw28609j1smjr33dkwn3n"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (with-directory-excursion "Java"
                 ;; Remove pre-compiled files.
                 (system* "rm" "-rf"
                          "*.class"
                          "QualFilter_July2016.java"
                          "QualFilter_RemoveDuplicate_July2016.java")
                 (system "ls -lh")
                 ;; Compile all java classes.
                 (system (string-append
                          (assoc-ref inputs "java")
                          "/bin/javac -cp ../3rdparty/sam-1.52.jar *.java"))
                 ;; Pack the Java classes into one jar.
                 (system (string-append
                          (assoc-ref inputs "java")
                          "/bin/jar -cvf ../PathSeq.jar *.class")))))
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (java-dir (string-append out "/share/java/user-classes")))
                 (install-file "PathSeq.jar" java-dir)))))))
      (inputs
       `(("java" ,icedtea-8 "jdk")))
      (native-search-paths
       (list (search-path-specification
              (variable "GUIX_JARPATH")
              (files (list "share/java/user-classes")))))
      (home-page "http://software.broadinstitute.org/pathseq/")
      (synopsis "Pipeline for identifying microbial sequences in human data")
      (description "PathSeq is a computational tool for the identification and
analysis of microbial sequences in high-throughput human sequencing data that
is designed to work with large numbers of sequencing reads in a scalable
manner.")
      ;; MIT license.
      (license license:expat))))
