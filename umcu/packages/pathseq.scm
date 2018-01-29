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
