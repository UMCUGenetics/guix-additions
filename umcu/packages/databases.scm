;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages databases)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public virtuoso-ose
  (package
    (name "virtuoso-ose")
    (version "7.2.5-unlimited")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/openlink/virtuoso-opensource/releases/"
             "download/v7.2.5/virtuoso-opensource-7.2.5.tar.gz"))
       (patches (list (search-patch "virtuoso-ose-increase-maxrows.patch")))
       (sha256
        (base32 "0r1xakclkfi69pzh8z2k16z3x0m49pxp764icj0ad4w4bb97fr42"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Tests require a network connection.
       ;; TODO: Removing the libsrc/zlib source directory breaks the build.
       ;; This indicates that the internal zlib code may still be used.
       #:configure-flags '("--without-internal-zlib"
                           "--with-readline")))
    (inputs
     `(("openssl" ,openssl-1.0)
       ("net-tools" ,net-tools)
       ("readline" ,readline)
       ("zlib" ,zlib)))
    (home-page "http://vos.openlinksw.com/owiki/wiki/VOS/")
    (synopsis "Multi-model database system")
    (description "Virtuoso is a scalable cross-platform server that combines
relational, graph, and document data management with web application server
and web services platform functionality.")
    ;; configure: error: ... can only be build on 64bit platforms
    (supported-systems '("x86_64-linux" "mips64el-linux" "aarch64-linux"))
    (license license:gpl2)))
