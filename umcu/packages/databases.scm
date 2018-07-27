;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls))

(define-public agensgraph
  (package
    (name "agensgraph")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/bitnine-oss/agensgraph/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0qbmzmkl36wv5d66k7s0p0xll4ai1ax3pczrfgxbmfk9i2j1gjzw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("zlib" ,zlib)
       ("glib" ,glib)
       ("readline" ,readline)
       ("flex" ,flex)
       ("bison" ,bison)))
    (home-page "http://bitnine.net")
    (synopsis "Transactional graph database based on PostgreSQL")
    (description "AgensGraph is a new generation multi-model graph database for
the modern complex data environment.  It is a multi-model database, which
supports the relational and graph data model at the same time that enables
developers to integrate the legacy relational data model and the flexible graph
data model in one database.  It supports ANSI-SQL and openCypher.  SQL queries
and Cypher queries can be integrated into a single query in AgensGraph.")
    (license license:asl2.0)))

(define-public virtuoso-ose-7.2.5-rc2
  (let ((commit "2125bdfc1f97c5e3096ac26e39674e09a019017a"))
    (package
      (name "virtuoso-ose")
      (version "7.2.5-rc2")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/openlink/virtuoso-opensource.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32 "188yppykz6rd44ccdq03g2fb2mrk2cnb0s9n7xv79m82dxan2ypf"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; Tests require a network connection.
         ;; TODO: Removing the libsrc/zlib source directory breaks the build.
         ;; This indicates that the internal zlib code may still be used.
         #:configure-flags '("--without-internal-zlib"
                             "--disable-all-vads"
                             "--disable-imsg"
                             "--disable-xmltest"
                             "--disable-hslookup"
                             "--disable-imagemagick"
                             "--disable-wbxml2"
                             "--with-readline"
                             "--with-pthreads"
                             "--with-gnu-ld")))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)
         ("libtool" ,libtool)
         ("gperf" ,gperf)
         ("bison" ,bison)
         ("flex" ,flex-2.6.1)))
      (inputs
       `(("openssl" ,openssl)
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
      (license license:gpl2))))
