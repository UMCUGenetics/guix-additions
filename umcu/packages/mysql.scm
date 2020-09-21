;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

;;
;; The recipe for MySQL 5.6.25 has been taken from the GNU Guix repository.
;; We keep it here to maintain this specific version.
;;

(define-module (umcu packages mysql)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages time)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public mysql-5.6.25
  (package
    (name "mysql")
    (version "5.6.25")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://dev.mysql.com/get/Downloads/MySQL-"
                   (version-major+minor version) "/"
                   name "-" version ".tar.gz"))
             (sha256
              (base32
               "1gbz5i1z3nswpq3q8f477vrx7g15j8n41pyb94k0jfnkhc5rq1qm"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       '("-DCMAKE_CXX_FLAGS=-fpermissive"
         "-DBUILD_CONFIG=mysql_release"
         "-DWITH_SSL=system"
         "-DWITH_ZLIB=system"
         "-DDEFAULT_CHARSET=utf8"
         "-DDEFAULT_COLLATION=utf8_general_ci"
         "-DMYSQL_DATADIR=/var/lib/mysql"
         "-DMYSQL_UNIX_ADDR=/run/mysqld/mysqld.sock"
         "-DINSTALL_INFODIR=share/mysql/docs"
         "-DINSTALL_MANDIR=share/man"
         "-DINSTALL_PLUGINDIR=lib/mysql/plugin"
         "-DINSTALL_SCRIPTDIR=bin"
         "-DINSTALL_INCLUDEDIR=include/mysql"
         "-DINSTALL_DOCREADMEDIR=share/mysql/docs"
         "-DINSTALL_SUPPORTFILESDIR=share/mysql"
         "-DINSTALL_MYSQLSHAREDIR=share/mysql"
         "-DINSTALL_DOCDIR=share/mysql/docs"
         "-DINSTALL_SHAREDIR=share/mysql"
         ;; Get rid of test data.
         "-DINSTALL_MYSQLTESTDIR="
         "-DINSTALL_SQLBENCHDIR=")))
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)))
    (inputs
     `(("libaio" ,libaio)
       ("openssl" ,openssl-1.0)
       ("zlib" ,zlib)
       ("ncurses" ,ncurses)))
    (home-page "http://www.mysql.com/")
    (synopsis "Fast, easy to use, and popular database")
    (description
     "MySQL is a fast, reliable, and easy to use relational database
management system that supports the standardized Structured Query
Language.")
    (license license:gpl2)))
