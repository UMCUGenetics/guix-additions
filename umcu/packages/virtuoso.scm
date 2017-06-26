;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages virtuoso)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls))

(define-public virtuoso-ose
  (package
    (name "virtuoso-ose")
    (version "7.2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/openlink/virtuoso-opensource/releases/"
             "download/v" version "/virtuoso-opensource-" version ".tar.gz"))
       (sha256
        (base32 "12dqam1gc1v93l0bj0vlpvjqppki6y1hqrlznywxnw0rrz9pb002"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; Tests require a network connection.
    (inputs
     `(("openssl" ,openssl)
       ("net-tools" ,net-tools)))
    (home-page "http://vos.openlinksw.com/owiki/wiki/VOS/")
    (synopsis "Multi-model database system")
    (description "Virtuoso is a scalable cross-platform server that combines
relational, graph, and document data management with web application server
and web services platform functionality.")
    (license license:gpl2)))
