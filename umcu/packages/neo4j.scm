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

(define-module (umcu packages neo4j)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public python-neo4j-driver
  (package
    (name "python-neo4j-driver")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "neo4j-driver" version))
              (sha256
               (base32
                "0cxyamw9kwfg91hklz9gl4gkcml9ajxx3302ggx93prf3qx32q5h"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (inputs
     `(("python2-tox" ,python2-tox)))
    (home-page "https://neo4j.com/developer/python/")
    (synopsis "Neo4j driver code written in Python")
    (description "The Neo4j Python driver is officially supported by
Neo4j and connects to the database using the binary protocol.  It aims to
be minimal, while being idiomatic to Python.")
    (license license:asl2.0)))

(define-public python-py2neo
  (package
    (name "python-py2neo")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "py2neo" version))
              (sha256
               (base32
                "0iahzfryiqa7xbb5cgrsg90ida8i9iwy2bs277xw4h1nk1y7y5wd"))))
    (build-system python-build-system)
    (home-page "http://py2neo.org")
    (synopsis "Library and toolkit for working with Neo4j in Python")
    (description "This package provides a client library and toolkit for
working with Neo4j from within Python applications and from the command
line.  The core library has no external dependencies and has been carefully
designed to be easy and intuitive to use.")
    (license license:asl2.0)))

(define-public python2-py2neo
  (package-with-python2 python-py2neo))
