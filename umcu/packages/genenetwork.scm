;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Pjotr Prins <pjotr.guix@thebird.nl>
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

(define-module (umcu packages genenetwork)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages ruby)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system ruby))

(define-public bio-vcf
  (package
   (name "bio-vcf")
   (version "0.9.2")
   (source (origin
            (method url-fetch)
            (uri (rubygems-uri "bio-vcf" version))
            (sha256
             (base32
              "1007bn0w8l11q867lxsyqnk0vgvv12skvk9gyglv7g44knr5vh4j"))))
   (build-system ruby-build-system)
   (arguments `(#:tests? #f)) ; There are no tests.
   (native-search-paths
    (package-native-search-paths ruby))
   (synopsis "Smart lazy multi-threaded parser for VCF format with useful
filtering and output rewriting (JSON, RDF etc.)")
   (description "Smart lazy multi-threaded parser for VCF format with useful
filtering and output rewriting (JSON, RDF etc.)")
   (home-page "http://github.com/pjotrp/bioruby-vcf")
   (license license:expat)))
