;;; GNU Guix --- Functional package management for GNU
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

(define-module (umcu packages pbgzip)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression))

(define-public pbgzip
  (let ((commit "2b09f97b5f20b6d83c63a5c6b408d152e3982974"))
    (package
      (name "pbgzip")
      (version (string-take commit 7))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nh13/pbgzip.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1mlmq0v96irbz71bgw5zcc43g1x32zwnxx21a5p1f1ch4cikw1yd"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autogen
             (lambda _
               (zero? (system* "sh" "autogen.sh")))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)))
      (inputs
       `(("zlib" ,zlib)))
      (home-page "https://github.com/nh13/pbgzip")
      (synopsis "Parallel Block GZIP")
      (description "This tool and API implements parallel block gzip.  For many
formats, in particular Genomics Data Formats, data are compressed in
fixed-length blocks such that they can be easily indexed based on a (genomic)
coordinate order, since typically each block is sorted according to this order.
This allows for each block to be individually compressed (deflated), or more
importantly, decompressed (inflated), with the latter enabling random retrieval
of data in large files (gigabytes to terabytes).  @code{pbgzip} is not limited
to any particular format, but certain features are tailored to Genomics Data
Formats when enabled (see below). Parallel decompression is somewhat faster,
but truly the speedup comes during compression.")
      (license license:expat))))

