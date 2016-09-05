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

(define-module (umcu packages mutationalpatterns)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages statistics))

(define-public r-mutationalpatterns
  (package
    (name "r-mutationalpatterns")
    (version "0.2-beta")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/CuppenResearch/MutationalPatterns/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h8zzkr9x8n7wv2r6gxqz2xbmggn5djsrshl24ljk4qpryji2pnd"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomicranges" ,r-genomicranges)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-iranges" ,r-iranges)
       ("r-nmf" ,r-nmf)
       ("r-plyr" ,r-plyr)
       ("r-pracma" ,r-pracma)
       ("r-reshape2" ,r-reshape2)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://github.com/CuppenResearch/MutationalPatterns")
    (synopsis "Package for extracting and visualizing mutational patterns in
SNV data")
    (description "This package provides an extensive toolset for the
characterization and visualization of a wide range of mutational patterns
in base substitution data.")
    (license license:expat)))
