;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pth)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (umcu packages grid-engine)
  #:use-module (umcu packages perl)
  #:use-module (umcu packages python))

(define-public picard-bin-1.141
  (package
   (name "picard")
   (version "1.141")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/broadinstitute/picard/releases/download/"
                  version "/picard-tools-" version ".zip"))
            (sha256
             (base32 "1ari9j37a0v8bm03c77pw729bqwbqqn6h15rw028jhl1iz4rgd5g"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("icedtea" ,icedtea-8)))
   (native-inputs
    `(("unzip" ,unzip)))
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'unpack
          (lambda _
            (zero? (system* "unzip" (assoc-ref %build-inputs "source")))))
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/picard/")))
              (chdir (string-append "picard-tools-" ,version))
              (install-file (string-append "htsjdk-" ,version ".jar") out)
              (install-file "libIntelDeflater.so" out)
              (install-file "picard-lib.jar" out)
              (install-file "picard.jar" out)))))))
   (home-page "http://broadinstitute.github.io/picard/")
    (synopsis "A set of Java command line tools for manipulating high-throughput
sequencing data (HTS) data and formats")
    (description "Picard comprises Java-based command-line utilities that
manipulate SAM files, and a Java API (HTSJDK) for creating new programs that
read and write SAM files. Both SAM text format and SAM binary (BAM) format are
supported.")
    (license license:expat)))

(define-public r-ascat
  (let ((commit "9fb25feaae2d7d25a17f5eff7b99666ad7afbba8"))
    (package
     (name "r-ascat")
     (version (string-append "2.5.1-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Crick-CancerGenomics/ascat.git")
                    (commit commit)))
              (sha256
               (base32
                "02fxhqv4yf9dby8mmjb39fyqd141k3z4nhj0p8m2h4n7a476bdsc"))))
     (build-system r-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
         (add-after 'unpack 'move-to-ascat-dir
           (lambda _
             (chdir "ASCAT"))))))
     (propagated-inputs
      `(("r-rcolorbrewer" ,r-rcolorbrewer)))
     (home-page "https://github.com/Crick-CancerGenomics/ascat")
     (synopsis "ASCAT copy number R package")
     (description "This package provides the ASCAT R package that can be used
to infer tumour purity, ploidy and allele-specific copy number profiles.")
     (license license:gpl3))))

(define-public cgp-battenberg-r
  (package
   (name "cgp-battenberg-r")
   (version "2.2.8")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Wedge-Oxford/battenberg/archive/v"
                  version ".tar.gz"))
            (sha256
             (base32 "0gi9zv8clr795mzplf1d3dm5agc78xz40kmwckcjqaji4dnbcik1"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-devtools" ,r-devtools)
      ("r-readr" ,r-readr)
      ("r-doparallel" ,r-doparallel)
      ("r-ggplot2" ,r-ggplot2)
      ("r-rcolorbrewer" ,r-rcolorbrewer)
      ("r-gridextra" ,r-gridextra)
      ("r-gtools" ,r-gtools)
      ("r-ascat" ,r-ascat)))
   (home-page "https://github.com/Wedge-Oxford/battenberg")
   (synopsis "Battenberg R package for subclonal copy number estimation")
   (description "This package contains the Battenberg R package for subclonal
copy number estimation.")
   (license license:gpl3)))

(define-public glibc-locales-2.27
  (package (inherit (make-glibc-locales glibc-2.27))
           (name "glibc-locales-2.27")))

(define-public glibc-locales-2.28
  (package (inherit (make-glibc-locales glibc-2.28))
           (name "glibc-locales-2.28")))
