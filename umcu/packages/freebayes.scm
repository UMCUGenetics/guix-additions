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

(define-module (umcu packages freebayes)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (umcu packages vcflib)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages node)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages linux))

(define-public freebayes-1.0.2
  (let ((commit "3ce827d8ebf89bb3bdc097ee0fe7f46f9f30d5fb")
        (revision "1"))
    (package
      (name "freebayes")
      (version (string-append "1.0.2-" revision "." (string-take commit 7)))
      (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/ekg/freebayes.git")
          (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32 "1sbzwmcbn78ybymjnhwk7qc5r912azy5vqz2y7y81616yc3ba2a2"))))
      (build-system gnu-build-system)
      (inputs
       `(("zlib" ,zlib)
         ("htslib" ,htslib)))
      (native-inputs
       `(("bc" ,bc) ; Needed for running tests.
         ("samtools" ,samtools) ; Needed for running tests.
         ("parallel" ,parallel) ; Needed for running tests.
         ("procps" ,procps) ; Needed for running tests.
         ("bamtools" ,bamtools)
         ("cmake" ,cmake)
         ("python" ,python-2)
         ("node" ,node)
         ("r" ,r)
         ("perl" ,perl)
         ("bamtools-src" ,(package-source bamtools))
         ("vcflib-src" ,(package-source vcflib-1.0.2))
         ;; These are submodules for the vcflib version used in freebayes
         ("tabixpp-src" ,(package-source tabixpp-freebayes))
         ("smithwaterman-src" ,(package-source smithwaterman))
         ("multichoose-src" ,(package-source multichoose))
         ("fsom-src" ,(package-source fsom))
         ("filevercmp-src" ,(package-source filevercmp))
         ("fastahack-src" ,(package-source fastahack))
         ("intervaltree-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ekg/intervaltree/archive/"
                   "dbb4c513d1ad3baac516fc1484c995daf9b42838" ".tar.gz"))
             (file-name "intervaltree-src.tar.gz")
             (sha256
              (base32 "19prwpn2wxsrijp5svfqvfcxl5nj7zdhm3jycd5kqhl9nifpmcks"))))
         ;; These submodules are needed to run the tests.
         ("bash-tap-src" ,(package-source bash-tap))
          ;; ,(origin
          ;;   (method url-fetch)
          ;;   (uri (string-append "https://github.com/illusori/bash-tap/archive/"
          ;;                       "c38fbfa401600cc81ccda66bfc0da3ea56288d03" ".tar.gz"))
          ;;   (file-name "bash-tap-src.tar.gz")
          ;;   (sha256
          ;;    (base32 "07ijb1p0aa65ajpg9nkghc183iha6lwiydkckay8pghapa01j6nz"))))
         ("test-simple-bash-src"
          ,(origin
            (method url-fetch)
            (uri (string-append "https://github.com/ingydotnet/test-simple-bash/archive/"
                                "124673ff204b01c8e96b7fc9f9b32ee35d898acc" ".tar.gz"))
            (file-name "test-simple-bash-src.tar.gz")
            (sha256
             (base32 "016xf3wbgqbav9dncvfdx5k0f10z5xwq8jdszajzmcvnhz5wis14"))))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (zero? (system* "tar" "xvf"
                                                 (assoc-ref inputs source)
                                                 "--strip-components=1"))))))
                 (and
                  (unpack "bamtools-src" "bamtools")
                  (unpack "vcflib-src" "vcflib")
                  ;;(unpack "intervaltree-src" "intervaltree")
                  (unpack "fastahack-src" "vcflib/fastahack")
                  (unpack "filevercmp-src" "vcflib/filevercmp")
                  (unpack "fsom-src" "vcflib/fsom")
                  (unpack "intervaltree-src" "vcflib/intervaltree")
                  (unpack "multichoose-src" "vcflib/multichoose")
                  (unpack "smithwaterman-src" "vcflib/smithwaterman")
                  (unpack "tabixpp-src" "vcflib/tabixpp")
                  (unpack "test-simple-bash-src" "test/test-simple-bash")
                  (unpack "bash-tap-src" "test/bash-tap")))))
           (add-after 'unpack-submodule-sources 'fix-makefile
             (lambda* (#:key inputs #:allow-other-keys)
               ;; We don't have the .git folder to get the version tag from.
               ;; For this checkout of the code, it's v1.0.0.
               (substitute* '("vcflib/Makefile")
                 (("^GIT_VERSION.*") "GIT_VERSION = v1.0.0"))))
           (replace 'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (and
               ;; Compile Bamtools before compiling the main project.
               (with-directory-excursion "bamtools"
                 (system* "mkdir" "build")
                 (with-directory-excursion "build"
                   (and (zero? (system* "cmake" "../"))
                        (zero? (system* "make")))))
               ;; Compile vcflib before we compiling the main project.
               (with-directory-excursion "vcflib"
                 (with-directory-excursion "tabixpp"
                   (let ((htslib-ref (assoc-ref inputs "htslib")))
                     (zero?
                      (system* "make" "HTS_HEADERS="
                               (string-append "HTS_LIB=" htslib-ref "/lib/libhts.a")
                               (string-append "LIBPATH=-L. -L" htslib-ref "/include")))))
                 (zero? (system* "make" "CC=gcc"
                   (string-append "CFLAGS=\"" "-Itabixpp "
                     "-I" (assoc-ref inputs "htslib") "/include " "\"") "all")))
               (with-directory-excursion "src"
                 (zero? (system* "make"))))))
           (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "bin/freebayes" bin)
                (install-file "bin/bamleftalign" bin))))
           ;; There are three tests that fail.  All because of the -P
           ;; (--perl-regexp) option in grep, which is not compiled into the
           ;; version of grep in Guix.
           (replace 'check
            (lambda* (#:key inputs #:allow-other-keys)
              (system* "make" "test"))))))
      (home-page "https://github.com/ekg/freebayes")
      (synopsis "Haplotype-based variant detector")
      (description "FreeBayes is a Bayesian genetic variant detector designed to
find small polymorphisms, specifically SNPs (single-nucleotide polymorphisms),
indels (insertions and deletions), MNPs (multi-nucleotide polymorphisms), and
complex events (composite insertion and substitution events) smaller than the
length of a short-read sequencing alignment.")
      (license license:expat))))
