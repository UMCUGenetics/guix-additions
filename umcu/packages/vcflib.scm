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

(define-module (umcu packages vcflib)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages node)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages statistics))

(define-public tabixpp
  (package
   (name "tabixpp")
   (version "1.0.0")
   (source (origin
     (method url-fetch)
     (uri (string-append "https://github.com/ekg/tabixpp/archive/v"
                         version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1s0lgks7qlvlhvcjhi2wm18nnza1bwcnic44ij7z8wfg88h4ivwn"))))
   (build-system gnu-build-system)
   (inputs
    `(("htslib" ,htslib)
      ("zlib" ,zlib)))
   (arguments
    `(#:tests? #f ; There are no tests to run.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; There is no configure phase.
        ;; The build phase needs overriding the location of htslib.
        (replace 'build
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((htslib-ref (assoc-ref inputs "htslib")))
              (zero?
               (system* "make"
                 (string-append "HTS_LIB=" htslib-ref "/lib/libhts.a")
                 "HTS_HEADERS=" ; No need to check for headers here.
                 (string-append "LIBPATH=-L. -L" htslib-ref "/include"))))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "tabix++" bin)))))))
   (home-page "https://github.com/ekg/tabixpp")
   (synopsis "C++ wrapper around tabix project")
   (description "This is a C++ wrapper around the Tabix project which abstracts
some of the details of opening and jumping in tabix-indexed files.")
   (license license:expat)))

;; This version works with FreeBayes while the released version doesn't. The
;; released creates a variable with the name "vcf" somewhere, which is also the
;; name of a namespace in vcflib.
(define-public tabixpp-freebayes
  (let ((commit "bbc63a49acc52212199f92e9e3b8fba0a593e3f7"))
    (package (inherit tabixpp)
      (name "tabixpp-freebayes")
      (version (string-append "0-1." (string-take commit 7)))
      (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ekg/tabixpp/archive/"
                            commit ".tar.gz"))
        (file-name (string-append name "-" version "-checkout.tar.gz"))
        (sha256
         (base32 "1s06wmpgj4my4pik5kp2lc42hzzazbp5ism2y4i2ajp2y1c68g77")))))))

(define-public vcflib-1.0.2
  (let ((commit "3ce827d8ebf89bb3bdc097ee0fe7f46f9f30d5fb"))
    (package
      (name "vcflib")
      (version (string-append "1.0.2-1." (string-take commit 7)))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/vcflib/vcflib/archive/"
                "5ac091365fdc716cc47cc5410bb97ee5dc2a2c92" ".tar.gz"))
         (file-name "vcflib-5ac0913.tar.gz")
         (sha256
          (base32 "0ywshwpif059z5h0g7zzrdfzzdj2gr8xvwlwcsdxrms3p9iy35h8"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("htslib" ,htslib)
         ("zlib" ,zlib)
         ("python" ,python-2)
         ("perl" ,perl)
         ("r" ,r)
         ("node" ,node)
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
              (base32 "19prwpn2wxsrijp5svfqvfcxl5nj7zdhm3jycd5kqhl9nifpmcks"))))))
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (zero? (system* "tar" "xvf"
                                        (assoc-ref inputs source)
                                        "--strip-components=1"))))))
                 (and
                  (unpack "intervaltree-src" "intervaltree")
                  (unpack "fastahack-src" "fastahack")
                  (unpack "filevercmp-src" "filevercmp")
                  (unpack "fsom-src" "fsom")
                  (unpack "multichoose-src" "multichoose")
                  (unpack "smithwaterman-src" "smithwaterman")
                  (unpack "tabixpp-src" "tabixpp")))))
           (add-after 'unpack-submodule-sources 'fix-makefile
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("Makefile")
                 (("^GIT_VERSION.*") "GIT_VERSION = v1.0.0"))))
           (replace
            'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (with-directory-excursion "tabixpp"
                (zero? (system* "make")))
              (zero? (system* "make" "CC=gcc"
                (string-append "CFLAGS=\"" "-Itabixpp "
                  "-I" (assoc-ref inputs "htslib") "/include " "\"") "all"))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    ;;(include (string-append (assoc-ref outputs "out") "/include"))
                    (lib (string-append (assoc-ref outputs "out") "/lib")))
                (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "bin" ".*"))
                ;; The header files do not correspond to libvcflib.a, therefore
                ;; I left them out.
                ;;(for-each (lambda (file)
                ;;           (install-file file include))
                ;;         (find-files "src" "\\.h$"))
                (install-file "libvcflib.a" lib)))))))
      (home-page "https://github.com/vcflib/vcflib/")
      (synopsis "Library for parsing and manipulating VCF files")
      (description "Vcflib provides methods to manipulate and interpret
sequence variation as it can be described by VCF. It is both an API for parsing
and operating on records of genomic variation as it can be described by the VCF
format, and a collection of command-line utilities for executing complex
manipulations on VCF files.")
      (license license:expat))))
