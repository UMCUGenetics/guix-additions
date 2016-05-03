;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
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

(define-module (umcu packages sambamba)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ldc)
  #:use-module (gnu packages llvm))

(define-public sambamba-0.5.9
  (let ((commit "c810c7ef14957f16288c205fd7b9d25c4ae7005d"))
  ;;(let ((commit "2ca5a2dbac5ab90c3b4c588519edc3edcb71df84"))
    (package
      (name "sambamba")
      (version (string-append "0.5.9-1." (string-take commit 7)))
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/roelj/sambamba.git")
              ;;(url "https://github.com/pjotrp/sambamba.git")
              (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "0c4c13f021sl7mf5xc2v8dbwsz775n8dlsrrn7qa6qgbx05n54dv"))))
          ;;"1f14wn9aaxwjkmla6pzq3s28741carbr2v0fd2v2mm1dcpwnrqz5"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("ldc" ,ldc)
         ;;("lz4" ,lz4)
         ("rdmd" ,rdmd)
         ("zlib" ,zlib)
         ("perl" ,perl) ; Needed for htslib
         ("ruby" ,ruby) ; Needed for htslib
         ("python" ,python) ; Needed for htslib
         ("gcc" ,gcc)
         ("lz4-src"
          ,(origin
             (method url-fetch)
             (uri "https://github.com/Cyan4973/lz4/archive/160661c7a4cbf805f4af74d2e3932a17a66e6ce7.tar.gz")
             (sha256
              (base32 "131nnbsd5dh7c8sdqzc9kawh3mi0qi4qxznv7zhzfszlx4g2fd20"))))
         ("htslib-src"
          ,(origin
             (method url-fetch)
             (uri "https://github.com/lomereiter/htslib/archive/2f3c3ea7b301f9b45737a793c0b2dcf0240e5ee5.tar.gz")
             ;;(uri "https://github.com/samtools/htslib/archive/1.3.tar.gz")
             ;;(file-name "htslib-1.3.tar.gz")
             (sha256
              (base32 "0bl6w856afnbgdsw8bybsxpqsyf2ba3f12rqh47hhpxvv866g08w"))))
              ;;(base32 "1bqkif7yrqmiqak5yb74kgpb2lsdlg7y344qa1xkdg7k1l4m86i9"))
             ;;(patches (list (search-patch "htslib-add-cram_to_bam.patch")))))
         ("biod-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/biod/BioD.git")
                   (commit "7efdb8a2f7fdcd71c9ad9596be48d1262bb1bd5b")))
             (file-name "biod-src")
             (sha256
              (base32 "09icc2bjsg9y4hxjim4ql275izadf0kh1nnmapg8manyz6bc8svf"))))))
      (arguments
       `(#:tests? #f
         #:make-flags (list "-f" "Makefile.guix")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (add-after 'unpack 'unpack-htslib-sources
             (lambda* (#:key inputs #:allow-other-keys)
               ;; The current build compiles htslib statically into the
               ;; executable.  On top of that, we need to patch the latest
               ;; version of htslib to have it working with Sambamba.
               (and (with-directory-excursion "htslib"
                      (zero? (system* "tar" "xvf" (assoc-ref inputs "htslib-src")
                                      "--strip-components=1")))
                    (with-directory-excursion "lz4"
                      (zero? (system* "tar" "xvf" (assoc-ref inputs "lz4-src")
                                      "--strip-components=1")))
                    (zero? (system* "rm" "-r" "BioD"))
                    (zero? (system* "ln" "--symbolic" "--no-target-directory"
                                    (assoc-ref inputs "biod-src") "BioD")))))
           (replace
            'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (zero? (system* "make" "sambamba-ldmd2-64" "CC=gcc" "D_COMPILER=ldc2"
                       (string-append "LDC_LIB_PATH="
                                             (assoc-ref inputs "ldc")
                                             "/lib")))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "build/sambamba" bin)))))))
      (home-page "https://github.com/lomereiter/sambamba")
      (synopsis "A tool for working with SAM and BAM files written in D.")
      (description
       "Sambamba is a high performance modern robust and fast tool (and
library), written in the D programming language, for working with SAM
and BAM files.  Current parallelised functionality is an important
subset of samtools functionality, including view, index, sort,
markdup, and depth.")
      (license license:gpl2+))))
