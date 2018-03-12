;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages biodynamics-group)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python))

(define-public spades
  (package
   (name "spades")
   (version "3.11.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://cab.spbu.ru/files/release"
                  version "/SPAdes-" version ".tar.gz"))
            (sha256
             (base32 "0x5l4nkkjrkdn41ifz2baz9bp2r5blgrf77impc5nnbxpy35vf1s"))))
   (build-system cmake-build-system)
   ;; Reported under section 2 "Installation", "SPAdes requires a 64-bit
   ;; system": http://cab.spbu.ru/files/release3.10.1/manual.html
   (supported-systems '("x86_64-linux"))
   (arguments
    `(#:tests? #f ; There is no test target.
      #:phases
      (modify-phases %standard-phases
        (add-before 'configure 'move-to-source-dir
          (lambda _
            (chdir "src"))))))
   ;; TODO:  While this build works fine, SPAdes bundles samtools, bwa, and
   ;; boost.  These packages are also available in GNU Guix, so we should
   ;; unbundle them.
   (inputs
    `(("bzip2" ,bzip2)
      ("zlib" ,zlib)
      ("perl" ,perl)
      ("python-2" ,python-2)))
   (home-page "http://cab.spbu.ru/software/spades")
   (synopsis "Genome assembly toolkit")
   (description "SPAdes is an assembly toolkit containing various assembly
pipelines.")
   (license license:gpl2)))

(define-public kaiju
  (package
   (name "kaiju")
   (version "1.5.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/bioinformatics-centre/kaiju/archive/v"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "0afbfalfw9y39bkwnqjrh9bghs118ws1pzj5h8l0nblgn3mbjdks"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'move-to-src-dir
          (lambda _ (chdir "src") #t))
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (mkdir-p bin)
              (chdir "..")
              (copy-recursively "bin" bin)
              (copy-recursively "util" bin)))))))
   (inputs
    `(("perl" ,perl)))
   (home-page "http://kaiju.binf.ku.dk/")
   (synopsis "Fast and sensitive taxonomic classification for metagenomics")
   (description "Kaiju is a program for sensitive taxonomic classification
of high-throughput sequencing reads from metagenomic whole genome sequencing
experiments.")
   (license license:gpl3+)))
