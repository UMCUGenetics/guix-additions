;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (umcu packages bwa)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pcre))

(define-public bwa-0.7.5a
  (package
    (name "bwa")
    (version "0.7.5a")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bio-bwa/bwa-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1pfpzxnmz9m5fgfh3r5cnzg6d5hkx74jycn9fmc24f5r22bxfmyi"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:phases
       (alist-replace
        'install
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((bin (string-append
                      (assoc-ref outputs "out") "/bin"))
                (doc (string-append
                      (assoc-ref outputs "out") "/share/doc/bwa"))
                (man (string-append
                      (assoc-ref outputs "out") "/share/man/man1")))
            (mkdir-p bin)
            (mkdir-p doc)
            (mkdir-p man)
            (install-file "bwa" bin)
            (install-file "README.md" doc)
            (install-file "bwa.1" man)))
        ;; no "configure" script
        (alist-delete 'configure %standard-phases))))
    (inputs `(("zlib" ,zlib)))
    ;; Non-portable SSE instructions are used so building fails on platforms
    ;; other than x86_64.
    (supported-systems '("x86_64-linux"))
    (home-page "http://bio-bwa.sourceforge.net/")
    (synopsis "Burrows-Wheeler sequence aligner")
    (description
     "BWA is a software package for mapping low-divergent sequences against a
large reference genome, such as the human genome.  It consists of three
algorithms: BWA-backtrack, BWA-SW and BWA-MEM.  The first algorithm is
designed for Illumina sequence reads up to 100bp, while the rest two for
longer sequences ranged from 70bp to 1Mbp.  BWA-MEM and BWA-SW share similar
features such as long-read support and split alignment, but BWA-MEM, which is
the latest, is generally recommended for high-quality queries as it is faster
and more accurate.  BWA-MEM also has better performance than BWA-backtrack for
70-100bp Illumina reads.")
    (license license:gpl3+)))

(define-public miniasm
  (package
   (name "miniasm")
   (version "0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/lh3/miniasm/archive/v" version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
               (base32
                "0g89pa98dvh34idv7w1zv12bsbyr3a11c4qb1cdcz68gyda88s4v"))))
   (build-system gnu-build-system)
   (inputs
    `(("zlib" ,zlib)))
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "miniasm" bin)
              (install-file "minidot" bin)))))))
   (home-page "https://github.com/lh3/miniasm")
   (synopsis "Ultrafast de novo assembly for long noisy reads")
   (description "Miniasm is a very fast OLC-based de novo assembler for noisy
long reads.  It takes all-vs-all read self-mappings (typically by minimap) as
input and outputs an assembly graph in the GFA format.  Different from
mainstream assemblers, miniasm does not have a consensus step.  It simply
concatenates pieces of read sequences to generate the final unitig sequences.
Thus the per-base error rate is similar to the raw input reads.")
   (license license:expat)))

(define-public assembly-stats
  (package
   (name "assembly-stats")
   (version "1.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/sanger-pathogens/assembly-stats/archive/v"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "0xc5ppmcs09d16f062nbb0mdb0cnfhbnkp0arlxnfi6jli6n3gh2"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags (list (string-append
                               "-DINSTALL_DIR:PATH="
                               %output
                               "/bin"))))
   (home-page "https://github.com/sanger-pathogens")
   (synopsis "Tool to extract assembly statistics from FASTA and FASTQ files")
   (description "This package provides a tool to extract assembly statistics
from FASTA and FASTQ files.")
   (license license:gpl3)))

(define-public fastq-tools
  (package
   (name "fastq-tools")
   (version "0.8")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://homes.cs.washington.edu/~dcjones/fastq-tools/"
                  name "-" version ".tar.gz"))
            (sha256
             (base32
              "0jz1y40fs3x31bw10097a1nhm0vhbsyxmd4n7dwdsl275sc9l1nz"))))
   (build-system gnu-build-system)
   (arguments `(#:tests? #f))
   (inputs
    `(("pcre" ,pcre "bin")
      ("zlib" ,zlib)))
   (home-page "https://homes.cs.washington.edu/~dcjones/fastq-tools/")
   (synopsis "")
   (description "")
   (license license:gpl3)))
