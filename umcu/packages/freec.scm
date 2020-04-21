;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages freec)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression))

(define-public freec-8.7
  (package
    (name "control-freec")
    (version "8.7")
    (source (origin
      (method url-fetch)
      (uri "http://bioinfo-out.curie.fr/projects/freec/src/FREEC_Linux64.tar.gz")
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "17vswfnlqykja03nlf9b7n29vypr2590d0hlyd07kad9zc36kbam"))))
    (build-system gnu-build-system)
    ;; The source code's filename indicates only a 64-bit Linux build.
    ;; We need to investigate whether this is true.
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There's no configure phase because there are no external
         ;; dependencies.
         (delete 'configure)
         ;; There are no tests.
         (delete 'check)
         (replace
          'unpack
          (lambda* (#:key source #:allow-other-keys)
            (and
             (zero? (system* "mkdir" "source"))
             (with-directory-excursion "source"
               (zero? (system* "tar" "xvf" source))))))
         (replace
          'build
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "source"
              (zero? (system* "make")))))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "source/freec" bin)))))))
    (home-page "http://bioinfo-out.curie.fr/projects/freec/")
    (synopsis "Tool for detection of copy-number changes and allelic imbalances
(including LOH) using deep-sequencing data")
    (description "Control-FREEC automatically computes, normalizes, segments
copy number and beta allele frequency (BAF) profiles, then calls copy number
alterations and LOH.  The control (matched normal) sample is optional for whole
genome sequencing data but mandatory for whole exome or targeted sequencing
data.  For whole genome sequencing data analysis, the program can also use
mappability data (files created by GEM). ")
    (license license:gpl2+)))

(define-public freec-10.4
  (package
    (name "freec")
    (version "10.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/BoevaLab/FREEC/archive/v"
                           version ".tar.gz"))
       (file-name (string-append "freec-" version ".tar.gz"))
       (sha256
        (base32 "1a7wfr18hljw9xlzpy3920vf29s4cbpn9f0p8z153whq4g2487yh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'move-to-src-dir
           (lambda _
             (chdir "src")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share/freec")))
               (mkdir-p bin)
               (mkdir-p share)
               (copy-recursively "../scripts" share)
               (install-file "freec" bin)))))))
    (inputs
     `(("perl" ,perl)))
    (propagated-inputs
     `(("r-rtracklayer" ,r-rtracklayer)))
    (home-page "http://bioinfo-out.curie.fr/projects/freec/")
    (synopsis "Tool for detection of copy-number changes and allelic imbalances
(including LOH) using deep-sequencing data")
    (description "Control-FREEC automatically computes, normalizes, segments
copy number and beta allele frequency (BAF) profiles, then calls copy number
alterations and LOH.  The control (matched normal) sample is optional for whole
genome sequencing data but mandatory for whole exome or targeted sequencing
data.  For whole genome sequencing data analysis, the program can also use
mappability data (files created by GEM).")
    (license license:gpl2+)))

(define-public freec-10.5
  (package (inherit freec-10.4)
    (version "10.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/BoevaLab/FREEC/archive/v"
                           version ".tar.gz"))
       (file-name (string-append "freec-" version ".tar.gz"))
       (sha256
        (base32 "0z657hbpnc76pkli7g1ka07q4bpl41zarjhq6fwh6g9s368id15j"))))))

(define-public freec-mappability-tracks
  (package
    (name "freec-mappability-tracks")
    (version "hg19_100bp")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://xfer.curie.fr/get/nil/7hZIk1C63h0/"
                    version ".tar.gz"))
              (sha256
               (base32
                "1qp05na2lb7w35nqii9gzv4clmppi3hnk5w3kzfpz5sz27fw1lym"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source-file (assoc-ref %build-inputs "source"))
               (output-dir (string-append %output "/share/freec"))
               (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (PATH (string-append (assoc-ref %build-inputs "gzip") "/bin")))
           (setenv "PATH" PATH)
           (mkdir-p output-dir)
           (with-directory-excursion output-dir
             (system* tar "-xvf" source-file))))))
    (inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "http://boevalab.com/FREEC")
    (synopsis "")
    (description "")
    (license #f)))

(define-public gccount
  (package
    (name "gccount")
    (version "0")
    (source (origin
      (method url-fetch)
      (uri "http://bioinfo-out.curie.fr/projects/freec/src/gccount.tar.gz")
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "1z9rs1fv29adbkwb2ns76v2j29zm2134916c53jqlpjqf6qzm6sw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gcc" ,gcc-5)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There's no configure phase because there are no external
         ;; dependencies.
         (delete 'configure)
         ;; There are no tests.
         (delete 'check)
         (replace
          'unpack
          (lambda* (#:key source #:allow-other-keys)
            (and
             (zero? (system* "mkdir" "source"))
             (with-directory-excursion "source"
               (zero? (system* "tar" "xvf" source))))))
         (replace
          'build
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "source"
              (substitute* "main.cpp"
                (("// main.cpp") "#include <unistd.h>"))
              (zero? (system* "make")))))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "source/gccount" bin)))))))
    (home-page "http://bioinfo-out.curie.fr/projects/freec/")
    (synopsis "Utility to generate a GC-content profile.")
    (description "This package provides the 'gccount' utility that can be
used with FREEC.")
    (license license:gpl2+)))
