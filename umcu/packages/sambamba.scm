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
  #:use-module (gnu packages base)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ldc)
  #:use-module (gnu packages llvm)
  #:use-module (srfi srfi-1))

;; Imported from gitlab.com/genenetwork/guix-bioinformatics
(define-public shunit2
  (let ((commit "60dd60bcd1573befe38465010263ab242e55811d"))
    (package
      (name "shunit2")
      (version (string-append "2.0.4-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kward/shunit2.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32 "11savxc6qliqv25kv59qak6j7syjv95hbpmq1szn1mzn32g2gc25"))))
    (inputs `(("coreutils" ,coreutils))) ; for mktemp and od
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ;; no test-suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (add-after 'unpack 'replace-binary-paths
                      (lambda _
                        (substitute* "source/2.0/src/shell/shunit2"
                                     (("/bin/sh") (which "sh"))
                                     (("exec mktemp") (string-append "exec " (which "mktemp")))
                                     (("/usr/bin/od") (which "od"))
                                     )#t))

           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (shunit2-exec (string-append bin "/shunit2")))
                 (write (file-exists? "source/2.0/src/shell/shunit2"))
                 (write (format #t "build directory: ~s~%" (getcwd)))
                 (mkdir-p bin)
                 (copy-file "source/2.0/src/shell/shunit2" shunit2-exec)
                 (chmod shunit2-exec #o555)
                 #t))))))
      (home-page "https://code.google.com/archive/p/shunit2/")
      (synopsis "xUnit based unit testing for Unix shell scripts")
      (description
       "shUnit2 is a xUnit unit test framework for Bourne based shell
scripts, and it is designed to work in a similar manner to JUnit,
PyUnit, etc. If you have ever had the desire to write a unit test for
a shell script, shUnit2 can do the job.")
      (license license:lgpl2.0))))

(define-public sambamba-0.6.6
  (let ((commit "52ab1eff7070cd3dafad94a15c9fd6e243a45a4d"))
    (package
      (name "sambamba")
      (version (string-append "0.6.6-" (string-take commit 7)))
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/pjotrp/sambamba.git")
              (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "16nfvmipa1gj7f8c9lqhh5yffdmaic1pzcrm9281dqwywl9w8hsg"))))
      (build-system gnu-build-system)
      (outputs '("out"     ; disable all checks for speed
                 "debug"))
      (inputs
       `(("samtools" ,samtools) ; for pileup
         ("bcftools" ,bcftools) ; for pileup
         ("lz4" ,lz4)
         ("zlib" ,zlib)
       ))
      (native-inputs
       `(("ldc" ,ldc)
         ("shunit2" ,shunit2)
         ("coreutils" ,coreutils) ; for env
         ("perl" ,perl) ; Needed for building htslib
         ("ruby" ,ruby) ; Needed for building htslib
         ("python" ,python-2) ; Needed for building htslib and sambamba
         ("gcc" ,gcc)
         ("which" ,which)
         ("htslib-src"
          ,(origin
             (method url-fetch)
             (uri "https://github.com/lomereiter/htslib/archive/2f3c3ea7b301f9b45737a793c0b2dcf0240e5ee5.tar.gz")
             ;;(uri "https://github.com/samtools/htslib/archive/1.3.tar.gz")
             (file-name "htslib-0.2.0-rc10-271-g2f3c3ea-dirty.tar.gz")
             (sha256
              (base32 "0bl6w856afnbgdsw8bybsxpqsyf2ba3f12rqh47hhpxvv866g08w"))))
              ;;(base32 "1bqkif7yrqmiqak5yb74kgpb2lsdlg7y344qa1xkdg7k1l4m86i9"))
             ;;(patches (list (search-patch "htslib-add-cram_to_bam.patch")))))
         ("biod-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/pjotrp/BioD.git")
                   (commit "b7f1db860d212ee5fb6f9adfb36c6e783aaeb6f5")))
             (file-name (string-append "biod-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "01xkdjdn9lb2b4b5ykzhnrk2rjikagav8b3fyac3zafcfq600cr4"))))
         ("dlang-undeaD-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/dlang/undeaD.git")
                   (commit "610234f159132f91046d4fb893889fb8ee14cd2f")))
             (file-name (string-append "dlang-undeaD-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "12zxsgvka4a82ghp2gaviph6kz13jzjb5pbc8v6i3rmcnifzpbrl"))))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (add-after 'unpack 'patch-pileup-d
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "sambamba/pileup.d"
                             (("string samtoolsBin     = null;") (string-append "string samtoolsBin = \"" (which "samtools") "\";"))
                             (("string bcftoolsBin     = null;") (string-append "string bcftoolsBin = \"" (which "bcftools") "\";"))
                             (("    this_app = args[0];") (string-append "    this_app = \"" (which "sambamba") "\";")))))
           (add-after 'unpack 'unpack-htslib-sources
             (lambda* (#:key inputs #:allow-other-keys)
               ;; The current build compiles htslib statically into the
               ;; executable.  On top of that, we need to patch the latest
               ;; version of htslib to have it working with Sambamba.
               (and (with-directory-excursion "htslib"
                      (zero? (system* "tar" "xvf" (assoc-ref inputs "htslib-src")
                                      "--strip-components=1")))
                    (copy-recursively (assoc-ref inputs "dlang-undeaD-src") "undeaD")
                    (copy-recursively (assoc-ref inputs "biod-src") "BioD"))))
           (replace
            'build
            (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
              (let* ((out        (assoc-ref outputs "out"))
                     (debug-out  (assoc-ref outputs "debug")))
                (zero? (system* "make" "-f" "Makefile.guix" "guix"
                                (string-append "LDC_LIB_PATH="
                                               (assoc-ref inputs "ldc")
                                               "/lib"))))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "build/sambamba" bin)))))))
      (home-page "https://github.com/lomereiter/sambamba")
      (synopsis "Fast tool for working with SAM, BAM and CRAM files written in D.")
      (description
       "Sambamba is a high performance modern robust and fast
tool (and library), written in the D programming language, for working
with SAM, BAM and CRAM files.  Current parallelised functionality is
an important subset of samtools functionality, including view, index,
sort, markdup, and depth.")
      (license license:gpl2+))))

(define-public sambamba-next
  (let ((commit "7cff06533b539a99b4e0db681fb573214d63aae2"))
    (package
      (name "sambamba")
      (version (string-append "0.6.7-pre1-" (string-take commit 7)))
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/pjotrp/sambamba.git")
              (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "11k2xqgmbvxwki569439kvzf2b0cqy2x21kbgjijwvpqk9j8czx4"))))
      (build-system gnu-build-system)
      (outputs '("out"     ; disable all checks for speed
                 "debug"))
      (inputs
       `(("samtools" ,samtools) ; for pileup
         ("bcftools" ,bcftools) ; for pileup
         ("lz4" ,lz4)
         ("zlib" ,zlib)))
      (native-inputs
       `(("ldc" ,ldc)
         ("shunit2" ,shunit2)
         ("coreutils" ,coreutils) ; for env
         ("perl" ,perl) ; Needed for building htslib
         ("ruby" ,ruby) ; Needed for building htslib
         ("python" ,python-2) ; Needed for building htslib and sambamba
         ("gcc" ,gcc)
         ("which" ,which)
         ("htslib-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/pjotrp/htslib.git")
                   (commit "2f3c3ea7b301f9b45737a793c0b2dcf0240e5ee5")))
             (file-name (string-append "htslib-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "0g38g8s3npr0gjm9fahlbhiskyfws9l5i0x1ml3rakzj7az5l9c9"))))
         ("biod-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/biod/BioD.git")
                   (commit "c778e4f2d8bacea7499283ce39f5577b232732c6")))
             (file-name (string-append "biod-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "1z90562hg47i63gx042wb3ak2vqjg5z7hwgn9bp2pdxfg3nxrw37"))))
         ("dlang-undeaD-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/dlang/undeaD.git")
                   (commit "92803d25c88657e945511f0976a0c79d8da46e89")))
             (file-name (string-append "dlang-undeaD-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "0vq6n81vzqvgphjw54lz2isc1j8lcxwjdbrhqz1h5gwrvw9w5138"))))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'patch-pileup-d
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "sambamba/pileup.d"
                             (("string samtoolsBin     = null;") (string-append "string samtoolsBin = \"" (which "samtools") "\";"))
                             (("string bcftoolsBin     = null;") (string-append "string bcftoolsBin = \"" (which "bcftools") "\";"))
                             (("    this_app = args[0];") (string-append "    this_app = \"" (which "sambamba") "\";")))))
           (add-after 'unpack 'unpack-htslib-sources
             (lambda* (#:key inputs #:allow-other-keys)
               ;; The current build compiles htslib statically into the
               ;; executable.  On top of that, we need to patch the latest
               ;; version of htslib to have it working with Sambamba.
               (and 
                    (copy-recursively (assoc-ref inputs "htslib-src") "htslib")
                    (copy-recursively (assoc-ref inputs "dlang-undeaD-src") "undeaD")
                    (copy-recursively (assoc-ref inputs "biod-src") "BioD"))))
           (replace
            'build
            (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
              (let* ((out        (assoc-ref outputs "out"))
                     (debug-out  (assoc-ref outputs "debug")))
                (zero? (system* "make" "-f" "Makefile.guix" "guix" "-j" "8"
                                (string-append "LDC_LIB_PATH="
                                               (assoc-ref inputs "ldc")
                                               "/lib"))))))
           (replace
            'check
            (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
              (let* ((out        (assoc-ref outputs "out"))
                     (debug-out  (assoc-ref outputs "debug")))
                (zero? (system* "make" "-f" "Makefile.guix" "check"
                                (string-append "LDC_LIB_PATH="
                                               (assoc-ref inputs "ldc")
                                               "/lib"))))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "build/sambamba" bin)))))))
      (home-page "https://github.com/lomereiter/sambamba")
      (synopsis "Fast tool for working with SAM, BAM and CRAM files written in D.")
      (description
       "Sambamba is a high performance modern robust and fast
tool (and library), written in the D programming language, for working
with SAM, BAM and CRAM files.  Current parallelised functionality is
an important subset of samtools functionality, including view, index,
sort, markdup, and depth.")
      (license license:gpl2+))))
