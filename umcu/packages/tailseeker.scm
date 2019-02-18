;;; Copyright © 2019 Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
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

(define-module (umcu packages tailseeker)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths))

(define-public all-your-base
  (let ((commit "b44212c66fe95e0b5ca51bfefcbe4eaa2c178fc4"))
    (package
     (name "all-your-base")
     (version (string-append "2-" (string-take commit 7)))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/hyeshik/AYB2.git")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32 "165ncgmw189qw72kjzg08y704sb0a6fi1wihibl2wr30p5091mcb"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests? #f
        #:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'move-to-src-directory
            (lambda _
              (chdir "src")
              #t))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((out (string-append (assoc-ref %outputs "out")))
                     (bin (string-append out "/bin")))
                (mkdir-p bin)
                (install-file "../bin/AYB" bin)))))))
     (inputs
      `(("bzip2" ,bzip2)
        ("zlib" ,zlib)
        ("lapack" ,lapack)))
     (home-page "https://github.com/hyeshik/AYB2")
     (synopsis "Base caller for the Illumina platform")
     (description "This package contains a fork of the All Your Base base 
caller, specifically targeting the use for tailseeker2.")
     (license license:gpl3+))))

(define-public tailseeker2
  (package
   (name "tailseeker")
   (version "2.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/hyeshik/tailseeker.git")
                  (commit "0edfa971df6616d3268d25676d2922bc386143a8")))
            (file-name (git-file-name name version))
            (sha256
             (base32 "1767wv5jkhkyqy6z7w2kn4jgkak852yphfppgr8xwf3id30s8bdf"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'move-to-src
          (lambda* (#:key inputs #:allow-other-keys)
            (chdir "src")
            (setenv "CFLAGS" (string-append
                              "-I"
                              (assoc-ref inputs "python-numpy")
                              "/lib/python3.7/site-packages/numpy/core/include"))
            (substitute* "Makefile"
              (("CFLAGS=	-O3 -Wall -Werror") "CFLAGS=	-O2 -Wall"))
            (invoke "make")))
        (add-after 'install 'install-python-modules
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin-dir     (string-append out "/bin"))
                   (python-path (string-append out "/lib/python3.7/site-packages/tailseeker"))
                   (python-cmd  (string-append (assoc-ref inputs "python") "/bin/python3"))
                   (ayb-cmd     (string-append (assoc-ref inputs "all-your-base") "/bin/AYB"))
                   (bgzip-cmd   (string-append (assoc-ref inputs "htslib") "/bin/bgzip"))
                   (tabix-cmd   (string-append (assoc-ref inputs "htslib") "/bin/tabix"))
                   (snake-cmd   (string-append (assoc-ref inputs "snakemake") "/bin/snakemake"))
                   (share-dir   (string-append out "/share/tailseeker"))
                   (conf-dir    (string-append share-dir "/conf")))
              (for-each mkdir-p (list python-path share-dir conf-dir))
              (symlink python-path (string-append share-dir "/tailseeker"))
              (copy-recursively "../tailseeker" python-path)
              (install-file "../bin/sqi2fq" bin-dir)
              (install-file "../bin/tailseq-retrieve-signals" bin-dir)
              (substitute* (string-append out "/bin/tailseeker")
                (("TAILSEEKER_DIR = ") (string-append "TAILSEEKER_DIR = '" share-dir "' #")))
              (install-file "../conf/defaults.conf" conf-dir)
              (install-file "../conf/defaults-hiseq.conf" conf-dir)
              (install-file "../conf/defaults-miseq.conf" conf-dir)

              ;; XXX: This is a run-time specific setting for which no good default exists.
              ;;(substitute* (string-append conf-dir "/defaults-miseq.conf")
              ;;  (("_exp: [73, GTCAG, 1]") "_exp: [72, CCCCCCCCCC, 1]"))

              (call-with-output-file (string-append conf-dir "/paths.conf")
                (lambda (port)
                  (for-each (lambda (item) (format port "~a: ~a~%" (car item) (cdr item)))
                            `(("tailseeker" . ,share-dir)
                              ("python3"    . ,python-cmd)
                              ("bgzip"      . ,bgzip-cmd)
                              ("tabix"      . ,tabix-cmd)
                              ("AYB"        . ,ayb-cmd)
                              ("snakemake"  . ,snake-cmd)))))

              ;; Tailseeker doesn't care about UNIX filesystems and expects to find binaries
              ;; in its own directory.
              (mkdir-p (string-append share-dir "/bin"))
              (symlink (string-append bin-dir "/tailseq-retrieve-signals")
                       (string-append share-dir "/bin/tailseq-retrieve-signals"))
              (symlink (string-append bin-dir "/sqi2fq")
                       (string-append share-dir "/bin/sqi2fq"))))))))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (inputs
    `(("python" ,python-3.7)
      ("all-your-base" ,all-your-base)))
   (propagated-inputs
    `(("python-biopython" ,python-biopython)
      ("python-scikit-learn" ,python-scikit-learn)
      ("python-scipy" ,python-scipy)
      ("python-numpy" ,python-numpy)
      ("python-pandas" ,python-pandas)
      ("python-matplotlib" ,python-matplotlib)
      ("python-pyyaml" ,python-pyyaml)
      ("ghmm" ,ghmm)
      ("snakemake" ,snakemake)
      ("htslib" ,htslib)))
   (home-page "https://github.com/hyeshik/tailseeker/tree/tailseeker2")
   (synopsis "Measure poly-A tail lengths from Illumina sequencers")
   (description "This package contains software for measuring poly(A) tail
length and 3′-end modifications using a high-throughput sequencer.")
   (license license:expat)))
