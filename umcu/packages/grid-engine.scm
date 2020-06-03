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

(define-module (umcu packages grid-engine)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages java)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls))

(define-public grid-engine-core
  (package
    (name "grid-engine-core")
    (version "8.1.9")
    (source (origin
              (method url-fetch)
              ;; The link for the tarballs is down.
              ;; So we have to (temporarily) host this ourselves.
              (uri ;; (string-append
                   ;;  "http://arc.liv.ac.uk/downloads/SGE/releases/"
                   ;;  version "/sge-" version ".tar.gz")
               (string-append "https://github.com/UMCUGenetics/guix-additions/"
                              "raw/master/missing-tarballs/sge-"
                              version ".tar.gz"))
              (sha256
               (base32
                "0ra7m9mf09zzcf815y3zqzqkj95v9zm24nhhvzmdh2bsqgdmk59w"))))
    (build-system gnu-build-system)
    (supported-systems '("x86_64-linux"))
    (inputs
     `(("bdb" ,bdb)
       ("tcsh" ,tcsh)
       ("inetutils" ,inetutils)
       ("hwloc" ,hwloc "lib")
       ("openssl" ,openssl-1.0)
       ("coreutils" ,coreutils)
       ("tcl" ,tcl)
       ("linux-pam" ,linux-pam)
       ("python" ,python-2.7)
       ("perl" ,perl)
       ("ruby" ,ruby)
       ("gawk" ,gawk)
       ("libtirpc" ,libtirpc)
       ("icedtea" ,icedtea-8)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-various-stuff
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "source/aimk"
               (("/usr/bin/uname") "uname")
               (("uname") (string-append (assoc-ref inputs "coreutils")
                                         "/bin/uname")))
             (substitute* "source/dist/util/arch"
               (("/bin/uname") (string-append (assoc-ref inputs "coreutils")
                                              "/bin/uname"))
               (("/lib64/libc.so.6") (string-append (assoc-ref inputs "libc")
                                                    "/lib/libc.so.6"))
               (("awk") (string-append (assoc-ref inputs "gawk") "/bin/gawk"))
               (("head") (string-append (assoc-ref inputs "coreutils")
                                        "/bin/head")))
             (substitute* "source/aimk"
               (("= cc") (string-append "= gcc")))
             (substitute* "source/configure"
               (("SHELL=") (string-append "SHELL=" (assoc-ref inputs "bash")
                                          "/bin/sh #")))
             ))
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (chdir "source")
             (setenv "SGE_INPUT_LDFLAGS" "-ltirpc")
             (setenv "SGE_INPUT_CFLAGS"
                     (string-append
                      "-I" (assoc-ref inputs "openssl") "/include "
                      "-I" (assoc-ref inputs "libtirpc") "/include/tirpc"))
             (setenv "JAVA_HOME" (assoc-ref inputs "icedtea"))
             (system "scripts/bootstrap.sh")
             #t))
         (replace 'build
           (lambda _
             (system "./aimk -only-core -no-java -no-jni")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The scripts/distinst would not work, so we copy the files
             ;; over manually.
             (chdir "LINUXAMD64")
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (lib (string-append (assoc-ref outputs "out") "/lib"))
                   (include (string-append (assoc-ref outputs "out")
                                           "/include")))
               (mkdir-p bin)
               (mkdir-p lib)
               (mkdir-p include)

               ;; Binaries
               (for-each (lambda (file)
                           (install-file file bin))
                         '("qacct" "qalter" "qconf" "qdel" "qevent" "qhost"
                           "qmod" "qping" "qquota" "qrdel" "qrstat" "qrsub"
                           "qsh" "qstat" "qsub" "sge_coshepherd" "sge_execd"
                           "sgepasswd" "sge_qmaster" "sge_shadowd"
                           "sge_share_mon" "sge_shepherd"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qalter")
                        (string-append bin "/qhold"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qalter")
                        (string-append bin "/qresub"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qalter")
                        (string-append bin "/qrls"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qsh")
                        (string-append bin "/qrsh"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qstat")
                        (string-append bin "/qselect"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qsh")
                        (string-append bin "/qlogin"))

               ;; Libraries
               (for-each (lambda (file)
                           (install-file file lib))
                         '("libdrmaa.so" "libjuti.so" "libspoolb.so"
                           "libspoolc.so" "pam_sge_authorize.so"
                           "pam_sge-qrsh-setup.so"))
               (system* "ln" "--symbolic"
                        (string-append lib "/libdrmaa.so")
                        (string-append lib "/libdrmaa.so.1"))
               (system* "ln" "--symbolic"
                        (string-append lib "/libdrmaa.so")
                        (string-append lib "/libdrmaa.so.1.0"))

               ;; Headers
               (install-file "../libs/japi/drmaa.h" include)
               (install-file "../libs/sched/sge_pqs_api.h" include)

               ;; Pkg-config file
               (mkdir-p (string-append lib "/pkgconfig"))
               (with-output-to-file (string-append lib "/pkgconfig/drmaa.pc")
                 (lambda _
                   (format #t "Name: drmaa~%Description: DRMAA interface~%Version: 8.1.9~%Requires:~%Libs: -L~a -ldrmaa~%Cflags: -I~a" lib include)))
               #t))))))
    (native-search-paths
     (list (search-path-specification
            (variable "LD_LIBRARY_PATH")
            (files '("lib")))
           (search-path-specification
            (variable "DRMAA_LIBRARY_PATH")
            (files '("lib/libdrmaa.so"))
            (file-type 'regular))))
    (home-page "https://arc.liv.ac.uk/trac/SGE")
    (synopsis "Implementation of a grid engine")
    (description "The Son of Grid Engine is a community project to continue
Sun's old gridengine project after Oracle shut down the site and stopped
contributing code.")
    (license (list license:asl2.0 license:gpl2+))))

(define-public grid-engine-core-8.1.8
  (package (inherit grid-engine-core)
    (version "8.1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append
                  "https://arc.liv.ac.uk/downloads/SGE/releases/"
                  version "/sge-" version".tar.gz"))
            (sha256
             (base32 "0pxk585kfs81clqa3gm3h1lfsddfd1dr4qzlr0mshgfqdh72a83c"))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-various-stuff
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "source/aimk"
               (("/usr/bin/uname") "uname")
               (("uname") (string-append (assoc-ref inputs "coreutils")
                                         "/bin/uname")))
             (substitute* "source/dist/util/arch"
               (("/bin/uname") (string-append (assoc-ref inputs "coreutils")
                                              "/bin/uname"))
               (("/lib64/libc.so.6") (string-append (assoc-ref inputs "libc")
                                                    "/lib/libc.so.6"))
               (("awk") (string-append (assoc-ref inputs "gawk") "/bin/gawk"))
               (("head") (string-append (assoc-ref inputs "coreutils")
                                        "/bin/head")))
             (substitute* "source/aimk"
               (("= cc") (string-append "= gcc")))
             ;(substitute* "source/configure"
             ;  (("SHELL=") (string-append "SHELL=" (assoc-ref inputs "bash")
             ;                             "/bin/sh #")))
             ))
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (chdir "source")
             (setenv "SGE_INPUT_LDFLAGS" "-ltirpc")
             (setenv "SGE_INPUT_CFLAGS"
                     (string-append
                      "-I" (assoc-ref inputs "openssl") "/include "
                      "-I" (assoc-ref inputs "libtirpc") "/include/tirpc"))
             (setenv "JAVA_HOME" (assoc-ref inputs "icedtea"))
             (system "scripts/bootstrap.sh")
             #t))
         (replace 'build
           (lambda _
             (system "./aimk -only-core -no-java -no-jni")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The scripts/distinst would not work, so we copy the files
             ;; over manually.
             (chdir "LINUXAMD64")
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (lib (string-append (assoc-ref outputs "out") "/lib"))
                   (include (string-append (assoc-ref outputs "out")
                                           "/include")))
               (mkdir-p bin)
               (mkdir-p lib)
               (mkdir-p include)

               ;; Binaries
               (for-each (lambda (file)
                           (install-file file bin))
                         '("qacct" "qalter" "qconf" "qdel" "qevent" "qhost"
                           "qmod" "qping" "qquota" "qrdel" "qrstat" "qrsub"
                           "qsh" "qstat" "qsub" "sge_coshepherd" "sge_execd"
                           "sgepasswd" "sge_qmaster" "sge_shadowd"
                           "sge_share_mon" "sge_shepherd"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qalter")
                        (string-append bin "/qhold"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qalter")
                        (string-append bin "/qresub"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qalter")
                        (string-append bin "/qrls"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qsh")
                        (string-append bin "/qrsh"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qstat")
                        (string-append bin "/qselect"))
               (system* "ln" "--symbolic"
                        (string-append bin "/qsh")
                        (string-append bin "/qlogin"))

               ;; Libraries
               (for-each (lambda (file)
                           (install-file file lib))
                         '("libdrmaa.so" "libjuti.so" "libspoolb.so"
                           "libspoolc.so" "pam_sge_authorize.so"
                           "pam_sge-qrsh-setup.so"))
               (system* "ln" "--symbolic"
                        (string-append lib "/libdrmaa.so")
                        (string-append lib "/libdrmaa.so.1"))
               (system* "ln" "--symbolic"
                        (string-append lib "/libdrmaa.so")
                        (string-append lib "/libdrmaa.so.1.0"))

               ;; Headers
               (install-file "../libs/japi/drmaa.h" include)
               (install-file "../libs/sched/sge_pqs_api.h" include)

               ;; Pkg-config file
               (mkdir-p (string-append lib "/pkgconfig"))
               (with-output-to-file (string-append lib "/pkgconfig/drmaa.pc")
                 (lambda _
                   (format #t "Name: drmaa~%Description: DRMAA interface~%Version: 8.1.8~%Requires:~%Libs: -L~a -ldrmaa~%Cflags: -I~a" lib include)))
               #t))))))))

(define-public qsub-slurm
  (let ((commit "fa98fd5ee1907b20cd6ab06a84a05260a44f23de"))
    (package
     (name "qsub-slurm")
     (version "0.0.5")
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/roelj/qsub-slurm.git")
                    (commit commit)))
              (sha256
               (base32
                "0v7n50fxadcgk2hrjcgrazr2qfzgfxd1jadm6zlxdbimwjkq2db6"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests? #f))
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("pkg-config" ,pkg-config)))
     (inputs
      `(("slurm" ,slurm)
        ("guile" ,guile-3.0)))
     (home-page "https://github.com/roelj/qsub-slurm")
     (synopsis "Compatibility tool to move from SGE to SLURM.")
     (description "This package an alternative @code{qsub} command that
will submit jobs to SLURM.")
     (license license:gpl3+))))

(define-public slurm-drmaa-1.1.1
  (package
    (name "slurm-drmaa")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/natefoo/slurm-drmaa/releases/download/"
                    version "/slurm-drmaa-" version ".tar.gz"))
              (sha256
               (base32
                "19r4cm88pcpm3wli4cc61zq7354pg67cg866f3a430p15hm1knrn"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; The tests require "bats".
    (inputs
     `(("slurm" ,slurm)))
    (native-inputs
     `(("which" ,which)))
    (home-page "https://github.com/natefoo/slurm-drmaa")
    (synopsis "Distributed resource management application API for SLURM")
    (description
     "PSNC DRMAA for Simple Linux Utility for Resource Management (SLURM) is
an implementation of Open Grid Forum DRMAA 1.0 (Distributed Resource
Management Application API) specification for submission and control of jobs
to SLURM.  Using DRMAA, grid applications builders, portal developers and ISVs
can use the same high-level API to link their software with different
cluster/resource management systems.")
    (license license:gpl3+)))
