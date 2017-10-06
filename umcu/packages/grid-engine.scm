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
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages java)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ruby)
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
       ("openssl" ,openssl)
       ("coreutils" ,coreutils)
       ("tcl" ,tcl)
       ("linux-pam" ,linux-pam)
       ("python" ,python-2.7)
       ("perl" ,perl)
       ("ruby" ,ruby)
       ("gawk" ,gawk)
       ("icedtea" ,icedtea)))
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
             (setenv "SGE_INPUT_CFLAGS"
                     (string-append "-I" (assoc-ref inputs "openssl") "/include"))
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
    (home-page "")
    (synopsis "")
    (description "")
    (license (list license:asl2.0 license:gpl2+))))

(define-public guile-drmaa
  (let ((commit "4ceb4093821ce8fca794e723dc50e03c3f2b33c3"))
    (package
      (name "guile-drmaa")
      (version "0.0.1")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/UMCUGenetics/guile-drmaa/releases/download/v"
               version "/guile-drmaa-" version ".tar.gz"))
         (sha256
          (base32 "1xny2iynspd94i2ps84w177s2iy1ylp08h3dm9bw3crc295889y0"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests.
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-scheme-module
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((module-dir (string-append (assoc-ref outputs "out")
                                                "/share/guile/site/2.0")))
                 (mkdir-p module-dir)
                 (substitute* "drmaa.scm"
                   (("libguile_drmaa.so") (string-append
                                           (assoc-ref outputs "out")
                                           "/lib/libguile_drmaa.so")))
                 (install-file "drmaa.scm" module-dir)

                 ;; Remove unneeded copy of drmaa.scm in /share/.
                 (delete-file (string-append (assoc-ref outputs "out")
                                             "/share/drmaa.scm"))))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("guile" ,guile-2.0)
         ("grid-engine-core" ,grid-engine-core)))
      (home-page #f)
      (synopsis #f)
      (description #f)
      (license license:gpl3+))))
