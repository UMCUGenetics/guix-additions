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

(define-module (umcu packages guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages version-control))

(define-public guixr
  (package
    (name "guixr")
    (version "1.1.0")
    (source #f)
    (build-system gnu-build-system)
    (propagated-inputs
     `(("socat" ,socat)
       ("bash" ,bash)
       ("guix" ,guix)
       ("git" ,git)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (with-output-to-file "guixr"
               (lambda _
                 (format #t "#!/bin/bash

set -u
set -e

#Configuration
guix_root=\"/gnu\"
guix_additional=\"/gnu/repositories/guix-additions\"
guix_pin=\"/gnu/repositories/guix\"
guix_profile=\"/gnu/profiles/base\"
guix=\"~a/bin/guix\"
git=\"~a/bin/git\"
socat=\"~a/bin/socat\"

# Avoid locale warnings.
export GUIX_LOCPATH=\"${guix_profile}/lib/locale\"

# Use /gnu as state directory.
export NIX_STATE_DIR=$guix_root

# Ensure the latest Guix packages are used.  Do not override
# the user's customizations (if any).
if [ ! -L $HOME/.config/guix/latest ]; then
  mkdir -p $HOME/.config/guix
  ln -s /gnu/repositories/guix $HOME/.config/guix/latest
fi

# Include our non-standard package repository
export GUIX_PACKAGE_PATH=\"$guix_additional${GUIX_PACKAGE_PATH:+:$GUIX_PACKAGE_PATH}\"

# Use guix with the given arguments
export GUIX_DAEMON_SOCKET=guix://10.100.7.235:9999
if [ $# -lt 1 ]; then
  ${guix}
elif [ \"$1\" == \"package\" ] && [ $# -ge 2 ] && ([ \"$2\" == \"--install\" ] || [ \"$2\" == \"--upgrade\" ] ||
         [ \"$2\" == \"-i\" ] || [ \"$2\" == \"-u\" ]); then
  ${guix} $@
  echo \"\"
  echo \"You may need the following versioning information for your paper:\";
  echo \"GNU Guix upstream repository:\";
  echo -n \"  \"; ${git} -C /gnu/repositories/guix describe --always;
  echo \"UMCU additional package repository:\";
  echo -n \"  \"; ${git} -C /gnu/repositories/guix-additions describe --always;
elif [ \"$1\" == \"pull\" ]; then
  echo \"This feature has been disabled.\";
elif [ \"$1\" == \"load-profile\" ]; then
  if [ $# -gt 1 ]; then
    if [ \"$2\" != \"--help\" ] && [ \"$2\" != \"-h\" ]; then
      ~a/bin/bash --init-file <(echo \"unset LIBRARY_PATH; unset LD_LIBRARY_PATH;\"; ${guix} package --search-paths -p $2; echo \"PS1=\\\"\\u@\\h \\W [env]\\\\$ \\\"\") -i \"${@:3}\"
    else
      printf \"Usage:\\n  $0 $1 /path/to/profile\\n\"
    fi
  else
    printf \"Usage:\\n  $0 $1 /path/to/profile\\n\"
  fi
else
  ${guix} $@
fi~%"
                         (assoc-ref inputs "guix")
                         (assoc-ref inputs "git")
                         (assoc-ref inputs "socat")
                         (assoc-ref inputs "bash"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p out)
               (install-file "guixr" out)
               (chmod (string-append out "/guixr") #o555)
               ;; It seems that Dutch people expect a 'q' before a 'u', which
               ;; leads to a typo of the 'guixr' command.  For the maximum
               ;; convenience level for users, we provide an alias.
               (system* "ln" "--symbolic"
                        (string-append out "/guixr")
                        (string-append out "/quixr"))))))))
    (home-page #f)
    (synopsis "GNU Guix remote")
    (description "This wrapper around GNU Guix enables remote guix-daemon
communication.  It can be used for cluster deployments with a single build
node.  This script was originally developed by Ricardo Wurmus.  This version
has been slightly modified to work on the UMC Utrecht cluster set-up.")
    (license #f)))

(define-public iotop-logger
  (package
    (name "iotop-logger")
    (version "1.0")
    (source #f)
    (build-system gnu-build-system)
    (propagated-inputs
     `(("iotop" ,iotop)
       ("bash" ,bash)
       ("grep" ,grep)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (with-output-to-file "iotop-logger"
               (lambda _
                 (format #t "#!/bin/bash~%")
                 (format #t "~a/sbin/iotop -Pbot --delay=1 | ~a/bin/egrep -v \"DISK READ|DISK WRITE\"~%"
                         (assoc-ref inputs "iotop")
                         (assoc-ref inputs "grep"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p out)
               (install-file "iotop-logger" out)
               (chmod (string-append out "/iotop-logger") #o555)))))))
    (home-page #f)
    (synopsis "Script to gather disk reads/writes information")
    (description "This script is a wrapper around iotop, to gather reads and
writes to the disk for various processes.")
    (license #f)))

(define-public collectl
  (package
   (name "collectl")
   (version "4.1.2")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/collectl/collectl/collectl-" version
                   "/collectl-" version ".src.tar.gz"))
             (sha256
              (base32
               "0cf44kwazxfh98yshyi1jcw34lzfy5ahqxpgqqsxa6ps7zlm89lp"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'build) ; There's nothing to build.
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "INSTALL"
              (("DESTDIR:=\"/\"") (format #f "DESTDIR:=~s"
                                          (assoc-ref outputs "out")))
              (("DESTDIR/usr") "DESTDIR"))))
        (replace 'install
                 (lambda _
                   (system* "./INSTALL"))))))
   (inputs
    `(("perl" ,perl)))
   (home-page "http://collectl.sourceforge.net")
   (synopsis "Performance data collector")
   (description "This package provides a program that collects various
performance measurement data like CPU, memory, disk and network performance
numbers.")
   (license license:artistic2.0)))

;; XXX: The web interface does not work because the static files are not
;; distributed in the output.  This needs additional code changes to GWL
;; to work.
(define-public gwl
  (package
    (name "gwl")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://git.roelj.com/guix/gwl/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "135nlph77i24n9psficil56ih1jqsrwfqkdaxjf71dvijxlj10k8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("guix" ,guix)
       ("guile" ,guile-2.0)))
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _ (zero? (system* "autoreconf" "-vif")))))))
    (home-page "https://gwl.roelj.com")
    (synopsis "Guix workflow extension")
    (description "This package provides a workflow management extension for
GNU Guix.  It can be used to build pipelines and execute them locally, or on
a computing cluster.  GWL currently only supports Sun Grid Engine.")
    (license license:agpl3+)))

(define-public vmtouch
  (package
   (name "vmtouch")
   (version "1.3.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/hoytech/vmtouch/archive/v"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "1src2byjwgjsbq3sd29r9qgmjwfb1f4c03p5cjqqwk42iw5rh5a6"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:make-flags `("CC=gcc"
                     ,(string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (inputs
    `(("perl" ,perl)))
   (home-page "https://github.com/hoytech/vmtouch")
   (synopsis "Virtual memory toucher")
   (description "vmtouch is a tool for learning about and controlling the file
system cache of unix and unix-like systems.")
   (license license:bsd-3)))
