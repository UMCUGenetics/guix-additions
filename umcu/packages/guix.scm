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
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages version-control))

(define-public guixr
  (package
    (name "guixr")
    (version "1.3.1")
    (source #f)
    (build-system gnu-build-system)
    (inputs
     `(("bash" ,bash)
       ("guix" ,guix)
       ("git" ,git)
       ("gawk" ,gawk)
       ("gwl" ,gwl)))
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
coreutils=\"~a\"
gawk=\"${coreutils}/bin/gawk\"
readlink=\"${coreutils}/bin/readlink\"
cut=\"~a/bin/cut\"
grep=\"~a/bin/grep\"
GWL_PATH=\"~a\"

# Avoid locale warnings.
export GUIX_LOCPATH=\"${guix_profile}/lib/locale\"

# Use /gnu as state directory.
export NIX_STATE_DIR=$guix_root

# Ensure the latest Guix packages are used.  Do not override
# the user's customizations (if any).
if [ ! -L $HOME/.config/guix/latest ]; then
  mkdir -p $HOME/.config/guix
  ln -s /gnu/repositories/guix $HOME/.config/guix/latest
# Renew the link as repository updates are managed centrally.
# This will avoid the warning of an outdated version of Guix.
elif [ \"$(${readlink} -f $HOME/.config/guix/latest)\" = \"/gnu/repositories/guix\" ]; then
  rm -f $HOME/.config/guix/latest
  ln -s /gnu/repositories/guix $HOME/.config/guix/latest
fi

# Include our non-standard package repository
export GUIX_PACKAGE_PATH=\"$guix_additional${GUIX_PACKAGE_PATH:+:$GUIX_PACKAGE_PATH}\"

# Set the Guile environment for GWL
export GUILE_LOAD_PATH=\"${GWL_PATH}/share/guile/site/2.2${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}\"
export GUILE_LOAD_COMPILED_PATH=\"${GWL_PATH}/lib/guile/2.2/ccache${GUILE_LOAD_COMPILED_PATH:+:$GUILE_LOAD_COMPILED_PATH}\"

# Set the X.509 certificates
export SSL_CERT_DIR=\"${guix_profile}/etc/ssl/certs\"
export SSL_CERT_FILE=\"${SSL_CERT_DIR}/ca-certificates.crt\"

# Use guix with the given arguments
export GUIX_DAEMON_SOCKET=guix://10.100.7.235:9999
if [ $# -lt 1 ]; then
  ${guix}
elif [ \"$1\" == \"package\" ] && [ $# -ge 2 ] && ([ \"$2\" == \"--install\" ] || [ \"$2\" == \"--upgrade\" ] ||
         [ \"$2\" == \"-i\" ] || [ \"$2\" == \"-u\" ]); then
  ${guix} $@
  echo \"\"
  echo \"The following repositories and versions were used:\";
  echo -n \" * GNU Guix upstream:         \";
  ${git} -C /gnu/repositories/guix rev-parse HEAD;
  echo -n \" * UMCU additional packages:  \";
  ${git} -C /gnu/repositories/guix-additions rev-parse HEAD;
elif [ \"$1\" == \"pull\" ]; then
  echo \"This feature has been disabled.\";
elif [ \"$1\" == \"gc\" ]; then
  echo \"This feature has been disabled.\";
elif [ \"$1\" == \"package\" ]; then
  echo \"\";
  echo \"Sorry for the inconvenience.  Guix is in maintenance mode.\";
  echo \"We are currently working on a major performance improvement\";
  echo \"for which various tests have to be conducted on the running\";
  echo \"system.\";
  echo \"\";
  echo \"The 'guixr load-profile' command is still available, as well\";
  echo \"as all previously installed programs.  Installing new programs\";
  echo \"has been disabled in the maintenance mode.\";
  echo \"\";
elif [ \"$1\" == \"environment\" ]; then
  echo \"\";
  echo \"Sorry for the inconvenience.  Guix is in maintenance mode.\";
  echo \"We are currently working on a major performance improvement\";
  echo \"for which various tests have to be conducted on the running\";
  echo \"system.\";
  echo \"\";
  echo \"The 'guixr load-profile' command is still available, as well\";
  echo \"as all previously installed programs.  Installing new programs\";
  echo \"has been disabled in the maintenance mode.\";
  echo \"\";
elif [ \"$1\" == \"load-profile\" ]; then
  if [ $# -gt 1 ]; then
    if [ \"$2\" != \"--help\" ] && [ \"$2\" != \"-h\" ]; then
      arguments=(\"$@\")
      profile_arguments=(\"${arguments[@]:1}\")
      profile_arguments=(\"${profile_arguments[@]/--}\")
      profiles=${profile_arguments[@]/%/\"/etc/profile\"}
      unset_output=$(${grep} -h \"^export\" $profiles | ${cut} -d '=' -f1 | ${gawk} '{ print \"unset \" $2 }')
      set_output=$(${grep} -h \"^export\" $profiles)
      ~a/bin/bash --init-file <(echo \"unset LIBRARY_PATH; unset LD_LIBRARY_PATH;\"; echo \"$unset_output\"; echo \"$set_output\"; echo \"PS1=\\\"\\u@\\h \\W [env]\\\\$ \\\"\") -i \"${@:$(($# + 1))}\"
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
                         (assoc-ref inputs "gawk")
                         (assoc-ref inputs "coreutils")
                         (assoc-ref inputs "grep")
                         (assoc-ref inputs "gwl")
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

(define-public gwl
  (let ((commit "58f7111627d76bb0b0931e1d58f7ec2e9ba5a718"))
    (package
     (name "gwl")
     (version (string-append "0.0.9-" (string-take commit 8)))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://git.roelj.com/guix/gwl.git")
                     (commit commit)))
               (file-name (string-append name "-" version "-checkout"))
               (sha256
                (base32
                 "1kmxg1rc5275nq55z2jvpa22m6p1nyih4g0h31rbafbrgdzjxlgr"))))
     (build-system gnu-build-system)
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
     (inputs
      `(("guix" ,guix)
        ("guile" ,guile-2.2)))
     (arguments
      `(#:tests? #f ; There are no tests.
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'autoconf
            (lambda _ (zero? (system* "autoreconf" "-vif"))))
          (add-after 'unpack 'change-static-path
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((dist-dir (string-append (assoc-ref outputs "out")
                                             "/share/gwl")))
                ;; Create the directory for static data.
                (mkdir-p dist-dir)
                (copy-recursively "static" dist-dir)

                ;; Update the code's static-root variable.
                (substitute* "www/config.scm"
                             (("\\(define %www-static-root \\(string-append %www-root \\\"/static\\\"\\)\\)")
                              (format #f "(define %www-static-root ~s)" dist-dir)))
                #t)))
          (add-before 'build 'silence-guile
            (lambda _
              (setenv "GUILE_WARN_DEPRECATED" "no")
              (setenv "GUILE_AUTO_COMPILE" "0")
              #t)))))
     (home-page "https://gwl.roelj.com")
     (synopsis "Guix workflow extension")
     (description "This package provides a workflow management extension for
GNU Guix.  It can be used to build pipelines and execute them locally, or on
a computing cluster.  GWL currently only supports Sun Grid Engine.")
     (license license:agpl3+))))

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
