;;; GNU Guix --- Functional package management for GNU
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

(define-module (umcu packages guix)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking))

(define-public guixr
  (package
    (name "guixr")
    (version "1.0")
    (source #f)
    (build-system gnu-build-system)
    (propagated-inputs
     `(("socat" ,socat)
       ("bash" ,bash)
       ("guix" ,guix)))
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
socketfile=\"${HOME}/.guix-daemon-socket-forwarder-$$\"
remote=\"10.100.7.226:9999\"
guix_root=\"/gnu\"
guix_additional=\"/gnu/repositories/guix-additions\"
guix_pin=\"/gnu/repositories/guix\"
guix_profile=\"/gnu/profiles/base\"
guix=\"~a/bin/guix\"
socat=\"~a/bin/socat\"

# Avoid locale warnings.
export GUIX_LOCPATH=\"${guix_profile}/lib/locale\"

# Use /gnu as state directory.
export NIX_STATE_DIR=$guix_root

# Ensure that the forwarding process is terminated on exit
function cleanup {
    ps -p ${SOCAT_PID} >/dev/null && kill -TERM ${SOCAT_PID}
    rm -f ${socketfile}
}
trap cleanup EXIT

# The socket file may not exist, or else socat will complain.
rm -f ${socketfile}

# Start forwarding
${socat} UNIX-LISTEN:${socketfile} TCP4:${remote} &
SOCAT_PID=$!

# Abort if socat is not running.
ps -p ${SOCAT_PID} >/dev/null || \
    (echo \"Socat failed to start.  Aborting.
\"; exit 1)

# Include our non-standard package repository
export GUIX_PACKAGE_PATH=\"$guix_additional${GUIX_PACKAGE_PATH:+:$GUIX_PACKAGE_PATH}\"

# Use guix with the given arguments
export GUIX_DAEMON_SOCKET=${socketfile}
${guix} $@"
                         (assoc-ref inputs "guix")
                         (assoc-ref inputs "socat"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p out)
               (install-file "guixr" out)
               (chmod (string-append out "/guixr") #o555)))))))
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
