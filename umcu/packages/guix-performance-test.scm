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

(define-module (umcu packages guix-performance-test)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux))

(define-public guix-performance-test
  (package
    (name "guix-performance-test")
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
