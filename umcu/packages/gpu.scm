;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
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

(define-module (umcu packages gpu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression))

(define-public nvidia-rtx-2080ti-driver
  (package
    (name "nvidia-rtx-2080ti-driver")
    (version "440.44")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://us.download.nvidia.com/XFree86/Linux-x86_64/"
                   version "/NVIDIA-Linux-x86_64-" version ".run"))
             (sha256
              (base32
               "057wq9p2vl87gy61f079b6d7clw2vhw3kq7rj411brhrnvr7shmd"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let ((installer (assoc-ref %build-inputs "source"))
             (cp        (string-append (assoc-ref %build-inputs "coreutils") "/bin/cp"))
             (libdir    (string-append (assoc-ref %outputs "out") "/lib"))
             (mkdir     (string-append (assoc-ref %build-inputs "coreutils") "/bin/mkdir"))
             (tail      (string-append (assoc-ref %build-inputs "coreutils") "/bin/tail"))
             (tar       (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
             (xz        (string-append (assoc-ref %build-inputs "xz") "/bin/xz")))

         ;; The installer script packs a xz-compressed tarball at the end of itself.
         ;; The $skip parameter defined in the script tells us where to look
         ;; (line 825 from the end).
         (system
          (string-append
           tail " -n +825 " installer " | " xz " -d | " tar " xvf -"))

         ;; We only copy the shared libraries to the output, because we have no
         ;; interest in building a kernel module.
         (system* mkdir "-p" libdir)
         (system (string-append cp " -r lib*.so* " libdir))

         #t)))
    (native-inputs
     `(("bash" ,bash)
       ("coreutils" ,coreutils)
       ("grep" ,grep)
       ("sed" ,sed)
       ("tar" ,tar)
       ("which" ,which)
       ("xz" ,xz)))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver for RTX 2080 Ti cards")
    (description "This package contains the proprietary NVIDIA driver
for RTX 2080 Ti cards.")
    ;; It's proprietary, so only poke at it with a loooong stick. :)
    (license #f)))
