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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
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
     `(("coreutils" ,coreutils)
       ("tar" ,tar)
       ("xz" ,xz)))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver for RTX 2080 Ti cards")
    (description "This package contains the proprietary NVIDIA driver
for RTX 2080 Ti cards.")
    ;; It's proprietary, so only poke at it with a loooong stick. :)
    (license #f)))

(define-public cuda-toolkit
  (package
   (name "cuda-toolkit")
   (version "10.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://developer.download.nvidia.com/compute/cuda/"
                  version "/Prod/local_installers/cuda_" version
                  ".89_440.33.01_linux.run"))
            (sha256
              (base32
               "04fasl9sjkb1jvchvqgaqxprnprcz7a8r52249zp2ijarzyhf3an"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (let* ((installer (assoc-ref %build-inputs "source"))
             (out       (assoc-ref %outputs "out"))
             (fake-out  (string-append (getcwd) "/fake-out"))
             (bin       (string-append out "/bin"))
             (lib       (string-append out "/lib"))
             (include   (string-append out "/include"))
             (share     (string-append out "/share"))
             (tmpdir    (string-append (getcwd) "/tmp"))
             (core-util (lambda (util)
                          (string-append
                           (assoc-ref %build-inputs "coreutils") "/bin/" util)))
             (bash   (string-append (assoc-ref %build-inputs "bash") "/bin/sh"))
             (mkdir  (core-util "mkdir")))

        (use-modules (guix build utils))
        (setenv "PATH" (string-append (core-util "") ":"
                                      (assoc-ref %build-inputs "tar") "/bin:"
                                      (assoc-ref %build-inputs "xz") "/bin:"
                                      (assoc-ref %build-inputs "gzip") "/bin"))
        (setenv "TMPDIR" tmpdir)
        (system* mkdir "-p" out bin lib include share tmpdir fake-out)
        ;; Run the installer to extract the files.  It will fail
        ;; to do anything else because it cannot find "cuda-installer",
        ;; which ends up in the /tmp directory while the script expects
        ;; it in the current working directory.
        (system (string-append bash " " installer
                               " --installpath=" fake-out
                               " --override"
                               " --driver"
                               " --keep"
                               " --toolkit"
                               " --target " tmpdir))

        ;; Copy the libraries to the output.
        (and (copy-recursively "tmp/builds/cuda-toolkit/bin" bin)
             (copy-recursively "tmp/builds/cuda-toolkit/targets/x86_64-linux/lib" lib)
             (copy-recursively "tmp/builds/cuda-toolkit/targets/x86_64-linux/include" include)
             (copy-recursively "tmp/builds/cuda-toolkit/share" share)))))
   (native-inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("gzip" ,gzip)
      ("tar" ,tar)
      ("xz" ,xz)))
   (home-page "https://www.nvidia.com")
   (synopsis "Proprietary NVIDIA CUDA toolkit")
   (description "This package contains the proprietary NVIDIA CUDA toolkit.")
   ;; It's proprietary, so only poke at it with a loooong stick. :)
   (license #f)))

(define-public cuda-cudnn
  (package
    (name "cuda-cudnn")
    (version "8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://developer.download.nvidia.com/compute/redist/"
                    "cudnn/v6.0/cudnn-" version "-linux-x64-v6.0.tgz"))
              (sha256
               (base32
                "173zpgrk55ri8if7s5yngsc89ajd6hz4pss4cdxlv6lcyh5122cv"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let* ((tarball   (assoc-ref %build-inputs "source"))
              (out       (assoc-ref %outputs "out"))
              (lib       (string-append out "/lib"))
              (include   (string-append out "/include")))

         (use-modules (guix build utils))
         (setenv "PATH" (string-append (assoc-ref %build-inputs "tar") "/bin:"
                                       (assoc-ref %build-inputs "gzip") "/bin"))
         (system* "tar" "-xvf" tarball)
         (mkdir-p lib)

         ;; Copy the libraries to the output.
         (and (install-file "cuda/lib64/libcudnn.so.6.0.21" lib)
              (install-file "cuda/lib64/libcudnn_static.a"  lib)
              (install-file "cuda/include/cudnn.h"          include))
         (with-directory-excursion lib
           (symlink "libcudnn.so.6.0.21" "libcudnn.so.6")
           (symlink "libcudnn.so.6.0.21" "libcudnn.so")))))
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA CUDA neural network library")
    (description "This package contains the proprietary NVIDIA CUDA neural
network libraries.")
    ;; It's proprietary, so only poke at it with a loooong stick. :)
    (license #f)))
