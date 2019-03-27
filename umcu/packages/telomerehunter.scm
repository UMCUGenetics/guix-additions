;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages telomerehunter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages python-xyz))

(define-public python-telomerehunter
(package
  (name "telomerehunter")
  (version "1.1.0")
  (source (origin
  (method url-fetch)
  (uri "https://files.pythonhosted.org/packages/e5/67/ce6ac292a88a078a733dc3d9adb3f153834692effbf0851b93a6f3e49b7a/telomerehunter-1.1.0-py2-none-any.whl")
   (sha256
    (base32
     "1055z4hs2hhsfwqnjm0kffkhh6ag041mp6l13i2gs5454xk02nwi"))))
  (build-system python-build-system)
  (arguments
   `(#:python ,python-2
     #:tests? #f
     #:phases
     (modify-phases %standard-phases
       (replace 'unpack
         (lambda* (#:key inputs #:allow-other-keys)
           (let ((unzip (string-append (assoc-ref inputs "unzip")
                                       "/bin/unzip")))
             (system* unzip (assoc-ref %build-inputs "source")))))
       (replace 'build
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let* ((python-libdir (string-append (assoc-ref outputs "out") 
                                                "/lib/python2.7/site-packages"))
                  (site-dir (string-append python-libdir "/telomerehunter"))
                  (bindir (string-append (assoc-ref outputs "out") "/bin")))
         (substitute* "telomerehunter/run_plot.sh"
           (("R --no-save") (string-append (assoc-ref inputs "r") "/bin/R --no-save")))
         (substitute* (list "telomerehunter/filter_telomere_reads.py"
                            "telomerehunter/normalize_TVR_counts.R")
           (("samtools ") (string-append (assoc-ref inputs "samtools") "/bin/samtools ")))
         (mkdir-p python-libdir)
         (copy-recursively "telomerehunter" site-dir)
         (mkdir-p bindir)
         (mkdir-p (string-append (assoc-ref outputs "out") "/site-library"))
         (install-file "telomerehunter-1.1.0.data/scripts/telomerehunter" bindir)
         (wrap-program (string-append bindir "/telomerehunter")
          `("PYTHONPATH" ":" prefix (,bindir ,(getenv "PYTHONPATH")
                                             ,site-dir))))))
       (delete 'install))))
  (inputs
   `(("unzip" ,unzip)
     ("samtools" ,samtools)))
  (propagated-inputs
    `(("r" ,r)
     ("python2-pypdf2", python2-pypdf2)
     ("python2-numpy", python2-numpy)
     ("python2-pysam", python2-pysam)
     ("r-ggplot2", r-ggplot2)
     ("r-reshape2", r-reshape2)
     ("r-gridextra", r-gridextra)
     ("r-rcolorbrewer" ,r-rcolorbrewer)
     ("r-cowplot", r-cowplot)
     ("r-svglite", r-svglite)
     ("r-dplyr", r-dplyr)))
  (native-search-paths
    (list (search-path-specification
      (variable "R_LIBS_SITE")
        (files (list "site-library/")))))
  (home-page "http://pypi.python.org/pypi/telomerehunter/")
  (synopsis "Estimation of Telomere Content from WGS Data")
  (description "TelomereHunter extracts, sorts and analyses telomeric reads
                from WGS Data. It is designed to take BAM files from a tumor and/or a control
                sample as input. The tool was developed at the German Cancer Research Center (DKFZ).")
  (license license:gpl3+)))
