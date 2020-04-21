;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages guile)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages tex)
  #:use-module (umcu packages guix))

(define-public hmf-glue
  (package
   (name "hmf-glue")
   (version "0.0.9")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/UMCUGenetics/hmf-glue/releases/download/"
                  version "/hmf-glue-" version ".tar.gz"))
            (sha256
             (base32
              "1hahxx9z30hs3nrj45yvc67izin84a0pk642x61hbp78ancqg76i"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-executable
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out            (assoc-ref outputs "out"))
                   (load-path      (string-append out "/share/guile/site/2.2:"
                                                  (getenv "GUILE_LOAD_PATH")))
                   (compiled-path  (string-append out "/lib/guile/2.2/site-ccache:"
                                                  (getenv "GUILE_LOAD_COMPILED_PATH"))))
              (substitute* (string-append out "/bin/hmf-reset-database")
               (("\\$\\{prefix\\}")  out))
              (for-each (lambda (program)
                          (wrap-program program
                            ;; Add $PATH so that optional components like job
                            ;; scheduler programs can be found at run-time.
                            `("PATH" ":" = (,(getenv "PATH") "$PATH"))
                            `("GUILE_LOAD_PATH" ":" = (,load-path))
                            `("GUILE_LOAD_COMPILED_PATH" ":" = (,compiled-path))))
                        (find-files (string-append out "/bin"))))
            #t)))))
   (inputs
    `(("guile" ,guile-2.2)
      ("guile-json" ,guile-json-1)
      ("gwl" ,gwl)
      ("guix" ,guix)))
   (home-page "https://github.com/UMCUGenetics/hmf-glue")
   (synopsis " Tools for extracting information using HMFtools")
   (description "This repository contains tools for extracting
information out of HMF pipeline output using HMFtools and importing the
information into a MySQL database. ")
   (license license:gpl3+)))

(define-public graph-cnv-analysis
  (package
   (name "graph-cnv-analysis")
   (version "0.0.4")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/UMCUGenetics/" name "/releases/download/"
                  version "/" name "-" version ".tar.gz"))
            (sha256
             (base32 "1iflni4gq9qvghwmzwz8cj3nvx05gvkf4rrv08i8ihfx3jkk9gqq"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-executable
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out            (assoc-ref outputs "out"))
                  (load-path      (string-append
                                   (assoc-ref outputs "out") "/share/guile/site/2.2:"
                                   (getenv "GUILE_LOAD_PATH")))
                  (compiled-path  (string-append
                                   (assoc-ref outputs "out") "/lib/guile/2.2/site-ccache:"
                                   (getenv "GUILE_LOAD_COMPILED_PATH"))))
              (wrap-program (string-append out "/bin/plot-cnv-regions")
               `("PATH" ":" = (,(getenv "PATH")))
               `("GUILE_LOAD_PATH" ":" = (,load-path))
               `("GUILE_LOAD_COMPILED_PATH" ":" = (,compiled-path))))
            #t)))))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("guile" ,guile-2.2)
      ("guile-sparql" ,guile-sparql)))
   (home-page "https://github.com/UMCUGenetics/graph-cnv-analysis")
   (synopsis "")
   (description "")
   (license license:gpl3+)))

(define-public daiane-analysis
  (package (inherit graph-cnv-analysis)
   (name "daiane-analysis")
   (version "0.0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/UMCUGenetics/" name "/releases/download/"
                  version "/" name "-" version ".tar.gz"))
            (sha256
             (base32 "1iv6lz4x9wj0dpyda2y4sp5rhz7kmfva9kh47n3w2x71ka0cxvmx"))))
   (arguments `(#:tests? #f)) ; There are no tests.
   (home-page "https://github.com/UMCUGenetics/daiane-analysis")))

(define-public graphdb-hpc-tools
  (package
    (name "graphdb-hpc-tools")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/UMCUGenetics/graphdb-hpc-tools/"
                    "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18rhjpmvqvjpliagh6r7v7vvyp67x8975ww8npq7myy391k57g60"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autoreconf
           (lambda _
             (system "autoreconf -vif")))
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (system "ls -lh bin/")
               (install-file "bin/run-database" bin))))
         (add-after 'install 'wrap-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out            (assoc-ref outputs "out"))
                   (load-path      (getenv "GUILE_LOAD_PATH"))
                   (compiled-path  (getenv "GUILE_LOAD_COMPILED_PATH")))
               (wrap-program (string-append out "/bin/run-database")
                 `("GUILE_LOAD_PATH" ":" = (,load-path))
                 `("GUILE_LOAD_COMPILED_PATH" ":" = (,compiled-path))))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("guile" ,guile-2.2)))
    (propagated-inputs
     `(("gwl" ,gwl)
       ("guix" ,guix)))
    (home-page #f)
    (synopsis "Tools for managing graph databases on Utrecht's HPC")
    (description "This package provides management tools for running graph
database instances on Utrecht's HPC.")
(license license:gpl3+)))

(define-public shallowcp
  (package
   (name "shallowcp")
   (version "0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/roelj/shallowcp/releases/download/"
                  version "/shallowcp-" version ".tar.gz"))
            (sha256
             (base32
              "0mivgpc1bnyl1jg5rajg569s4zpjzz8a2y4afmb4szli94444w1r"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        ;; There's no compiler involved.
        (delete 'build))))
   (inputs
    `(("guile" ,guile-2.2)
      ("pkg-config" ,pkg-config)))
   (home-page "https://github.com/roelj/shallowcp")
   (synopsis "Tool for making shallow copies of a directory")
   (description "This package provides @code{shallowcp}, which can create
shallow copies of a directory (symlinks to files instead of a full copy).")
   (license license:gpl3+)))
