;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mark van Roosmalen <m.j.vanroosmalen-3@prinsesmaximacentrum.nl>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
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

;;;
;;; WARNING: This is work in progress.
;;;

(define-module (umcu packages cgp-cavemanwrapper)
  #:use-module (umcu packages cgp-battenberg)
  #:use-module (umcu packages caveman)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages web)
  #:use-module (umcu packages samtools)
  #:use-module (umcu packages vcftools)
  #:use-module (umcu packages perl)
  #:use-module (srfi srfi-1))

(define-public cgp-cavemanwrapper-1.15.2
  (package
    (name "cgp-cavemanwrapper")
    (version "1.15.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cancerit/cgpCaVEManWrapper/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (list (search-patch "cgp-cavemanwrapper-fix-script.patch")))
              (sha256
               (base32
                "017b03j1sm64v6barbxx69420d3s1gx5nlcqg7rzn6mczj47g264"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'install)
         ;; The Perl in Guix does not support threads.
         ;; The forks module is a drop-in replacement for it, so it
         ;; is easier to use that instead of recompiling Perl.
         (add-after 'unpack 'enable-threads
           (lambda _
             (substitute* "bin/caveman_merge_results.pl"
               (("use strict;") "use forks;\nuse strict;"))))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin-dir (string-append (assoc-ref outputs "out") "/bin"))
                   (lib-dir (string-append (assoc-ref outputs "out")
                            "/lib/perl5/site_perl/5.28.0")))
               (mkdir-p bin-dir)
               (mkdir-p lib-dir)
               (install-file "bin/caveman_merge_results.pl" bin-dir)
               (copy-recursively "lib/Sanger" lib-dir)
               #t))))))
    (propagated-inputs
     `(("perl-file-path" ,perl-file-path)
       ("perl-file-which", perl-file-which)
       ("perl-const-fast", perl-const-fast)
       ("perl-capture-tiny", perl-capture-tiny)
       ("perl-ipc-system-simple", perl-ipc-system-simple)
       ("perl-autodie", perl-autodie)
       ("perl-try-tiny", perl-try-tiny)
       ("perl-carp", perl-carp)
       ("perl-forks", perl-forks)
       ("pcap-core" ,pcap-core)
       ("perl" ,perl)))
    (home-page "https://github.com/cancerit/cgpCaVEManWrapper")
    (synopsis "Reference implementation of CGP workflow for CaVEMan")
    (description "This package provides the reference implementation of CGP
workflow for CaVEMan SNV analysis.")
    (license license:agpl3+)))
