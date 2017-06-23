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

(define-module (umcu packages fastqc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression))

(define-public fastqc-bin-0.11.4
  (package
    (name "fastqc")
    (version "0.11.4")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/fastqc_v"
            version ".zip"))
      (sha256
       (base32 "1rqz7p9xc8ki97afx15v7yd1pv6z59868rkikvljzc77zbwk7cmd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests for binary release.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No configure phase for binary release.
         (delete 'build) ; No build phase for binary release.
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin"))
                    (create-and-copy
                     (lambda (dir)
                       (mkdir (string-append bin "/" dir))
                       (copy-recursively dir (string-append bin "/" dir)))))
               (install-file "cisd-jhdf5.jar" bin)
               (install-file "jbzip2-0.9.jar" bin)
               (install-file "sam-1.103.jar" bin)
               (map create-and-copy '("net" "org" "uk" "Templates" "Help"
                                      "Configuration"))
               (install-file "fastqc" bin)
               ;; Make the script executable.
               (chmod (string-append bin "/fastqc") #o555)))))))
    (propagated-inputs
     `(("perl" ,perl) ; Used for a runner script for the Java program.
       ("jdk" ,icedtea-7)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/")
    (synopsis "A quality control tool for high throughput sequence data")
    (description
     "FastQC aims to provide a QC report which can spot problems which originate
either in the sequencer or in the starting library material.  It can either run
as a stand alone interactive application for the immediate analysis of small
numbers of FastQ files, or it can be run in a non-interactive mode where it
would be suitable for integrating into a larger analysis pipeline for the
systematic processing of large numbers of files.")
    ;; FastQC is licensed GPLv3+, but one of its dependencies (JHDF5) is
    ;; licensed ASL2.0.
    (license (list license:gpl3+ license:asl2.0))))
