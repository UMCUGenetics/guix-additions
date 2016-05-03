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

(define-module (umcu packages bash-tap)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages))

(define-public bash-tap
  (package
    (name "bash-tap")
    (version "1.0.2")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/illusori/bash-tap/archive/"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "0qs1qi38bl3ns4mpagcawv618dsk2q1lgrbddgvs0wl3ia12cyz5"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)
                     ("tar" ,tar)
                     ("gzip" ,gzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
                         (path (string-append (assoc-ref %build-inputs "gzip") "/bin"))
                         (bin (string-append %output "/bin"))
                         (source (string-append (assoc-ref %build-inputs "source"))))
                     (setenv "PATH" path)
                     (mkdir-p bin)
                     (with-directory-excursion bin
                       (zero? (system* tar "xvf" source
                                       "--strip-components=1"
                                       "--no-anchored"
                                       "bash-tap"
                                       "bash-tap-bootstrap"
                                       "bash-tap-mock")))))))
    (home-page "http://www.illusori.co.uk/projects/bash-tap/")
    (synopsis "Bash port of a Test::More/Test::Builder-style TAP-compliant
test library")
    (description "Bash TAP is a TAP-compliant Test::More-style testing library
for Bash shell scripts and functions.  Along with the Test::More-style testing
helpers it provides helper functions for mocking commands and functions and
in-process output capturing.")
    ;; The author didn't specify a license.
    (license license:public-domain)))
