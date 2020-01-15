;;; GNU Guix --- Functional package management for GNU
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

(define-module (umcu packages icgc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages java))

(define-public score-client
  (package
   (name "score-client")
   (version "3.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://artifacts.oicr.on.ca/artifactory/dcc-release/bio/"
                  "overture/score-client/" version "/score-client-" version
                  "-dist.tar.gz"))
            (sha256
             (base32 "0hsn7gm8znv5rq8p62d1md8cyh5n5kgiqmvvacq7kdlvnqydha9d"))))
   ;; We use the GNU build system mainly for its patch-shebang phases.
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is just copying a binary, so no tests to perform.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; No configuration, just copying.
        (delete 'build)     ; No building, just copying.
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (etc (string-append out "/etc/score-client"))
                   (bin (string-append out "/bin"))
                   (lib (string-append out "/lib")))

              (for-each mkdir-p (list out etc bin lib))

              (substitute* "bin/score-client"
               (("`dirname \\$0`/..") out)
               (("\\$\\(cd \\$\\{BASE_DIR\\} && pwd -P\\)") out)
               (("exec java") (string-append
                               "exec " (assoc-ref inputs "openjdk")
                               "/bin/java"))
               (("-Dlogging.path=\\$\\{BASE_DIR\\}/logs")
                "-Dlogging.path=${HOME}")
               (("type -p java")
                (string-append "type -p "
                               (assoc-ref inputs "openjdk")
                               "/bin/java"))
               (("_java=java")
                (string-append "_java="
                               (assoc-ref inputs "openjdk")
                               "/bin/java"))
               (("\\$\\{CLIENT_DIR\\}/conf") etc))

              (copy-recursively "bin" bin)
              (copy-recursively "conf" etc)
              (copy-recursively "lib" lib)))))))
   (inputs
    `(("openjdk" ,openjdk11)))
   (home-page "https://docs.icgc.org/software/download/#score-client")
   (synopsis "Tool to view ICGC data")
   (description "This package provides a tool to download or view data in
the cloud environments of ICGC.")
   (license license:gpl3)))
