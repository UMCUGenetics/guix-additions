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

(define-module (umcu packages igvtools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages java)
  #:use-module (gnu packages zip))

(define-public igv
  (package
    (name "igv")
    (version "2.3.92")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://data.broadinstitute.org/igv/projects/downloads/IGV_"
             version ".zip"))
       (sha256
        (base32 "0jrdqfkjb8ygb7rsw2ibwbqz6slfq7y3qbv5i0dyzh41drq9qjaf"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("icedtea" ,icedtea-7)))
    (native-inputs
     `(("unzip" ,unzip)))
    (arguments
     `(#:tests? #f  ; No tests available.
       #:phases
       (modify-phases %standard-phases 
         (delete 'configure) ; Nothing to configure.
         (delete 'build) ; This is a binary package only.
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "igv.jar" bin)
               (install-file "igv.sh" bin)
               (install-file "batik-codec__V1.7.jar" bin)
               (install-file "goby-io-igv__V1.0.jar" bin)))))))
   (home-page "http://www.broadinstitute.org/software/igv/")
   (synopsis "Integrative Genomics Viewer")
   (description "The Integrative Genomics Viewer (IGV) is a high-performance
visualization tool for interactive exploration of large, integrated genomic
datasets.  It supports a wide variety of data types, including array-based and
next-generation sequence data, and genomic annotations.")
   ;; No license specified.
   (license license:non-copyleft)))

(define-public igv-3.0-beta
  (package (inherit igv)
    (name "igv")
    (version "3.0-beta")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://data.broadinstitute.org/igv/projects/downloads/"
             "3.0_beta/IGV_3.0_beta.zip"))
       (sha256
        (base32 "1g4wis3h3r9v13pc1kn4s5xlcn3my0ipighn603aja0pdf7dg6hy"))))
    (propagated-inputs
     `(("icedtea" ,icedtea-8)))))
    
(define-public igvtools-bin-2.3.71
  (package
   (name "igvtools")
   (version "2.3.71")
   (source (origin
     (method url-fetch)
     (uri (string-append
           "http://data.broadinstitute.org/igv/projects/downloads/igvtools_"
           version ".zip"))
     (sha256
      (base32 "1z7fx79jfsqm0ry89mchifxxrj7vl1h9f98x6p2r2vcbx8f4zvi8"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("icedtea" ,icedtea-7)))
   (native-inputs
    `(("unzip" ,unzip)))
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/share/java/" ,name)))
               (install-file "igvtools.jar" bin)
               (install-file "igvtools" bin)
               (mkdir (string-append bin "/genomes"))
               (copy-recursively "genomes" (string-append bin "/genomes"))))))))
   (home-page "http://www.broadinstitute.org/software/igv/")
   (synopsis "Integrative Genomics Viewer")
   (description "The Integrative Genomics Viewer (IGV) is a high-performance
visualization tool for interactive exploration of large, integrated genomic
datasets.  It supports a wide variety of data types, including array-based and
next-generation sequence data, and genomic annotations.")
   ;; No license specified.
   (license license:non-copyleft)))

(define-public igvtools-bin-2.3.60
  (package (inherit igvtools-bin-2.3.71)
   (name "igvtools")
   (version "2.3.60")
   (source (origin
     (method url-fetch)
     (uri (string-append
           "http://data.broadinstitute.org/igv/projects/downloads/igvtools_"
           version ".zip"))
      (sha256
        (base32 "11k713nip68j06mzk7zkbsyajwrlprix7j38ybfrxblp666g3jm2")
      )))))
