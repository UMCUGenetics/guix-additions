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

(define-module (umcu packages circos)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages)
  #:use-module (umcu packages perl))

(define-public circos
  (package
    (name "circos")
    (version "0.69-3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://circos.ca/distribution/circos-" version ".tgz"))
              (sha256
               (base32 "0cdf9pbp7din531lpqa9asa507jv7jnxshrwvhaqvr08rzilzn93"))
              (patches (list (search-patch "circos-remove-findbin.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (datapath (string-append out "/share/Circos"))
                    (error (string-append out "/share/Circos/error"))
                    (fonts (string-append out "/share/Circos/fonts"))
                    (data (string-append out "/share/Circos/data"))
                    (tiles (string-append out "/share/Circos/tiles"))
                    (etc (string-append out "/share/Circos/etc"))
                    (lib (string-append out "/lib/perl5/site_perl/"
                                        ,(package-version perl)))
                    (install-directory (lambda (source target)
                                         (mkdir-p target)
                                         (copy-recursively source target))))
               ;; Circos looks into a relative path for its configuration
               ;; files.  We need to provide an absolute path towards the
               ;; corresponding paths in the store.
               (substitute* '("bin/circos" "etc/colors_fonts_patterns.conf"
                              "etc/gddiag.conf" "etc/brewer.conf" "README")
                 (("<<include etc") (string-append "<<include " etc)))
               (substitute* '("etc/colors.conf" "etc/image.black.conf"
                              "etc/patterns.conf" "etc/image.conf")
                 (("<<include ") (string-append "<<include " etc "/")))
               (substitute* '("etc/fonts.conf" "fonts/README.fonts")
                 (("= fonts") (string-append "= " fonts)))
               (substitute* "etc/patterns.conf"
                 (("= tiles") (string-append "= " tiles)))
               (substitute* "lib/Circos/Error.pm"
                 (("error/configuration.missing.txt")
                  (string-append error "/configuration.missing.txt")))
               (substitute* "etc/housekeeping.conf"
                 (("# data_path = /home/martink/circos-tutorials ")
                  (string-append "data_path = " datapath)))
               (substitute* "lib/Circos/Configuration.pm"
                 (("my @possibilities = \\(")
                  (string-append "my @possibilities = ("
                                 "catfile( \"" datapath "\", $arg ), "
                                 "catfile( \"" etc "\", $arg ), "
                                 "catfile( \"" etc "/tracks\", $arg ), ")))
               (for-each install-directory
                         (list "error" "fonts" "data" "tiles" "etc" "lib")
                         (list error fonts data tiles etc lib))
               (install-file "bin/circos" bin)
               #t))))))
    (propagated-inputs
     `(("perl" ,perl)
       ("perl-carp" ,perl-carp)
       ("perl-clone" ,perl-clone)
       ("perl-config-general" ,perl-config-general)
       ("perl-digest-md5" ,perl-digest-md5)
       ("perl-file-temp" ,perl-file-temp)
       ("perl-font-ttf" ,perl-font-ttf)
       ("perl-gd" ,perl-gd)
       ("perl-getopt-long" ,perl-getopt-long)
       ("perl-list-allutils" ,perl-list-allutils)
       ("perl-math-bezier" ,perl-math-bezier)
       ("perl-math-round" ,perl-math-round)
       ("perl-math-vecstat" ,perl-math-vecstat)
       ("perl-memoize" ,perl-memoize)
       ("perl-number-format" ,perl-number-format)
       ("perl-params-validate" ,perl-params-validate)
       ("perl-readonly" ,perl-readonly)
       ("perl-regexp-common" ,perl-regexp-common)
       ("perl-set-intspan" ,perl-set-intspan)
       ("perl-statistics-basic" ,perl-statistics-basic)
       ("perl-svg" ,perl-svg)
       ("perl-text-balanced" ,perl-text-balanced)
       ("perl-text-format" ,perl-text-format)
       ("perl-time-hires" ,perl-time-hires)))
    (home-page "http://circos.ca/")
    (synopsis "Generation of circularly composited renditions")
    (description
     "Circos is a program for the generation of publication-quality, circularly
composited renditions of genomic data and related annotations.")
    (license #f)))
