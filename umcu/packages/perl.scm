;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system perl)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check))

(define-public perl-pegex
  (package
   (name "perl-pegex")
   (version "0.64")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/I/IN/INGY/Pegex-"
           version ".tar.gz"))
     (sha256
      (base32
       "1kb7y2cc3nibbn8i8y3vrzz1f9h3892nbf8jj88c5fdgpmj05q17"))))
   (build-system perl-build-system)
   (native-inputs
    `(("perl-file-sharedir-install" ,perl-file-sharedir-install)
      ("perl-yaml-libyaml" ,perl-yaml-libyaml)))
   (home-page "http://search.cpan.org/dist/Pegex/")
   (synopsis "Acmeist PEG Parser Framework")
   (description "Pegex is an Acmeist parser framework.  It allows you to easily
create parsers that will work equivalently in lots of programming languages!
The inspiration for Pegex comes from the parsing engine upon which the
postmodern programming language Perl 6 is based on.  Pegex brings this beauty
to the other justmodern languages that have a normal regular expression engine
available.")
   (license (package-license perl))))

(define-public perl-inline
  (package
   (name "perl-inline")
   (version "0.80")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/I/IN/INGY/Inline-"
           version ".tar.gz"))
     (sha256
      (base32
       "1xnf5hykcr54271x5jsnr61bcv1c7x39cy4kdcrkxm7bn62djavy"))))
   (build-system perl-build-system)
   (native-inputs
    `(("perl-test-warn" ,perl-test-warn)))
   (home-page "http://search.cpan.org/dist/Inline/")
   (synopsis "Write Perl subroutines in other programming languages")
   (description "The @code{Inline} module allows you to put source code
from other programming languages directly (inline) in a Perl script or
module.  The code is automatically compiled as needed, and then loaded
for immediate access from Perl.")
   (license (package-license perl))))

(define-public perl-inline-c
  (package
  (name "perl-inline-c")
  (version "0.78")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/T/TI/TINITA/Inline-C-"
             version ".tar.gz"))
      (sha256
        (base32
          "1izv7vswd17glffh8h83bi63gdk208mmhxi17l3qd8q1bkc08y4s"))))
  (build-system perl-build-system)
  (native-inputs
    `(("perl-file-copy-recursive" ,perl-file-copy-recursive)
      ("perl-file-sharedir-install" ,perl-file-sharedir-install)
      ("perl-test-warn" ,perl-test-warn)
      ("perl-yaml-libyaml" ,perl-yaml-libyaml)))
  (propagated-inputs
    `(("perl-inline" ,perl-inline)
      ("perl-parse-recdescent" ,perl-parse-recdescent)
      ("perl-pegex" ,perl-pegex)))
  (home-page
    "http://search.cpan.org/dist/Inline-C/")
  (synopsis "C Language Support for Inline")
  (description "The @code{Inline::C} module allows you to write Perl
subroutines in C. Since version 0.30 the @code{Inline} module supports multiple
programming languages and each language has its own support module.  This
document describes how to use Inline with the C programming language.  It also
goes a bit into Perl C internals.")
  (license (package-license perl))))
