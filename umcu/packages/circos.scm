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
  #:use-module (gnu packages))

(define-public perl-font-ttf
  (package
    (name "perl-font-ttf")
    (version "1.06")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/B/BH/BHALLISSY/Font-TTF-"
                    version ".tar.gz"))
              (sha256
               (base32
                "14y29ja3lsa3yw0ll20lj96f3zz5zydjqi1c5nh9wxar8927ssab"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-io-string" ,perl-io-string)))
    (home-page "http://search.cpan.org/dist/Font-TTF")
    (synopsis "TTF font support for Perl")
    (description "")
    (license license:artistic2.0)))

(define-public perl-memoize
  (package
    (name "perl-memoize")
    (version "1.03")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MJ/MJD/Memoize-"
                    version".tgz"))
              (sha256
               (base32
                "1wysq3wrmf1s7s3phimzn7n0dswik7x53apykzgb0l2acigwqfaj"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Memoize")
    (synopsis "Make functions faster by trading space for time")
    (description "")
    (license #f)))

(define-public perl-math-round
  (package
    (name "perl-math-round")
    (version "0.07")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/G/GR/GROMMEL/Math-Round-"
                    version ".tar.gz"))
              (sha256
               (base32
                "09wkvqj4hfq9y0fimri967rmhnq90dc2wf20lhlmqjp5hsd359vk"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Math-Round")
    (synopsis "Perl extension for rounding numbers")
    (description "")
    (license #f)))

(define-public perl-math-vecstat
  (package
    (name "perl-math-vecstat")
    (version "0.08")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/A/AS/ASPINELLI/Math-VecStat-"
                    version ".tar.gz"))
              (sha256
               (base32
                "03bdcl9pn2bc9b50c50nhnr7m9wafylnb3v21zlch98h9c78x6j0"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Math-VecStat")
    (synopsis "Some basic numeric stats on vectors")
    (description "")
    (license #f)))

(define-public perl-number-format
  (package
    (name "perl-number-format")
    (version "1.75")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/W/WR/WRW/Number-Format-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1wspw9fybik76jq9w1n1gmvfixd4wvlrq6ni8kyn85s62v5mkml2"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Number-Format")
    (synopsis "Perl extension for formatting numbers")
    (description "")
    (license (package-license perl))))

(define-public perl-statistics-basic
  (package
    (name "perl-statistics-basic")
    (version "1.6611")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JE/JETTERO/Statistics-Basic-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1ywl398z42hz9w1k0waf1caa6agz8jzsjlf4rzs1lgpx2mbcwmb8"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-number-format" ,perl-number-format)))
    (home-page "http://search.cpan.org/dist/Statistics-Basic")
    (synopsis #f)
    (description "")
    (license #f)))

(define-public perl-text-format
  (package
    (name "perl-text-format")
    (version "0.60")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SH/SHLOMIF/Text-Format-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1f52jak0a2gwi4qcisp4nfbniq04dmmv5j8zkvzj8ik0f0sk2kv6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "http://search.cpan.org/dist/Text-Format")
    (synopsis "Format text")
    (description "")
    (license (package-license perl))))

(define-public perl-time-hires
  (package
    (name "perl-time-hires")
    (version "1.9746")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JH/JHI/Time-HiRes-"
                    version ".tar.gz"))
              (sha256
               (base32
                "15pmypmkmh1xrig8cqm4izrmh4bf3q3m1v4fr44cjyw2pf0qqh49"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Time-HiRes")
    (synopsis "High resolution alarm, sleep, gettimeofday, interval timers")
    (description "")
    (license (package-license perl))))

(define-public perl-carp
  (package
    (name "perl-carp")
    (version "1.38")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RJ/RJBS/Carp-"
                    version ".tar.gz"))
              (sha256
               (base32
                "00bijwwc0ix27h2ma3lvsf3b56biar96bl9dikxgx7cmpcycxad5"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Carp")
    (synopsis "alternative warn and die for modules")
    (description "")
    (license (package-license perl))))

(define-public perl-set-intspan
  (package
    (name "perl-set-intspan")
    (version "1.19")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SW/SWMCD/Set-IntSpan-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1l6znd40ylzvfwl02rlqzvakv602rmvwgm2xd768fpgc2fdm9dqi"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Set-IntSpan")
    (synopsis "Manages sets of integers, newsrc style")
    (description "")
    (license #f)))

(define-public perl-math-bezier
  (package
    (name "perl-math-bezier")
    (version "0.01")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/A/AB/ABW/Math-Bezier-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1f5qwrb7vvf8804myb2pcahyxffqm9zvfal2n6myzw7x8py1ba0i"))))
    (build-system perl-build-system)
    (home-page
     "http://search.cpan.org/dist/Math-Bezier")
    (synopsis "solution of Bezier Curves")
    (description "")
    (license #f)))

(define-public circos
  (package
    (name "circos")
    (version "0.69-3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://circos.ca/distribution/circos-" version ".tgz"))
              (sha256
               (base32 "0cdf9pbp7din531lpqa9asa507jv7jnxshrwvhaqvr08rzilzn93"))))
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
