;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
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

;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.

(define-module (umcu packages star)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (umcu packages circos))

(define-public star-2.4.2a
  (package
    (name "star")
    (version "2.4.2a")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alexdobin/STAR/archive/"
                                  "STAR_" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c3rnm7r5l0kl3d04gl1g7938xqf1c2l0mla87rlplqg1hcns5mc"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "source/Makefile"
                    (("/bin/rm") "rm"))
                  ;; Remove pre-built binaries and bundled htslib sources.
                  (delete-file-recursively "bin/MacOSX_x86_64")
                  (delete-file-recursively "bin/Linux_x86_64")
                  (delete-file-recursively "source/htslib")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no check target
       #:make-flags '("STAR")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source-dir
           (lambda _ (chdir "source") #t))
         (add-after 'enter-source-dir 'do-not-use-bundled-htslib
           (lambda _
             (substitute* "Makefile"
               (("(Depend.list: \\$\\(SOURCES\\) parametersDefault\\.xxd) htslib"
                 _ prefix) prefix))
             (substitute* '("BAMfunctions.cpp"
                            "signalFromBAM.h"
                            ;"bam_cat.h"
                            "bam_cat.c"
                            "STAR.cpp"
                            "bamRemoveDuplicates.cpp")
               (("#include \"htslib/([^\"]+\\.h)\"" _ header)
                (string-append "#include <" header ">")))
             (substitute* "IncludeDefine.h"
               (("\"htslib/(htslib/[^\"]+.h)\"" _ header)
                (string-append "<" header ">")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "STAR" bin))
             #t))
         (delete 'configure))))
    (native-inputs
     `(("vim" ,vim))) ; for xxd
    (inputs
     `(("htslib" ,htslib)
       ("zlib" ,zlib)))
    (home-page "https://github.com/alexdobin/STAR")
    (synopsis "Universal RNA-seq aligner")
    (description
     "The Spliced Transcripts Alignment to a Reference (STAR) software is
based on a previously undescribed RNA-seq alignment algorithm that uses
sequential maximum mappable seed search in uncompressed suffix arrays followed
by seed clustering and stitching procedure.  In addition to unbiased de novo
detection of canonical junctions, STAR can discover non-canonical splices and
chimeric (fusion) transcripts, and is also capable of mapping full-length RNA
sequences.")
    ;; STAR is licensed under GPLv3 or later; htslib is MIT-licensed.
    (license license:gpl3+)))

(define-public perl-pathtools
  (package
   (name "perl-pathtools")
   (version "3.62")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/PathTools-"
                         version ".tar.gz"))
     (sha256
      (base32 "1lmlqzqmzhjvcb83gk1hcqy7wi3fa05gi49kl1xl6wc8yl90wd9n"))))
   (build-system perl-build-system)
   (arguments '(#:tests? #f))
   (inputs
    `(("coreutils" ,coreutils)))
   (home-page "http://search.cpan.org/dist/PathTools")
   (synopsis "Get pathname of current working directory")
   (description "")
   (license #f)))

(define-public perl-threads
  (package
   (name "perl-threads")
   (version "2.15")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://cpan/authors/id/J/JD/JDHEDDEN/threads-"
                         version ".tar.gz"))
     (sha256
      (base32 "1nwh6yvb5zm1m1515hrry200jsfdw640a6gj1rzpqckpp3wxbjcq"))))
   (build-system perl-build-system)
   (home-page "http://search.cpan.org/dist/threads")
   (synopsis "Perl interpreter-based threads")
   (description "")
   (license (package-license perl))))

(define-public perl-storable
  (package
   (name "perl-storable")
   (version "2.51")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://cpan/authors/id/A/AM/AMS/Storable-"
           version ".tar.gz"))
     (sha256
      (base32 "1gphq8yhqzrwwlx2i5a8914ccw41ywmpl7gc648s5frb269bfrm5"))))
   (build-system perl-build-system)
   (home-page "http://search.cpan.org/dist/Storable")
   (synopsis "Persistence for Perl data structures")
   (description "")
   (license #f)))

(define perl-set-intervaltree
  (package
   (name "perl-set-intervaltree")
   (version "0.10")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/B/BE/BENBOOTH/Set-IntervalTree-"
           version ".tar.gz"))
     (sha256
      (base32 "1g1yam3fwl11wvy489yhhfzrfdlqaj1dh7pgks3myjq71p7rrgg3"))))
   (build-system perl-build-system)
   (home-page "http://search.cpan.org/dist/Set-IntervalTree")
   (synopsis "Perform range-based lookups on sets of ranges.")
   (description "")
   (license #f)))

(define-public star-fusion
  (package
   (name "star-fusion")
   (version "1.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/STAR-Fusion/STAR-Fusion/releases/"
                  "download/v" version "/STAR-Fusion-v" version
                  ".FULL.tar.gz"))
            (sha256
             (base32 "19p5lwq2f95hgii7fdidz03845nkhf3pjfvp8v3midrsb0s6p7df"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There is no test phase.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; There is nothing to configure.
        (delete 'build) ; There is nothing to compile/build.
        (add-before 'install 'patch-external-tools
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((samtools (string-append (assoc-ref inputs "samtools") "/bin/samtools"))
                  (gunzip (string-append (assoc-ref inputs "gzip") "/bin/gunzip"))
                  (zcat (string-append (assoc-ref inputs "gzip") "/bin/zcat"))
                  (cat (string-append (assoc-ref inputs "coreutils") "/bin/cat"))
                  (wc (string-append (assoc-ref inputs "coreutils") "/bin/wc"))
                  (sort (string-append (assoc-ref inputs "coreutils") "/bin/sort"))
                  (mkdir (string-append (assoc-ref inputs "coreutils") "/bin/mkdir")))
              (substitute* "util/append_breakpoint_junction_info.pl"
                (("samtools") samtools))
              (substitute* "util/incorporate_FFPM_into_final_report.pl"
                (("gunzip") gunzip))
              (substitute* "util/STAR-Fusion.predict" (("gunzip") gunzip))
              (substitute* "util/incorporate_FFPM_into_final_report.pl" (("wc") wc))
              (substitute* "util/convert_to_FFPM.pl" (("wc") wc))
              (substitute* "util/incorporate_FFPM_into_final_report.pl"
                (("cat \\$fq_file") (string-append cat " $fq_file")))
              (substitute* "util/partition_FUSION_EVIDENCE_fastqs_by_fusion.pl"
                (("sort \\$tmp_paired") (string-append sort " $tmp_paired")))
              (substitute* "util/convert_to_FFPM.pl"
                (("\"cat \\$fq_filename") (string-append "\"" cat " $fq_filename")))
              (substitute* "util/convert_to_FFPM.pl"
                (("zcat \\$fq_filename") (string-append zcat " $fq_filename")))
              (substitute* "util/partition_FUSION_EVIDENCE_fastqs_by_fusion.pl"
                (("mkdir") mkdir))
              (substitute* "util/STAR-Fusion.filter" (("mkdir") mkdir))
              (substitute* "util/STAR-Fusion.predict" (("mkdir") mkdir)))))
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (mkdir-p bin)
              (install-file "STAR-Fusion" bin)
              (copy-recursively "PerlLib" (string-append bin "/PerlLib"))
              (copy-recursively "util" (string-append bin "/util"))
              (copy-recursively "FusionFilter"
                                (string-append bin "/FusionFilter"))))))))
   (inputs
    `(("perl" ,perl)
      ("samtools" ,samtools)
      ("coreutils" ,coreutils)
      ("gzip" ,gzip)))
   (propagated-inputs
    `(("perl-carp" ,perl-carp)
      ("perl-pathtools" ,perl-pathtools)
      ("perl-db-file" ,perl-db-file)
      ("perl-uri" ,perl-uri)
      ("perl-storable" ,perl-storable)
      ("perl-set-intervaltree" ,perl-set-intervaltree)))
   (home-page "https://github.com/STAR-Fusion/STAR-Fusion/")
   (synopsis "")
   (description "")
   (license #f)))
