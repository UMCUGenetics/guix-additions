;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019 2020 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pth)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (umcu packages perl)
  #:use-module (umcu packages python))

(define-public spades
  (package
   (name "spades")
   (version "3.11.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://cab.spbu.ru/files/release"
                  version "/SPAdes-" version ".tar.gz"))
            (sha256
             (base32 "0x5l4nkkjrkdn41ifz2baz9bp2r5blgrf77impc5nnbxpy35vf1s"))))
   (build-system cmake-build-system)
   ;; Reported under section 2 "Installation", "SPAdes requires a 64-bit
   ;; system": http://cab.spbu.ru/files/release3.10.1/manual.html
   (supported-systems '("x86_64-linux"))
   (arguments
    `(#:tests? #f ; There is no test target.
      #:phases
      (modify-phases %standard-phases
        (add-before 'configure 'move-to-source-dir
          (lambda _
            (chdir "src"))))))
   ;; TODO:  While this build works fine, SPAdes bundles samtools, bwa, and
   ;; boost.  These packages are also available in GNU Guix, so we should
   ;; unbundle them.
   (inputs
    `(("bzip2" ,bzip2)
      ("zlib" ,zlib)
      ("perl" ,perl)
      ("python-2" ,python-2)))
   (home-page "http://cab.spbu.ru/software/spades")
   (synopsis "Genome assembly toolkit")
   (description "SPAdes is an assembly toolkit containing various assembly
pipelines.")
   (license license:gpl2)))

(define-public bwa-0.7.5a
  (package
    (name "bwa")
    (version "0.7.5a")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bio-bwa/bwa-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1pfpzxnmz9m5fgfh3r5cnzg6d5hkx74jycn9fmc24f5r22bxfmyi"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:phases
       (alist-replace
        'install
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((bin (string-append
                      (assoc-ref outputs "out") "/bin"))
                (doc (string-append
                      (assoc-ref outputs "out") "/share/doc/bwa"))
                (man (string-append
                      (assoc-ref outputs "out") "/share/man/man1")))
            (mkdir-p bin)
            (mkdir-p doc)
            (mkdir-p man)
            (install-file "bwa" bin)
            (install-file "README.md" doc)
            (install-file "bwa.1" man)))
        ;; no "configure" script
        (alist-delete 'configure %standard-phases))))
    (inputs `(("zlib" ,zlib)))
    ;; Non-portable SSE instructions are used so building fails on platforms
    ;; other than x86_64.
    (supported-systems '("x86_64-linux"))
    (home-page "http://bio-bwa.sourceforge.net/")
    (synopsis "Burrows-Wheeler sequence aligner")
    (description
     "BWA is a software package for mapping low-divergent sequences against a
large reference genome, such as the human genome.  It consists of three
algorithms: BWA-backtrack, BWA-SW and BWA-MEM.  The first algorithm is
designed for Illumina sequence reads up to 100bp, while the rest two for
longer sequences ranged from 70bp to 1Mbp.  BWA-MEM and BWA-SW share similar
features such as long-read support and split alignment, but BWA-MEM, which is
the latest, is generally recommended for high-quality queries as it is faster
and more accurate.  BWA-MEM also has better performance than BWA-backtrack for
70-100bp Illumina reads.")
    (license license:gpl3+)))

(define-public miniasm
  (package
   (name "miniasm")
   (version "0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/lh3/miniasm/archive/v" version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
               (base32
                "0g89pa98dvh34idv7w1zv12bsbyr3a11c4qb1cdcz68gyda88s4v"))))
   (build-system gnu-build-system)
   (inputs
    `(("zlib" ,zlib)))
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "miniasm" bin)
              (install-file "minidot" bin)))))))
   (home-page "https://github.com/lh3/miniasm")
   (synopsis "Ultrafast de novo assembly for long noisy reads")
   (description "Miniasm is a very fast OLC-based de novo assembler for noisy
long reads.  It takes all-vs-all read self-mappings (typically by minimap) as
input and outputs an assembly graph in the GFA format.  Different from
mainstream assemblers, miniasm does not have a consensus step.  It simply
concatenates pieces of read sequences to generate the final unitig sequences.
Thus the per-base error rate is similar to the raw input reads.")
   (license license:expat)))

(define-public assembly-stats
  (package
   (name "assembly-stats")
   (version "1.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/sanger-pathogens/assembly-stats/archive/v"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "0xc5ppmcs09d16f062nbb0mdb0cnfhbnkp0arlxnfi6jli6n3gh2"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags (list (string-append
                               "-DINSTALL_DIR:PATH="
                               %output
                               "/bin"))))
   (home-page "https://github.com/sanger-pathogens")
   (synopsis "Tool to extract assembly statistics from FASTA and FASTQ files")
   (description "This package provides a tool to extract assembly statistics
from FASTA and FASTQ files.")
   (license license:gpl3)))

(define-public fastq-tools
  (package
   (name "fastq-tools")
   (version "0.8")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://homes.cs.washington.edu/~dcjones/fastq-tools/"
                  name "-" version ".tar.gz"))
            (sha256
             (base32
              "0jz1y40fs3x31bw10097a1nhm0vhbsyxmd4n7dwdsl275sc9l1nz"))))
   (build-system gnu-build-system)
   (arguments `(#:tests? #f))
   (inputs
    `(("pcre" ,pcre "bin")
      ("zlib" ,zlib)))
   (home-page "https://homes.cs.washington.edu/~dcjones/fastq-tools/")
   (synopsis "")
   (description "")
   (license license:gpl3)))

(define-public samtools-1.2
  (package
    (name "samtools")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "1akdqb685pk9xk1nb6sa9aq8xssjjhvvc06kp4cpdqvz2157l3j2"))))
    (build-system gnu-build-system)
    (arguments
     `(;; There are 87 test failures when building on non-64-bit architectures
       ;; due to invalid test data.  This has since been fixed upstream (see
       ;; <https://github.com/samtools/samtools/pull/307>), but as there has
       ;; not been a new release we disable the tests for all non-64-bit
       ;; systems.
       #:tests? ,(string=? (or (%current-system) (%current-target-system))
                           "x86_64-linux")
       #:modules ((ice-9 ftw)
                  (ice-9 regex)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:make-flags (list "LIBCURSES=-lncurses"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (alist-cons-after
        'unpack
        'patch-tests
        (lambda* (#:key inputs #:allow-other-keys)
          (let ((bash (assoc-ref inputs "bash")))
            (substitute* "test/test.pl"
              ;; The test script calls out to /bin/bash
              (("/bin/bash")
               (string-append bash "/bin/bash"))
              ;; There are two failing tests upstream relating to the "stats"
              ;; subcommand in test_usage_subcommand ("did not have Usage"
              ;; and "usage did not mention samtools stats"), so we disable
              ;; them.
              (("(test_usage_subcommand\\(.*\\);)" cmd)
               (string-append "unless ($subcommand eq 'stats') {" cmd "};")))))
        (alist-cons-after
         'install 'install-library
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
             (install-file "libbam.a" lib)))
         (alist-cons-after
          'install 'install-headers
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((include (string-append (assoc-ref outputs "out")
                                          "/include/samtools/")))
              (for-each (lambda (file)
                          (install-file file include))
                        (scandir "." (lambda (name) (string-match "\\.h$" name))))
              #t))
          (alist-delete 'configure %standard-phases))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("ncurses" ,ncurses)
              ("perl" ,perl)
              ("python" ,python)
              ("zlib" ,zlib)))
    (home-page "http://samtools.sourceforge.net")
    (synopsis "Utilities to efficiently manipulate nucleotide sequence alignments")
    (description
     "Samtools implements various utilities for post-processing nucleotide
sequence alignments in the SAM, BAM, and CRAM formats, including indexing,
variant calling (in conjunction with bcftools), and a simple alignment
viewer.")
    (license license:expat)))

(define-public contra-2.0.6
  (package
    (name "contra")
    (version "2.0.6")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "mirror://sourceforge/contra-cnv/CONTRA.V2.0/CONTRA.v" version ".tar.gz"))
      (sha256
       (base32
        "0agpcm2xh5f0i9n9sx1kvln6mzdksddmh11bvzj6bh76yw5pnw91"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("python" ,python-2)
       ("r" ,r)
       ;; ("r-dnacopy" ,r-dnacopy) <-- missing in Pjotr's tree
       ("bedtools" ,bedtools)
       ("samtools" ,samtools-1.2)))
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build) ; We can use Guix's BEDtools instead.
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/contra")))
               (mkdir-p bin)
               (mkdir-p doc)
               (and
                (zero? (system* "cp" "--recursive" "scripts" bin))
                (zero? (system* "cp" "contra.py" bin))
                (zero? (system* "cp" "baseline.py" bin))
                ;; There's only a pre-built PDF available.
                (zero? (system* "cp" "CONTRA_User_Guide.2.0.pdf" doc)))))))))
    (home-page "http://contra-cnv.sourceforge.net/")
    (synopsis "Tool for copy number variation (CNV) detection for targeted
resequencing data")
    (description "CONTRA is a tool for copy number variation (CNV) detection
for targeted resequencing data such as those from whole-exome capture data.
CONTRA calls copy number gains and losses for each target region with key
strategies including the use of base-level log-ratios to remove GC-content
bias, correction for an imbalanced library size effect on log-ratios, and the
estimation of log-ratio variations via binning and interpolation.  It takes
standard alignment formats (BAM/SAM) and outputs in variant call format
(VCF 4.0) for easy integration with other next generation sequencing analysis
package.")
    (license license:gpl3+)))

(define-public python-setuptools-vtags ; use this specific version for CWL to deal with self.vtags
  (package
    (name "python-setuptools-vtags")
    (version "40.6.2")
    (source
     (origin
      (method url-fetch)
      (uri "https://files.pythonhosted.org/packages/37/1b/b25507861991beeade31473868463dad0e58b1978c209de27384ae541b0b/setuptools-40.6.3.zip"
             )
      (sha256
       (base32
        "1y085dnk574sxw9aymdng9gijvrsbw86hsv9hqnhv7y4d6nlsirv"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Remove included binaries which are used to build self-extracting
          ;; installers for Windows.
          ;; TODO: Find some way to build them ourself so we can include them.
          (for-each delete-file (find-files "setuptools" "^(cli|gui).*\\.exe$"))
          #t))))
    (build-system python-build-system)
    ;; FIXME: Tests require pytest, which itself relies on setuptools.
    ;; One could bootstrap with an internal untested setuptools.
    (arguments
     `(#:tests? #f))
    (home-page "https://pypi.python.org/pypi/setuptools")
    (synopsis
     "Library designed to facilitate packaging Python projects")
    (description
     "Setuptools is a fully-featured, stable library designed to facilitate
packaging Python projects, where packaging includes:
Python package and module definitions,
distribution package metadata,
test hooks,
project installation,
platform-specific details,
Python 3 support.")
    ;; TODO: setuptools now bundles the following libraries:
    ;; packaging, pyparsing, six and appdirs. How to unbundle?
    (license (list license:psfl        ; setuptools itself
                   license:expat       ; six, appdirs, pyparsing
                   license:asl2.0      ; packaging is dual ASL2/BSD-2
                   license:bsd-2))))

(define-public python2-setuptools-vtags
  (package-with-python2 python-setuptools-vtags))


(define-public python-typing-extensions; guix candidate
  (package
   (name "python-typing-extensions")
   (version "3.6.6")
   (source
    (origin
     (method url-fetch)
     (uri "https://files.pythonhosted.org/packages/fc/e6/3d2f306b12f01bde2861d67458d32c673e206d6fcc255537bf452db8f80c/typing_extensions-3.6.6.tar.gz")
     (sha256
      (base32
       "07vhddjnd3mhdijyc3s0mwi9jgfjp3rr056nxqiavydbvkrvgrsi"))))
   (build-system python-build-system)
   (home-page "https://pypi.python.org/pypi/typing_extensions")
   (synopsis
    "Python typing_extensions.")
   (description
    "Python typing_extensions.")
   (license license:gpl2))
  )

(define-public python-subprocess32 ; guix candidate
  (package
   (name "python-subprocess32")
   (version "0.2.9")
   (source
    (origin
     (method url-fetch)
     (uri "https://files.pythonhosted.org/packages/be/2b/beeba583e9877e64db10b52a96915afc0feabf7144dcbf2a0d0ea68bf73d/subprocess32-3.5.3.tar.gz")
     (sha256
      (base32
       "1hr5fan8i719hmlmz73hf8rhq74014w07d8ryg7krvvf6692kj3b"))))
   (build-system python-build-system)
   (arguments `(#:tests? #f)) ;; No tests.
   (home-page "https://pypi.python.org/pypi/subprocess32")
   (synopsis
    "Python subprocess32.")
   (description
    "Python subprocess32.")
   (license license:gpl2))
  )

(define-public python-rdflib-jsonld ; guix ready
  (package
    (name "python-rdflib-jsonld")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rdflib-jsonld" version))
        (sha256
          (base32
            "0bdw2pbjmpy1l4p6slsjn54bqy6crk5hk4san84xxirgd9w78iql"))))
    (build-system python-build-system)
    (inputs
      `(("python-setuptools-vtags" ,python-setuptools-vtags)))
    (propagated-inputs
     `(("python-rdflib" ,python-rdflib)
       ("python-isodate" ,python-isodate)
       ("python-pyparsing" ,python-pyparsing)
       ("python-html5lib" ,python-html5lib)
       ("python-nose" ,python-nose)
))
    (home-page
      "https://github.com/RDFLib/rdflib-jsonld")
    (synopsis
      "rdflib extension adding JSON-LD parser and serializer")
    (description
      "rdflib extension adding JSON-LD parser and serializer")
    (license license:bsd-3)))

(define-public python2-rdflib-jsonld
  (package-with-python2 python-rdflib-jsonld))

(define-public python-prov ; guix candidate
(package
  (name "python-prov")
  (version "1.5.3")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "prov" version))
      (sha256
        (base32
          "1a9h406laclxalmdny37m0yyw7y17n359akclbahimdggq853jd0"))))
  (build-system python-build-system)
  (inputs
       `(("python-rdflib" ,python-rdflib)
       ("python-lxml" ,python-lxml)
       ("python-networkx" ,python-networkx)
       ("python-dateutil" ,python-dateutil)
       ("python-pydot" ,python-pydot)
       ("graphviz" ,graphviz) ; for testing
       ))
  (home-page "https://github.com/trungdong/prov")
  (synopsis
    "A library for W3C Provenance Data Model supporting PROV-JSON, PROV-XML and PROV-O (RDF)")
  (description
    "A library for W3C Provenance Data Model supporting PROV-JSON, PROV-XML and PROV-O (RDF)")
  (license license:expat)))

(define-public python-avro ; guix ready - used by CWL
(package
  (name "python-avro")
  (version "1.8.2")
  (source
    (origin
      (method url-fetch)
        (uri (pypi-uri "avro" version))
      (sha256
        (base32
          "0nabn1hzj1880qsp7fkg7923c0xdqk4i35s15asmy2xp604f97lg"))))
  (build-system python-build-system)
  (inputs
    `(("python-setuptools-vtags" ,python-setuptools-vtags)))
  (home-page "http://hadoop.apache.org/avro")
  (synopsis
    "Avro is a serialization and RPC framework.")
  (description
    "Avro is a serialization and RPC framework.")
  (license #f)))

(define-public python2-avro
  (package-with-python2 python-avro))

(define-public python-shellescape ; guix ready
(package
  (name "python-shellescape")
  (version "3.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://pypi.python.org/packages/source/s/shellescape/shellescape-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0n5ky1b2vw2y0d4xl3qybyp2rk0gq5frjs8nr8ak6mgj2fyb4676"))))
  (build-system python-build-system)
  (inputs
    `(("python-setuptools-vtags" ,python-setuptools-vtags)))
  (home-page
    "https://github.com/chrissimpkins/shellescape")
  (synopsis
    "Shell escape a string to safely use it as a token in a shell command (backport of Python shlex.quote for Python versions 2.x & < 3.3)")
  (description
    "Shell escape a string to safely use it as a token in a shell command (backport of Python shlex.quote for Python versions 2.x & < 3.3)")
  (license expat))
)

(define-public python2-shellescape
  (package-with-python2 python-shellescape))

(define-public python2-htmlgen-gn ; guix obsolete
(package
  (name "python2-htmlgen-gn")
  (version "2.2.2")
  (source (origin
           (method url-fetch)
           ;; http://files.genenetwork.org/software/contrib/htmlgen-2.2.2-gn.tar.gz
           (uri (string-append
                 "http://files.genenetwork.org/software/contrib/htmlgen-"
version "-gn.tar.gz"))
           (sha256
            (base32
             "1lwsk56rymhrma46cbyh3g64ksmq1vsih3qkrc2vh0lpba825y7r"))
           ;;(patches (list
           ;;          (search-patch "python2-htmlgen-Applied-Deb-patch.patch")
           ;;          (search-patch "python2-htmlgen-Fix-test-for-random.patch")
            ))
  (build-system python-build-system)
  (outputs '("out"))
  (native-inputs
   `(("make" ,gnu-make)
     ))
  (propagated-inputs
   `(("python2" ,python-2)))
  (arguments
   `(#:phases (modify-phases %standard-phases
     (replace 'build
              (lambda _
                (system* "python2" "-m" "compileall" ".")))
     (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (include (string-append out "/include"))
                              (lib2 (string-append out "/lib/htmlgen"))
                              (lib (string-append (assoc-ref %outputs "out") "/lib/python2.7/site-packages/htmlgen"))
                              (pkgconfig (string-append out "/lib/pkgconfig"))
                              (doc (string-append out "/share/doc")))
                         ;; Install libs and headers.
                         ;; (copy-file "HTMLgen.pyc" "HTMLgen2.pyc")
                         (install-file "HTMLgen.pyc" lib)
                         (install-file "HTMLgen2.pyc" lib)
                         (install-file "imgsize.pyc" lib)
                         (install-file "ImageH.pyc" lib)
                         (install-file "ImagePaletteH.pyc" lib)
                         (install-file "__init__.pyc" lib)
              ))) ; install
     ) ; phases
     #:tests? #f))
  (home-page
    "https://packages.debian.org/unstable/python/python-htmlgen")
  (synopsis "Genenetwork version of Python2 HTMLgen (defunkt
project)")
  (description #f)
  (license #f)))


(define-public python2-parallel ; guix fix number of things
  (package
    (name "python2-parallel")
    (version "1.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.parallelpython.com/downloads/pp/pp-" version ".zip"
             ))
       (sha256
        (base32
         "1bw3j0zn7bj56636vp1vx4m91p2mlp661gn2nfhpbph3prgxzv82"))))
    (native-inputs
     `(("unzip" ,unzip)))

    (build-system python-build-system)
    ;; (native-inputs
    ;; `(("python-setuptools-vtags" ,python-setuptools-vtags)))
    (arguments
     `(#:python ,python-2
       #:tests? #f
       ))   ; no 'setup.py test' really!
    (home-page #f)
    (synopsis "Parallel python lib")
    (description #f)
    (license #f)))

(define-public python2-numarray ; guix: obsolete lib
  (package
    (name "python2-numarray")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/numpy/numarray-" version ".tar.gz"
             ))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x1i4j7yni7k4p9kjxs1lgln1psdmyrz65wp2yr35yn292iw2vbg"))))
    (build-system python-build-system)
    (native-inputs
     `(("python2-setuptools-vtags" ,python2-setuptools-vtags)))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "python" "setup.py" "build"))))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; Build and install the Python bindings.  The underlying
                    ;; C++ library is apparently not meant to be installed.
                    (let ((out (assoc-ref outputs "out")))
                      (system* "python" "setup.py" "install"
                               (string-append "--prefix=" out))))))
       #:tests? #f))   ; no 'setup.py test' really!
    (home-page "http://www.numpy.org/")
    (synopsis "Numerical library array processing of numbers, strings, records and objects")
    (description
     "Numarray is an (OBSOLETE) array processing package designed to
efficiently manipulate large multi-dimensional arrays. Numarray is
modelled after Numeric and features c-code generated from python
template scripts, the capacity to operate directly on arrays in files,
and improved type promotions. Numarray provides support for
manipulating arrays consisting of numbers, strings, records, or
objects using the same basic infrastructure and syntax.  Numarray is
now part of the numpy package, though some legacy software still uses
the older versions.")
    (license license:gpl2))) ; actualy PyRAF http://www.stsci.edu/resources/software_hardware/pyraf/LICENSE

(define-public python-htmlgen
  (package
    (name "python-htmlgen")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://github.com/srittau/python-htmlgen/archive/v"
	     version ".tar.gz"))
       (sha256
	(base32
	 "1rwgqxhmc93l60wf4ay7ph619710kvyp73s22i0snjpm5i0bhc46"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis "Python HTML 5 Generator")
    (description "This is a python library for generating html from classes.")
    (home-page "https://github.com/srittau/python-htmlgen")
    (license license:expat)))

(define-public python-version
(let ((commit "e5aadc720bb74c535f29e5a2de5cd9697efe8d7c"))
(package
  (name "python-version")
  (version "0.1.2")
  (source
    (origin
      (method git-fetch)
      (uri (git-reference
      ; (url "https://github.com/genenetwork/pylmm.git")
        (url "https://github.com/keleshev/version.git") ; version not in pypi
        (commit commit)))
      (file-name (string-append name "-" commit))
      (sha256
        (base32
          "1rc8kf72v180qlygkh1y0jwv2fxqpx7n97bqfhbwgnn31iwai9g3"))))
  (build-system python-build-system)
  (propagated-inputs
    `(
    ("python-more-itertools" ,python-more-itertools)
    ("python-pytest" ,python-pytest)))
  (home-page "http://github.com/halst/version")
  (synopsis "Implementation of semantic version")
  (description
    "Implementation of semantic version")
  (license license:expat)
)))

(define-public python-mypy-extensions
(package
  (name "python-mypy-extensions")
  (version "0.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "mypy_extensions" version))
      (sha256
        (base32
          "04h8brrbbx151dfa2cvvlnxgmb5wa00mhd2z7nd20s8kyibfkq1p"))))
  (build-system python-build-system)
  (propagated-inputs
    `(("python-version" ,python-version)
                                        ; ("python-typing" ,python-typing)))
      ))
  (home-page "http://www.mypy-lang.org/")
  (synopsis
    "Experimental type system extensions for programs checked with the mypy typechecker.")
  (description
    "Experimental type system extensions for programs checked with the mypy typechecker.")
  (license #f))
)


(define-public python-arcp
(package
  (name "python-arcp")
  (version "0.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "arcp" version))
      (sha256
        (base32
          "0h8sn0mlb6vb8wqqnqc4pxdklrkyx3p72afdhm7b9kyalrqzd7dd"))))
  (build-system python-build-system)
  (home-page "http://arcp.readthedocs.io/")
  (synopsis
    "arcp (Archive and Package) URI parser and generator")
  (description
    "arcp (Archive and Package) URI parser and generator")
  (license license:asl2.0))
)

(define-public python-bagit; guix candidate
  (package
   (name "python-bagit")
   (version "1.7.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://files.pythonhosted.org/packages/ee/11/7a7fa81c0d43fb4d449d418eba57fc6c77959754c5c2259a215152810555/bagit-1.7.0.tar.gz")
     (sha256
      (base32
       "1m6y04qmig0b5hzb35lnaw3d2yfydb7alyr1579yblvgs3da6j7j"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools-scm" ,python-setuptools-scm)
      ("python-coverage" ,python-coverage)
      ("python-mock" ,python-mock)
      ))
   (arguments `(#:tests? #f)) ;; No tests.
   (home-page "https://pypi.python.org/pypi/bagit")
   (synopsis
    "Python bagit.")
   (description
    "Python bagit.")
   (license license:gpl2)))

(define-public python-cachecontrol ; schema-salad requires a specific version
  (package
    (name "python-cachecontrol")
    (version "0.11.7")
    (source
     (origin
       (method url-fetch)
       ;; Pypi does not have tests.
       (uri (string-append
             "https://github.com/ionrock/cachecontrol/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1yfhwihx1b1xjsx0r19va2m0r2s91im03x4d7pwzp87368f2lkkp"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Drop test that requires internet access.
             (delete-file "tests/test_regressions.py")
             (setenv "PYTHONPATH"
                     (string-append (getcwd) "/build/lib:"
                                    (getenv "PYTHONPATH")))
             ; (invoke "py.test" "-vv") requires cherry-py with too many dependencies
             #t)))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-redis" ,python-redis)
       ("python-webtest" ,python-webtest)
       ("python-mock" ,python-mock)))
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-lockfile" ,python-lockfile)))
    (home-page "https://github.com/ionrock/cachecontrol")
    (synopsis "The httplib2 caching algorithms for use with requests")
    (description "CacheControl is a port of the caching algorithms in
@code{httplib2} for use with @code{requests} session objects.")
    (license license:asl2.0)))

(define-public python2-cachecontrol
  (package-with-python2 python-cachecontrol))

(define-public cwltool ; guix: needs work
  (let ((commit "15539fba76993f951af9eba913bea6d677c74005"))
  (package
    (name "cwltool")
    (version "1.0.20181012180214")
    (source
      (origin
        ; (method url-fetch)
        ; (uri (string-append
        ;        "https://pypi.python.org/packages/source/c/cwltool/cwltool-"
        ;        version
        ;       ".tar.gz"))
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/genenetwork/cwltool.git")
               (commit commit)))
         (file-name (git-file-name name version))
        (sha256
          (base32
            "1qwfa82car7477sy0cb5bj4964w7zq7dcw2bdcls6c2i9qdp0586"))))
    (build-system python-build-system)
    ; (inputs
    ;  `(("python@3.6.5" ,python-3.6.5)))
    (propagated-inputs ; a lot of these are used for testing
     `(("git" ,git)
       ("node" ,node)
       ("python-bagit" ,python-bagit)
       ("python-cachecontrol" ,python-cachecontrol) ; requires 0.12
       ("python-arcp" ,python-arcp)
       ("python-setuptools-vtags" ,python-setuptools-vtags)
       ("python-dateutil" ,python-dateutil)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-prov" ,python-prov)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-rdflib" ,python-rdflib)
       ("python-pyparsing" ,python-pyparsing)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-mock" ,python-mock)
       ("python-subprocess32" ,python-subprocess32)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-cachecontrol" ,python-cachecontrol)
       ("python-lxml" ,python-lxml)
       ("python-mypy-extensions" ,python-mypy-extensions)
       ("python-mistune" ,python-mistune)
       ("python-networkx" ,python-networkx)
       ("python-schema-salad" ,python-schema-salad)
       ("python-html5lib" ,python-html5lib)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)
       ; ("python-typing-extensions" ,python-typing-extensions)
       ("python-scandir" ,python-scandir)
       ("python-psutil" ,python-psutil)
       ))
    (arguments
     `(;#:phases
       ; (modify-phases %standard-phases
       ;   (replace 'check
       ;     (lambda* (#:key inputs outputs #:allow-other-keys)
       ;       (invoke "python" "-m" "pytest")
       ;       )))
       #:tests? #f))   ; Disable for now because 3 out of 100+ tests are failing
    (home-page
      "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis
      "Common workflow language reference implementation")
    (description
      "Common workflow language reference implementation")
    (license license:asl2.0))))

(define-public python-schema-salad
  (let ((commit "eb85c3d49b99b7643e8a12248e2dc05504910c1e"))
  (package
    (name "python-schema-salad")
    (version "3.0.20181206233650")
    (source
      (origin
        ; (method url-fetch)
        ; (uri (pypi-uri "schema-salad" version))
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/genenetwork/schema_salad.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "174f224zzjr0nbjlq3ypciyfijnibasysrgjswvx8yhan2dizlhr"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;; CWL includes no tests.
    (inputs
     `(("python-cachecontrol" ,python-cachecontrol) ; requires 0.12
       ("python-cython" ,python-cython)
       ("python-setuptools-vtags" ,python-setuptools-vtags)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)
       ("python-mistune" ,python-mistune)))
    (propagated-inputs
     `(("python-rdflib" ,python-rdflib)
       ("python-avro" ,python-avro)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)
       ("python-shellescape" ,python-shellescape)
       ))
    (home-page
      "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis
      "Schema Annotations for Linked Avro Data (SALAD)")
    (description
      "Schema Annotations for Linked Avro Data (SALAD)")
    (license license:asl2.0))))

(define-public fast5
  (package
   (name "fast5")
   (version "0.6.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/mateidavid/fast5/archive/v"
                                version ".tar.gz"))
            (sha256
             (base32 "06pfg2ldra5g6d14xrxprn35y994w44g0zik2d7npddd0wncxcgq"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (setenv "HDF5_INCLUDE_DIR" (string-append (assoc-ref inputs "hdf5") "/include"))
            (setenv "HDF5_LIB_DIR" (string-append (assoc-ref inputs "hdf5") "/lib"))
            ;; TODO: Check for return value.
            (substitute* "python/Makefile"
              (("install: check_virtualenv") "install:"))
            (system* "make" "-C" "python" "install")))
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (python (assoc-ref inputs "python"))
                   (site-dir (string-append out "/lib/python3.6"
                                            "/site-packages/fast5/"))
                   (bin (string-append out "/bin"))
                   (include (string-append out "/include")))
              (mkdir-p site-dir)
              (mkdir-p bin)
              (install-file "python/fast5/fast5.pyx" site-dir)
              (install-file "python/bin/f5ls" bin)
              (install-file "python/bin/f5pack" bin)

              ;; Patch /usr/bin/env.
              (substitute* (list (string-append bin "/f5ls")
                                 (string-append bin "/f5pack"))
                (("/usr/bin/env python") (string-append python "/bin/python3")))

              ;; Wrap the environments of main programs so that
              ;; these work as expected.
              (wrap-program (string-append bin "/f5ls")
                `("PYTHONPATH" ":" prefix (,bin ,(getenv "PYTHONPATH")
                                                ,site-dir)))
              (wrap-program (string-append bin "/f5pack")
                `("PYTHONPATH" ":" prefix (,bin ,(getenv "PYTHONPATH")
                                                ,site-dir)))

              (for-each (lambda (file)
                          (install-file file include))
                        (find-files "src")))
            #t)))))
   (native-inputs
    `(("which" ,which)))
   (inputs
    `(("python" ,python-3)
      ("python-setuptools" ,python-setuptools)
      ("python-cython" ,python-cython)
      ("hdf5" ,hdf5)
      ("gcc" ,gcc-9)))
   (propagated-inputs
    `(("python-dateutil" ,python-dateutil)))
   (home-page "https://github.com/mateidavid/fast5")
   (synopsis "Library for accessing Oxford Nanopore sequencing data")
   (description "This package provides a lightweight C++ library for accessing
Oxford Nanopore Technologies sequencing data.")
   (license license:expat)))

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

(define-public fastqc-0.11.8
  (package (inherit fastqc)
    (name "fastqc")
    (version "0.11.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/s-andrews/FastQC/archive/v"
                           version ".zip"))
       (sha256
        (base32
         "1cbbwpdxhpv4b731k9wycibx9dnisz7w4bwn9yvjsh7bj2zszmjw"))))))

(define-public freec-8.7
  (package
    (name "control-freec")
    (version "8.7")
    (source (origin
      (method url-fetch)
      (uri "http://bioinfo-out.curie.fr/projects/freec/src/FREEC_Linux64.tar.gz")
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "17vswfnlqykja03nlf9b7n29vypr2590d0hlyd07kad9zc36kbam"))))
    (build-system gnu-build-system)
    ;; The source code's filename indicates only a 64-bit Linux build.
    ;; We need to investigate whether this is true.
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There's no configure phase because there are no external
         ;; dependencies.
         (delete 'configure)
         ;; There are no tests.
         (delete 'check)
         (replace
          'unpack
          (lambda* (#:key source #:allow-other-keys)
            (and
             (zero? (system* "mkdir" "source"))
             (with-directory-excursion "source"
               (zero? (system* "tar" "xvf" source))))))
         (replace
          'build
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "source"
              (zero? (system* "make")))))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "source/freec" bin)))))))
    (home-page "http://bioinfo-out.curie.fr/projects/freec/")
    (synopsis "Tool for detection of copy-number changes and allelic imbalances
(including LOH) using deep-sequencing data")
    (description "Control-FREEC automatically computes, normalizes, segments
copy number and beta allele frequency (BAF) profiles, then calls copy number
alterations and LOH.  The control (matched normal) sample is optional for whole
genome sequencing data but mandatory for whole exome or targeted sequencing
data.  For whole genome sequencing data analysis, the program can also use
mappability data (files created by GEM). ")
    (license license:gpl2+)))

(define-public freec-10.4
  (package
    (name "freec")
    (version "10.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/BoevaLab/FREEC/archive/v"
                           version ".tar.gz"))
       (file-name (string-append "freec-" version ".tar.gz"))
       (sha256
        (base32 "1a7wfr18hljw9xlzpy3920vf29s4cbpn9f0p8z153whq4g2487yh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'move-to-src-dir
           (lambda _
             (chdir "src")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share/freec")))
               (mkdir-p bin)
               (mkdir-p share)
               (copy-recursively "../scripts" share)
               (install-file "freec" bin)))))))
    (inputs
     `(("perl" ,perl)))
    (propagated-inputs
     `(("r-rtracklayer" ,r-rtracklayer)))
    (home-page "http://bioinfo-out.curie.fr/projects/freec/")
    (synopsis "Tool for detection of copy-number changes and allelic imbalances
(including LOH) using deep-sequencing data")
    (description "Control-FREEC automatically computes, normalizes, segments
copy number and beta allele frequency (BAF) profiles, then calls copy number
alterations and LOH.  The control (matched normal) sample is optional for whole
genome sequencing data but mandatory for whole exome or targeted sequencing
data.  For whole genome sequencing data analysis, the program can also use
mappability data (files created by GEM).")
    (license license:gpl2+)))

(define-public freec-10.5
  (package (inherit freec-10.4)
    (version "10.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/BoevaLab/FREEC/archive/v"
                           version ".tar.gz"))
       (file-name (string-append "freec-" version ".tar.gz"))
       (sha256
        (base32 "0z657hbpnc76pkli7g1ka07q4bpl41zarjhq6fwh6g9s368id15j"))))))

(define-public freec-11.5
  (package (inherit freec-10.5)
    (version "11.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/BoevaLab/FREEC/archive/v"
                           version ".tar.gz"))
       (file-name (string-append "freec-" version ".tar.gz"))
       (sha256
        (base32 "19q5wzbhlzk0wbz41n82vd75a59rfs1qxvgqlpjmrsr8nrnlrwih"))))))


(define-public freec-mappability-tracks
  (package
    (name "freec-mappability-tracks")
    (version "hg19_100bp")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://xfer.curie.fr/get/nil/7hZIk1C63h0/"
                    version ".tar.gz"))
              (sha256
               (base32
                "1qp05na2lb7w35nqii9gzv4clmppi3hnk5w3kzfpz5sz27fw1lym"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source-file (assoc-ref %build-inputs "source"))
               (output-dir (string-append %output "/share/freec"))
               (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (PATH (string-append (assoc-ref %build-inputs "gzip") "/bin")))
           (setenv "PATH" PATH)
           (mkdir-p output-dir)
           (with-directory-excursion output-dir
             (system* tar "-xvf" source-file))))))
    (inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "http://boevalab.com/FREEC")
    (synopsis "")
    (description "")
    (license #f)))

(define-public gccount
  (package
    (name "gccount")
    (version "0")
    (source (origin
      (method url-fetch)
      (uri "http://bioinfo-out.curie.fr/projects/freec/src/gccount.tar.gz")
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "1z9rs1fv29adbkwb2ns76v2j29zm2134916c53jqlpjqf6qzm6sw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gcc" ,gcc-5)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There's no configure phase because there are no external
         ;; dependencies.
         (delete 'configure)
         ;; There are no tests.
         (delete 'check)
         (replace
          'unpack
          (lambda* (#:key source #:allow-other-keys)
            (and
             (zero? (system* "mkdir" "source"))
             (with-directory-excursion "source"
               (zero? (system* "tar" "xvf" source))))))
         (replace
          'build
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "source"
              (substitute* "main.cpp"
                (("// main.cpp") "#include <unistd.h>"))
              (zero? (system* "make")))))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "source/gccount" bin)))))))
    (home-page "http://bioinfo-out.curie.fr/projects/freec/")
    (synopsis "Utility to generate a GC-content profile.")
    (description "This package provides the 'gccount' utility that can be
used with FREEC.")
    (license license:gpl2+)))

(define-public maven-bin
  ;; XXX: This package is only a binary inclusion of Maven.  It is different
  ;; from any other Guix package and you should NOT use this package.
  (package
   (name "maven")
   (version "3.5.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://apache.cs.uu.nl/maven/maven-3/" version
                                "/binaries/apache-maven-" version "-bin.tar.gz"))
            (sha256
             (base32 "0kd1jzlz3b2kglppi85h7286vdwjdmm7avvpwgppgjv42g4v2l6f"))))
   ;; We use the GNU build system mainly for its patch-shebang phases.
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is just copying a binary, so no tests to perform.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; No configuration, just copying.
        (delete 'build)     ; No building, just copying.
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((outdir (assoc-ref outputs "out")))
              (mkdir-p (string-append outdir))
              (copy-recursively "." outdir)
              (delete-file (string-append outdir "/README.txt"))
              (delete-file (string-append outdir "/NOTICE"))
              (delete-file (string-append outdir "/LICENSE"))))))))
   (propagated-inputs
    `(("which" ,which)))
   (home-page "https://maven.apache.org/")
   (synopsis "Build and dependency management tool for Java")
   (description "Apache Maven is a software project management and comprehension tool.
Based on the concept of a project object model (POM), Maven can manage a project's
build, reporting and documentation from a central piece of information.")
   (license license:asl2.0)))

(define-public r-gsalib
  (package
   (name "r-gsalib")
   (version "2.1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "gsalib" version))
     (sha256
      (base32
       "1k3zjdydzb0dfh1ihih08d4cw6rdamgb97cdqna9mf0qdjc3pcp1"))))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/gsalib")
   (synopsis "Utility Functions For GATK")
   (description "This package contains utility functions used by the Genome
Analysis Toolkit (GATK) to load tables and plot data.  The GATK is a toolkit
for variant discovery in high-throughput sequencing data.")
   (license license:expat)))

(define-public r-naturalsort
  (package
   (name "r-naturalsort")
   (version "0.1.3")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "naturalsort" version))
            (sha256
             (base32
              "0mz801y9mzld9ypp3xmsjw2d8l9q97sdnv09wrci9xi3yg2sjf6d"))))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/naturalsort")
   (synopsis "Natural Ordering")
   (description "This package provides functions related to human natural
ordering.  It handles adjacent digits in a character sequence as a number
so that natural sort function arranges a character vector by their numbers,
not digit characters.  It is typically seen when operating systems lists
file names.  For example, a sequence a-1.png, a-2.png, a-10.png looks
naturally ordered because 1 < 2 < 10 and natural sort algorithm arranges
so whereas general sort algorithms arrange it into a-1.png, a-10.png,
a-2.png owing to their third and fourth characters.")
   (license license:bsd-3)))

(define-public r-hmm
  (package
   (name "r-hmm")
   (version "1.0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "HMM" version))
            (sha256
             (base32
              "0z0hcqfixx1l2a6d3lpy5hmh0n4gjgs0jnck441akpp3vh37glzw"))))
   (properties `((upstream-name . "HMM")))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/HMM")
   (synopsis "Hidden Markov Models (HMM)")
   (description "Easy to use library to setup, apply and make inference with
discrete time and discrete space Hidden Markov Models")
   (license license:gpl2+)))

(define-public gatk-bin-3.8.1-no-intel-deflation
  (package
    (name "gatk")
    (version "3.8.1-aa8764d6c")
    (source (origin
             (method url-fetch)
             (uri "https://www.roelj.com/gatk-3.8.1-aa8764d6c.jar")
             (sha256
              (base32
               "1w46s2jh1q7h1r8shjw09y8yw27q15wlkviiqby3wv20haaqqjcg"))))
    (build-system gnu-build-system)
    (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'unpack)
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/" ,name "/")))
              (mkdir-p out)
              (copy-file (assoc-ref %build-inputs "source")
                         (string-append out "/GenomeAnalysisTK.jar"))))))))
    (propagated-inputs
     `(("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)
       ("gatk-queue-bin-3.8-1" ,gatk-queue-bin-3.8-1)))
    (home-page "https://www.broadinstitute.org/gatk/")
    (synopsis "Package for analysis of high-throughput sequencing")
    (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
    ;; There are additional restrictions, so it's nonfree.
    (license license:expat)))

(define-public gatk-queue-bin-3.8-1
  (package
    (name "gatk-queue")
    (version "3.8-1-0-gf15c1c3ef")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.roelj.com/gatk-queue-" version ".tar.bz2"))
              (sha256
               (base32 "0435lf2751w3l2m86m3h6girwr09kpiqahq3pj49gibqnyylx4sq"))))
    (build-system gnu-build-system)
    (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/gatk/")))
              (mkdir-p out)
              (install-file "Queue.jar" out)))))))
    (home-page "https://www.broadinstitute.org/gatk/")
    (synopsis "Package for analysis of high-throughput sequencing")
    (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
    ;; There are additional restrictions, so it's nonfree.
    (license license:expat)))

;;
;; FIXME: This package builds fine, but it doesn't include the R scripts needed by
;; various GATK and Queue processes.  It will therefore crash at the end of your
;; computation.  To solve this problem, we need to remove
;; -Dresource.bundle.skip=true from the compile options, but in that case the
;; build fails in the build environment because Javadoc seems to hardcode /bin/sh
;; somewhere, which is not available in the build environment.
(define-public gatk-full-3.5
  (package
    (name "gatk")
    (version "3.5-e91472d")
    (source (origin
              (method url-fetch)
              (uri "https://github.com/broadgsa/gatk-protected/archive/3.5.tar.gz")
              (sha256
               (base32 "0g07h5a7ajsyapgzh7nxz0yjp3d2v4fwhfnkcs0sfnq7s2rpsh9z"))
              (patches (list (search-patch "gatk-disable-vectorloglesscaching.patch")
                             (search-patch "gatk-apply-area-51-restrictions.patch")))))
    (build-system gnu-build-system)
    (arguments
      `(#:tests? #f ; Tests are run in the install phase.
        #:phases
        (modify-phases %standard-phases
          (delete 'configure) ; Nothing to configure
          (replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((build-dir (getcwd))
                     (home-dir (string-append build-dir "/home"))
                     (settings-dir (string-append build-dir "/mvn"))
                     (settings (string-append settings-dir "/settings.xml"))
                     (m2-dir (string-append build-dir "/m2/repository"))
                     (fakebin (string-append build-dir "/fakebin")))

                ;; Turns out that there's an unused import that breaks the build.
                ;; Fortunately, we can easily remove it.
                (substitute* (string-append "public/gatk-tools-public/src/main"
                                            "/java/org/broadinstitute/gatk"
                                            "/tools/walkers/varianteval"
                                            "/VariantEval.java")
                  (("import oracle.jrockit.jfr.StringConstantPool;")
                   "//import oracle.jrockit.jfr.StringConstantPool;"))

                ;; Patch hardcoded /bin/sh entries.
                (substitute* (string-append "public/gatk-queue/src/test/scala"
                                            "/org/broadinstitute/gatk/queue"
                                            "/util/ShellUtilsUnitTest.scala")
                  (("/bin/sh")
                   (string-append (assoc-ref %build-inputs "bash") "/bin/sh")))

                (mkdir-p settings-dir)
                (mkdir-p m2-dir)

                ;; Unpack the dependencies downloaded using maven.
                (with-directory-excursion m2-dir
                  (zero? (system* "tar" "xvf" (assoc-ref inputs "maven-deps"))))

                ;; Because the build process does not have a home directory in
                ;; which the 'm2' directory can be created (the directory
                ;; that will contain all downloaded dependencies for maven),
                ;; we need to set that directory to some other path.  This is
                ;; done using an XML configuration file of which a minimal
                ;; variant can be found below.
                (with-output-to-file settings
                  (lambda _
                    (format #t "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<settings xmlns=\"http://maven.apache.org/SETTINGS/1.0.0\"
          xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
          xsi:schemaLocation=\"http://maven.apache.org/SETTINGS/1.0.0 http://maven.apache.org/xsd/settings-1.0.0.xsd\">
<localRepository>~a</localRepository>
</settings>" m2-dir)))

                ;; Set JAVA_HOME to help maven find the JDK.
                (setenv "JAVA_HOME" (string-append (assoc-ref inputs "icedtea") "/jre"))
                (mkdir-p home-dir)
                (setenv "HOME" home-dir)

                (mkdir-p m2-dir)
                (mkdir-p settings-dir)

                ;; Compile using maven's compile command.
                (let ((compile-options (string-append
                                        "-fn " ; Javadoc targets fail.
                                        ;"-Dresource.bundle.skip=true "
                                        "-Dmaven.tests.skip=true "
                                        "--offline")))
                  (system (format #f "mvn compile ~a --global-settings ~s"
                                  compile-options settings))
                  (system (format #f "mvn verify ~a --global-settings ~s"
                                  compile-options settings))
                  (system (format #f "mvn package ~a --global-settings ~s"
                                  compile-options settings))))))
          (replace 'install
            (lambda _
              (let ((out (string-append (assoc-ref %outputs "out")
                                        "/share/java/user-classes/")))
                (mkdir-p out)
                (install-file "target/GenomeAnalysisTK.jar" out)
                (install-file "target/Queue.jar" out)))))))
    (native-inputs
     `(("maven-deps"
        ,(origin
          (method url-fetch)
          (uri (string-append "https://raw.githubusercontent.com/"
                              "UMCUGenetics/guix-additions/master/blobs/"
                              "gatk-mvn-dependencies.tar.gz"))
          (sha256
           (base32
            "1rrc7clad01mw83zyfgc4bnfn0nqvfc0mabd8wnj61p64xrigny9"))))))
    (inputs
     `(("icedtea" ,icedtea-7 "jdk")
       ("maven" ,maven-bin)
       ("bash" ,bash)
       ("perl" ,perl)
       ("r" ,r)))
    (propagated-inputs
     `(("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_JARPATH")
            (files (list "share/java/user-classes")))))
    (home-page "https://github.com/broadgsa/gatk-protected")
    (synopsis "Package for analysis of high-throughput sequencing")
   (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
   ;; There are additional restrictions, so it's nonfree.
   (license license:expat)))

(define-public gatk-full-3.5-patched-bin
  (package
    (name "gatk")
    (version "3.5-e91472d-patched")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/"
                                  "UMCUGenetics/guix-additions/master/blobs/"
                                  "gatk-patched-prebuilt.tar.gz"))
              (sha256
               (base32
                "14j9k3jscm278r5scydn9afb5d11yd2iij5km0yddidpxfnpn0r7"))))
    (build-system gnu-build-system)
    (arguments
      `(#:tests? #f ; Tests are run in the install phase.
        #:phases
        (modify-phases %standard-phases
          (delete 'configure) ; Nothing to configure
          (delete 'build) ; Nothing to build
          (replace 'install
            (lambda _
              (let ((out (string-append (assoc-ref %outputs "out")
                                        "/share/java/user-classes/")))
                (mkdir-p out)
                (install-file "GenomeAnalysisTK.jar" out)
                (install-file "Queue.jar" out)))))))
    (propagated-inputs
     `(("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_JARPATH")
            (files (list "share/java/user-classes")))))
    (home-page "https://github.com/broadgsa/gatk-protected")
    (synopsis "Package for analysis of high-throughput sequencing")
   (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
   ;; There are additional restrictions, so it's nonfree.
   (license license:expat)))

(define-public python-theano
  (package
    (name "python-theano")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Theano" version))
              (sha256
               (base32
                "1pmb5754qwiy1x2irciwn4xzsvwapdpi5agwwq8p1898sc1y0s37"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "http://deeplearning.net/software/theano/")
    (synopsis "Optimizing compiler for evaluating mathematical expressions on CPUs and GPUs.")
    (description "Optimizing compiler for evaluating mathematical expressions on CPUs and GPUs.")
    (license license:bsd-3)))

(define-public python-pymc3
  (package
  (name "python-pymc3")
  (version "3.5")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "pymc3" version))
      (sha256
        (base32
          "1vi11z1cjhc1hxbjvxay9n7a599z13p583qa6lhvnc6pqs1yd230"))))
  (build-system python-build-system)
  (arguments `(#:tests? #f ))
  (propagated-inputs
    `(;("python-enum34" ,python-enum34)
      ("python-h5py" ,python-h5py)
      ("python-joblib" ,python-joblib)
      ("python-numpy" ,python-numpy)
      ("python-pandas" ,python-pandas)
      ("python-patsy" ,python-patsy)
      ("python-six" ,python-six)
      ("python-theano" ,python-theano)
      ("python-tqdm" ,python-tqdm)))
  (home-page "http://github.com/pymc-devs/pymc3")
  (synopsis
    "Probabilistic Programming in Python: Bayesian Modeling and Probabilistic Machine Learning with Theano")
  (description
    "Probabilistic Programming in Python: Bayesian Modeling and Probabilistic Machine Learning with Theano")
  (license license:asl2.0)))

(define-public python-keras-preprocessing
  (package
    (name "python-keras-preprocessing")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras_Preprocessing" version))
       (sha256
        (base32
         "152i7k01xd3r7kin2s329ddi23b0ym6rb2ha1shnxh7cfxivljc6"))))
    (build-system python-build-system)
    (inputs
     `(("python-six" ,python-six)
       ("python-scipy" ,python-scipy)))
    (home-page
     "https://github.com/keras-team/keras-preprocessing")
    (synopsis
     "Easy data preprocessing and data augmentation for deep learning models")
    (description
     "Easy data preprocessing and data augmentation for deep learning models")
    (license license:expat)))

(define-public python-keras
  (package
    (name "python-keras")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras" version))
       (sha256
        (base32
         "1grl2znv1yssrci3r0vc4qzbqzhjfkkqjdg3bqd7y8dgaz8rk12v"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    ;(propagated-inputs
    ; `(("python-keras-preprocessing" ,python-keras-preprocessing)))
    (home-page "https://github.com/keras-team/keras")
    (synopsis "Deep Learning for humans")
    (description "Deep Learning for humans")
    (license license:expat)))

(define-public gatk4
  (package
    (name "gatk4")
    (version "4.1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/broadinstitute/gatk/releases/download/"
                    version "/gatk-" version ".zip"))
              (sha256
               (base32 "02nzqdc2d6v2sp9fz1y7aywx9r0mmgin57v2lkm032jsn41lzzan"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((unzip   (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
               (tarball (assoc-ref %build-inputs "source"))
               (out     (string-append %output "/share/java/user-classes"))
               (bin     (string-append %output "/bin")))
           (mkdir-p out)
           (mkdir-p bin)
           (system (string-append unzip " " tarball))
           (chdir (string-append "gatk-" ,version))
           (install-file "gatk-package-4.1.3.0-local.jar" out)
           (symlink (string-append out "/gatk-package-4.1.3.0-local.jar")
                    (string-append out "/gatk.jar"))
           (install-file "gatk-package-4.1.3.0-spark.jar" out)
           (symlink (string-append out "/gatk-package-4.1.3.0-spark.jar")
                    (string-append out "/gatk-spark.jar"))
           (substitute* "gatk"
             (("/usr/bin/env python") (string-append
                                       (assoc-ref %build-inputs "python2")
                                       "/bin/python"))
             (("return \\[\"java\"\\]")
              (string-append "return [\"" (assoc-ref %build-inputs "icedtea-8") "/bin/java\"]"))
             (("findJar\\(\"local.jar\", envVariableOverride=GATK_LOCAL_JAR_ENV_VARIABLE\\)")
              (string-append "\"" out "/gatk.jar\""))
             (("findJar\\(\"spark.jar\", envVariableOverride=GATK_SPARK_JAR_ENV_VARIABLE\\)")
              (string-append "\"" out "/gatk-spark.jar\"")))
           (install-file "gatk" bin)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("python2" ,python-2.7)
       ("icedtea-8" ,icedtea-8)))
    (propagated-inputs
     `(("r" ,r)
       ("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)
       ("python-certifi",python-certifi)
       ("openmpi" ,openmpi)
       ("openssl" ,openssl)
       ("python" ,python-3.7)
       ("readline" ,readline)
       ("python-setuptools" ,python-setuptools)
       ("sqlite" ,sqlite)
       ("tk" ,tk)
       ("python-wheel" ,python-wheel)
       ("xz" ,xz)
       ("zlib" ,zlib)
       ("python-bleach" ,python-bleach)
       ("python-cycler" ,python-cycler)
       ("python-h5py" ,python-h5py)
       ("python-html5lib" ,python-html5lib-0.9)
       ("python-joblib" ,python-joblib)
       ("python-keras" ,python-keras)
       ("python-markdown" ,python-markdown)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-patsy" ,python-patsy)
       ("python-protobuf" ,python-protobuf)
       ("python-pymc3" ,python-pymc3)
       ("python-pyparsing" ,python-pyparsing)
       ("python-dateutil" ,python-dateutil)
       ("python-pytz" ,python-pytz)
       ("python-pyyaml" ,python-pyyaml)
       ("python-scipy" ,python-scipy)
       ("python-six" ,python-six)
       ("python-theano" ,python-theano)
       ("python-tqdm" ,python-tqdm)
       ("python-werkzeug" ,python-werkzeug)
       ("python3" ,python-3.7)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

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

(define-public igv
  (package
    (name "igv")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://data.broadinstitute.org/igv/projects/downloads/"
             "2.7/IGV_Linux_" version ".zip"))
       (sha256
        (base32 "08h9bzwwykchnqfhz9m60c5vsn2wxik3skj2xmx6afnwlna4786b"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("openjdk11" ,openjdk11)))
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
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (share (string-append out "/share/igv")))
               (mkdir-p share)
               (mkdir-p lib)
               (mkdir-p bin)
               (copy-recursively "lib" lib)
               (substitute* "igv.sh"
                 (("prefix=")
                  (string-append "prefix=" lib " # "))
                 (("\\$\\{prefix\\}/igv.args")
                  (string-append share "/igv.args"))
                 (("--module-path=\"\\$\\{prefix\\}/lib\"")
                  (string-append "--module-path=" lib))
                 (("exec java")
                  (string-append "exec " (assoc-ref %build-inputs "openjdk11")
                                 "/bin/java")))
               (install-file "igv.args" share)
               (install-file "igv.sh" bin)))))))
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
        (base32 "1vpaj4zpa77pzix2dplrc1lz398bdv8yvg2p48f4i8px14qpdlvn"))))
    (propagated-inputs
     `(("icedtea" ,icedtea-8)))))
    
(define-public igvtools-bin-2.3.71
  (package
   (name "igvtools")
   (version "2.3.71")
   (source (origin
     (method url-fetch)
     (uri (string-append
           "http://data.broadinstitute.org/igv/projects/downloads/2.3/igvtools_"
           version ".zip"))
     (sha256
      (base32 "1z7fx79jfsqm0ry89mchifxxrj7vl1h9f98x6p2r2vcbx8f4zvi8"))))
   (build-system gnu-build-system)
   (inputs
    `(("icedtea" ,icedtea-8)))
   (native-inputs
    `(("unzip" ,unzip)))
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (add-before 'install 'fix-java-command
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "igvtools"
              (("java -D") (string-append
                            (assoc-ref inputs "icedtea")
                            "/bin/java -D")))))
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
           "http://data.broadinstitute.org/igv/projects/downloads/2.3/igvtools_"
           version ".zip"))
      (sha256
        (base32 "11k713nip68j06mzk7zkbsyajwrlprix7j38ybfrxblp666g3jm2"))))))

(define-public metamaps
  (let ((commit "e23f8a8688159ff0d092557a40305dbc7acc2342"))
    (package
     (name "metamaps")
     (version (string-append "0.0-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DiltheyLab/MetaMaps.git")
                    (commit commit)))
              (sha256
               (base32
                "0h9ahkv7axw4qzgbvhsz4r699swiv64hlwjy6h8s11vjls2dslrp"))))
     (build-system gnu-build-system)
     (arguments
      `(#:configure-flags (list (string-append
                                 "--with-boost="
                                 (assoc-ref %build-inputs "boost")))
        #:tests? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'shared-boost
            (lambda _
              (substitute* "configure.ac"
               (("libboost_math_c99.a") "libboost_math_c99.so")))))))
     (native-inputs
      `(("autoconf" ,autoconf)))
     (inputs
      `(("boost" ,boost)
        ("zlib" ,zlib)
        ("gsl" ,gsl)))
     (home-page "https://github.com/DiltheyLab/MetaMaps")
     (synopsis "Long-read metagenomic analysis")
     (description "MetaMaps is tool specifically developed for the analysis
of long-read (PacBio/Oxford Nanopore) metagenomic datasets.")
     (license license:public-domain))))

(define-public king-bin-2.1.2
  (package
    (name "king")
    (version "2.1.2")
    ;; WARNING: There's no source code.  This downloads a tarball with the
    ;; executable.
    (source (origin
      (method url-fetch)
      (uri "http://people.virginia.edu/~wc9c/KING/Linux-king.tar.gz")
      (file-name (string-append name "-" version "-bin.tar.gz"))
      (sha256
       (base32 "0asrgj4m20mll0psk2238asda4w1brzb5wlqjmaijknhflw60pj0"))))
    (build-system gnu-build-system)
    ;; The executable is linked to 64-bit libraries.
    (supported-systems '("x86_64-linux"))
    ;; WARNING: The host system's libz.so.1 is used because we only have an
    ;; executable that is linked already.
    (native-inputs
     `(("zlib" ,zlib)))
    (arguments
     `(#:tests? #f ; There are no tests to run.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'validate-runpath) ; It uses the host's libraries anyway.
         (replace 'unpack
          (lambda _
            (mkdir-p "king")
            (chdir "king")
            (zero? (system* "tar" "xvf" (assoc-ref %build-inputs "source")))))
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out") "/bin")))
               (mkdir-p out)
               (copy-file "king" (string-append out "/king"))))))))
    (home-page "http://people.virginia.edu/~wc9c/KING/")
    (synopsis "Program making use of high-throughput SNP data")
    (description "KING is a toolset making use of high-throughput SNP data
typically seen in a genome-wide association study (GWAS) or a sequencing
project.  Applications of KING include family relationship inference and
pedigree error checking, population substructure identification, forensics,
gene mapping, etc.")
    ;; WARNING: There's no license specified.  This is non-free software.
    (license license:non-copyleft)))

(define-public minimap2
  (package
    (name "minimap2")
    (version "2.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lh3/minimap2/"
                           "releases/download/v" version "/"
                           "minimap2-" version ".tar.bz2"))
       (sha256
        (base32
         "0hi7i9pzxhvjj44khzzzj1lrn5gb5837arr4wgln7k1k5n4ci2mn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:make-flags
       (list "CC=gcc"
             (let ((system ,(or (%current-target-system)
                                (%current-system))))
               (cond
                ((string-prefix? "x86_64" system)
                 "all")
                ((or (string-prefix? "armhf" system)
                     (string-prefix? "aarch64" system))
                 "arm_neon=1")
                (_ "sse2only=1"))))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "minimap2" bin)
               (mkdir-p man)
               (install-file "minimap2.1" man))
             #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://lh3.github.io/minimap2/")
    (synopsis "Pairwise aligner for genomic and spliced nucleotide sequences")
    (description "Minimap2 is a versatile sequence alignment program that
aligns DNA or mRNA sequences against a large reference database.  Typical use
cases include:

@enumerate
@item mapping PacBio or Oxford Nanopore genomic reads to the human genome;
@item finding overlaps between long reads with error rate up to ~15%;
@item splice-aware alignment of PacBio Iso-Seq or Nanopore cDNA or Direct RNA
  reads against a reference genome;
@item aligning Illumina single- or paired-end reads;
@item assembly-to-assembly alignment;
@item full-genome alignment between two closely related species with
  divergence below ~15%.
@end enumerate\n")
    (license license:expat)))

(define-public pbgzip
  (let ((commit "2b09f97b5f20b6d83c63a5c6b408d152e3982974"))
    (package
      (name "pbgzip")
      (version (string-take commit 7))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nh13/pbgzip.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1mlmq0v96irbz71bgw5zcc43g1x32zwnxx21a5p1f1ch4cikw1yd"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autogen
             (lambda _
               (zero? (system* "sh" "autogen.sh")))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)))
      (inputs
       `(("zlib" ,zlib)))
      (home-page "https://github.com/nh13/pbgzip")
      (synopsis "Parallel Block GZIP")
      (description "This tool and API implements parallel block gzip.  For many
formats, in particular Genomics Data Formats, data are compressed in
fixed-length blocks such that they can be easily indexed based on a (genomic)
coordinate order, since typically each block is sorted according to this order.
This allows for each block to be individually compressed (deflated), or more
importantly, decompressed (inflated), with the latter enabling random retrieval
of data in large files (gigabytes to terabytes).  @code{pbgzip} is not limited
to any particular format, but certain features are tailored to Genomics Data
Formats when enabled (see below). Parallel decompression is somewhat faster,
but truly the speedup comes during compression.")
      (license license:expat))))

(define-public picard-bin-1.141
  (package
   (name "picard")
   (version "1.141")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/broadinstitute/picard/releases/download/"
                  version "/picard-tools-" version ".zip"))
            (sha256
             (base32 "1ari9j37a0v8bm03c77pw729bqwbqqn6h15rw028jhl1iz4rgd5g"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("icedtea" ,icedtea-8)))
   (native-inputs
    `(("unzip" ,unzip)))
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'unpack
          (lambda _
            (zero? (system* "unzip" (assoc-ref %build-inputs "source")))))
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/picard/")))
              (chdir (string-append "picard-tools-" ,version))
              (install-file (string-append "htsjdk-" ,version ".jar") out)
              (install-file "libIntelDeflater.so" out)
              (install-file "picard-lib.jar" out)
              (install-file "picard.jar" out)))))))
   (home-page "http://broadinstitute.github.io/picard/")
    (synopsis "A set of Java command line tools for manipulating high-throughput
sequencing data (HTS) data and formats")
    (description "Picard comprises Java-based command-line utilities that
manipulate SAM files, and a Java API (HTSJDK) for creating new programs that
read and write SAM files. Both SAM text format and SAM binary (BAM) format are
supported.")
    (license license:expat)))

(define-public rmblast
  (package (inherit blast+)
    (name "rmblast")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/"
                    version "/ncbi-blast-" version "+-src.tar.gz"))
              (sha256
               (base32
                "15n937pw5aqmyfjb6l387d18grqbb96l63d5xj4l7yyh0zbf2405"))
              (patches (search-patches "rmblast-isb-2.6.0+-changes-vers2.patch"
                                       "blast+-fix-makefile.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled bzip2, zlib and pcre.
                  (delete-file-recursively "c++/src/util/compress/bzip2")
                  (delete-file-recursively "c++/src/util/compress/zlib")
                  (delete-file-recursively "c++/src/util/regexp")
                  (substitute* "c++/src/util/compress/Makefile.in"
                    (("bzip2 zlib api") "api"))
                  ;; Remove useless msbuild directory
                  (delete-file-recursively
                   "c++/src/build-system/project_tree_builder/msbuild")
                  #t))))))

(define-public pathseq-pipeline-tools
  (let ((commit "2a4f15d5dec1b2fbf707cab4a8517eedff070a33"))
    (package
      (name "pathseq-pipeline-tools")
      (version "1.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ChandraPedamallu/PathSeq.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "141fwk1knknvmrddpgpqiqcdcz7iarqrw28609j1smjr33dkwn3n"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (with-directory-excursion "Java"
                 ;; Remove pre-compiled files.
                 (system* "rm" "-rf"
                          "*.class"
                          "QualFilter_July2016.java"
                          "QualFilter_RemoveDuplicate_July2016.java")
                 (system "ls -lh")
                 ;; Compile all java classes.
                 (system (string-append
                          (assoc-ref inputs "java")
                          "/bin/javac -cp ../3rdparty/sam-1.52.jar *.java"))
                 ;; Pack the Java classes into one jar.
                 (system (string-append
                          (assoc-ref inputs "java")
                          "/bin/jar -cvf ../PathSeq.jar *.class")))))
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (java-dir (string-append out "/share/java/user-classes")))
                 (install-file "PathSeq.jar" java-dir)))))))
      (inputs
       `(("java" ,icedtea-8 "jdk")))
      (native-search-paths
       (list (search-path-specification
              (variable "GUIX_JARPATH")
              (files (list "share/java/user-classes")))))
      (home-page "http://software.broadinstitute.org/pathseq/")
      (synopsis "Pipeline for identifying microbial sequences in human data")
      (description "PathSeq is a computational tool for the identification and
analysis of microbial sequences in high-throughput human sequencing data that
is designed to work with large numbers of sequencing reads in a scalable
manner.")
      ;; MIT license.
      (license license:expat))))

(define-public xqilla
  (package
   (name "xqilla")
   (version "2.3.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/xqilla/XQilla-"
                                version ".tar.gz"))
            (sha256
             (base32 "1mjgcyar3qyizpnb0h9lxaj6p9yq4vj09qd8qan1bwv6z6sbjxlg"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags (list (string-append
                               "--with-xerces="
                               (assoc-ref %build-inputs "xerces-c")))))
   (inputs
    `(("xerces-c" ,xerces-c)))
   (home-page "http://xqilla.sourceforge.net/")
   (synopsis "XQuery and XPath utility")
   (description "XQilla is an XQuery and XPath 2 library and command line
utility written in C++ implemented on top of the Xerces-C library.")
   (license license:asl2.0)))

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
      ("perl-set-intervaltree" ,perl-set-intervaltree)))
   (home-page "https://github.com/STAR-Fusion/STAR-Fusion/")
   (synopsis "")
   (description "")
   (license #f)))

(define-public snpeff-bin-4.1
  (package
   (name "snpeff")
   (version "4.1")
   (source (origin
             (method url-fetch)
            (uri "mirror://sourceforge/snpeff/snpEff_v4_1_core.zip")
            (sha256
             (base32 "1vjgj6aacjsw6iczy09h18q5kx8ppxrrcq8w38g159zq7y3732kb"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((current-dir (getcwd))
                   (out (assoc-ref %outputs "out"))
                   (bin (string-append out "/share/java/" ,name))
                   (share (string-append out "/share/snpeff"))
                   (clinvar-file (string-append
                                  (assoc-ref inputs "clinvar")
                                  "/share/clinvar/GRCh37/clinvar.vcf.gz"))
                   (snpeff-db-dir (string-append share "/data"))
                   (snpeff-db (assoc-ref inputs "snpeff-database"))
                   (dbsnp-file (string-append (assoc-ref inputs "dbsnp")
                                             "/share/dbsnp/dbSnp.vcf.gz"))
                   (create-and-copy
                    (lambda (dir)
                      (mkdir (string-append bin "/" dir))
                      (copy-recursively dir (string-append bin "/" dir)))))
              (mkdir-p bin)
              (mkdir-p share)
              (substitute* "snpEff.config"
                (("data.dir = ./data/")
                 (string-append "data.dir = " share "/data"))
                (("database.local.clinvar      = ./db/GRCh38/clinvar/clinvar-latest.vcf.gz")
                 (string-append "database.local.clinvar      = " clinvar-file))
                (("database.local.dbsnp        = ./db/GRCh38/dbSnp/dbSnp.vcf.gz")
                 (string-append "database.local.dbsnp        = " dbsnp-file)))
              (chdir share)
              (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db)
              (chdir current-dir)

              (install-file "snpEff.config" bin)
              (install-file "snpEff.jar" bin)
              (install-file "SnpSift.jar" bin)
              (map create-and-copy '("scripts" "galaxy"))))))))
   (native-inputs
    `(("unzip" ,unzip)
      ("perl" ,perl)
      ("python" ,python-2)
      ("bash" ,bash)
      ("r" ,r)))
   (inputs
    `(("perl" ,perl)
      ("python" ,python)
      ("bash" ,bash)
      ("r" ,r)
      ("icedtea" ,icedtea-7)
      ("clinvar" ,clinvar-grch37)
      ("gwascatalog" ,gwascatalog)
      ("dbnsfp" ,dbnsfp)
      ("snpeff-database"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_1/"
               "snpEff_v4_1_GRCh37.74.zip"))
         (sha256
          (base32 "1p02n1dd4b04vf425wm7c5b749rjxj6va78ibbfzdhggl38wg345"))))
      ("dbsnp" ,dbsnp)))
   (home-page "http://snpeff.sourceforge.net/")
   (synopsis "Genetic variant annotation and effect prediction toolbox.")
   (description "Genetic variant annotation and effect prediction toolbox.
It annotates and predicts the effects of variants on genes (such as amino
acid changes).")
   ;; No license specified.
   (license license:non-copyleft)))

(define-public snpeff-bin-4.1h
 (package (inherit snpeff-bin-4.1)
  (name "snpeff")
  (version "4.1h")
  (source (origin
      (method url-fetch)
      (uri "mirror://sourceforge/snpeff/snpEff_v4_1h_core.zip")
      (sha256
        (base32 "1j45jp4y8wj0q01clxsx46w1f4jm2wh85yl1mbrha7qbqs8c1qn3"))))))

(define-public snpeff-bin-4.3t
 (package (inherit snpeff-bin-4.1)
  (name "snpeff")
  (version "4.3t")
  (source (origin
      (method url-fetch)
      (uri "mirror://sourceforge/snpeff/snpEff_v4_3t_core.zip")
      (sha256
       (base32 "0i12mv93bfv8xjwc3rs2x73d6hkvi7kgbbbx3ry984l3ly4p6nnm"))))
  (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (chdir "../snpEff")
            (let* ((current-dir (getcwd))
                   (out (assoc-ref %outputs "out"))
                   (bin (string-append out "/share/java/" ,name))
                   (patch-bin (string-append (assoc-ref %build-inputs "patch")
                                             "/bin/patch"))
                   (share (string-append out "/share/snpeff"))
                   (clinvar-file (string-append
                                  (assoc-ref inputs "clinvar")
                                  "/share/clinvar/GRCh37/clinvar.vcf.gz"))
                   (snpeff-db-dir (string-append share "/data"))
                   (snpeff-db (assoc-ref inputs "snpeff-database"))
                   (snpeff-db-GRCm38.86 (assoc-ref inputs "snpeff-database-GRCm38.86"))
                   (snpeff-db-GRCh37.75 (assoc-ref inputs "snpeff-database-GRCh37.75"))
                   (snpeff-db-UMD3.1.86 (assoc-ref inputs "snpeff-database-UMD3.1.86"))
                   (snpeff-db-GRCh38.86 (assoc-ref inputs "snpeff-database-GRCh38.86"))
                   (dbsnp-dir (string-append (assoc-ref inputs "dbsnp")
                                             "/share/dbsnp/"))
                   (gwascatalog-file (string-append
                                      (assoc-ref inputs "gwascatalog")
                                      "/share/gwascatalog/gwascatalog.txt"))
                   (dbnsfp-file (string-append
                                 (assoc-ref inputs "dbnsfp")
                                 "/share/dbnsfp/dbNSFP2.9_gene.complete.gz"))
                   (create-and-copy
                    (lambda (dir)
                      (mkdir (string-append bin "/" dir))
                      (copy-recursively dir (string-append bin "/" dir)))))
              (mkdir-p bin)
              (mkdir-p share)
              (substitute* "snpEff.config"
                (("data.dir = ./data/")
                 (string-append "data.dir = " share "/data"))
                (("database.clinvar.GRCh37                 = ./db/GRCh37/clinvar/clinvar-latest.vcf.gz")
                 (string-append "database.clinvar.GRCh37      = " clinvar-file))
                (("database.dbsnp.GRCh37                   = ./db/GRCh37/dbSnp/")
                 (string-append "database.dbsnp.GRCh37        = " dbsnp-dir))
                (("database.gwascatalog.GRCh37             = ./db/GRCh37/gwasCatalog/gwascatalog.txt")
                 (string-append "database.gwascatalog.GRCh37        = " gwascatalog-file))
                (("database.dbnsfp.GRCh37                  = ./db/GRCh37/dbNSFP/dbNSFP.txt.gz")
                 (string-append "database.dbnsfp.GRCh37                  = " dbnsfp-file)))
              (chdir share)
              (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db)
              (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db-GRCm38.86)
	      (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db-GRCh37.75)
	      (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db-GRCh38.86)
	      (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db-UMD3.1.86)
                                      
              (chdir current-dir)
              (install-file "snpEff.config" bin)
              (install-file "snpEff.jar" bin)
              (install-file "SnpSift.jar" bin)
              (for-each create-and-copy '("scripts" "galaxy"))

              ;; Backport settings from an older snpEff version by
              ;; applying the following patch.
              (with-directory-excursion bin
                (format #t "Applying patches... ")
                (let ((patch-file (assoc-ref %build-inputs "patch-file")))
                  (format #t
                   (if (zero? (system (string-append patch-bin " < " patch-file)))
                       " Succeeded.~%"
                       " Failed.~%"))))

              #t))))))
  (native-inputs
    `(("unzip" ,unzip)
      ("perl" ,perl)
      ("python" ,python-2)
      ("bash" ,bash)
      ("r" ,r)
      ("patch" ,patch)
      ("patch-file"
       ,(origin
         (method url-fetch)
         (uri (search-patch "snpeff-4.3t-backport-settings.patch"))
         (sha256
          (base32
           "1hw44vzcb6k8fq66740kd7kcdmb68bf5zbibc467bcxiiay8xpca"))))))
  (inputs
    `(("perl" ,perl)
      ("python" ,python)
      ("bash" ,bash)
      ("r" ,r)
      ("icedtea" ,icedtea-7)
      ("clinvar" ,clinvar-grch37)
      ("gwascatalog" ,gwascatalog)
      ("dbnsfp" ,dbnsfp)
      ("snpeff-database"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_hg19.zip"))
         (sha256
          (base32 "0rnaa858shjgxx284m73ikf2a1k11n3gc7861svczm2f98wwhar2"))))
    ("snpeff-database-GRCm38.86"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_GRCm38.86.zip"))
         (sha256
          (base32 "0rsdgv01yc33ppr8z412gk07xq098vsl8qhhii7s34kchk0qa746"))))
    ("snpeff-database-UMD3.1.86"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_UMD3.1.86.zip"))
         (sha256
          (base32 "0h4d7w3n5pr1lfbmf921z4rx163n93qfw2klv94qw7syl3db6lli"))))
    ("snpeff-database-GRCh38.86"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_GRCh38.86.zip"))
         (sha256
          (base32 "1rf8q7l732ayjq2lpny4s75zpij05j00151374nqblk4wri2mz0i"))))

    ("snpeff-database-GRCh37.75"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_GRCh37.75.zip"))
         (sha256
          (base32 "19c8wwx91vq47z7j7f455vsv8jw067x5rd7449d1z0nln82zpmhm"))))
      ("dbsnp" ,dbsnp)))))

(define-public strelka-1.0.15
  (package
    (name "strelka")
    (version "1.0.15")
    (source (origin
      (method url-fetch)
      (uri (string-append
            ;;"ftp://strelka:''@ftp.illumina.com/v1-branch/v"
            ;;version "/strelka_workflow-" version ".tar.gz"))
            "https://sites.google.com/site/strelkasomaticvariantcaller/home/"
            "download/" name "_workflow-" version ".tar.gz"))
      (sha256
       (base32 "1cwad2wlhdk09702ivblfiyv921af0al7s1gm1dn2d3b0v31qrp2"))
      (patches (list (search-patch "strelka-disable-tests.patch")
                     (search-patch "strelka-disable-install.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'build-some-more
           (lambda _
             (with-directory-excursion "strelka"
               (zero? (system* "make" "-j" (number->string
                                            (parallel-job-count))
                               "install")))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perl-lib-dir (string-append out "/lib/perl5/site_perl/"
                                                 ,(package-version perl)))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (libexec (string-append out "/libexec")))

               ;; Substitute the binary directories for samtools and bgzip.
               (substitute* '("src/perl/bin/configureStrelkaWorkflow.pl"
                              "src/perl/libexec/callSomaticVariants.pl"
                              "src/perl/libexec/consolidateResults.pl")
                            (("my \\$samtoolsDir = File::Spec->catdir\\(\\$optDir,'samtools'\\);")
                             (string-append "my $samtoolsDir = \""
                                            (assoc-ref inputs "samtools") "/bin\";"))
                            (("my \\$samtoolsBin = File::Spec->catfile\\(\\$optDir,'samtools','samtools'\\);")
                             (string-append "my $samtoolsBin = \""
                                            (assoc-ref inputs "samtools")
                                            "/bin/samtools\";")))

               (substitute* "src/perl/libexec/consolidateResults.pl"
                 (("my \\$bgzipBin = File::Spec->catfile\\(\\$optDir,'tabix','bgzip'\\);")
                  (string-append "my $bgzipBin = \"" (assoc-ref inputs "htslib") "/bin/bgzip\";"))
                 (("my \\$getHeaderCmd = \"bash")
                  (string-append "my $getHeaderCmd = \"" (assoc-ref inputs "bash") "/bin/bash")))

               (mkdir-p perl-lib-dir)
               (mkdir-p lib)
               (mkdir-p libexec)

               ;; Instead of patching out $optDir throughout the code, we can create
               ;; an empty directory so that these checks pass.  We already patched the
               ;; path to samtools and bgzip, so this should be fine.
               (mkdir-p (string-append out "/opt/samtools"))

               (install-file "src/c++/libexec/countFastaBases" libexec)
               (install-file "src/perl/bin/configureStrelkaWorkflow.pl" bin)
               (install-file "src/perl/libexec/consolidateResults.pl" libexec)
               (install-file "src/perl/libexec/filterSomaticVariants.pl" libexec)
               (install-file "src/perl/libexec/callSomaticVariants.pl" libexec)
               (install-file "src/perl/lib/Utils.pm" perl-lib-dir)
               (install-file "strelka/src/bin/strelka2" bin)
               (install-file "strelka/src/bin/starling2" bin)
               (install-file "strelka/src/bin/strelkaSiteSimulator" bin)

               ;; Also add Utils.pm to the lib folder, because strelka manipulates
               ;; its own Perl path to search in this folder.
               (install-file "src/perl/lib/Utils.pm" lib)

               ;; The configureStrelkaWorkflow.pl script looks for the
               ;; strelka2 binary in the libexec directory.
               (system* "ln" "--symbolic"
                        (string-append bin "/strelka2")
                        (string-append libexec "/strelka2"))))))))
    (inputs
     `(("boost" ,boost)
       ("perl" ,perl)
       ("bash" ,bash)
       ("zlib" ,zlib)
       ("samtools" ,samtools)))
    (native-inputs
     `(("bash" ,bash)
       ("python" ,python-2)
       ("gcc" ,gcc-5)))
    (propagated-inputs
     `(("vcftools" ,vcftools)
       ("htslib" ,htslib)))
    (native-search-paths (package-native-search-paths perl))
    (home-page "https://sites.google.com/site/strelkasomaticvariantcaller/")
    (synopsis "Somatic variant calling workflow for matched tumor-normal samples")
    (description "Analysis package designed to detect somatic SNVs and small
indels from the aligned sequencing reads of matched tumor-normal samples")
    ;; WARNING: The license is "Illumina Open Source Software License 1".
    ;; This effectively makes it nonfree software.
    (license license:non-copyleft)))

(define-public strelka-1.0.14
  (package (inherit strelka-1.0.15)
    (name "strelka")
    (version "1.0.14")
    (source (origin
      (method url-fetch)
      (uri (string-append
        "ftp://strelka:''@ftp.illumina.com/v1-branch/v"
        version "/strelka_workflow-" version ".tar.gz"))
      (sha256
        (base32 "0f9g2pkr1f7s4r8sxl53jxr2cjpyx53zf3va0jj8fxzavxiwmbmk"))
      (patches (list (search-patch "strelka-disable-tests.patch")
                     (search-patch "strelka-disable-install.patch")))))
    (propagated-inputs
     `(("vcftools" ,vcftools)
       ("htslib" ,htslib)))))

(define-public codemin
  (package
   (name "codemin")
   (version "1.0.5")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Illumina/strelka/raw/"
                  "5a993884687f2d92f794109e171d0bdeb95e504d"
                  "/redist/CodeMin-1.0.5.tar.bz2"))
            (sha256
             (base32 "1y8wsli1q626i80p3dmrc65p77ch164hj2sbxv497i9y89kvk35s"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((include-dir (string-append
                                (assoc-ref outputs "out") "/include")))
              (mkdir-p include-dir)
              (copy-recursively "include" include-dir)))))))
   (home-page "https://github.com/Illumina/strelka/tree/master/redist")
   (synopsis "Set of lightweight minimization functions.")
   (description "The CodeMin minimization library provides a set of lightweight
minimization functions originally developed for the CodeAxe phylogenetic
analysis package.")
   ;; MIT license.
   (license license:expat)))

(define-public strelka-2.9.2
  (package
   (name "strelka")
   (version "2.9.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Illumina/strelka/releases/download/v"
                  version "/strelka-" version ".release_src.tar.bz2"))
            (sha256
             (base32 "19bq2wzlxmnv8rx112y8z0sfvgsajnd0m945njmfy9p170qjqr27"))
            (patches
             (list (search-patch "strelka2-unbundle-dependencies.patch")))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'unbundle-dependencies
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "redist/CMakeLists.txt"
              ;; HTSlib
              (("superset\\(HTSLIB_DIR \"\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/\\$\\{HTSLIB_PREFIX\\}\"\\)")
               (format #f "superset(HTSLIB_DIR \"~a/bin\")" (assoc-ref inputs "htslib")))
              (("superset\\(HTSLIB_LIBRARY \"\\$\\{HTSLIB_DIR\\}/libhts.a\"\\)")
               (format #f "superset(HTSLIB_LIBRARY \"~a/lib/libhts.so\")"
                       (assoc-ref inputs "htslib")))
              ;; SAMtools
              (("set\\(SAMTOOLS_DIR \"\\$\\{CMAKE_CURRENT_BINARY_DIR}/\\$\\{SAMTOOLS_PREFIX\\}\"\\)")
               (format #f "set(SAMTOOLS_DIR \"~a/bin\")"
                       (assoc-ref inputs "samtools")))
              (("set\\(SAMTOOLS_LIBRARY \"\\$\\{SAMTOOLS_DIR\\}/libbam.a\"\\)")
               (format #f "set(SAMTOOLS_LIBRARY \"~a/lib/libbam.a\")"
                       (assoc-ref inputs "samtools"))))))
        (add-after 'install 'install-shared-libraries
          (lambda* (#:key inputs outputs  #:allow-other-keys)
            (let ((libdir (string-append (assoc-ref outputs "out") "/lib")))
              (mkdir-p libdir)
              (map (lambda (file)
                     (copy-file file (string-append libdir "/" (basename file))))
                   (find-files "." "\\.so")))))
        (add-after 'install 'patch-python-bin
          (lambda* (#:key inputs outputs  #:allow-other-keys)
            (let ((patch-path (string-append (assoc-ref outputs "out") "/lib/python")))
              (substitute* (list (string-append patch-path "/makeRunScript.py")
                                 (string-append patch-path "/pyflow/pyflow.py"))
                (("/usr/bin/env python")
                 (string-append (assoc-ref inputs "python") "/bin/python")))))))))
   (inputs
    `(("boost" ,boost)
      ("perl" ,perl)
      ("bash" ,bash)
      ("zlib" ,zlib)
      ("samtools" ,samtools)
      ("rapidjson" ,rapidjson)
      ("codemin" ,codemin)
      ("curl" ,curl)
      ("xz" ,xz)
      ("openssl" ,openssl)
      ("samtools" ,samtools)
      ("zlib" ,zlib)
      ("python" ,python)))
   (native-inputs
    `(("bash" ,bash)
      ("python" ,python-2)
      ("doxygen" ,doxygen)
      ("graphviz" ,graphviz)))
   (propagated-inputs
    `(("vcftools" ,vcftools)
      ("htslib" ,htslib)))
   (native-search-paths (package-native-search-paths perl))
   (home-page "https://github.com/Illumina/strelka")
   (synopsis "Small variant caller")
   (description "Strelka2 is a fast and accurate small variant caller optimized
for analysis of germline variation in small cohorts and somatic variation in
tumor/normal sample pairs.  The germline caller employs an efficient tiered
haplotype model to improve accuracy and provide read-backed phasing, adaptively
selecting between assembly and a faster alignment-based haplotyping approach at
each variant locus.  The germline caller also analyzes input sequencing data
using a mixture-model indel error estimation method to improve robustness to
indel noise.  The somatic calling model improves on the original Strelka method
for liquid and late-stage tumor analysis by accounting for possible tumor cell
contamination in the normal sample.  A final empirical variant re-scoring step
using random forest models trained on various call quality features has been
added to both callers to further improve precision.")
   (license license:gpl3+)))

(define-public strelka strelka-2.9.2)

(define-public all-your-base
  (let ((commit "b44212c66fe95e0b5ca51bfefcbe4eaa2c178fc4"))
    (package
     (name "all-your-base")
     (version (string-append "2-" (string-take commit 7)))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/hyeshik/AYB2.git")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32 "165ncgmw189qw72kjzg08y704sb0a6fi1wihibl2wr30p5091mcb"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests? #f
        #:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'move-to-src-directory
            (lambda _
              (chdir "src")
              #t))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((out (string-append (assoc-ref %outputs "out")))
                     (bin (string-append out "/bin")))
                (mkdir-p bin)
                (install-file "../bin/AYB" bin)))))))
     (inputs
      `(("bzip2" ,bzip2)
        ("zlib" ,zlib)
        ("lapack" ,lapack)))
     (home-page "https://github.com/hyeshik/AYB2")
     (synopsis "Base caller for the Illumina platform")
     (description "This package contains a fork of the All Your Base base 
caller, specifically targeting the use for tailseeker2.")
     (license license:gpl3+))))

(define-public tailseeker2
  (package
   (name "tailseeker")
   (version "2.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/hyeshik/tailseeker.git")
                  (commit "0edfa971df6616d3268d25676d2922bc386143a8")))
            (file-name (git-file-name name version))
            (sha256
             (base32 "1767wv5jkhkyqy6z7w2kn4jgkak852yphfppgr8xwf3id30s8bdf"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'remove-pythonpath-hack
          (lambda _
            (substitute* "tailseeker/snakesupport.py"
              (("'export PYTHONPATH=\"\\{PYTHONPATH\\}\" '\n") ""))))
        (add-before 'build 'move-to-src
          (lambda* (#:key inputs #:allow-other-keys)
            (chdir "src")
            (setenv "CFLAGS" (string-append
                              "-I"
                              (assoc-ref inputs "python-numpy")
                              "/lib/python3.7/site-packages/numpy/core/include"))
            (substitute* "Makefile"
              (("CFLAGS=	-O3 -Wall -Werror") "CFLAGS=	-O2 -Wall"))
            (invoke "make")))
        (add-after 'install 'install-python-modules
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin-dir     (string-append out "/bin"))
                   (python-path (string-append out "/lib/python3.7/site-packages/tailseeker"))
                   (python-cmd  (string-append (assoc-ref inputs "python-3") "/bin/python3"))
                   (ayb-cmd     (string-append (assoc-ref inputs "all-your-base") "/bin/AYB"))
                   (bgzip-cmd   (string-append (assoc-ref inputs "htslib") "/bin/bgzip"))
                   (tabix-cmd   (string-append (assoc-ref inputs "htslib") "/bin/tabix"))
                   (snake-cmd   (string-append (assoc-ref inputs "snakemake") "/bin/snakemake"))
                   (share-dir   (string-append out "/share/tailseeker"))
                   (conf-dir    (string-append share-dir "/conf")))
              (for-each mkdir-p (list python-path share-dir conf-dir))
              (symlink python-path (string-append share-dir "/tailseeker"))
              (copy-recursively "../tailseeker" python-path)
              (install-file "../bin/sqi2fq" bin-dir)
              (install-file "../bin/tailseq-retrieve-signals" bin-dir)
              (substitute* (string-append out "/bin/tailseeker")
                (("TAILSEEKER_DIR = ") (string-append "TAILSEEKER_DIR = '" share-dir "' #")))
              (install-file "../conf/defaults.conf" conf-dir)
              (install-file "../conf/defaults-hiseq.conf" conf-dir)
              (install-file "../conf/defaults-miseq.conf" conf-dir)

              ;; XXX: This is a run-time specific setting for which no good default exists.
              ;;(substitute* (string-append conf-dir "/defaults-miseq.conf")
              ;;  (("_exp: [73, GTCAG, 1]") "_exp: [72, CCCCCCCCCC, 1]"))

              (call-with-output-file (string-append conf-dir "/paths.conf")
                (lambda (port)
                  (for-each (lambda (item) (format port "~a: ~a~%" (car item) (cdr item)))
                            `(("tailseeker" . ,share-dir)
                              ("python3"    . ,python-cmd)
                              ("bgzip"      . ,bgzip-cmd)
                              ("tabix"      . ,tabix-cmd)
                              ("AYB"        . ,ayb-cmd)
                              ("snakemake"  . ,snake-cmd)))))

              ;; Tailseeker doesn't care about UNIX filesystems and expects to find binaries
              ;; in its own directory.
              (mkdir-p (string-append share-dir "/bin"))
              (symlink (string-append bin-dir "/tailseq-retrieve-signals")
                       (string-append share-dir "/bin/tailseq-retrieve-signals"))
              (symlink (string-append bin-dir "/sqi2fq")
                       (string-append share-dir "/bin/sqi2fq"))

              ;; Same for scripts.
              (mkdir-p (string-append share-dir "/scripts"))
              (copy-recursively "../scripts" (string-append share-dir "/scripts"))))))))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (inputs
    `(("python-3" ,python-3)
      ("all-your-base" ,all-your-base)))
   (propagated-inputs
    `(("python-biopython" ,python-biopython)
      ("python-scikit-learn" ,python-scikit-learn)
      ("python-scipy" ,python-scipy)
      ("python-numpy" ,python-numpy)
      ("python-pandas" ,python-pandas)
      ("python-matplotlib" ,python-matplotlib)
      ("python-pyyaml" ,python-pyyaml)
      ("ghmm" ,ghmm)
      ("snakemake" ,snakemake)
      ("htslib" ,htslib)))
   (home-page "https://github.com/hyeshik/tailseeker/tree/tailseeker2")
   (synopsis "Measure poly-A tail lengths from Illumina sequencers")
   (description "This package contains software for measuring poly(A) tail
length and 3′-end modifications using a high-throughput sequencer.")
   (license license:expat)))

(define (varscan version commit hash)
  (let ((jar-file (string-append "varscan-" version ".jar")))
    (package
      (name "varscan")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/dkoboldt/varscan/raw/"
                      commit "/VarScan.v" version ".source.jar"))
                (sha256 (base32 hash))))
      (build-system ant-build-system)
      (arguments
       `(#:tests? #f ; No test target.
         #:phases
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda _
               (mkdir "source")
               (chdir "source")
               (and
                ;; Unpack the Java archive containing the source files.
                (zero? (system* "jar" "xf" (assoc-ref %build-inputs "source")))
                ;; Remove existing compiled output.
                (with-directory-excursion "net/sf/varscan/"
                  (for-each (lambda (file)
                              (unless (string= (string-take-right file 5) ".java")
                                (zero? (system* "rm" file))))
                            (find-files "." #:directories? #f))))))
           (replace 'build
             (lambda _
               ;; Keep a list of files to be included in the JAR.
               (let ((out-files '("META-INF/MANIFEST.MF"))
                     (sources-dir "net/sf/varscan/"))
                 (and
                  (with-directory-excursion sources-dir
                    (for-each
                     (lambda (file)
                       (when (string= (string-take-right file 5) ".java")
                         ;; Compile the source files.
                         (zero? (system* "javac" file))
                         ;; Add to list of files to be included in the JAR.
                         (set! out-files
                               (append
                                out-files
                                (list (string-append sources-dir
                                  (string-drop-right (string-drop file 2) 5)
                                  ".class"))))))
                     (find-files "." #:directories? #f)))
                  ;; Construct the Java archive.
                  (let ((params (append '("jar" "cfm" ,jar-file) out-files)))
                    (zero? (apply system* params)))))))
           (replace 'install
             (lambda _
               (let ((out (string-append (assoc-ref %outputs "out")
                                         "/share/java/varscan/")))
                 (install-file ,jar-file out)))))))
      (home-page "http://dkoboldt.github.io/varscan/")
      (synopsis "Variant detection in massively parallel sequencing data")
      (description "")
      ;; Free for non-commercial use by academic, government, and
      ;; non-profit/not-for-profit institutions
      (license license:non-copyleft))))

(define-public varscan-2.4.0
  (varscan "2.4.0" "ed3227992f31725548d6106dc7fcd0bd8879ff1e"
           "1qyl93awj31qg4pbwaicm5vgq4zv5b9aqa10dpna9qrvbcqfdz90"))

(define-public varscan-2.4.1
  (varscan "2.4.1" "91f116629b2addce523a2eabe118b1cd7a538444"
           "0y45ympkza7qwcbcisg006286pwjbr5978n03hx5nvl09f0mapk8"))

(define-public varscan-2.4.2
  (varscan "2.4.2" "18425ce00e3ced8afc624bd86de142b1cd1e0eb0"
           "14f7fp0yaj3lsif1dpjdci7kz3b2fd9qic3299a2bvgk3rv3lp6n"))

(define-public pindel
  (package
   (name "pindel")
   (version "0.2.5b8")
   (source (origin
     (method url-fetch)
     (uri (string-append "https://github.com/genome/pindel/archive/v"
                         version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "06bsf0psxwf7h5p3j97xkh9k5qrwhxh6xn942y1j1m2inyhgs8bz"))))
   (build-system gnu-build-system)
   (inputs
    `(("samtools" ,samtools)
      ("htslib" ,htslib)
      ("zlib" ,zlib)))
   (native-inputs
    `(("cppcheck" ,cppcheck)
      ("python" ,python-2)
      ("perl" ,perl)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'configure) ; There is no configure phase.
        ;; The build needs to run 'make' twice for the reasons described below.
        (replace 'build
          (lambda* (#:key inputs #:allow-other-keys)
            ;; The first run creates a Makefile.local file.  Make will report
            ;; the failure to find Makefile.local, but we can ignore this error,
            ;; since that file is created by this run.
            (system* "make" (string-append "SAMTOOLS=" (assoc-ref inputs "samtools")))
            ;; The second run actually compiles the program.  Now Makefile.local
            ;; is available, and we should treat an exiting make with an error as
            ;; a true error.
            (zero? (system* "make"))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "src/pindel" bin)
              (install-file "src/pindel2vcf" bin)
              (install-file "src/pindel2vcf4tcga" bin)
              (install-file "src/sam2pindel" bin))
            #t))
        ;; There are multiple test targets, so in order to run all
        ;; tests, we must run the separate make targets.
        (replace 'check
          (lambda* (#:key inputs #:allow-other-keys)
            (and
             (zero? (system* "make" "acceptance-tests"))
             (zero? (system* "make" "cppcheck"))
             ;; These tests take a _very_ long time.
             ;(zero? (system* "make" "coverage-tests"))
             ;;(zero? (system* "make" "functional-tests"))
             ;;(zero? (system* "make" "regression-tests"))
             ))))))
   (home-page "https://github.com/genome/pindel")
   (synopsis "Structural variants detector for next-gen sequencing data")
   (description "Pindel can detect breakpoints of large deletions, medium sized
insertions, inversions, tandem duplications and other structural variants at
single-based resolution from next-gen sequence data.  It uses a pattern growth
approach to identify the breakpoints of these variants from paired-end short
reads.")
   (license license:gpl3+)))

(define-public manta
  (package
   (name "manta")
   (version "1.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Illumina/manta/releases/download/v"
                  version "/manta-" version ".release_src.tar.bz2"))
            (file-name (string-append name "-" version ".tar.bz2"))
            (sha256
             (base32 "0d4fp0jq3b3d97jz81hv0kd3av12ycbjk28mirhbmwh32zm2d54k"))
            (patches (list (search-patch "manta-use-system-zlib.patch")
                           (search-patch "manta-use-system-htslib.patch")
                           (search-patch "manta-use-system-samtools.patch")))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        ;; The unit tests are written in a way that need the bundled
        ;; dependencies.  We took those out, so the unit tests fail.
        (add-before 'configure 'disable-failing-unit-tests
          (lambda _
            (for-each (lambda (directory-name)
                        (with-output-to-file (string-append
                                              "src/c++/lib/"
                                              directory-name
                                              "/test/CMakeLists.txt")
                          (lambda _ "# Disabled by the package recipe.")))
                      '("alignment"
                        "applications/GenerateSVCandidates"
                        "assembly"
                        "blt_util"
                        "common"
                        "htsapi"
                        "manta"
                        "svgraph"))))
        ;; The 'manta-use-system-samtools.patch' sets the samtools path to
        ;; '/usr/bin'.  This allows us to substitute it for the actual path
        ;; of samtools in the store.
        (add-before 'configure 'patch-samtools-path
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "redist/CMakeLists.txt"
                (("set\\(SAMTOOLS_DIR \"/usr/bin\"\\)")
                 (string-append "set(SAMTOOLS_DIR \""
                                (assoc-ref inputs "samtools") "/bin\")")))
            #t))
        (add-before 'configure 'use-dynamic-boost
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; By default, it looks for static libraries.  This substitution
            ;; makes sure it looks for dynamically linked versions of Boost.
            (substitute* "src/cmake/boost.cmake"
              (("Boost_USE_STATIC_LIBS ON")
               "Boost_USE_STATIC_LIBS OFF"))
            #t))
        (add-before 'configure 'fix-tool-paths
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "src/python/lib/mantaOptions.py"
              (("bgzipBin=joinFile\\(libexecDir,exeFile\\(\"bgzip\"\\)\\)")
               (string-append "bgzipBin=\"" (string-append
                                             (assoc-ref inputs "htslib")
                                             "/bin/bgzip") "\""))
              (("htsfileBin=joinFile\\(libexecDir,exeFile\\(\"htsfile\"\\)\\)")
               (string-append "htsfileBin=\"" (string-append
                                               (assoc-ref inputs "htslib")
                                               "/bin/htsfile") "\""))
              (("tabixBin=joinFile\\(libexecDir,exeFile\\(\"tabix\"\\)\\)")
               (string-append "tabixBin=\"" (string-append
                                           (assoc-ref inputs "htslib")
                                           "/bin/tabix" "\"")))
              (("samtoolsBin=joinFile\\(libexecDir,exeFile\\(\"samtools\"\\)\\)")
               (string-append "samtoolsBin=\"" (string-append
                                              (assoc-ref inputs "samtools")
                                              "/bin/samtools" "\""))))
            (substitute* '("src/python/lib/makeRunScript.py"
                           "src/demo/runMantaWorkflowDemo.py"
                           "src/python/bin/configManta.py"
                           "src/python/libexec/filterBam.py"
                           "src/python/libexec/sortVcf.py"
                           "src/python/libexec/mergeBam.py"
                           "src/python/libexec/extractSmallIndelCandidates.py"
                           "src/python/libexec/sortEdgeLogs.py"
                           "src/python/libexec/vcfCmdlineSwapper.py"
                           "src/python/libexec/denovo_scoring.py"
                           "src/python/libexec/cat.py"
                           "src/python/libexec/mergeChromDepth.py"
                           "src/python/libexec/ploidyFilter.py"
                           "src/python/libexec/sortBam.py"
                           "src/python/lib/makeRunScript.py"
                           "src/srcqc/run_cppcheck.py")
                         (("/usr/bin/env python") (string-append
                                                   (assoc-ref inputs "python")
                                                   "/bin/python")))
            #t))
        (add-after 'install 'fix-pyflow-shebang
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* (string-append (assoc-ref outputs "out")
                                        "/lib/python/pyflow/pyflow.py")
              (("#!/usr/bin/env python")
               (string-append "#!" (assoc-ref inputs "python")
                              "/bin/python")))
            #t)))))
    (inputs
     `(("cmake" ,cmake)
       ("boost" ,boost)
       ("pyflow" ,pyflow-2)
       ("python" ,python-2)
       ("cppcheck" ,cppcheck)
       ("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("htslib" ,htslib)
       ;; The command-line interface has changed between 1.2 and 1.5.
       ;; Manta expects the command-line interface of 1.2.
       ("samtools" ,samtools-1.2)
       ("zlib" ,zlib)
       ("bash" ,bash)))
    (home-page "https://github.com/Illumina/manta")
   (synopsis "Structural variant and indel caller for mapped sequencing data")
   (description "Manta calls structural variants (SVs) and indels from mapped
paired-end sequencing reads.  It is optimized for analysis of germline variation
in small sets of individuals and somatic variation in tumor/normal sample pairs.
Manta discovers, assembles and scores large-scale SVs, medium-sized indels and
large insertions within a single efficient workflow.")
   (license license:gpl3)))

(define-public iq-tree
  (package
    (name "iq-tree")
    (version "1.6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Cibiv/IQ-TREE/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "11528sxv9hkmg20r2fy9zyq0rylbrhcjnh6cr61dwmx1wwnhj54n"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DIQTREE_FLAGS=omp")
       #:tests? #f))
    (inputs
     `(("eigen" ,eigen)
       ("zlib" ,zlib)))
    (home-page "http://www.iqtree.org/")
    (synopsis "Efficient software for phylogenomic inference")
    (description
     "This package provides software for phylogenomic inference.")
    (license license:gpl2)))

(define-public cat
  (package
   (name "cat")
   (version "5.0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/dutilh/CAT/archive/v"
                  version".tar.gz"))
            (sha256
             (base32
              "1gbq4vj0i7srylvb56ipmzcnh8zk34kfmyx76fbxh2cljp0p4k7x"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                  (share (string-append (assoc-ref outputs "out") "/share/CAT")))
              (mkdir-p bin)
              (mkdir-p share)
              (with-directory-excursion "CAT_pack"
                (install-file "CAT" bin)

                ;; Don't pollute the "bin" directory with Python libraries.
                (map (lambda (file)
                       (when (string-suffix? ".py" file)
                         (install-file file share)))
                     (find-files "."))

                ;; Make sure CAT can find its Python libraries.
                (wrap-program (string-append bin "/CAT")
                 `("PYTHONPATH" ":" = (,share "$PYTHONPATH"))))))))))
   (inputs
    `(("diamond" ,diamond)
      ("prodigal" ,prodigal)
      ("python" ,python)))
   (home-page "https://github.com/dutilh/CAT")
   (synopsis "Tool for taxonomic classification of contigs and metagenome-assembled genomes")
   (description "Contig Annotation Tool (CAT) and Bin Annotation Tool (BAT)
are pipelines for the taxonomic classification of long DNA sequences and
metagenome assembled genomes (MAGs/bins) of both known and (highly) unknown
microorganisms, as generated by contemporary metagenomics studies.  The core
algorithm of both programs involves gene calling, mapping of predicted ORFs
against the nr protein database, and voting-based classification of the entire
contig / MAG based on classification of the individual ORFs.  CAT and BAT can
be run from intermediate steps if files are formated appropriately")
   (license license:expat)))

(define-public clinvar
  (package
   (name "clinvar-vcf")
   (version "GRCh38-20200316")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/vcf_GRCh38/clinvar.vcf.gz"))
            (sha256
             (base32
	      "0pidjv3bf0ckf8wc3nw7zlvzyrh428xskkhz51y7xbbf6pdw0wdp"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let ((source-file (assoc-ref %build-inputs "source"))
              (output-dir (string-append %output "/share/clinvar/GRCh38")))
          (mkdir-p output-dir)
          (copy-file source-file
                     (string-append output-dir "/clinvar.vcf.gz"))))))
   (home-page "https://www.ncbi.nlm.nih.gov/clinvar/")
   (synopsis "Public archive of reports of human genetic variation")
   (description "ClinVar is a freely accessible, public archive of reports
of the relationships among human variations and phenotypes, with supporting
evidence.  ClinVar thus facilitates access to and communication about the
relationships asserted between human variation and observed health status,
and the history of that interpretation.  ClinVar processes submissions
reporting variants found in patient samples, assertions made regarding their
clinical significance, information about the submitter, and other supporting
data.  The alleles described in submissions are mapped to reference sequences,
and reported according to the HGVS standard.  ClinVar then presents the data
for interactive users as well as those wishing to use ClinVar in daily
workflows and other local applications.  ClinVar works in collaboration with
interested organizations to meet the needs of the medical genetics community
as efficiently and effectively as possible.")
   (license #f)))

(define-public clinvar-grch37
  (package (inherit clinvar)
    (version "GRCh37-20200316")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/vcf_GRCh37/clinvar.vcf.gz"))
             (sha256
              (base32
               "1kvvj5i14xc1w7v5pb5x0rlkj1mixv1apa9m4nqg501schavmih1"))))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let ((source-file (assoc-ref %build-inputs "source"))
              (output-dir (string-append %output "/share/clinvar/GRCh37")))
          (mkdir-p output-dir)
          (copy-file source-file
                     (string-append output-dir "/clinvar.vcf.gz"))))))))

(define-public dbsnp
  (package
    (name "dbsnp")
    (version "human_9606")
    (source (origin
              (method url-fetch)
              (uri ;"ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606/"
                   ;"VCF/00-All.vcf.gz"
                   "https://www.roelj.com/00-All.vcf.gz")
              (sha256
               (base32
                "0f2zzi0br0c1dvlx6wfgfm6f7rgp0kb19gb6p0kxzbs3n92viiqa"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source-file (assoc-ref %build-inputs "source"))
                (output-dir  (string-append %output "/share/dbsnp"))
                (output-file (string-append output-dir "/dbSnp.vcf.gz")))
           (mkdir-p output-dir)
           (copy-file source-file output-file)
           (symlink output-file (string-append output-dir "/00-All.vcf.gz"))))))
    (home-page "https://www.ncbi.nlm.nih.gov/projects/SNP/")
    (synopsis "Short genetic variations")
    (description "")
    (license #f)))

(define-public 1000genomes-phase1-indels
  (package
    (name "1000genomes-phase1-indels")
    (version "b37")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://"
                                  "gsapubftp-anonymous@"
                                  "ftp.broadinstitute.org/bundle/b37/"
                                  "1000G_phase1.indels.b37.vcf.gz"))
              (sha256
               (base32 "173kkmyvyvfa55v2rbpywsrp7159yyl1sx30y243jkxzkjrgc7bc"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source-file (assoc-ref %build-inputs "source"))
                (output-dir (string-append %output "/share/1000G"))
                (output-file-uncompressed (string-append output-dir
                                            "/1000G_phase1.indels.b37.vcf"))
                (output-file (string-append output-file-uncompressed ".gz"))
                (java (string-append (assoc-ref %build-inputs "icedtea")
                                     "/bin/java"))
                (igvtools (string-append (assoc-ref %build-inputs "igvtools")
                                         "/share/java/igvtools/igvtools.jar"))
                (path (string-append (assoc-ref %build-inputs "htslib") "/bin:"
                                     (assoc-ref %build-inputs "gzip") "/bin")))
           ;; The gunzip command needs to find gzip in PATH.
           (setenv "PATH" path)
           (mkdir-p output-dir)
           (copy-file source-file output-file)

           ;; To create the index, we need to compress the VCF file with
           ;; bgzip, instead of the regular gzip.
           (system* "gunzip" output-file)
           (system* "bgzip" output-file-uncompressed)

           ;; Finally, we can index the file using igvtools.
           (system* java "-jar" igvtools "index" output-file)))))
    (inputs
     `(("icedtea" ,icedtea-7)
       ("igvtools" ,igvtools-bin-2.3.71)
       ("htslib" ,htslib)
       ("gzip" ,gzip)))
    (home-page "http://www.internationalgenome.org/")
    (synopsis "Initial map of insertions and deletions in the human genome")
    (description "")
    (license #f)))

(define-public mills-1000G-gold-standard-indels
  (package
    (name "1000genomes-mills-gold-standard-indels")
    (version "b37")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://"
                                  "gsapubftp-anonymous@"
                                  "ftp.broadinstitute.org/bundle/b37/"
                                  "Mills_and_1000G_gold_standard.indels.b37.vcf.gz"))
              (sha256
               (base32 "1n9bf6chfr9pxhk0mfiiqy28pmkyb0xpxz0rwvwrw031cw39dc1l"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source-file (assoc-ref %build-inputs "source"))
                (output-dir (string-append %output "/share/1000G"))
                (output-file-wo-ext
                 (string-append output-dir
                                "/Mills_and_1000G_gold_standard.indels.b37"))
                (bcf-output-file (string-append output-file-wo-ext ".bcf"))
                (output-file-uncompressed (string-append output-file-wo-ext ".vcf"))
                (output-file (string-append output-file-uncompressed ".gz"))
                (java (string-append (assoc-ref %build-inputs "icedtea")
                                     "/bin/java"))
                (igvtools (string-append (assoc-ref %build-inputs "igvtools")
                                         "/share/java/igvtools/igvtools.jar"))
                (path (string-append (assoc-ref %build-inputs "htslib") "/bin:"
                                     (assoc-ref %build-inputs "gzip") "/bin:"
                                     (assoc-ref %build-inputs "bcftools") "/bin:"
                                     (assoc-ref %build-inputs "grep") "/bin")))

           ;; The gunzip command needs to find gzip in PATH.
           (setenv "PATH" path)
           (mkdir-p output-dir)
           (copy-file source-file output-file)

           ;; To create the index, we need to compress the VCF file with
           ;; bgzip, instead of the regular gzip.
           (system* "gunzip" output-file)
           (chmod output-file-uncompressed #o644)

           ;; The "vcf" file seems to be actually a "bcf" file.  We can use bcftools to
           ;; convert it to a VCF file.
           (rename-file output-file-uncompressed bcf-output-file)
           (system (string-append "bcftools view "
                                  bcf-output-file
                                  " | grep -v bcftools_view > "
                                  output-file-uncompressed))

           (system* "bgzip" output-file-uncompressed)
           (delete-file bcf-output-file)

           ;; Finally, we can index the file using igvtools.
           (system* java "-jar" igvtools "index" output-file)))))
    (inputs
     `(("icedtea" ,icedtea-7)
       ("igvtools" ,igvtools-bin-2.3.71)
       ("htslib" ,htslib)
       ("gzip" ,gzip)
       ("bcftools" ,bcftools)
       ("grep" ,grep)))
    (home-page "http://www.internationalgenome.org/")
    (synopsis "Initial map of insertions and deletions in the human genome")
    (description "")
    (license #f)))

(define-public dbsnp-138
  (package
    (name "dbsnp")
    (version "138-b37")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://"
                                  "gsapubftp-anonymous@"
                                  "ftp.broadinstitute.org/bundle/b37/"
                                  "dbsnp_138.b37.vcf.gz"))
              (sha256
               (base32 "0c7i6qw6j6chhqni826jr98b4kfjg72mql36wdfydiiv7679zx5n"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source-file (assoc-ref %build-inputs "source"))
                (output-dir (string-append %output "/share/1000G"))
                (output-file-uncompressed (string-append output-dir
                                            "/dbsnp_138.b37.vcf"))
                (output-file (string-append output-file-uncompressed ".gz"))
                (java (string-append (assoc-ref %build-inputs "icedtea")
                                     "/bin/java"))
                (igvtools (string-append (assoc-ref %build-inputs "igvtools")
                                         "/share/java/igvtools/igvtools.jar"))
                (path (string-append (assoc-ref %build-inputs "htslib") "/bin:"
                                     (assoc-ref %build-inputs "gzip") "/bin")))
           ;; The gunzip command needs to find gzip in PATH.
           (setenv "PATH" path)
           (mkdir-p output-dir)
           (copy-file source-file output-file)

           ;; To create the index, we need to compress the VCF file with
           ;; bgzip, instead of the regular gzip.
           (system* "gunzip" output-file)
           (system* "bgzip" output-file-uncompressed)

           ;; Finally, we can index the file using igvtools.
           (system* java "-jar" igvtools "index" output-file)))))
    (inputs
     `(("icedtea" ,icedtea-7)
       ("igvtools" ,igvtools-bin-2.3.71)
       ("htslib" ,htslib)
       ("gzip" ,gzip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public dx-tracks
  (package
    (name "dx-tracks")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/UMCUGenetics/Dx_tracks/releases/"
                    "download/v" version "/v" version ".tar.gz"))
              (sha256
               (base32 "0vcyd888yq6qqal5n9l5g361nzx3wq70zlbn9bhza2qkhfd3n5pp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (input-file (assoc-ref %build-inputs "source"))
               (output-dir (string-append %output "/share/data/dx-tracks"))
               (PATH (string-append (assoc-ref %build-inputs "gzip") "/bin")))
           (setenv "PATH" PATH)
           (mkdir-p output-dir)
           (with-directory-excursion output-dir
             (system* tar "-xvf" input-file "--strip-components=1"))))))
    (inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "https://github.com/UMCUGenetics/Dx_tracks")
    (synopsis "")
    (description "")
    ;; The files are licensed CC-BY-ND.  The NoDerivatives clause makes it
    ;; non-free, and therefore, the license cannot be added to Guix upstream.
    (license #f)))

(define-public dbnsfp
  (package
    (name "dbnsfp")
    (version "2.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://dbnsfp:dbnsfp@dbnsfp.softgenetics.com/dbNSFPv"
                    version ".zip"))
              (sha256
               (base32
                "132z7rayqdwc04b8bw19amvwyhg67vyscyv1zrb486r49icf73mz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source-file (assoc-ref %build-inputs "source"))
               (output-dir  (string-append %output "/share/dbnsfp"))
               (unzip       (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
               (gzip        (string-append (assoc-ref %build-inputs "gzip") "/bin/gzip")))
           (mkdir-p output-dir)
           (with-directory-excursion output-dir
             (system* unzip source-file)
             (for-each (lambda (file)
                         (format #t "Compressing ~s~%" file)
                         (system* gzip file))
                       (find-files output-dir)))))))
    (inputs
     `(("unzip" ,unzip)
       ("gzip" ,gzip)))
    (home-page "https://sites.google.com/site/jpopgen/dbNSFP")
    (synopsis "Database for functional prediction of non-synonymous SNPs")
    (description " dbNSFP is a database developed for functional prediction and
annotation of all potential non-synonymous single-nucleotide variants (nsSNVs)
in the human genome.")
    (license #f)))

(define-public giab-na12878-high-confidence-regions
  (package
    (name "giab-na12878-high-confidence-regions")
    (version "NISTv3.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp-trace.ncbi.nlm.nih.gov/giab/ftp/release/"
                    "NA12878_HG001/" version "/NA12878_GIAB_highconf_IllFB"
                    "-IllGATKHC-CG-Ion-Solid_ALLCHROM_v3.2.2_highconf.bed"))
              (sha256
               (base32 "1adj878im498lfplklkir7v2chv1bxamgw3y2a62599wvbhap79q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source-file (assoc-ref %build-inputs "source"))
               (output-dir (string-append %output "/share/giab")))
           (mkdir-p output-dir)
           (copy-file source-file
                      (string-append output-dir "/NA12878_GIAB_highconf_IllFB"
                                     "-IllGATKHC-CG-Ion-Solid_ALLCHROM_v3.2.2"
                                     "_highconf.bed"))))))
    (home-page "http://jimb.stanford.edu/giab")
    (synopsis "")
    (description "")
    (license #f)))

(define-public gwascatalog
  (package
   (name "gwascatalog")
   (version "GRCh37")
   (source (origin
            (method url-fetch)
            ;(uri "http://www.genome.gov/admin/gwascatalog.txt")
            (uri "http://www.roelj.com/gwascatalog.txt")
            (sha256
             (base32
              "137xb3r3w6k8syj6dh6a856fvszcjlylwpzp98m35w5q52vxhdnx"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let ((source-file (assoc-ref %build-inputs "source"))
              (output-dir (string-append %output "/share/gwascatalog")))
          (mkdir-p output-dir)
          (copy-file source-file
                     (string-append output-dir "/gwascatalog.txt"))))))
   (home-page "http://www.genome.gov/")
   (synopsis "Extra data sets used by snpEff.")
   (description "This package contains extra data sets used by snpEff.")
   (license #f)))

;; Ensembl provides the GRCh37 separated by chromosome.  This function
;; can be used as a template for each separate file.
(define (ensembl-grch37-dna-chromosome chromosome hash)
  (package
    (name (string-append "ensembl-grch37-dna-chr" chromosome))
    (version "88")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.ensembl.org/pub/grch37/release-" version "/fasta"
                    "/homo_sapiens/dna/Homo_sapiens.GRCh37.dna.chromosome"
                    "." chromosome ".fa.gz"))
              (sha256
               (base32 hash))))
    (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let ((genomes-dir (string-append
                                      %output "/share/genomes/ensembl"
                                      "/per-chromosome"))
                        (source (assoc-ref %build-inputs "source")))
                    (mkdir-p genomes-dir)
                    (copy-file source
                               (string-append
                                genomes-dir
                                "/Homo_sapiens.GRCh37.dna.chromosome."
                                ,chromosome ".fa.gz"))))))
   (native-inputs `(("source" ,source)))
   (home-page "http://www.ensembl.org")
   (synopsis (string-append
              "Genome Reference Consortium Human reference genome 37, "
              "chromosome " chromosome))
   (description (string-append
                 "This package contains the data for GRCh37 chromosome "
                 chromosome "."))
   ;; I couldn't find licensing information.
   (license #f)))

(define-public ensembl-grch37-dna-chromosome-1
  (ensembl-grch37-dna-chromosome
   "1" "15mbgahavid28ch4n0lnccdndg9dl4mhaq0dv3bmzc3qyag364rg"))

(define-public ensembl-grch37-dna-chromosome-2
  (ensembl-grch37-dna-chromosome
   "2" "0kp4d7asgmky8wa7pk9cn2fp49zyznq1zbabwl0hl66pj4gis4y4"))

(define-public ensembl-grch37-dna-chromosome-3
  (ensembl-grch37-dna-chromosome
   "3" "1hkffvs1mwlh34jnwnm8is7xaa826fw8zv1hx5mfkbnnn9n92sj0"))

(define-public ensembl-grch37-dna-chromosome-4
  (ensembl-grch37-dna-chromosome
   "4" "0c652i45n9bnz7xqhs9in7c08rmi3wy44398zh2d5p9iq13ysk34"))

(define-public ensembl-grch37-dna-chromosome-5
  (ensembl-grch37-dna-chromosome
   "5" "0bg86m9igrm744phwgvdmfgihf81jflarbv706vci4nrnqb7gsqi"))

(define-public ensembl-grch37-dna-chromosome-6
  (ensembl-grch37-dna-chromosome
   "6" "1zmsnfm9sjc21h4q4j4bjbmmaw3lj5nwzizgma1lwq68yhgm0bx5"))

(define-public ensembl-grch37-dna-chromosome-7
  (ensembl-grch37-dna-chromosome
   "7" "1mfjwrb27s6l57abs4cgcl90w7invdm9lg1j2z26ci5k01m0phhq"))

(define-public ensembl-grch37-dna-chromosome-8
  (ensembl-grch37-dna-chromosome
   "8" "1l0g83b3z2c1qla04prgcfwb5bcc7k9lwvcssvdg9p9kd5zxxjzq"))

(define-public ensembl-grch37-dna-chromosome-9
  (ensembl-grch37-dna-chromosome
   "9" "0k7kjhjasw9lshykdlga30cvc69nxaiah1d9hahhwk9zhdmjxbsx"))

(define-public ensembl-grch37-dna-chromosome-10
  (ensembl-grch37-dna-chromosome
   "10" "1420vyjspx3wbjnybz0nrh9aixg89k9xag3y1z5bhcb6s24js10q"))

(define-public ensembl-grch37-dna-chromosome-11
  (ensembl-grch37-dna-chromosome
   "11" "0cry7ln0j4k6c8adlxr1dw4x9pgkbi8nv61iysyjnwp2rd1zpbs4"))

(define-public ensembl-grch37-dna-chromosome-12
  (ensembl-grch37-dna-chromosome
   "12" "0iflw5c2y826wbb7gdhia94db3kz6n08nbbifihblfrb7181akji"))

(define-public ensembl-grch37-dna-chromosome-13
  (ensembl-grch37-dna-chromosome
   "13" "1x21y3arw9i2gm0wdbpv88sgjydrrdwrvrgf4ih4y83z64528iav"))

(define-public ensembl-grch37-dna-chromosome-14
  (ensembl-grch37-dna-chromosome
   "14" "0vfnvpbkg3s0b9d3r2bg493wlbiqxaa747zljx5sdsgh8ny8m142"))

(define-public ensembl-grch37-dna-chromosome-15
  (ensembl-grch37-dna-chromosome
   "15" "1xki8vz14ia95xf6w3qghybs76n5j0kn21871b9d0gy045lz4xlc"))

(define-public ensembl-grch37-dna-chromosome-16
  (ensembl-grch37-dna-chromosome
   "16" "1qn4qys9q945ia90q6g5nv8zhndx7yis692rznf2wyzglyagibhm"))

(define-public ensembl-grch37-dna-chromosome-17
  (ensembl-grch37-dna-chromosome
   "17" "0sjw1vj9hliyih7d18iigdfqzylp945aidsnf4h2424zcg7ininf"))

(define-public ensembl-grch37-dna-chromosome-18
  (ensembl-grch37-dna-chromosome
   "18" "0fis1pdjz4a996nd4l399d4sgnv7y33xd7i9f7l9vms56bg82i1d"))

(define-public ensembl-grch37-dna-chromosome-19
  (ensembl-grch37-dna-chromosome
   "19" "1c6xv1pwknqh48nc5yja5b4sc0b05a32q6hfp2q30zh6xv6y9b3g"))

(define-public ensembl-grch37-dna-chromosome-20
  (ensembl-grch37-dna-chromosome
   "20" "1h93w6lh5hgdz8dcfk8slw9khv3gri59dp5dsl2plw5xb4pava37"))

(define-public ensembl-grch37-dna-chromosome-21
  (ensembl-grch37-dna-chromosome
   "21" "16j7iqflirlf8kwargarijkmhsb4c9rh7l7agylp4amnvv7rlbir"))

(define-public ensembl-grch37-dna-chromosome-22
  (ensembl-grch37-dna-chromosome
   "22" "1xrlwaxig47nrnligasc5yw94yklm22dsg8zdj9kady0yd9dii52"))

(define-public ensembl-grch37-dna-chromosome-x
  (ensembl-grch37-dna-chromosome
   "X" "0apg4g2g1qh26f44a2brgcb3cwk59pbl9van5nwqxbkbgnb7m2yr"))

(define-public ensembl-grch37-dna-chromosome-y
  (ensembl-grch37-dna-chromosome
   "Y" "16gf1kipmns96dk7hz2mf6q2znd4d8kbjxd2cw45m71l6nirqr8n"))

(define-public ensembl-grch37-dna-chromosome-mt
  (ensembl-grch37-dna-chromosome
   "MT" "12pb4nv9nqzrwpbgllqlkl97f8ypawiqddrcnxrr2ziq91z25ngc"))


;; Ensembl provides the GRCh38 separated by chromosome.  This function
;; can be used as a template for each separate file.
(define (ensembl-grch38-dna-chromosome chromosome hash)
  (package
    (name (string-append "ensembl-grch38-dna-chr" chromosome))
    (version "88")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.ensembl.org/pub/release-" version "/fasta"
                    "/homo_sapiens/dna/Homo_sapiens.GRCh38.dna.chromosome"
                    "." chromosome ".fa.gz"))
              (sha256
               (base32 hash))))
    (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let ((genomes-dir (string-append
                                      %output "/share/genomes/ensembl"
                                      "/per-chromosome"))
                        (source (assoc-ref %build-inputs "source")))
                    (mkdir-p genomes-dir)
                    (copy-file source
                               (string-append
                                genomes-dir
                                "/Homo_sapiens.GRCh38.dna.chromosome."
                                ,chromosome ".fa.gz"))))))
   (native-inputs `(("source" ,source)))
   (home-page "http://www.ensembl.org")
   (synopsis (string-append
              "Genome Reference Consortium Human reference genome 38, "
              "chromosome " chromosome))
   (description (string-append
                 "This package contains the data for GRCh38 chromosome "
                 chromosome "."))
   ;; I couldn't find licensing information.
   (license #f)))
   
(define-public ensembl-grch38-dna-chromosome-1
  (ensembl-grch38-dna-chromosome
   "1" "1cfc1dlwawhd8g4k8hw6450wibs84zmd8rj81hb5wgbnn06qxck8"))

(define-public ensembl-grch38-dna-chromosome-2
  (ensembl-grch38-dna-chromosome
   "2" "14453v8in8kkrhiffygimi15a7ibwzl8c7mkvf5chgh5657x2c7y"))

(define-public ensembl-grch38-dna-chromosome-3
  (ensembl-grch38-dna-chromosome
   "3" "03zk59qypqk11gs9masm8nmk23rgj1cggv3m47jhf8rf0qsra6vc"))

(define-public ensembl-grch38-dna-chromosome-4
  (ensembl-grch38-dna-chromosome
   "4" "098pvn4anlk9kp902zk6shbqzimqax2qqwyxzikvx2fcgs7aj7nh"))

(define-public ensembl-grch38-dna-chromosome-5
  (ensembl-grch38-dna-chromosome
   "5" "0mrmiwb76qpqm7swa6pfdwsqgdfp0kv01jjxwb3rzqv11shfxswv"))

(define-public ensembl-grch38-dna-chromosome-6
  (ensembl-grch38-dna-chromosome
   "6" "0wqcm9dpvb8gzjmxl07bg2k8nigq2g05563zyrzh5cv1m3sgc98w"))

(define-public ensembl-grch38-dna-chromosome-7
  (ensembl-grch38-dna-chromosome
   "7" "1pwd054bjg9nbnh0lhgw1786nz0j0zx17cz5karjhs4xwbfps0vx"))

(define-public ensembl-grch38-dna-chromosome-8
  (ensembl-grch38-dna-chromosome
   "8" "11qh05jsxqm7fcc4r614v81dhi4hx9v4l6wcl67w0hvnhi3qk4fn"))

(define-public ensembl-grch38-dna-chromosome-9
  (ensembl-grch38-dna-chromosome
   "9" "1y7hsj6mb50s7997iy8jraf74j799j2vn3pf4xx68x953hf6cgyb"))

(define-public ensembl-grch38-dna-chromosome-10
  (ensembl-grch38-dna-chromosome
   "10" "08yxkb7hlv70i6nbm7ndh0rlvxw6fmykxky89cfhnbw82zcnaa2j"))

(define-public ensembl-grch38-dna-chromosome-11
  (ensembl-grch38-dna-chromosome
   "11" "0pyh06c85g9yr4xv9m8ckvqv7q0xw3krjw669y006ram05xf9mq8"))

(define-public ensembl-grch38-dna-chromosome-12
  (ensembl-grch38-dna-chromosome
   "12" "0hvdlkpsmmxszyza6pi1i13p719rf5cj4rjlmnrjm4fjj86s87w4"))

(define-public ensembl-grch38-dna-chromosome-13
  (ensembl-grch38-dna-chromosome
   "13" "0x5fw9x2z87c2c7zs2xhgykqvf2p44p5c5d8aqqqc6x80zjjhvwq"))

(define-public ensembl-grch38-dna-chromosome-14
  (ensembl-grch38-dna-chromosome
   "14" "1xq3993zkjkb9jlrpkc8cmxalxkgldaav3jvig3mm6j819b0h4yj"))

(define-public ensembl-grch38-dna-chromosome-15
  (ensembl-grch38-dna-chromosome
   "15" "1f4irm483cgxllqck1ax7yfx3a7pnqxqxcald3ja5hmzsxghx4pn"))

(define-public ensembl-grch38-dna-chromosome-16
  (ensembl-grch38-dna-chromosome
   "16" "1y0zpjwxvcz7pabi0qwrqi05iipgpdn1rvl1rmynm7idaxrhp5bi"))

(define-public ensembl-grch38-dna-chromosome-17
  (ensembl-grch38-dna-chromosome
   "17" "0l73l44b4a44s3qbd6hidvdynmgmhznzc2raffvg172zbl6wnlgq"))

(define-public ensembl-grch38-dna-chromosome-18
  (ensembl-grch38-dna-chromosome
   "18" "1yzlr2707qvpv5mhmsiv50kcansfaxnbd4giaars5sk7ilwwai6i"))

(define-public ensembl-grch38-dna-chromosome-19
  (ensembl-grch38-dna-chromosome
   "19" "12d8g93y27xhav8mm8gslp48jrkiya9fn36jjq18xkqbj971ilm6"))

(define-public ensembl-grch38-dna-chromosome-20
  (ensembl-grch38-dna-chromosome
   "20" "00d8j47i72ii54xjgnpyv2pryyxr69b7rd3sb1h7i7yhwkw3spsa"))

(define-public ensembl-grch38-dna-chromosome-21
  (ensembl-grch38-dna-chromosome
   "21" "1rnfhsr81wg9p98mcfk8mqr9frhvir3b787zi0dw20y2iyagabhz"))

(define-public ensembl-grch38-dna-chromosome-22
  (ensembl-grch38-dna-chromosome
   "22" "1bqpy3zp2fpwzisrvs31rmjl845sd8n2jimfil2jiz0bzih7k55w"))

(define-public ensembl-grch38-dna-chromosome-x
  (ensembl-grch38-dna-chromosome
   "X" "0mdclb5zhl2kcjzrgm4bbpa96wg9h3a5352b6snsck900jdav13s"))

(define-public ensembl-grch38-dna-chromosome-y
  (ensembl-grch38-dna-chromosome
   "Y" "0lgddp73jpi6a8bnjnjn5pba4cd01vfrlxkhl5bk62shvl92621x"))

(define-public ensembl-grch38-dna-chromosome-mt
  (ensembl-grch38-dna-chromosome
   "MT" "0f9gy1aqracnpqpvdyx99nkkac2gkm3sap5k2naxjpw35r4yms10"))
