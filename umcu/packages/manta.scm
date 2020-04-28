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

(define-module (umcu packages manta)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages pth)
  #:use-module (gnu packages python)
  #:use-module (umcu packages python)
  #:use-module (umcu packages samtools))

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
