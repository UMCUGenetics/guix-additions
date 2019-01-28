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

;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.

(define-module (umcu packages snpeff)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (umcu packages datasets))

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
      ("dbsnp" ,dbsnp)))))
