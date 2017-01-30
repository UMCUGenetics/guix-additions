;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (umcu workflows rnaseq)
  #:use-module (guix processes)
  #:use-module (guix workflows)
  #:use-module (guix gexp)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages statistics)
  #:use-module (umcu packages bioconductor)
  #:use-module (umcu packages fastqc)
  #:use-module (umcu packages picard)
  #:use-module (umcu packages sambamba)
  #:use-module (umcu packages star))

(define-public rnaseq-quality-control
  (process
   (name "quality-control")
   (version "1.0")
   (package-inputs
    `(("fastqc" ,fastqc-bin-0.11.4)))
   (data-inputs
    '("/path/to/sample_R1_001.fastq.gz"))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 100 1024 1024)) ; Hundred megabytes
              (time #f))) ; One minute
   (procedure
    #~(begin
        (unless (stat #$output-path #f)
          (mkdir #$output-path))
        (map (lambda (sample)
               (system*
                (string-append #$@(assoc-ref package-inputs "fastqc") "/bin/fastqc")
                sample "-o" #$output-path))
             '#$data-inputs)))
   (synopsis "Generate quality control reports for FastQ files")
   (description "This process generates quality control reports for FastQ
files.  It takes each item from the list DATA-INPUTS as a FastQ file.")))

(define-public rnaseq-align
  (process
   (name "align")
   (version "1.0")
   (package-inputs
    `(("star" ,star-2.4.2a)))
   (data-inputs
    '(("samples"          '("/path/to/sample_R1_001.fastq.gz"))
      ("genome-directory" "/path/to/GENOMES/STAR")
      ("species"          "Homo_sapiens.GRCh37")))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (+ (stat:size
                         (stat (string-append
                                (car (assoc-ref data-inputs "genome-directory")) "/"
                                (car (assoc-ref data-inputs "species")) "/Genome")))
                        (* 500 1024 1024))) ; Genome size + 500 megabytes
              (time (* 60 10)) ; Ten minutes
              (threads 32)))
   (procedure
    #~(begin
        (unless (stat #$output-path #f)
          (mkdir #$output-path))
        (for-each (lambda (sample)
                    (system*
                     (string-append #$@(assoc-ref package-inputs "star") "/bin/STAR")
                     "--genomeDir"
                     #$(string-append
                        (car (assoc-ref data-inputs "genome-directory")) "/"
                        (car (assoc-ref data-inputs "species")))
                     "--runThreadN" #$(number->string (complexity-threads run-time))
                     "--outFileNamePrefix" (string-append sample "_")
                     "--outReadsUnmapped" "Fastx"
                     "--outSAMtype" "BAM" "SortedByCoordinate"
                     "--readFilesCommand" "zcat"
                     "--readFilesIn" (string-append
                                      (for-each
                                       (lambda (sample)
                                         (format #f "~a," sample))
                                       (assoc-ref data-inputs "samples")))
                  #$(assoc-ref data-inputs "samples"))))))
   (synopsis "Align reads from FastQ files against reference genome")
   (description "Produce a BAM file, a chimeric junctions table, a chimeric
junctions SAM, a splice junction table and logfiles.")))

(define-public rnaseq-add-read-groups
  (process
   (name "add-read-groups")
   (version "1.0")
   (package-inputs
    `(("picard" ,picard-bin-1.141)))
   (data-inputs
    '(("samples" '(""))))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 16 1024 1024 1024))
              (time (* 6 3600))))
   (procedure #~(begin (display "Not implemented yet!")))
   (synopsis "Add read group information to the BAM files")
   (description "This process adds read group information to BAM files
using Picard's AddOrReplaceReadGroups function.")))

(define-public rnaseq-sort-and-index
  (process
   (name "sort-and-index")
   (version "1.0")
   (package-inputs
    `(("sambamba" ,sambamba)))
   (data-inputs
    '(("samples" '(""))))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 2 1024 1024 1024))
              (time (* 6 3600))))
   (procedure #~(begin (display "Not implemented yet!")))
   (synopsis "Sort and index BAM files")
   (description "This process sorts and indexes BAM files.")))

(define-public rnaseq-feature-readcount
  (process
   (name "feature-readcount")
   (version "1.0")
   (package-inputs
    `(("htseq" ,htseq)))
   (data-inputs
    '(("samples" '(""))))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 2 1024 1024 1024))
              (time (* 6 3600))))
   (procedure #~(begin (display "Not implemented yet!")))
   (synopsis "Number of mapped reads per feature")
   (description "")))

(define-public rnaseq-collect-alignment-metrics
  (process
   (name "collect-alignment-metrics")
   (version "1.0")
   (package-inputs
    `(("picard" ,picard-bin-1.141)))
   (data-inputs
    '(("samples" '(""))))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 2 1024 1024 1024))
              (time (* 6 3600))))
   (procedure #~(begin (display "Not implemented yet!")))
   (synopsis "Report quality statistics of sorted BAM files")
   (description "")))

(define-public rnaseq-merge-read-features
  (process
   (name "merge-read-features")
   (version "1.0")
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 2 1024 1024 1024))
              (time (* 6 3600))))
   (procedure #~(begin (display "Not implemented yet!")))
   (synopsis "Merge feature read count into one table")
   (description "")))

(define-public rnaseq-compute-rpkm-values
  (process
   (name "compute-rpkm-values")
   (version "1.0")
   (package-inputs
    `(("r" ,r)
      ("r-edger" ,r-edger)))
   (data-inputs
    '(("samples" '(""))))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 2 1024 1024 1024))
              (time (* 6 3600))))
   (procedure #~(begin (display "Not implemented yet!")))
   (synopsis "Compute reads per kilobase per million (RPKM) values")
   (description "")))

(define-public rnaseq-variant-calling
  (process
   (name "variant-calling")
   (version "1.0")
   (package-inputs
    `(("picard" ,picard-bin-1.141)))
   (data-inputs
    '(("samples" '(""))))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 2 1024 1024 1024))
              (time (* 6 3600))))
   (procedure #~(begin (display "Not implemented yet!")))
   (synopsis "Variant calling, filtering and annotation")
   (description "This process calls variants, filters and annotates these
variants using Picard.")))

(define-public rnaseq-normalize-read-counts
  (process
   (name "normalize-read-counts")
   (version "1.0")
   (package-inputs
    `(("r" ,r)
      ("r-deseq" ,r-deseq)))
   (data-inputs
    '(("samples" '(""))))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 2 1024 1024 1024))
              (time (* 6 3600))))
   (procedure #~(begin (display "Not implemented yet!")))
   (synopsis "Normalize read counts by their size factor")
   (description "")))

(define-public rnaseq-differential-expression
  (process
   (name "differential-expression")
   (version "1.0")
   (package-inputs
    `(("r" ,r)
      ("r-deseq2" ,r-deseq2)))
   (data-inputs
    '(("samples" '(""))))
   (output-path "/path/to/rnaseq-out")
   (run-time (complexity
              (space (* 2 1024 1024 1024))
              (time (* 6 3600))))
   (procedure #~(begin (display "Not implemented yet!")))
   (synopsis "Do a differential expression analysis")
   (description "")))

(define (rnaseq-pipeline input output)
  (workflow
   (name "rnaseq-pipeline")
   (version "1.0")
   (input input)
   (output output)
   (processes
    `(,rnaseq-quality-control
      ,rnaseq-align
      ,rnaseq-add-read-groups
      ,rnaseq-sort-and-index
      ,rnaseq-feature-readcount
      ,rnaseq-collect-alignment-metrics
      ,rnaseq-variant-calling
      ,rnaseq-merge-read-features
      ,rnaseq-compute-rpkm-values
      ,rnaseq-normalize-read-counts
      ,rnaseq-differential-expression))
   (restrictions
    `((,rnaseq-add-read-groups ,rnaseq-align)
      (,rnaseq-sort-and-index ,rnaseq-add-read-groups)
      (,rnaseq-variant-calling ,rnaseq-sort-and-index)
      (,rnaseq-collect-alignment-metrics ,rnaseq-sort-and-index)
      (,rnaseq-feature-readcount ,rnaseq-sort-and-index)
      (,rnaseq-merge-read-features ,rnaseq-feature-readcount)
      (,rnaseq-compute-rpkm-values ,rnaseq-merge-read-features)
      (,rnaseq-normalize-read-counts ,rnaseq-merge-read-features)
      (,rnaseq-differential-expression ,rnaseq-merge-read-features)))
   (synopsis "RNA sequencing pipeline used at the UMC Utrecht.")
   (description "The RNAseq pipeline can do quality control on FastQ and BAM
files; align reads against a reference genome; count reads in features;
normalize read counts; calculate RPKMs and perform DE analysis of standard
designs.")))

(define-public rnaseq-workflow (rnaseq-pipeline "input" "output"))
