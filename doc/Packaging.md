Adding new packages to GNU Guix
===============================

Table of contents
-----------------

* [Introduction](#introduction)
* [A sample package recipe](#a-sample-package-recipe)
* [Automatic import from common sources](#automatic-import-from-common-sources)

Introduction
------------

In this document, we will look into the process of writing a package recipe for
a program called `delly`.

A sample package recipe
-----------------------

Let's take a look at the [package recipe for delly](https://github.com/CuppenResearch/guix-additions/blob/master/umcu/packages/delly.scm).  
Because this recipe is not entirely trivial, we encounter the mechanisms to
deal with compilation problems.

```scheme
(define-public delly-0.7.2
  (package
    (name "delly")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/tobiasrausch/"
                                  name "/archive/v" version ".tar.gz"))
              (sha256
               (base32 "173mmg43dbxqkyq0kiffz63xbmggr2kzd55mwxci9yfh5md1zprn"))
              (patches 
                (list (search-patch "delly-use-system-libraries.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests to run.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (replace 'install
           (lambda _
             (let ((bin (string-append (assoc-ref %outputs "out") "/bin")))
               (install-file "src/cov" bin)
               (install-file "src/delly" bin)
               (install-file "src/extract" bin)
               (install-file "src/iover" bin)
               (install-file "src/stats" bin)))))))
    (native-inputs
     `(("python" ,python-2)))
    (inputs
     `(("boost" ,boost-1.57) ; Use version 1.57.0 specifically.
       ("htslib" ,htslib)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)))
    (home-page "https://github.com/tobiasrausch/delly")
    (synopsis "Integrated structural variant prediction method")
    (description "Delly is an integrated structural variant prediction method
that can discover and genotype deletions, tandem duplications, inversions and
translocations at single-nucleotide resolution in short-read massively parallel
sequencing data.  It uses paired-ends and split-reads to sensitively and
accurately delineate genomic rearrangements throughout the genome.  Structural
variants can be visualized using Delly-maze and Delly-suave.")
    (license license:gpl3+)))
```

For the most part, the recipe can be seen as a form to fill out basic
information:

```scheme
(define-public delly-0.7.2
  (package
    (name "<name>")
    (version "<version>")
    (source <a way to download and verify the source code>)
    (build-system <the most suitable build system procedure to use>)
    (inputs <inputs>)
    (home-page "<home-page>")
    (synopsis "<single-line description>")
    (description "<description>")
    (license <license-symbol>)))
```

The `name`, `version`, `home-page`, `synopsis`, and `description` fields should
be straightforward to determine values for.  However, the `source`,
`build-system`, `inputs`, and `license` fields need some more work.  We will
work through the example for `delly` to find out how to fill in the remaining
parts.

### The `source` field

In this field, we need to specify a way to obtain and verify the source code
for the program we are writing a package recipe for.  We call this the `origin`
of the package.  

#### Obtaining the source code

There are several ways (or `methods`) to obtain source code.  One is simply
downloading a ``source tarball''.  Another is checking out a revision of a
version control system (like Git or Subversion).

In our case, we can use the simplest method, and that's downloading a source
tarball from a website.  The corresponding `method` for this is called
`url-fetch`.

Following the syntax of the package recipe language, we can express this
information in the following way:

```scheme
(source (origin
          (method url-fetch)
          (uri "<download-link>")))
```

#### Verifying the source code

We can only expect a reproducible output if we use *the same* origin.
Therefore, we need to verify that what we downloaded from the Internet is what
we expect it to be.  We do this by calculating a SHA256 hash of the downloaded
content.  The easiest way to obtain this hash is to use GNU Guix to download
the content:

```bash
guix download <url-of-the-source-tarball>
```

It will print the corresponding SHA256 hash, which can be copy-and-pasted into
the recipe:

```scheme
(sha256
  (base32 "<paste-the-hash-here>"))
```

Because this is part of the `origin` specification, we combine obtain the
following piece of the recipe:

```scheme
(source (origin
          (method url-fetch)
          (uri "<download-link>")
          (sha256
            (base32 "<paste-the-hash-here>"))))
```

In [Modifying the source code through patches](#modifying-the-source-code-through-patches),
we will find out how to customize the source code before it gets compiled.
Keep in mind that such ``patching'' of source code is rarely needed.

### Determine the build system

Most software projects consist of many source code files.  These projects use a
common ``build system'' to construct a runnable program from this source code.
Such build systems characterize themselves in the steps needed to produce a
runnable program.  One such build system is the GNU build system which
characterizes itself in the following steps:
```bash
./configure
make
make check
make install
```

In a package recipe, we don't need to repeatedly specify these commands.  We
only need to specify which build system the project uses to construct a
runnable program from its source code.

When we look at the top-level of the source code, we find a `Makefile`.  This
indicates it mostly matches the steps of the GNU build system, except for the
`./configure` step.

### Determine the inputs

The `inputs` of the package consist of all packages that are needed by the
program.  In our case, `delly` uses functions from `htslib`, `boost`, `zlib`,
and `bzip2`.

When a dependency is missing, the construction of the program will fail.  The
error message often hints at which dependency is missing.

#### `native-inputs` and `propagated-inputs`

The `inputs` can be seen as the libraries needed at *compile-time*.  These
dependencies do not cover *run-time* dependencies.  Here's an example piece
of code that explains the difference:

```c++
#include <boost/array.hpp>
#include <cstdlib>

int
main (int argc, char **argv)
{
  /* Boost is needed at compile-time because the compiler needs to link
   * this function into the runnable program.
   */
  boost::array<int, 4> array = {{1,2,3,4}};

  /* The compiler does not care whether the execution of the system
   * command will work.  We only care at run-time about this.  Other than
   * installing this program, we also need to install samtools before
   * the program functions correctly.
   *
   * Therefore, we need to install samtools along with this program
   * (propagating it).
   */
  system("samtools faidx myfile.fa");

  return 0;
}
```

Native inputs are the tools that need to be available on the machine that
builds the program.  Examples of `native-inputs` are `make` and `tar`.  Most
of these tools are automatically included by the build system.

### Determine the license

Most programs include a license file or a license header inside their source
files.  For `delly`, we can find the license in `delly.cpp`, which states:

```
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.
```

Which we can describe using the following line:
```scheme
(license license:gpl3+)
```

### Modifying the source code through patches

When we need to modify the source code in the source tarball before GNU Guix
compiles it, we can do so by providing a patch.

In the case of Delly, the patch disables the bundled `htslib` and `boost`
dependencies, and uses those provided by other package recipes.

Once we created a patch, we can add it to the root of our repository and add it
to the recipe with the following line:

```scheme
(patches (list (search-patch "delly-use-system-libraries.patch")))
```

This is considered part of `origin`, so it must be inside the scope of
`origin`.

Automatic import from common sources
------------------------------------

GNU Guix can obtain a partial recipe for common source archives like CPAN
and CRAN.  (For a full list of available importers run `guix import --help`.)

Obtaining the partial recipe can be done using:
```bash
guix import <source archive> <package name>
```

For example:
```bash
guix import cpan Test
```

Will display:
```scheme
(package
  (name "perl-test")
  (version "1.26")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/J/JE/JESSE/Test-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "14pksj0vklblv88ailj8mspacgzm72mvp0d1x4pzirq5iv91nw7p"))))
  (build-system perl-build-system)
  (home-page "http://search.cpan.org/dist/Test")
  (synopsis
    "provides a simple framework for writing test scripts")
  (description fill-in-yourself!)
  (license #f))
```

From here on, we must fill in the remaining parts and verify that it builds
correctly.

