Pipeline support for GNU Guix
=============================

This repository contains additional packages for GNU Guix to support the
software in the pipelines developed at the Cuppen research group.

Enabling the packages
---------------------

Add the repository to your `GUIX_PACKAGE_PATH` environment variable:
```bash
git clone https://github.com/CuppenResearch/guix-additions.git $HOME/guix-additions
export GUIX_PACKAGE_PATH=$HOME/guix-additions
```

**Note:** GATK and GATK-Queue do not provide public download links and require
agreeing to licensing terms.  If you really need these packages, download these
packages manually and update the source location in the package descriptions
accordingly.

Visualize the dependency graph
------------------------------

To get an overview of the packages involved in the pipelines, we can use the
`guix graph` command to visualize the dependency tree.

Example:
```bash
guix graph --type=package cuppenresearch-iap
```

To get a list of possible graphs, and a description of what they represent:
```bash
guix graph --list-types
```

**Note:** The dependencies for nonfree packages will not show in these graphs
because they "leak" out of the self-referencing parts of GNU Guix.  This means
that those packages are NOT guaranteed to work portably, and this guarantee
can never be made.  This is a serious problem the authors of those software
packages should fix.

Moving packages to upstream
---------------------------

Packages for VCFlib and FreeBayes are currently under review for inclusion in
the upstream GNU Guix repository, so once accepted, the versions provided here
will likely disappear.
