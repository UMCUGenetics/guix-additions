Additional packages for GNU Guix
================================

This repository provides packages in addition to those that come with GNU Guix.
Packages may move from here to the upstream Guix repository.

Using this repository
---------------------

There are two ways to use this repository.  Choose one.

### Using `GUIX_PACKAGE_PATH`

Add the repository to the `GUIX_PACKAGE_PATH` environment variable:
```bash
git clone https://github.com/UMCUGenetics/guix-additions.git $HOME/guix-additions
export GUIX_PACKAGE_PATH=$HOME/guix-additions
```

You have to keep track of updates in this repository yourself by running
`git pull`.

### As a “channel”

Adding the following to `$HOME/.config/guix/channels.scm` automatically updates
this repository whenever you run `guix pull`:

```
(cons (channel
       (name 'guix-additions)
       (url "https://github.com/UMCUGenetics/guix-additions.git"))
      %default-channels)
```

Writing package recipes
----------------------------

To get started on writing package recipes, see the chapter
[Programming Interface](https://www.gnu.org/software/guix/manual/html_node/Programming-Interface.html#Programming-Interface)
in the GNU Guix reference manual.

The [Utilities](https://www.gnu.org/software/guix/manual/html_node/Utilities.html#Utilities)
chapter includes useful tools to aid in generating, building, and testing
recipes.

Adding packages to this repository
----------------------------------

This repository stores its package modules in `umcu/packages`, so make sure to
place new package recipes in there too.

The following steps get you started on developing GNU Guix package recipes.  A
working GNU Guix on your system is assumed.

1. Clone this repository:
```bash
git clone https://github.com/UMCUGenetics/guix-additions.git
```

2. Set `GUIX_PACKAGE_PATH` to the repository's root directory:
```bash
export GUIX_PACKAGE_PATH=$(pwd)/guix-additions
```

3. Add/edit/remove a package.

4. Build the package:
```bash
guix build <your-new-package-name>
```

When the build completes succesfully, you can commit your changes and
send a patch (or pull request).
