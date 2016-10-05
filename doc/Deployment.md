Installing packages from this repository
========================================

The software packages described in this repository are additional packages to
[packages in GNU Guix](https://www.gnu.org/software/guix/packages/).  These
packages either have licensing issues (and should be avoided) or simply haven't
been submitted for inclusion in the upstream repository.

Before you can deploy the packages described in this repository, you must
install GNU Guix.  You could follow the steps in the [installation instructions](https://www.gnu.org/software/guix/manual/guix.html#Installation)
from the manual, or perform the steps describe in [Basic installation][Basic installation].

For deployment in a restricted environment (where you don't have super user privileges),
see [Restricted environment installation][Restricted environment installation].

Table of Contents
-----------------

* [Basic installation](#basic-installation)
* [Restricted environment installation](#restricted-environment-installation)

Basic installation
------------------

In this installation, we will build GNU Guix from source using the default
options.

### Build GNU Guix
#### Get the code

```bash
git clone git://git.sv.gnu.org/guix.git
cd guix/
```

#### Perform the pre-build steps

```bash
./bootstrap   # You need to have GNU Autotools installed
./configure   # Install the missing dependencies
```

#### Compile and install the software

```bash
make && sudo make install
```

### Create build users

When building programs, GNU Guix isolates the individual build processes to
ensure no external factor can influence the build output.  To be able to do
this, it needs to run each build process as a separate user.  So for each
build process that can run in parallel, we need a user on the system.

So, if your build host can run 10 processes in parallel, you need to create
10 users and a group that each user is a member of:

```bash
groupadd --system guixbuild
for i in `seq -w 1 10`;
do
  useradd -g guixbuild -G guixbuild           \
          -d /var/empty -s `which nologin`    \
          -c "Guix build user $i" --system    \
          guixbuilder$i;
done
```

Restricted environment installation
-----------------------------------

The idea behind the restricted environment installation is that we prepare a
self-contained environment that can be readily copied to the restricted target
environment.  This implies the ability to write to the target environment in
some directory, and the availability of a ``build host'' where super user
privileges are available.

![Overview of the deployment construction](https://github.com/CuppenResearch/guix-additions/blob/master/doc/figures/restricted-deployment.png)

### Steps to perform on the build host

First, we need to perform the [Basic installation][Basic installation] with some
additional configuration.  When performing the `./configure` step, we need to
set:
* `--with-store-dir` to a place we can write the programs to on the restricted environment.
* `--localstatedir` to a place we can write the profiles to on the restricted environment.

So if you can write to `/shared/mountpoint/`, you can set these properties with
the command:
```bash
./configure --with-store-dir=/shared/mountpoint/guix/store \
            --localstatedir=/shared/mountpoint/guix/state
```

*Note:* Do not add a trailing slash to the end of the paths.

The default `store-dir` is set to `/gnu/store`, and the default `localstatedir`
is set to `/var/guix`.

Once we complete the remaining steps in the [Basic installation][Basic installation],
we can now build the essential binaries for our self-contained deployment:

#### As privileged user:

First, we need to set environment variables for the `localstatedir` and the
`store-dir` we changed before.  Note, that these environment variables
originated from Nix.

```bash
export NIX_STORE_DIR=/shared/mountpoint/guix/store
export NIX_STATE_DIR=/shared/mountpoint/guix/state
```

After setting the environment variables, we can run the `guix-daemon`:

```bash
guix-daemon --cores=<number-of-cpus>       \
            --max-jobs=<number-of-cpus>    \
            --no-substitutes               \
            --build-users-group=guixbuild
```

*Note:*  If your shell cannot find `guix-daemon` you may need to add
`/usr/local/bin` to your `PATH`.

Because we set custom state and store directories, we cannot use binary
substitutes from the GNU Guix project, so we should set `--no-substitutes`
here to avoid attempts to download substitutes.

#### As regular user:
```bash
export NIX_STORE_DIR=/shared/mountpoint/guix/store
export NIX_STATE_DIR=/shared/mountpoint/guix/state

guix build bootstrap-tarballs
```

This will build the essential tools to construct all other packages in the
GNU Guix distribution.  So after this step you can install packages to a
profile of your choice using:

```bash
guix package --install=emacs --profile=/path/to/your/profiles
```

Once you've installed the packages you need, you can copy them to the
restricted environment and run them there.  Keeping the build hosts's store
and profiles in sync with the restricted environment's store and profiles, you
could use the following commands:

```bash
rsync -lrt --delete /shared/mountpoint/guix/store user@restricted-host:/shared/mountpoint
rsync -lrt --delete /shared/mountpoint/guix/state user@restricted-host:/shared/mountpoint
```

After synchronizing your build host's copy with the target host's copy,
you must fix the permissions of the file system structure to enable
multiple users to be able to run the deployed programs.  Perform these 
steps on the restricted environment's host.

```bash
cd /shared/mountpoint/guix/store
chmod o+rx `ls -lh | grep ^d | awk '{ print $9 }'`
chmod g+rx `ls -lh | grep ^d | awk '{ print $9 }'`
```

*Note:* Do not give _write_ permissions to the store, as this allows anyone
to change the programs inside, and therefore tamper with the integrity of
the store.

### Steps to perform on the restricted environment

Create the directory you've configured with `--with-store-dir` and
`--localstatedir`:

```bash
mkdir -p /shared/mountpoint/guix
```
