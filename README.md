`sandman` helps manage Cabal sandboxes so that you can avoid rebuilding
packages that you use often.

It does so by managing a global collection of sandboxes that were built
separately. You can `mix` any number of these sandboxes into the package
database for your project-specific sandbox.

    Usage: sandman COMMAND

    Available options:
      -h,--help                Show this help text

    Available commands:
      list                     List sandman sandboxes or the packages in them
      new                      Create a new sandman sandbox
      destroy                  Delete a sandman sandbox
      install                  Install a new package
      mix                      Mix a sandman sandbox into the current project
      clean                    Remove all mixed sandboxes from the current project

# Example usage

First, we create a sandbox that will contain packages we commonly use for
development.

    $ sandman list
    lens (25 packages)

    $ sandman new common
    [..]
    Created sandbox common.

Managed sandboxes can be told to use specific versions of GHC. This information
will be propagated to projects with which this sandbox is mixed.

    $ sandman new common --with-ghc ghc-7.6.3

We install our commonly used packages

    $ sandman install common classy-prelude 
    [..]
    Configuring classy-prelude-0.10.2...
    Building classy-prelude-0.10.2...
    Installed classy-prelude-0.10.2

    $ sandman list
    lens (25 packages)
    common (45 packages)

    $ sandman list common
    [..]
    classy-prelude-0.10.2
    [..]

    $ sandman destroy lens
    Removed sandbox lens.

    $ sandman list
    common (45 packages)

    $ sandman install common optparse-applicative aeson

Next, we mix it into an existing project.

    $ cd my_project
    $ cabal sandbox init
    $ cabal sandbox hc-pkg list | grep classy-prelude
    <nothing>

    $ sandman mix common
    Mixing 45 new packages into package DB at [..]
    Rebuilding package cache.

    $ cabal sandbox hc-pkg list | grep classy-prelude
      classy-prelude-0.10.2

    $ cabal repl
    GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    λ> import ClassyPrelude
    λ> 

    $ sandman clean
    Removing all mixed sandboxes.
    Removed 45 packages.
    Rebuilding package cache.

`sandman` can also mix in only specific packages and their dependencies from
managed sandboxes.

    $ sandman mix common --only system-filepath --only system-fileio
    Mixing 3 new packages into package DB at [..]
    Rebuilding package cache.

    $ cabal sandbox hc-pkg list
    [..]
      system-fileio-0.3.16
      system-filepath-0.4.13.1
      text-1.2.0.4

The `--executables` option may be used to include executables from a sandbox.

    $ sandman mix common --executables --only hspec
    Mixing 15 new packages into package DB at [..]
    Rebuilding package cache.

    $ ls .cabal-sandbox/bin
    hspec-discover

# Stack

`sandman` also supports mixing in packages from a [`stack`] snapshot package
database.

    $ sandman mix stack

This mixes in all packages from the default snapshot database into the current
Cabal sandbox. The `-o/--only` options may be used to limit the packages to a
minimal subset.

    $ sandman mix stack -o text

  [`stack`]: https://github.com/commercialhaskell/stack

# Status

Sandman is stable enough for basic use cases but there are surely a lot of
unexplored corner cases. Feel free to try it out. Keep in mind that since
you're breaking sandbox boundaries, there is a higher chance of running into
version conflicts.

# Installation

You can download and install `sandman` from Hackage by using,

    $ cabal install sandman

Or if you would rather not pollute your global package database, install it
into a sandbox and copy the executable somewhere on your `$PATH`.

    $ mkdir tmp && cd tmp
    $ cabal sandbox init
    $ cabal install sandman
    $ cp .cabal-sandbox/bin/sandman ~/bin

Or simply use `stack`:

    $ stack install sandman
