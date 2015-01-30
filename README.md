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

    $ sandbox new common
    [..]
    Created sandbox common.

    $ sandbox install common classy-prelude
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

# Status

This repository contains a working prototype. I am still in the process of
evaluating how well this works for my work flow. Feel free to try it out. Keep
in mind that since you're breaking sandbox boundaries, there is a higher chance
of running into version conflicts.
