`sandman` helps manage Cabal sandboxes so that you can avoid rebuilding
packages that you use often.

It does so by managing a global collection of sandboxes that were built
separately. You can `mix` any number of these sandboxes into the package
database for your project-specific sandbox.

# Example usage

First, we create a sandbox that will contain packages we commonly use for
development.

    $ sandman list
    No sandboxes available.
    $ sandman new common
    Created sandbox common.
    $ sandman install --to common classy-prelude
    [..]
    $ sandman list
    common (contains 32 packages)

Next, we mix it into an existing project.

    $ cd my_project
    $ cabal sandbox init
    $ cabal hc-pkg list | grep classy-prelude
    <nothing>
    $ sandman mix common
    Mixing sandbox common into sandbox at path/to/.cabal-sandbox.
    Added 32 new packages.
    $ sandman mix lens
    Mixing sandbox lens into sandbox at path/to/.cabal-sandbox.
    Added 9000 new packages.
    $ sandman clean
    Removing all mixed packages from path/to/.cabal-sandbox.
    Removed 9032 packages.

# Status

Minimal proof-of-concept prototype implemented. Still evaluating how well it
will work.
