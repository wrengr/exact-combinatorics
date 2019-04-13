exact-combinatorics
===================
[![Hackage version](https://img.shields.io/hackage/v/exact-combinatorics.svg?style=flat)](https://hackage.haskell.org/package/exact-combinatorics) 
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/exact-combinatorics.svg?style=flat)](http://packdeps.haskellers.com/specific?package=exact-combinatorics)
[![TravisCI Build Status](https://img.shields.io/travis/wrengr/exact-combinatorics.svg?style=flat)](https://travis-ci.org/wrengr/exact-combinatorics) 

Efficient exact computation of combinatoric functions.


## Install

This is a simple package and should be easy to install via any of
the standared methods:

    -- With cabal-install and without the source:
    $> cabal new-install exact-combinatorics
    -- or:
    $> cabal install exact-combinatorics
    
    -- With cabal-install and with the source already:
    $> cd exact-combinatorics
    $> cabal install
    
    -- Without cabal-install, but with the source already:
    $> cd exact-combinatorics
    $> runhaskell Setup.hs configure --user
    $> runhaskell Setup.hs build
    $> runhaskell Setup.hs test
    $> runhaskell Setup.hs haddock --hyperlink-source
    $> runhaskell Setup.hs copy
    $> runhaskell Setup.hs register

The test step is optional and currently does nothing. The Haddock
step is also optional.


## Portability

An attempt has been made to keep this library as portable as possible.
It is entirely Haskell98 with the exception of one use of BangPatterns.
BangPatterns are supported in GHC as far back as [version
6.6.1][ghc-bangpatterns], and are also supported by [JHC][jhc-bangpatterns]
and [UHC][uhc-bangpatterns]. As of 2010, they were [not supported
by Hugs][hugs-bangpatterns]; but alas Hugs is pretty much dead now.

[ghc-bangpatterns]: 
    https://downloads.haskell.org/~ghc/6.6.1/docs/html/users_guide/sec-bang-patterns.html
[jhc-bangpatterns]:
    http://repetae.net/computer/jhc/manual.html#code-options
[uhc-bangpatterns]:
    https://github.com/UU-ComputerScience/uhc-js/issues/1
[hugs-bangpatterns]: 
    https://mail.haskell.org/pipermail/haskell-cafe/2010-July/079946.html

## Links

* [Website](https://wrengr.org/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/exact-combinatorics)
* [GitHub](https://github.com/wrengr/exact-combinatorics)
