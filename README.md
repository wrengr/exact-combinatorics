exact-combinatorics
===================
[![Hackage version](https://img.shields.io/hackage/v/exact-combinatorics.svg?style=flat)](https://hackage.haskell.org/package/exact-combinatorics) 
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/exact-combinatorics.svg?style=flat)](http://packdeps.haskellers.com/specific?package=exact-combinatorics)
[![TravisCI Build Status](https://img.shields.io/travis/wrengr/exact-combinatorics.svg?style=flat)](https://travis-ci.org/wrengr/exact-combinatorics) 
[![CircleCI Build Status](https://circleci.com/gh/wrengr/exact-combinatorics.svg?style=shield&circle-token=b57517657c556be6fd8fca92b843f9e4cffaf8d1)](https://circleci.com/gh/wrengr/exact-combinatorics)

Efficient exact computation of combinatoric functions.


## Install

This is a simple package and should be easy to install. You should
be able to use one of the following standard methods to install it.

    -- With cabal-install and without the source:
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
It is Haskell98 except for the use of CPP, in order to get the
Haddock documentation to come out right.

## Links

* [Website](http://cl.indiana.edu/~wren/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/exact-combinatorics)
* [Darcs](http://code.haskell.org/~wren/exact-combinatorics)
* [GitHub (clone)](https://github.com/wrengr/exact-combinatorics)
* [Haddock (Darcs version)
    ](http://code.haskell.org/~wren/exact-combinatorics/dist/doc/html/exact-combinatorics)
