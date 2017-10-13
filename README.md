
# Servant.RawM

[![Build Status](https://secure.travis-ci.org/cdepillabout/servant-rawm.svg)](http://travis-ci.org/cdepillabout/servant-rawm)
[![Hackage](https://img.shields.io/hackage/v/servant-rawm.svg)](https://hackage.haskell.org/package/servant-rawm)
[![Stackage LTS](http://stackage.org/package/servant-rawm/badge/lts)](http://stackage.org/lts/package/servant-rawm)
[![Stackage Nightly](http://stackage.org/package/servant-rawm/badge/nightly)](http://stackage.org/nightly/package/servant-rawm)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

`servant-rawm` provides a way to embed a WAI
[`Application`](https://hackage.haskell.org/package/wai-3.2.1.1/docs/Network-Wai.html#t:Application)
in a Servant handler. It is more convenient and powerful than the
[`Raw`](https://hackage.haskell.org/package/servant-0.11/docs/Servant-API-Raw.html#t:Raw)
type provided by [`servant`](https://hackage.haskell.org/package/servant).

See the
[Haddocks on Hackage for `servant-rawm`](https://hackage.haskell.org/package/servant-rawm)
for an explanation of how to use the
[`RawM`](https://hackage.haskell.org/package/servant-rawm/docs/Servant-RawM.html#t:RawM)
type.
