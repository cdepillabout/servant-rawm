# Servant.RawM.Server

[![CI](https://github.com/cdepillabout/servant-rawm/actions/workflows/ci.yml/badge.svg)](https://github.com/cdepillabout/servant-rawm/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/servant-rawm-server.svg)](https://hackage.haskell.org/package/servant-rawm-server)
[![Stackage LTS](http://stackage.org/package/servant-rawm-server/badge/lts)](http://stackage.org/lts/package/servant-rawm-server)
[![Stackage Nightly](http://stackage.org/package/servant-rawm-server/badge/nightly)](http://stackage.org/nightly/package/servant-rawm-server)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

This is the server-side library for
[`servant-rawm`](https://github.com/cdepillabout/servant-rawm).

See the `servant-rawm`
[`README.md`](https://github.com/cdepillabout/servant-rawm) and
[Haddocks](side://hackage.haskell.org/package/servant-rawm)
for a short explanation of how to use this package.

After `servant-rawm` 1.0.0.0, the implementations of the `RawM` endpoint are
divided into `servant-rawm-client`, `servant-rawm-docs`, and
`servant-rawm-server` to avoid introducing unnecessary dependencies and reduce
the compilation overhead.
