# Servant.RawM.Docs

[![CI](https://github.com/cdepillabout/servant-rawm/actions/workflows/ci.yml/badge.svg)](https://github.com/cdepillabout/servant-rawm/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/servant-rawm-docs.svg)](https://hackage.haskell.org/package/servant-rawm-docs)
[![Stackage LTS](http://stackage.org/package/servant-rawm-docs/badge/lts)](http://stackage.org/lts/package/servant-rawm-docs)
[![Stackage Nightly](http://stackage.org/package/servant-rawm-docs/badge/nightly)](http://stackage.org/nightly/package/servant-rawm-docs)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

This is the documentation-generation library for
[`servant-rawm`](https://github.com/cdepillabout/servant-rawm).

See the `servant-rawm`
[`README.md`](https://github.com/cdepillabout/servant-rawm) and
[Haddocks](side://hackage.haskell.org/package/servant-rawm)
for a short explanation of how to use this package.

After `servant-rawm` 1.0.0.0, the implementations of the `RawM` endpoint are
divided into `servant-rawm-client`, `servant-rawm-docs`, and
`servant-rawm-server` to avoid introducing unnecessary dependencies and reduce
the compilation overhead.
