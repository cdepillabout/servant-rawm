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
[Haddocks on Hackage for `servant-rawm`](https://hackage.haskell.org/package/servant-rawm/docs/Servant-RawM.html)
for an explanation of how to use the
[`RawM`](https://hackage.haskell.org/package/servant-rawm/docs/Servant-RawM.html#t:RawM)
type.

After `servant-rawm` 1.0.0.0, the implementations of the `RawM` endpoint are
divided into `servant-rawm-client`, `servant-rawm-docs`, and
`servant-rawm-server` to avoid introducing unnecessary dependencies and reduce
the compilation overhead.

You will need to add either of the implementations to your dependencies, and
import the corresponding implementation (`Servant.RawM.Server`,
`Servant.RawM.Client`, or `Servant.RawM.Docs`) for the `RawM` endpoint to
function correctly.

## Example

There is code for an example server, client, and documentation located
in [`servant-rawm-examples-and-tests/example/`](servant-rawm-examples-and-tests/example/). The following section describes how to run the
example executables.

### Building

The example executables can be built with the following command:

```sh
$ stack build servant-rawm-examples-and-tests
```

### Server

After building, the server can be run with the following command:

```sh
$ stack exec -- servant-rawm-example-server
```

This runs a server on port 8201 serving files
in [`servant-rawm-examples-and-tests/example/files/`](servant-rawm-examples-and-tests/example/files/).

It can be accessed from `curl` like the following:

```sh
$ curl http://localhost:8201/serve-directory/foo.txt
This is an example text file.
```

### Client

After building and running the server, the client can be run like the following:

```sh
$ stack exec -- servant-rawm-example-client
Successfully got file ./example/files/foo.txt:

This is an example text file.
```

### Documentation

After building, the documentation can be generated like the following. This is
documentation for the API defined in
[servant-rawm-examples-and-tests/example/Api.hs](servant-rawm-examples-and-tests/example/Api.hs):

```sh
$ stack exec -- servant-rawm-example-docs
...
```

## Maintainers

- [@cdepillabout](https://github.com/cdepillabout)
- [@Krasjet](https://github.com/Krasjet)
