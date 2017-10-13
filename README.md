
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

## Example

There is code for an example server, client, and documentation located
in [`example/`](example/). The following section describes how to run the
example executables.

### Building

The example executables can be built with the following command:

```sh
$ stack build --flag servant-rawm:buildexample
```

### Server

After building, the server can be run with the following command:

```sh
$ stack exec -- servant-rawm-example-server
```

This runs a server on port 8201 serving files
in [`example/files/`](example/files/).

It can be accessed from `curl` like the following:

```sh
$ curl http://localhost:8201/serve-directory/foo.txt
This is an example text file.
```

### Client

After building, the client can be run like the following:

```sh
$ stack exec -- servant-rawm-example-client
Successfully got file ./example/files/foo.txt:

This is an example text file.
```

### Documentation

After building, the documentation can be generated like the following. This is
documentation for the API defined in [example/Api.hs](example/Api.hs):

```sh
$ stack exec -- servant-rawm-example-docs
...
```
