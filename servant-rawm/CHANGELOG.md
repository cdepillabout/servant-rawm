## 1.0.0.0

*   The implementations of the `RawM` endpoint are divided into
    `servant-rawm-client`, `servant-rawm-docs`, and `servant-rawm-server` to
    avoid introducing unnecessary dependencies and reduce the compilation
    overhead. You will need to add either of the implementations to your
    dependencies, and import the corresponding implementation
    (`Servant.RawM.Server`, `Servant.RawM.Client`, or `Servant.RawM.Docs`) for
    the `RawM` endpoint to function correctly.
    [#16](https://github.com/cdepillabout/servant-rawm/pull/16)
    Thanks [@Krasjet](https://github.com/Krasjet)!

## 0.3.2.0

*   Add an instance for
    [`HasLink`](http://hackage.haskell.org/package/servant-0.16.2/docs/Servant-Links.html#t:HasLink)
    to `RawM` [#12](https://github.com/cdepillabout/servant-rawm/pull/12).
    Thanks [@amesgen](https://github.com/amesgen)!

## 0.3.1.0

*   Update to work with servant-0.16
    [#10](https://github.com/cdepillabout/servant-rawm/pull/10). Thanks
    [@vlatkoB](https://github.com/vlatkoB)!

## 0.3.0.0

*   Update to work with the latest version of Servant (0.14) and remove support from older versions of Servant.
*   Remove support from Stackage LTS versions older than 12.

## 0.2.0.2

*   Add missing test file to extra-source-files in the cabal file.

## 0.2.0.1

*   Add tests and more documentation.

## 0.2.0.0

*   (commit 30a2cd48488d) Add a phantom type to `RawM` to allow the user to
    change the output documentation.
*   (commit 30a2cd48488d) Add a `HasDocs` instance for `RawM` that uses
    the `HasDocs` instance for the phantom type.

## 0.1.0.0

*   Initial release.
