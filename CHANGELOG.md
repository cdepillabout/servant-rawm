
## 0.3.1.0

* Update to work with servant-0.16
  [#10](https://github.com/cdepillabout/servant-rawm/pull/10). Thanks
  [@vlatkoB](https://github.com/vlatkoB)!

## 0.3.0.0

* Update to work with the latest version of Servant (0.14) and remove support from older versions of Servant.
* Remove support from Stackage LTS versions older than 12.

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
