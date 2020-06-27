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

