# STAC

[![Release](http://img.shields.io/github/release/purescript-contrib/purescript-argonaut.svg)](https://github.com/purescript-contrib/purescript-argonaut/releases)
[![Maintainer: jisantuc](https://img.shields.io/badge/maintainer-jisantuc-teal.svg)](http://github.com/jisantuc)

[STAC](github.com/jisantuc/purescript-stac) is a PureScript library for modeling [SpatioTemporal Asset Catalogs](https://stacspec.org/) (STACs) in PureScript.

## Installation

Install `STAC` with [Spago](https://github.com/purescript/spago).

```sh
spago install stac
```

## Quick start

The main entrypoint for the STAC module is `Data.Stac`:

```purescript
> import Data.Stac
> :t StacCollection
```

`Data.Stac` re-exports data defined in the `Client` and `Model` modules. The `Model` module includes types for constructing valid STAC JSON objects. The `Client` module includes methods for querying [STAC APIs](https://github.com/radiantearth/stac-api-spec).

Check out the [Client QuickStart](./src/Client/README.md#QuickStart) and [Model QuickStart](./src/Model/README.md#QuickStart) for more information.

## Documentation

forthcoming

## Contributing

forthcoming