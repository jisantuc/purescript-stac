# STAC

[![CI](https://github.com/jisantuc/purescript-stac/workflows/CI/badge.svg?branch=main)](https://github.com/jisantuc/purescript-stac/actions?query=workflow%3ACI+branch%3Amain)
[![Release](http://img.shields.io/github/release/jisantuc/purescript-stac.svg)](https://github.com/jisantuc/purescript-stac/releases)
[![Maintainer: jisantuc](https://img.shields.io/badge/maintainer-jisantuc-teal.svg)](http://github.com/jisantuc)

[STAC](https://github.com/jisantuc/purescript-stac) is a PureScript library for modeling [SpatioTemporal Asset Catalogs](https://stacspec.org/) (STACs) in PureScript.

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

Check out the [Client Quick Start](./src/Client/README.md#quick-start) and [Model Quick Start](./src/Model/README.md#quick-start) for more information.

## Documentation

forthcoming

## Contributing

forthcoming