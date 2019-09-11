# purescript-argonaut

[![Latest Release](http://img.shields.io/github/release/purescript-contrib/purescript-argonaut.svg)](https://github.com/purescript-contrib/purescript-argonaut/releases)
[![Build Status](https://travis-ci.org/purescript-contrib/purescript-argonaut.svg)](https://travis-ci.org/purescript-contrib/purescript-argonaut)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-argonaut/badge)](http://pursuit.purescript.org/packages/purescript-argonaut/)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-lightgrey.svg)](http://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

[Argonaut](https://github.com/purescript-contrib/purescript-argonaut) is a collection of libraries for working with JSON in PureScript. This library bundles together the following:

- [purescript-argonaut-core](https://github.com/purescript-contrib/purescript-argonaut-core) provides basic parsing, printing, folding and types for `Json`.
- [purescript-argonaut-codecs](https://github.com/purescript-contrib/purescript-argonaut-codecs) provides codecs based on `EncodeJson` and `DecodeJson` type classes (along with instances for common data types) and combinators for encoding and decoding `Json` values.
- [purescript-argonaut-traversals](https://github.com/purescript-contrib/purescript-argonaut-traversals) defines prisms, traversals, zippers, and `JCursor` for the `Json` type.

## Installation

```sh
# with Spago
spago install argonaut

# with Bower
bower install purescript-argonaut
```

## Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-argonaut). 

- To learn more about Argonaut, its basic data types, and its approach to JSON, see [purescript-argonaut-core](https://github.com/purescript-contrib/purescript-argonaut-core).
- To learn more about encoding and decoding PureScript types to and from `Json`, see [purescript-argonaut-codecs](https://github.com/purescript-contrib/purescript-argonaut-codecs).
- To learn more about manipulating `Json` using optics and `JCursor`, see [purescript-argonaut-traversals](https://github.com/purescript-contrib/purescript-argonaut-traversals)


## Contributing

Read the [contribution guidelines](https://github.com/purescript-contrib/purescript-argonaut/blob/master/.github/contributing.md) to get started and see helpful related resources.
