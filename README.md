# Purescript-Chirashi

[![Build Status](https://travis-ci.org/justinwoo/purescript-chirashi.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-chirashi)

[Docs on Pursuit](https://pursuit.purescript.org/packages/purescript-chirashi)

An easy way to work with `Error`s by inserting a `Variant`, and reading it out later.

Works by subclassing Error.

![](https://i.imgur.com/468wLLl.jpg)

## Usage

```purs
type ErrorVariant = Variant
  ( apple :: String
  , banana :: { info :: String }
  , cherry :: Unit
  )

main = do
  let
    knownVariant :: ErrorVariant
    knownVariant = Variant.inj (SProxy :: SProxy "banana") { info: "hello" }
    knownError = Chirashi.mkVariantError knownVariant
    resultKnownError = Chirashi.readVariant knownError

  Assert.assertEqual
    { expected: Just knownVariant
    , actual: resultKnownError
    }
```

See tests for more examples.

## Other links

For more ideas on what you could do with this library, see [Ochadzuke](https://github.com/justinwoo/purescript-ochadzuke#usage).
