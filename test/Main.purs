module Test.Main where

import Prelude

import Chirashi as Chirashi
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Exception as Exception
import Test.Assert as Assert
import Type.Prelude (SProxy(..))

type ErrorVariant = Variant
  ( apple :: String
  , banana :: { info :: String }
  , cherry :: Unit
  )

main :: Effect Unit
main = do
  let
    knownVariant :: ErrorVariant
    knownVariant = Variant.inj (SProxy :: SProxy "banana") { info: "hello" }
    knownError = Chirashi.mkVariantError knownVariant
    resultKnownError = Chirashi.readVariant knownError

  Assert.assertEqual
    { expected: "VariantError"
    , actual: Exception.message knownError
    }

  Assert.assertEqual
    { expected: Just knownVariant
    , actual: resultKnownError
    }

  let
    unknownVariant :: Variant (unknown :: Unit)
    unknownVariant = Variant.inj (SProxy :: SProxy "unknown") unit
    unknownError = Chirashi.mkVariantError unknownVariant
    resultUnknownError :: Maybe ErrorVariant
    resultUnknownError = Chirashi.readVariant unknownError

  Assert.assertEqual
    { expected: "VariantError"
    , actual: Exception.message unknownError
    }

  Assert.assertEqual
    { expected: Nothing
    , actual: resultUnknownError
    }

  let
    plainError = Exception.error "plain error"
    resultPlainError :: Maybe ErrorVariant
    resultPlainError = Chirashi.readVariant plainError

  Assert.assertEqual
    { expected: "plain error"
    , actual: Exception.message plainError
    }

  Assert.assertEqual
    { expected: Nothing
    , actual: resultPlainError
    }
