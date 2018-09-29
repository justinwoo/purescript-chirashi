module Chirashi where

import Prelude

import Data.Function.Uncurried as FU
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect.Exception (Error)
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, RLProxy(..), RProxy(..), SProxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

-- | An instance of the VariantError class, a subclass of Error, which holds a variant for extracting error information.
foreign import data VariantError :: # Type -> Type

-- | Create an Error using a Variant value
mkVariantError :: forall r. Variant r -> Error
mkVariantError = upcastVariantError <<< _mkVariantError

-- | Create a VariantError using a Variant value
mkVariantError' :: forall r. Variant r -> VariantError r
mkVariantError' = _mkVariantError

-- | Upcast a VariantError to Error
upcastVariantError :: forall r. VariantError r -> Error
upcastVariantError = unsafeCoerce

-- | Read a Variant from a Error
readVariant :: forall r. MatchKey r => Error -> Maybe (Variant r)
readVariant err = _getVariant <$> FU.runFn4  _readVariantError (matchKey (RProxy :: RProxy r)) Nothing Just err

-- | Read a VariantError from a Error
readVariantError :: forall r. MatchKey r => Error -> Maybe (VariantError r)
readVariantError err = FU.runFn4  _readVariantError (matchKey (RProxy :: RProxy r)) Nothing Just err

-- | Get the Variant value out of a VariantError
getVariant :: forall r. VariantError r -> Variant r
getVariant = _getVariant

foreign import _mkVariantError :: forall a r. a -> VariantError r
foreign import _readVariantError :: forall a b r. FU.Fn4 (String -> Boolean) (Maybe b) (a -> Maybe a) Error (Maybe (VariantError r))
foreign import _getVariant :: forall r. VariantError r -> Variant r

class MatchKey (r :: # Type) where
  matchKey :: RProxy r -> String -> Boolean

instance matchKeyInst ::
  ( MatchKeyImpl rl
  , RL.RowToList r rl
  ) => MatchKey r where
  matchKey _ = matchKeyImpl (RLProxy :: RLProxy rl)

class MatchKeyImpl (rl :: RL.RowList) where
  matchKeyImpl :: RLProxy rl -> String -> Boolean

instance nilMatchKey :: MatchKeyImpl RL.Nil where
  matchKeyImpl _ _ = false

instance consMatchKey ::
  ( MatchKeyImpl tail
  , IsSymbol name
  ) => MatchKeyImpl (RL.Cons name ty tail) where
  matchKeyImpl _ s = do
    let curr = reflectSymbol (SProxy :: SProxy name)
    if s == curr
       then true
       else matchKeyImpl (RLProxy :: RLProxy tail) s
