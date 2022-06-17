module JS.Object.Builder where

-- | Pure method chain builders from foreing object spec

import Prelude

import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Newtype (class Newtype, un)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import JS.Object (EffectMth0, EffectMth1, EffectMth2, EffectMth3, EffectMth4, EffectMth5, EffectProp, JSObject, runEffectMth0, runEffectMth1, runEffectMth2, runEffectMth3, runEffectMth4, runEffectMth5)
import Prim.Row (class Cons) as Row
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Builder obj = Builder (obj -> Effect Unit)

instance Contravariant Builder where
  cmap f (Builder b) = Builder (b <<< f)

instance Semigroup (Builder t) where
  append (Builder b1) (Builder b2) = Builder \obj -> do
    b1 obj
    b2 obj

instance Monoid (Builder t) where
  mempty = Builder (const $ pure unit)

builderMth0 :: forall b mths_ mths s. IsSymbol s => Row.Cons s (EffectMth0 b) mths_ mths => Proxy s -> Builder (JSObject mths)
builderMth0 m = do
  Builder (\obj -> runEffectMth0 m obj $> unit)

builderMth1 :: forall a1 b mths_ mths s. IsSymbol s => Row.Cons s (EffectMth1 a1 b) mths_ mths => Proxy s -> a1 -> Builder (JSObject mths)
builderMth1 m = do
  \a1 -> Builder (\obj -> runEffectMth1 m obj a1 $> unit)

builderMth2 :: forall a1 a2 b mths_ mths s. IsSymbol s => Row.Cons s (EffectMth2 a1 a2 b) mths_ mths => Proxy s -> a1 -> a2 -> Builder (JSObject mths)
builderMth2 m = do
  \a1 a2 -> Builder (\obj -> runEffectMth2 m obj a1 a2 $> unit)

builderMth3 :: forall a1 a2 a3 b mths_ mths s. IsSymbol s => Row.Cons s (EffectMth3 a1 a2 a3 b) mths_ mths => Proxy s -> a1 -> a2 -> a3 -> Builder (JSObject mths)
builderMth3 m = do
  \a1 a2 a3 -> Builder (\obj -> runEffectMth3 m obj a1 a2 a3 $> unit)

builderMth4 :: forall a1 a2 a3 a4 b mths_ mths s. IsSymbol s => Row.Cons s (EffectMth4 a1 a2 a3 a4 b) mths_ mths => Proxy s -> a1 -> a2 -> a3 -> a4 -> Builder (JSObject mths)
builderMth4 m = do
  \a1 a2 a3 a4 -> Builder (\obj -> runEffectMth4 m obj a1 a2 a3 a4 $> unit)

builderMth5 :: forall a1 a2 a3 a4 a5 b mths_ mths s. IsSymbol s => Row.Cons s (EffectMth5 a1 a2 a3 a4 a5 b) mths_ mths => Proxy s -> a1 -> a2 -> a3 -> a4 -> a5 -> Builder (JSObject mths)
builderMth5 m = do
  \a1 a2 a3 a4 a5 -> Builder (\obj -> runEffectMth5 m obj a1 a2 a3 a4 a5 $> unit)

-- TODO: Generalize over constructor
mkBuild :: forall t. Effect t -> Builder t -> t
mkBuild constructor (Builder b) = unsafePerformEffect do
  t <- constructor
  b t
  pure t

newtype Person = Person
  ( JSObject
      ( firstName :: EffectProp String
      , lastName :: EffectProp String
      , setFirstName :: EffectMth1 String Unit
      , setLastName :: EffectMth1 String Unit
      )
  )

derive instance Newtype Person _

setFirstName :: String -> Builder Person
setFirstName = cmap (un Person) <<< builderMth1 (Proxy :: Proxy "setFirstName")

setLastName :: String -> Builder Person
setLastName = cmap (un Person) <<< builderMth1 (Proxy :: Proxy "setFirstName")

fakeConstructor :: Effect Person
fakeConstructor = unsafeCoerce unit

buildPerson :: Builder Person -> Person
buildPerson = mkBuild fakeConstructor

mkPerson :: String -> String -> Person
mkPerson firstName lastName = buildPerson $ setFirstName firstName <> setLastName lastName
