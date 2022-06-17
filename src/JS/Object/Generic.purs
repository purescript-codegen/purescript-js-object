module JS.Object.Generic where

import Prelude

import Data.Newtype (class Newtype)
import Data.Symbol (reflectSymbol)
import Effect (Effect)
import Effect.Uncurried (runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import JS.Object (EffectMth0, EffectMth1, EffectMth2, EffectMth3, EffectProp, JSObject, unsafeRunEffectMth0, unsafeRunEffectMth1, unsafeRunEffectMth2, unsafeRunEffectMth3, unsafeRunEffectProp)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList) as RL
import Record (insert) as Record
import Type.Prelude (class IsSymbol, Proxy(..))

data MkFFIStep :: forall k. k -> Type
data MkFFIStep obj = MkFFIStep

class UnsafeEffectMthToEffectFn :: forall k1 k2. k1 -> k2 -> Type -> Type -> Constraint

class UnsafeEffectMthToEffectFn mthName mthType obj fn | mthName mthType obj -> fn where
  mthFn :: Proxy mthName -> Proxy mthType -> obj -> fn

instance (IsSymbol mthName) => UnsafeEffectMthToEffectFn mthName (EffectProp res) obj (Effect res) where
  mthFn mthName _ = runEffectFn2 unsafeRunEffectProp (reflectSymbol mthName)

instance (IsSymbol mthName) => UnsafeEffectMthToEffectFn mthName (EffectMth0 res) obj (Effect res) where
  mthFn mthName _ = runEffectFn2 unsafeRunEffectMth0 (reflectSymbol mthName)

instance (IsSymbol mthName) => UnsafeEffectMthToEffectFn mthName (EffectMth1 a1 res) obj (a1 -> Effect res) where
  mthFn mthName _ = runEffectFn3 unsafeRunEffectMth1 (reflectSymbol mthName)

instance (IsSymbol mthName) => UnsafeEffectMthToEffectFn mthName (EffectMth2 a1 a2 res) obj (a1 -> a2 -> Effect res) where
  mthFn mthName _ = runEffectFn4 unsafeRunEffectMth2 (reflectSymbol mthName)

instance (IsSymbol mthName) => UnsafeEffectMthToEffectFn mthName (EffectMth3 a1 a2 a3 res) obj (a1 -> a2 -> a3 -> Effect res) where
  mthFn mthName _ = runEffectFn5 unsafeRunEffectMth3 (reflectSymbol mthName)

instance
  ( IsSymbol mthName
  , Row.Cons mthName mthType mths_ mths
  , UnsafeEffectMthToEffectFn mthName mthType (JSObject mths) fn
  , Row.Cons mthName (JSObject mths -> fn) ffi ffi'
  , Row.Lacks mthName ffi
  ) =>
  FoldingWithIndex (MkFFIStep (JSObject mths)) (Proxy mthName) { | ffi } value { | ffi' } where
  foldingWithIndex _ mthName ffi _ = Record.insert mthName (mthFn mthName (Proxy :: Proxy mthType)) ffi

else instance
  ( IsSymbol mthName
  , Newtype t (JSObject mths)
  , Row.Cons mthName mthType mths_ mths
  , UnsafeEffectMthToEffectFn mthName mthType t fn
  , Row.Cons mthName (t -> fn) ffi ffi'
  , Row.Lacks mthName ffi
  ) =>
  FoldingWithIndex (MkFFIStep t) (Proxy mthName) { | ffi } value { | ffi' } where
  foldingWithIndex _ mthName ffi _ = Record.insert mthName (mthFn mthName (Proxy :: Proxy mthType)) ffi

mkFFI
  :: forall mths mthsL rout
   . RL.RowToList mths mthsL
  => HFoldlWithIndex (MkFFIStep (JSObject mths)) {} (Proxy mthsL) { | rout }
  => Proxy (JSObject mths)
  -> { | rout }
mkFFI _ = hfoldlWithIndex (MkFFIStep :: MkFFIStep (JSObject mths)) {} (Proxy :: Proxy mthsL)

newtype Person = Person
  ( JSObject
      ( firstName :: EffectProp String
      , setFirstName :: EffectMth1 String Unit
      , lastName :: EffectProp String
      , setLastName :: EffectMth1 String Unit
      )
  )

derive instance Newtype Person _

mkNewtypedFFI
  :: forall mths mthsL rout t
   . RL.RowToList mths mthsL
  => Newtype t (JSObject mths)
  => HFoldlWithIndex (MkFFIStep t) {} (Proxy mthsL) { | rout }
  => Proxy t
  -> { | rout }
mkNewtypedFFI _ = hfoldlWithIndex (MkFFIStep :: MkFFIStep t) {} (Proxy :: Proxy mthsL)

_Person ::
  { firstName :: Person -> Effect String
  , lastName :: Person -> Effect String
  , setFirstName :: Person -> String -> Effect Unit
  , setLastName :: Person -> String -> Effect Unit
  }
_Person = mkNewtypedFFI (Proxy :: Proxy Person)
