module JS.Object.Immutable.Bounded where

import Prelude

import Data.Newtype (class Newtype)
import Data.Symbol (reflectSymbol)
import Effect (Effect)
import Effect.Uncurried (runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import JS.Object (EffectMth0, EffectMth1, EffectMth2, EffectMth3, EffectProp, JSObject, unsafeRunEffectMth0, unsafeRunEffectMth1, unsafeRunEffectMth2, unsafeRunEffectMth3, unsafeRunEffectProp)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (class IsSymbol, Proxy(..))


-- class BoundedProps :: Type -> Row Type -> Constraint
-- class BoundedProps o props | o -> props
--   bindObject :: forall props. BoundedObject o props => o -> { | props }

data BindStep a aFFI = BindStep a { | aFFI }

instance
  ( IsSymbol mthName
  , Row.Cons mthName (a -> res) aFFI_ aFFI
  , Row.Cons mthName res boundedProps boundedProps'
  , Row.Lacks mthName boundedProps
  ) =>
  FoldingWithIndex (BindStep a aFFI) (Proxy mthName) { | boundedProps } (a -> res) { | boundedProps' } where
  foldingWithIndex (BindStep a aFFI) mthName boundedProps _ = do
    let
      mth = Record.get mthName aFFI
    Record.insert mthName (mth a) boundedProps

-- instance
--   (
--   )
--   => Props (Immutable props) props' where
-- 
