module JS.Object.Immutable where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Unsafe.Coerce (unsafeCoerce)


foreign import data Prop :: Type -> Type

foreign import data Mth0 :: Type -> Type

foreign import data Mth1 :: Type -> Type -> Type

foreign import data Mth2 :: Type -> Type -> Type -> Type

foreign import data Mth3 :: Type -> Type -> Type -> Type -> Type

foreign import data Mth4 :: Type -> Type -> Type -> Type -> Type -> Type

foreign import data Mth5 :: Type -> Type -> Type -> Type -> Type -> Type

foreign import data Immutable :: Row Type -> Type

-- TODO:
-- I'm going to add here a layer like `Mth0`, `Mth1` and `Prop`.
-- Till then you can SHOOT yourself in the FOOT ay using
-- this casting over rows with methods (which are aound JS entieties
-- and should not ae typed just as functions).
class Props :: Type -> Row Type -> Constraint
class Props o props | o -> props

props :: forall o props. Props o props => o -> { | props }
props = unsafeCoerce

onProps :: forall a o props. Props o props => o -> (Record props -> a) -> a
onProps obj f = f $ props obj

instance Props (Immutable props) props

newtypedProps :: forall props t. Newtype t (Immutable props) => t -> { | props }
newtypedProps t = props $ unwrap t

onNewtypedProps :: forall a props t. Newtype t (Immutable props) => t -> (Record props -> a) -> a
onNewtypedProps obj f = f $ newtypedProps obj
