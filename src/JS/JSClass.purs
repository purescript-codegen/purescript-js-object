module JS.JSClass where

import Control.Category ((<<<))
import JS.Object (class ConstrFn, new)
import Unsafe.Coerce (unsafeCoerce)

-- | JSClass contains:
-- | * constructor type (please check `JS.JSObject.ConstructorEffectFn*`) families of
-- | functions. They encode the final instance object shape as well.
-- | * row of static methods and properties.
-- |
-- | This is simplified view on classes and instances from JS land. We don't care about
-- | providing direct support for `this.constructor`, `this.prototype` etc.
-- | On the other hand it is pretty easy to provide generic helpers or perform
-- | codegen to this representation.
foreign import data JSClass :: Type -> Row Type -> Type

instance (ConstrFn constr constrFn) => ConstrFn (JSClass constr c) constrFn where
  new = new <<< (unsafeCoerce :: JSClass constr c -> constr)

