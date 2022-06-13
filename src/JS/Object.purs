-- | By using helpers from this module you should be able to bind to JS object
-- | methods without actually writing any JS.
-- | Please use curring when using `runEffectMth*` so your binding can stay performant.
-- | When you pass the symbol to a particular helper it compiles down to a single function call
-- | (reflection of the `Symbol` value and other dicts are resolved and allready applied to it):
-- | ```
-- | type Counter = JSObject (increase :: EffectMth0 Unit)
-- |
-- | increase :: Counter -> Effect Unit
-- | increase = runEffectMth1 (Proxy :: Proxy "increase")
-- | ```

module JS.Object where


import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, EffectFn7, EffectFn8, EffectFn9, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5, runEffectFn6, runEffectFn7, runEffectFn8, runEffectFn9)
import Prim.Row as Row
import Type.Prelude (Proxy)


-- | This is predefined representation for JS object which could be
-- | indexed with methods and used with functions from this modules.
-- |
-- | All the helpers are polymorphic over the actual "object" type
-- | so you are not forced to use it.
foreign import data JSObject :: Row Type -> Type

foreign import data EffectProp :: Type -> Type

-- | We could do something like `\obj -> pure (Record.Unsafe.unsafeGet m' (unsafeCoerce obj))`
-- | but the above results in three (`pure` + `unsafeGet` + `unsafeCoerce`) function calls instead of just...
-- | two (`function(obj) { return function() { unsafeRunEffectProp(obj, mth'); }}`). I'm not counting `Effect`
-- | wrapper function here.
foreign import unsafeRunEffectProp :: forall b obj. EffectFn2 String obj b

runEffectProp :: forall b mth_ mth obj s. IsSymbol s => Row.Cons s (EffectProp b) mth_ mth => Proxy s -> obj mth -> Effect b
runEffectProp m = do
  let
    m' = reflectSymbol m
  runEffectFn2 unsafeRunEffectProp m'

-- | We use `Type` kind to represent methods of JS object. We could introduce a `kind` like:
-- | ```
-- | foreign import kind JSMethod
-- | ```
-- | and use it:
-- | ```
-- | force import data EffectMth1 :: Type -> Type -> JSMethod
-- | ```
-- | So JS object methods could not be treated as values but it would enforce object
-- | row to only contain methods and would be probably too limiting.
foreign import data EffectMth0 :: Type -> Type

foreign import unsafeRunEffectMth0 :: forall b obj. EffectFn2 String obj b

runEffectMth0 :: forall b mth_ mth obj s. IsSymbol s => Row.Cons s (EffectMth0 b) mth_ mth => Proxy s -> obj mth -> Effect b
runEffectMth0 m = do
  let
    m' = reflectSymbol m
  runEffectFn2 unsafeRunEffectMth0 m'


foreign import data EffectMth1 :: Type -> Type -> Type

foreign import unsafeRunEffectMth1 :: forall a1 b obj. EffectFn3 String obj a1 b

runEffectMth1 :: forall a1 b mth_ mth obj s. IsSymbol s => Row.Cons s (EffectMth1 a1 b) mth_ mth => Proxy s -> obj mth -> a1 -> Effect b
runEffectMth1 m = do
  let
    m' = reflectSymbol m
  runEffectFn3 unsafeRunEffectMth1 m'


foreign import data EffectMth2 :: Type -> Type -> Type -> Type

foreign import unsafeRunEffectMth2 :: forall a1 a2 b obj. EffectFn4 String obj a1 a2 b

runEffectMth2 :: forall a1 a2 b mth_ mth obj s. IsSymbol s => Row.Cons s (EffectMth2 a1 a2 b) mth_ mth => Proxy s -> obj mth -> a1 -> a2 -> Effect b
runEffectMth2 m = do
  let
    m' = reflectSymbol m
  runEffectFn4 unsafeRunEffectMth2 m'

foreign import data EffectMth3 :: Type -> Type -> Type -> Type -> Type

foreign import unsafeRunEffectMth3 :: forall a1 a2 a3 b obj. EffectFn5 String obj a1 a2 a3 b

runEffectMth3 :: forall a1 a2 a3 b mth_ mth obj s. IsSymbol s => Row.Cons s (EffectMth3 a1 a2 a3 b) mth_ mth => Proxy s -> obj mth -> a1 -> a2 -> a3 -> Effect b
runEffectMth3 m = do
  let
    m' = reflectSymbol m
  runEffectFn5 unsafeRunEffectMth3 m'

foreign import data EffectMth4 :: Type -> Type -> Type -> Type -> Type -> Type

foreign import unsafeRunEffectMth4 :: forall a1 a2 a3 a4 b obj. EffectFn6 String obj a1 a2 a3 a4 b

runEffectMth4 :: forall a1 a2 a3 a4 b mth_ mth obj s. IsSymbol s => Row.Cons s (EffectMth4 a1 a2 a3 a4 b) mth_ mth => Proxy s -> obj mth -> a1 -> a2 -> a3 -> a4 -> Effect b
runEffectMth4 m = do
  let
    m' = reflectSymbol m
  runEffectFn6 unsafeRunEffectMth4 m'

foreign import data EffectMth5 :: Type -> Type -> Type -> Type -> Type -> Type -> Type

foreign import unsafeRunEffectMth5 :: forall a1 a2 a3 a4 a5 b obj. EffectFn7 String obj a1 a2 a3 a4 a5 b

runEffectMth5 :: forall a1 a2 a3 a4 a5 b mth_ mth obj s. IsSymbol s => Row.Cons s (EffectMth5 a1 a2 a3 a4 a5 b) mth_ mth => Proxy s -> obj mth -> a1 -> a2 -> a3 -> a4 -> a5 -> Effect b
runEffectMth5 m = do
  let
    m' = reflectSymbol m
  runEffectFn7 unsafeRunEffectMth5 m'

foreign import data EffectMth6 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type

foreign import unsafeRunEffectMth6 :: forall a1 a2 a3 a4 a5 a6 b obj. EffectFn8 String obj a1 a2 a3 a4 a5 a6 b

runEffectMth6 :: forall a1 a2 a3 a4 a5 a6 b mth_ mth obj s. IsSymbol s => Row.Cons s (EffectMth6 a1 a2 a3 a4 a5 a6 b) mth_ mth => Proxy s -> obj mth -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Effect b
runEffectMth6 m = do
  let
    m' = reflectSymbol m
  runEffectFn8 unsafeRunEffectMth6 m'


foreign import data EffectMth7 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type

foreign import unsafeRunEffectMth7 :: forall a1 a2 a3 a4 a5 a6 a7 b obj. EffectFn9 String obj a1 a2 a3 a4 a5 a6 a7 b

runEffectMth7 :: forall a1 a2 a3 a4 a5 a6 a7 b mth_ mth obj s. IsSymbol s => Row.Cons s (EffectMth7 a1 a2 a3 a4 a5 a6 a7 b) mth_ mth => Proxy s -> obj mth -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Effect b
runEffectMth7 m = do
  let
    m' = reflectSymbol m
  runEffectFn9 unsafeRunEffectMth7 m'

