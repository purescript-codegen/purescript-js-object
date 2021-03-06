# purescript-js-object

Access js object methods and properties without writing JS bindings... ...or just generate mutable JS object FFI (without any codegen) from the type.


## The Problem

In PureScript we usually write FFI to object methods by implementing dedicated functions on both sides. It seems that we can provide set of generic helpers which without sacrificing the performance which are able to bind to properties and methods of a JS object (by using "uncurried" approach similar to `Effect.Uncurried` from `purescript-effect` under the hood).

## Usage

Let's imagine that we have a simple counter prototype defined on the JS side and we expose an "effectful" function which creates an instance for us (we are not able to use `new` directly from PS side):

```javascript
exports.counter = (function () {
  let Counter = function () {
    this.value = 0;
  };
  Counter.prototype.increase = function () {
    this.value++;
  };
  Counter.prototype.decrease = function () {
    this.value--;
  };
  // I'm working to cover constructors as well soon.
  // At the moment we need to expose a function ourselves.
  return function () {
    return new Counter();
  };
})();
```

Now we should be able to bind to this interface using generic helpers provided by this library:

```purescript
import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import JS.Object (EffectMth0, EffectMth1, EffectProp, JSObject, runEffectMth0, runEffectMth1, runEffectProp)
import JS.Object.Generic (mkFFI, mkNewtypedFFI)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type Counter = JSObject (increase :: EffectMth0 Unit, decrease :: EffectMth0 Unit, value :: EffectProp Int)

foreign import counter :: Effect Counter


-- You can generate this FFI record during the compilation time.
-- There is no runtime footprint over the manual binding.
-- Type signature is derived automatically but because we don't
-- use newtype... yet it would be fully expanded by default.
_Counter ::
  { increase :: Counter -> Effect Unit
  , decrease :: Counter -> Effect Unit
  , value :: Effect Int
  }
_Counter = mkFFI (Proxy :: Proxy Counter)


-- If you want you can use newtypes as well.
-- Here is a binding for hypothetical Person `JSObject`.
newtype Person = Person
  ( JSObject
      ( firstName :: EffectProp String
      , setFirstName :: EffectMth1 String Unit
      , lastName :: EffectProp String
      , setLastName :: EffectMth1 String Unit
      )
  )

derive instance Newtype Person _

_Person ::
  { firstName :: Person -> Effect String
  , lastName :: Person -> Effect String
  , setFirstName :: Person -> String -> Effect Unit
  , setLastName :: Person -> String -> Effect Unit
  }
_Person = mkNewtypedFFI (Proxy :: Proxy Person)


-- You can also use lower level functions to construct bindings yourself.
increase :: Counter -> Effect Unit
increase = runEffectMth0 (Proxy :: Proxy "increase")

decrease :: Counter -> Effect Unit
decrease = runEffectMth0 (Proxy :: Proxy "increase")

value :: Counter -> Effect Int
value = runEffectProp (Proxy :: Proxy "value")

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "JS.Object" do
    it "property access" do
      c <- liftEffect counter
      v <- liftEffect $ value c
      v `shouldEqual` 0
    it "method call" do
      v <- liftEffect $ do
        c <- counter
        increase c
        increase c
        v <- value c
        pure v
      v `shouldEqual` 2

```

There are two nice properties of this generic method of binding to JS object:

  * whenever we feed the "method name proxy" to the one of `runEffectMth*` helpers there is no additional overhead - we get back a function which is not dependent on any type class dict and is only passing the rest of the arguments to the uncurried object method by using standard `runEffectFn*` under the hood.

  * we can use the same binding functions to different objects as long as they share a particular method signature / interface. We can think about the row as the TS interface and about the particular object as an instance which implements it. The above example could be written as:

  ```purescript
  type IncreaseInterface r = ( increase :: EffectMth0 Unit | r)

  type DecreaseInterface r = (decrease :: EffectMth0 Unit | r)

  type ValueInterface r = (value :: EffectProp Int | r)

  increase :: forall r. JSObject (IncreaseInterface r) -> Effect Unit
  increase = runEffectMth0 (Proxy :: Proxy "increase")

  decrease :: forall r. JSObject (DecreaseInterface r) -> Effect Unit
  decrease = runEffectMth0 (Proxy :: Proxy "decrease")

  value :: forall r. JSObject (ValueInterface r) -> Effect Int
  value = runEffectProp (Proxy :: Proxy "value")

  type Counter = JSObject (IncreaseInterface + DecreaseInterface + ValueInterface + ())

  ```


## Testing

```shell
$ spago --config devel.dhall test
```
