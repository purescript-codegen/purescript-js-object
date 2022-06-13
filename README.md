# purescript-js-object

Access js object methods and properties without writing JS bindings.

## The Problem

In PureScript we usually write FFI to object methods by implementing dedicated functions on both sides. It seems we can provide generic function without sacrificing the performance to bind to properties and methods of a JS object using "uncurried" approach similar to `Effect.Uncurried` (from `purescript-effect`).

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
  return function () {
    return new Counter();
  };
})();
```

Now we should be able to bind to this interface using generic helpers provided by this library:

```purescript
import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import JS.Object (EffectMth0, EffectMth1, EffectProp, JSObject, runEffectMth0, runEffectMth1, runEffectProp)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type Counter = JSObject (increase :: EffectMth0 Unit, decrease :: EffectMth0, value :: EffectProp Int)

foreign import counter :: Effect Counter

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

There are two cool properties of this generic method of binding to JS object:

  * whenever we feed the "method name proxy" to the one of `runEffectMth*` helpers there is no additional overhead - we get back a function which is not dependent on any type class dict and is only passing the rest of the arguments to the uncurried object method by using standard `runEffectFn*` under the hood.

  * we can reuse binding functions to different objects as long as they share a particular method signature - we can even think about parts of the row as parts of TS interface and about the particular object as an implementation of it. The above example could be written as:

  ```purescript
  type IncreaseInterface r = ( increase :: EffectMth0 Unit | r)

  type DecreaseInterface r = (decrease :: EffectMth1 Int Unit | r)

  type ValueInterface r = (value :: EffectProp Int | r)

  increase :: forall r. JSObject (IncreaseInterface r) -> Effect Unit
  increase = runEffectMth0 (Proxy :: Proxy "increase")

  decrease :: forall r. JSObject (DecreaseInterface r) -> Effect Unit
  decrease = runEffectMth0 (Proxy :: Proxy "decrease")

  value :: forall r. JSObject (ValueInterface r) -> Effect Int
  value = runEffectProp (Proxy :: Proxy "value")

  type Counter = JSObject (IncreaseInterface + DecreaseInterface + ValueInterface + ())

  ```

