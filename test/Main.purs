module Test.Main where

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

type IncreaseRow r = ( increase :: EffectMth0 Unit | r)

type IncreaseByRow r = (increaseBy :: EffectMth1 Int Unit | r)

type ValueRow r = (value :: EffectProp Int | r)

increase :: forall r. JSObject (IncreaseRow r) -> Effect Unit
increase = runEffectMth0 (Proxy :: Proxy "increase")

increaseBy :: forall r. JSObject (IncreaseByRow r) -> Int -> Effect Unit
increaseBy = runEffectMth1 (Proxy :: Proxy "increaseBy")

value :: forall r. JSObject (ValueRow r) -> Effect Int
value = runEffectProp (Proxy :: Proxy "value")

type Counter = JSObject (IncreaseRow + IncreaseByRow + ValueRow + ())

foreign import counter :: Effect Counter

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
        increaseBy c 4
        v <- value c
        pure v
      v `shouldEqual` 5
