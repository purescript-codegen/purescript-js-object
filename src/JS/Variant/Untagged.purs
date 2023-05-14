module JS.Variant.Untagged where

import Prelude

import Data.Function.Uncurried (Fn4, runFn4)
import Data.Nullable (Nullable, notNull, null)
import Data.Symbol (class IsSymbol)
import Foreign (Foreign, isNull)
import Foreign (typeOf) as Foreign
import Foreign.Object (Object) as Foreign
import JS.Unsafe.Stringify (unsafeStringify)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Record as Record
import Type.Prelude (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data TypeName :: Type

instance Eq TypeName where
  eq = unsafeRefEq

boolean :: TypeName
boolean = unsafeCoerce "boolean"

function :: TypeName
function = unsafeCoerce "function"

number :: TypeName
number = unsafeCoerce "number"

object :: TypeName
object = unsafeCoerce "object"

string :: TypeName
string = unsafeCoerce "string"

undefined :: TypeName
undefined = unsafeCoerce "undefined"

typeOf :: Foreign -> TypeName
typeOf = unsafeCoerce <<< Foreign.typeOf

newtype Untagged :: Row Type -> Type
newtype Untagged r = Untagged Foreign

type BooleanRow r = (boolean :: Boolean | r)

type NumberRow r = (number :: Number | r)

type ObjectRow r = (object :: Foreign.Object Foreign | r)

type StringRow r = (string :: String | r)

foreign import data Undefined :: Type

foreign import data Null :: Type

type UndefinedRow r = (undefined :: Undefined | r)

caseNullable :: forall a b. Nullable a -> (a -> b) -> (Unit -> b) -> b
caseNullable v handle cont = runFn4 caseNullableImpl v handle cont unit

foreign import caseNullableImpl :: forall a b. Fn4 (Nullable a) (a -> b) (Unit -> b) Unit b

-- | I use term `narrow` which I borrow from `TypeScript`
type NarrowFn a b = Foreign -> (a -> b) -> (Unit -> b) -> b


type NarrowPrimitiveRow b =
  ( boolean :: NarrowFn Boolean b
  , null :: NarrowFn Null b
  , number :: NarrowFn Number b
  , object :: NarrowFn Foreign b
  , string :: NarrowFn String b
  , undefined :: NarrowFn Undefined b
  )

narrowPrimitive
  :: forall b
   . { | NarrowPrimitiveRow b }
narrowPrimitive = do
  let
    narrow :: forall a. (Foreign -> Boolean) -> NarrowFn a b
    narrow check foreignValue handleValue cont =
      if check foreignValue then handleValue (unsafeCoerce foreignValue)
      else cont unit

    narrow' :: forall a. TypeName -> NarrowFn a b
    narrow' expected = narrow (eq expected <<< typeOf)
  { boolean: narrow' boolean
  , null: narrow isNull
  , number: narrow' number
  , object: narrow' object
  , string: narrow' string
  , undefined: narrow' undefined
  }

onPrimitive
  :: forall a b prim' r r' sym
   . Row.Cons sym (NarrowFn a b) prim' (NarrowPrimitiveRow b)
  => Row.Cons sym a r' r
  => IsSymbol sym
  => Proxy sym
  -> (a -> b)
  -> (Untagged r' -> b)
  -> Untagged r
  -> b
onPrimitive label handle cont = do
  let
    narrow :: NarrowFn a b
    narrow = Record.get label (narrowPrimitive :: { | NarrowPrimitiveRow b})
  \(Untagged value) -> do
     let
        value' :: Untagged r'
        value' = (Untagged value)
     narrow value handle (const (cont value') :: Unit -> b)

case_ :: forall a. Untagged () -> a
case_ (Untagged foreignValue) = unsafeCrashWith $ do
  let
    repr = unsafeStringify foreignValue
  "JS.Variant.Untagged: pattern match failure [" <> repr <> "]"

type CastFn a = Foreign -> Nullable a

type CastPrimitiveRow =
  ( boolean :: CastFn Boolean
  , null :: CastFn Null
  , number :: CastFn Number
  , object :: CastFn Foreign
  , string :: CastFn String
  , undefined :: CastFn Undefined
  )

castFn :: forall a. (forall b. NarrowFn a b) -> Foreign -> Nullable a
castFn narrowFn value = narrowFn value notNull (const $ null)

-- TODO: When
castPrimitive :: { | CastPrimitiveRow }
castPrimitive = do
  { boolean: castFn narrowPrimitive.boolean
  , null: castFn narrowPrimitive.null
  , number: castFn narrowPrimitive.number
  , object: castFn narrowPrimitive.object
  , string: castFn narrowPrimitive.string
  , undefined: castFn narrowPrimitive.undefined
  }


-- match
--   ∷ ∀ rl r r1 r2 b
--   . RL.RowToList r rl
--   ⇒ VariantMatchCases rl r1 b
--   ⇒ R.Union r1 () r2
--   ⇒ Record r
--   → Variant r2
--   → b
-- match r = case_ # onMatch r

