{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Serializable (Program, Serializable(..), Command, command, program, eval, showTypes) where

import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)
import Text.Read (readEither)
import Data.Aeson (Result(..), ToJSON, FromJSON, Value, fromJSON, toJSON)

{- | Serialization with type information

Type parameters:
 * x .. type that we want to serialize
 * s .. serialization format

This law must hold:

  deserialize . serialize == Right
-}
class Typeable x => Serializable x s where
  serialize   :: x -> s
  deserialize :: s -> Either String x


-- Instance for JSON values, ignoring lossy number representation issues
instance (Typeable x, ToJSON x, FromJSON x) => Serializable x Value where
  serialize = toJSON
  deserialize v = case fromJSON v of
    Error e -> Left e
    Success a -> Right a


instance (Typeable x, Show x, Read x) => Serializable x String where
  serialize = show
  deserialize = readEither


-- | `Command` is a function (x -> y) transformed to work on serialized data. Type information is stored alongside.
-- This allows us to defer type checking to run time (performed in the `program` constructor).
data Command s = Command
  SomeTypeRep  -- ^ Input type
  SomeTypeRep  -- ^ Output type
  (s -> s)     -- ^ Function

-- | Construct a `Command`, saving type information as runtime values
--
-- `fromJust` is safe here, since we can evaluate only a program type-checked by `program`
command :: forall s x y. (Serializable x s, Serializable y s) => (x -> y) -> Command s
command f = Command
  (someTypeRep (Proxy @x))
  (someTypeRep (Proxy @y))
  (serialize @y @s . f . either error id . deserialize @x @s)


-- | Chain of commands which can be applied in a sequence. That is, the output type of a command
-- matches the input type of the next command. Also, the input type of the first command is `x`,
-- and the output type of the last program is `y`.
data Program s x y = Program [Command s]

-- | Show the sequence of types inside a `Program` as t1 ~> t2 ~> ... ~> tN.
showTypes :: forall s x y. (Serializable x s) => Program s x y -> String
showTypes (Program cs) = intercalate " ~> " $ map show (someTypeRep (Proxy @x) : map ty cs) where
  ty (Command _ t _) = t

-- | Construct a program, validating correct type usage.
program :: forall s x y. (Serializable x s, Serializable y s) => [Command s] -> Maybe (Program s x y)
program cs = if verify (someTypeRep (Proxy @x)) (someTypeRep (Proxy @y)) cs
  then Just (Program cs)
  else Nothing
  where

  verify :: SomeTypeRep -> SomeTypeRep -> [Command s] -> Bool
  verify x y [] = x == y
  verify x y (Command a b f : cs) = x == a && verify b y cs

-- | Evaluate a program. This cannot fail, because only valid programs can be constructed by `program`.
eval :: forall x y s. (Serializable x s, Serializable y s) => Program s x y -> x -> y
eval (Program cs) = either error id . deserialize @y @s . go cs . serialize @x @s where
  go [] x = x
  go (Command _ _ f : cs) x = go cs (f x)
