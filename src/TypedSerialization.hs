{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypedSerialization (Program, TypedSerialize(..), Command, command, program, eval, showTypes) where

import Data.Maybe (fromJust)
import Data.List (intercalate)

{- | Serialization with type information

Type parameters:
 * x .. type that we want to serialize (Integer, Bool, etc.)
 * s .. serialization format (ByteString, JSON, etc.)
 * t .. runtime identification of type `x` (Text, JSON, etc.)

Class laws:
1. normal round-trip
   forall x.  deserialize (serialize x) == Just x
2. `type_` output corresponds tightly to types
   forall x1 x2 s t.  @x1 == @x2  <=>  (type_ @x1 @s @t) == (type_ @x2 @s @t)

These laws give us the ability to type-check deserializations ahead of time.
-}
class Eq t => TypedSerialize x s t where
  serialize   :: x -> s
  deserialize :: s -> Maybe x
  type_       :: t


-- | `Command` is a function (x -> y) transformed to work on serialized data. Type information is stored alongside.
-- This allows us to defer type checking to run time (performed in the `program` constructor).
data Command s t = Command
  t         -- ^ Input type
  t         -- ^ Output type
  (s -> s)  -- ^ Function

-- | Construct a `Command`, saving type information as runtime values
--
-- `fromJust` is safe here, since we can evaluate only a program type-checked by `program`
command :: forall s t x y. (TypedSerialize x s t, TypedSerialize y s t) => (x -> y) -> Command s t
command f = Command (type_ @x @s @t) (type_ @y @s @t) (serialize @y @s @t . f . fromJust . deserialize @x @s @t)


-- | Chain of commands which can be applied in a sequence. That is, the output type of a command
-- matches the input type of the next command. Also, the input type of the first command is `x`,
-- and the output type of the last program is `y`.
data Program s t x y = Program [Command s t]

-- | Show the sequence of types inside a `Program` as t1 ~> t2 ~> ... ~> tN.
showTypes :: forall s t x y. (TypedSerialize x s t, Show t) => Program s t x y -> String
showTypes (Program cs) = intercalate " ~> " $ map show (type_ @x @s @t : map ty cs) where
  ty (Command _ t _) = t

-- | Construct a program, validating correct type usage.
program :: forall s t x y. (TypedSerialize x s t, TypedSerialize y s t) => [Command s t] -> Maybe (Program s t x y)
program cs = if verify (type_ @x @s @t) (type_ @y @s @t) cs then Just (Program cs) else Nothing where

  verify :: t -> t -> [Command s t] -> Bool
  verify x y [] = x == y
  verify x y (Command a b f : cs) = x == a && verify b y cs

-- | Evaluate a program. This cannot fail, because only valid programs can be constructed by `program`.
eval :: forall x y s t. (TypedSerialize x s t, TypedSerialize y s t) => Program s t x y -> x -> y
eval (Program cs) = fromJust . deserialize @y @s @t . go cs . serialize @x @s @t where
  go [] x = x
  go (Command _ _ f : cs) x = go cs (f x)
