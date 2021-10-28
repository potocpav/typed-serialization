{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypedSerialization (Program, TypedSerialize(..), Command, command, program, eval, showTypes) where

import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Type.Reflection

{- | Serialization with type information

Type parameters:
 * x .. type that we want to serialize
 * s .. serialization format

This law must hold:

  deserialize . serialize == Just
-}
class Typeable x => TypedSerialize x s where
  serialize   :: x -> s
  deserialize :: s -> Maybe x


-- | `Command` is a function (x -> y) transformed to work on serialized data. Type information is stored alongside.
-- This allows us to defer type checking to run time (performed in the `program` constructor).
data Command s = Command
  SomeTypeRep  -- ^ Input type
  SomeTypeRep  -- ^ Output type
  (s -> s)     -- ^ Function

-- | Construct a `Command`, saving type information as runtime values
--
-- `fromJust` is safe here, since we can evaluate only a program type-checked by `program`
command :: forall s x y. (TypedSerialize x s, TypedSerialize y s) => (x -> y) -> Command s
command f = Command
  (someTypeRep (Proxy @x))
  (someTypeRep (Proxy @y))
  (serialize @y @s . f . fromJust . deserialize @x @s)


-- | Chain of commands which can be applied in a sequence. That is, the output type of a command
-- matches the input type of the next command. Also, the input type of the first command is `x`,
-- and the output type of the last program is `y`.
data Program s x y = Program [Command s]

-- | Show the sequence of types inside a `Program` as t1 ~> t2 ~> ... ~> tN.
showTypes :: forall s x y. (TypedSerialize x s) => Program s x y -> String
showTypes (Program cs) = intercalate " ~> " $ map show (someTypeRep (Proxy @x) : map ty cs) where
  ty (Command _ t _) = t

-- | Construct a program, validating correct type usage.
program :: forall s x y. (TypedSerialize x s, TypedSerialize y s) => [Command s] -> Maybe (Program s x y)
program cs = if verify (someTypeRep (Proxy @x)) (someTypeRep (Proxy @y)) cs
  then Just (Program cs)
  else Nothing
  where

  verify :: SomeTypeRep -> SomeTypeRep -> [Command s] -> Bool
  verify x y [] = x == y
  verify x y (Command a b f : cs) = x == a && verify b y cs

-- | Evaluate a program. This cannot fail, because only valid programs can be constructed by `program`.
eval :: forall x y s. (TypedSerialize x s, TypedSerialize y s) => Program s x y -> x -> y
eval (Program cs) = fromJust . deserialize @y @s . go cs . serialize @x @s where
  go [] x = x
  go (Command _ _ f : cs) x = go cs (f x)
