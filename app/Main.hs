{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Example usage of the TypedSerialization library.

module Main where

import TypedSerialization (TypedSerialize(..), Command, Program, command, program, eval, showTypes)
import Text.Read (readMaybe)


data TypeRep = Int' | Bool' deriving (Show, Eq)


instance TypedSerialize Int String TypeRep where
  serialize = show
  deserialize = readMaybe
  type_ = Int'

instance TypedSerialize Bool String TypeRep where
  serialize = show
  deserialize = readMaybe
  type_ = Bool'


-- we will serialize both types and values to Strings
type CommandT = Command String TypeRep
type ProgramT = Program String TypeRep


main :: IO ()
main = do
  let
    -- data will be passed through Strings, but we can still work with Haskell values
    -- without worrying about serialization failures. Ordinary Haskell functions can be lifted
    -- to `Command`s.

    equal :: Int -> CommandT
    equal n = command (== n)

    notBool :: CommandT
    notBool = command not

  -- construct and typecheck a program.
  notFiveProg <- case program [equal 5, notBool] :: Maybe (ProgramT Int Bool) of
    Just p -> pure p
    Nothing -> error "Type mismatch"

  -- show program type
  putStrLn $ "Type: " <> showTypes notFiveProg

  -- test on some values
  putStrLn $ "5 == 4: " <> show (eval notFiveProg 4)
  putStrLn $ "5 == 5: " <> show (eval notFiveProg 5)
