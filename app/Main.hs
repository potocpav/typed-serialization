{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

-- | Example usage of the TypedSerialization library.

module Main where

import TypedSerialization (TypedSerialize(..), Command, Program, command, program, eval, showTypes)
import Text.Read (readMaybe)


instance TypedSerialize Int String where
  serialize = show
  deserialize = readMaybe

instance TypedSerialize Bool String where
  serialize = show
  deserialize = readMaybe


main :: IO ()
main = do
  let

    -- data will be passed through Strings, but we can still work with Haskell values
    -- without worrying about serialization failures. Ordinary Haskell functions can be lifted
    -- to `Command`s.

    equal :: Int -> Command String
    equal n = command (== n)

    notBool :: Command String
    notBool = command not

  -- construct and typecheck a program.
  notFiveProg <- case program [equal 5, notBool] :: Maybe (Program String Int Bool) of
    Just p -> pure p
    Nothing -> error "Type mismatch"

  -- show program type
  putStrLn $ "Type: " <> showTypes notFiveProg

  -- test on some values
  putStrLn $ "5 == 4: " <> show (eval notFiveProg 4)
  putStrLn $ "5 == 5: " <> show (eval notFiveProg 5)
