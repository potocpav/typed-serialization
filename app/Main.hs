{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import MyLib (TypedSerialize(..), Command, Program, command, program, eval, showTypes)
import Text.Read (readMaybe)


instance TypedSerialize Int String String where
  serialize = show
  deserialize = readMaybe
  type_ = "Int"

instance TypedSerialize Bool String String where
  serialize = show
  deserialize = readMaybe
  type_ = "Bool"


-- we will serialize both types and values to Strings
type CommandT = Command String String
type ProgramT = Program String String

main :: IO ()
main = do
  let
    -- data will be passed through Strings, but we can still work with Haskell values
    -- without worrying about serialization failures.

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
