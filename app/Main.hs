{-# LANGUAGE LambdaCase #-}

module Main where

import           Lib

parseInt :: String -> Maybe Int
parseInt s =
  case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

main :: IO ()
main = do
  print "What ID should we search?"
  (fmap parseInt $ getLine) >>= \case
    Nothing -> print "Please provide an integer ID"
    Just id -> run id >>= print
