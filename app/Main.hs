{-# LANGUAGE LambdaCase        #-}

module Main where

import           Lib
import Control.Monad

parseInt :: String -> Maybe Int
parseInt s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

main :: IO ()
main = do
    (liftM parseInt $ getLine) >>= \case
        Nothing -> print "Please provide an integer ID"
        Just id ->
            run id >>= \case
                Left s -> print s
                Right p -> print p
