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
    (Just id) <- liftM parseInt $ getLine
    run id >>= \case
        Left s -> print s
        Right p -> print p
