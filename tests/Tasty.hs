module Main (main) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = sequence tests >>= defaultMain . testGroup "jml-web-service"
  where
    tests = []
