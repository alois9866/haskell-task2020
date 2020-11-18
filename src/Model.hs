{-# LANGUAGE TemplateHaskell #-}

module Model where

import Data.Aeson.TH
import Data.Char

data Result = Result
  { operator :: Operator,
    arguments :: [Double],
    result :: Maybe Double,
    err :: Maybe String
  }

data Operator = Add | Sub | Mul | Div | Sqrt | Pow

$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Operator)
$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Result)
