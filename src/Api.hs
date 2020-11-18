{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Model
import Servant

type API = "add"  :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] Result
      :<|> "sub"  :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] Result
      :<|> "mul"  :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] Result
      :<|> "div"  :> Capture "x" Double :> Capture "y" Double :> Get '[JSON] Result
      :<|> "sqrt" :> Capture "x" Double :> Get '[JSON] Result
      :<|> "pow"  :> Capture "x" Double :> Capture "n" Integer :> Get '[JSON] Result

api :: Proxy API
api = Proxy

server :: Server API
server = addHandler :<|> subHandler :<|> mulHandler :<|> divHandler :<|> sqrtHandler :<|> powHandler

addHandler :: Double -> Double -> Handler Result
addHandler x y = return Result
  { operator = Add
  , arguments = [x, y]
  , result = Just . round6 $ x + y
  , err = Nothing
  }

subHandler :: Double -> Double -> Handler Result
subHandler x y = return Result
  { operator = Sub
  , arguments = [x, y]
  , result = Just . round6 $ x - y
  , err = Nothing
  }

mulHandler :: Double -> Double -> Handler Result
mulHandler x y = return Result
  { operator = Mul
  , arguments = [x, y]
  , result = Just . round6 $ x * y
  , err = Nothing
  }

divHandler :: Double -> Double -> Handler Result
divHandler x 0 = return Result
  { operator = Div
  , arguments = [x, 0]
  , result = Nothing
  , err = Just "division by zero"
  }
divHandler x y = return Result
  { operator = Div
  , arguments = [x, y]
  , result = Just . round6 $ x / y
  , err = Nothing
  }

sqrtHandler :: Double -> Handler Result
sqrtHandler x
  | x >= 0 = return Result
    { operator = Sqrt
    , arguments = [x]
    , result = Just . round6 $ sqrt x
    , err = Nothing
    }
  | otherwise = return Result
    { operator = Sqrt
    , arguments = [x]
    , result = Nothing
    , err = Just "square root of negative number"
    }

powHandler :: Double -> Integer -> Handler Result
powHandler x n = let dn = fromInteger n in
  if x == 0 && dn <= 0 then
    return Result
      { operator = Pow
      , arguments = [x, dn]
      , result = Nothing
      , err = Just "exponentiation of zero to non-positive power"
      }
  else
    return Result
      { operator = Pow
      , arguments = [x, dn]
      , result = Just . round6 $ x ** dn
      , err = Nothing
      }

round6 :: Double -> Double
round6 = (/1e6) . fromIntegral . round . (*1e6)
