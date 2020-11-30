module Lib
  ( startApp,
    app,
  )
where

import Api
import Network.Wai.Handler.Warp
import Servant

startApp :: Int -> IO ()
startApp port = run port app

app :: Application
app = serve api server
