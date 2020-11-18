module Lib
  ( startApp,
    app,
  )
where

import Api
import Network.Wai.Handler.Warp
import Servant

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server
