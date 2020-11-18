{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /add/1/2" $ do
    it "responds with 200" $ do
      get "/add/1/2" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"add\",\"arguments\":[1.0,2.0],\"result\":3.0,\"err\":null}"
       in get "/add/1/2" `shouldRespondWith` result
  describe "GET /add/1" $ do
    it "responds with 404" $ do
      get "/add/1" `shouldRespondWith` 404
  describe "GET /add/1/2/3" $ do
    it "responds with 404" $ do
      get "/add/1/2/3" `shouldRespondWith` 404
  describe "GET /add/1.123/2.456789" $ do
    it "responds with 200" $ do
      get "/add/1.123/2.456789" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"add\",\"arguments\":[1.123,2.456789],\"result\":3.579789,\"err\":null}"
       in get "/add/1.123/2.456789" `shouldRespondWith` result
  describe "GET /add/1.123/-0" $ do
    it "responds with 200" $ do
      get "/add/1.123/-0" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"add\",\"arguments\":[1.123,-0.0],\"result\":1.123,\"err\":null}"
       in get "/add/1.123/-0" `shouldRespondWith` result
  describe "GET /add/-1/2" $ do
    it "responds with 200" $ do
      get "/add/-1/2" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"add\",\"arguments\":[-1.0,2.0],\"result\":1.0,\"err\":null}"
       in get "/add/-1/2" `shouldRespondWith` result
  describe "GET /add/1/-2" $ do
    it "responds with 200" $ do
      get "/add/1/-2" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"add\",\"arguments\":[1.0,-2.0],\"result\":-1.0,\"err\":null}"
       in get "/add/1/-2" `shouldRespondWith` result
  describe "GET /sub/1/2" $ do
    it "responds with 200" $ do
      get "/sub/1/2" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"sub\",\"arguments\":[1.0,2.0],\"result\":-1.0,\"err\":null}"
       in get "/sub/1/2" `shouldRespondWith` result
  describe "GET /mul/5/3" $ do
    it "responds with 200" $ do
      get "/mul/5/3" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"mul\",\"arguments\":[5.0,3.0],\"result\":15.0,\"err\":null}"
       in get "/mul/5/3" `shouldRespondWith` result
  describe "GET /div/10/2" $ do
    it "responds with 200" $ do
      get "/div/10/2" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"div\",\"arguments\":[10.0,2.0],\"result\":5.0,\"err\":null}"
       in get "/div/10/2" `shouldRespondWith` result
  describe "GET /div/10/3" $ do
    it "responds with 200" $ do
      get "/div/10/3" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"div\",\"arguments\":[10.0,3.0],\"result\":3.333333,\"err\":null}"
       in get "/div/10/3" `shouldRespondWith` result
  describe "GET /div/10/0" $ do
    it "responds with 200" $ do
      get "/div/10/0" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"div\",\"arguments\":[10.0,0.0],\"result\":null,\"err\":\"division by zero\"}"
       in get "/div/10/0" `shouldRespondWith` result
  describe "GET /sqrt/16/2" $ do
    it "responds with 404" $ do
      get "/sqrt/16/2" `shouldRespondWith` 404
  describe "GET /sqrt/16" $ do
    it "responds with 200" $ do
      get "/sqrt/16" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"sqrt\",\"arguments\":[16.0],\"result\":4.0,\"err\":null}"
       in get "/sqrt/16" `shouldRespondWith` result
  describe "GET /sqrt/-16" $ do
    it "responds with 200" $ do
      get "/sqrt/-16" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"sqrt\",\"arguments\":[-16.0],\"result\":null,\"err\":\"square root of negative number\"}"
       in get "/sqrt/-16" `shouldRespondWith` result
  describe "GET /pow/2/5" $ do
    it "responds with 200" $ do
      get "/pow/2/5" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"pow\",\"arguments\":[2.0,5.0],\"result\":32.0,\"err\":null}"
       in get "/pow/2/5" `shouldRespondWith` result
  describe "GET /pow/0/5" $ do
    it "responds with 200" $ do
      get "/pow/0/5" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"pow\",\"arguments\":[0.0,5.0],\"result\":0.0,\"err\":null}"
       in get "/pow/0/5" `shouldRespondWith` result
  describe "GET /pow/2/-2" $ do
    it "responds with 200" $ do
      get "/pow/2/-2" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"pow\",\"arguments\":[2.0,-2.0],\"result\":0.25,\"err\":null}"
       in get "/pow/2/-2" `shouldRespondWith` result
  describe "GET /pow/2/1.234" $ do
    it "responds with 400" $ do
      get "/pow/2/1.234" `shouldRespondWith` 400
  describe "GET /pow/0/-1" $ do
    it "responds with 200" $ do
      get "/pow/0/-1" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"pow\",\"arguments\":[0.0,-1.0],\"result\":null,\"err\":\"exponentiation of zero to non-positive power\"}"
       in get "/pow/0/-1" `shouldRespondWith` result
  describe "GET /pow/0/0" $ do
    it "responds with 200" $ do
      get "/pow/0/0" `shouldRespondWith` 200
    it "responds with Result" $ do
      let result = "{\"operator\":\"pow\",\"arguments\":[0.0,0.0],\"result\":null,\"err\":\"exponentiation of zero to non-positive power\"}"
       in get "/pow/0/0" `shouldRespondWith` result
