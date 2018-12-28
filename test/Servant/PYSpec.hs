{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.PYSpec where

import           Data.Aeson
import           Data.Monoid                                ()
import           Data.Proxy()
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           GHC.TypeLits()
import           Prelude                                    ()
import           Prelude.Compat
import           Data.Proxy
import           GHC.Generics
import qualified Data.ByteString.Char8                      as B
import           Test.Hspec                                 hiding
                                                             (shouldContain,
                                                             shouldNotContain)
import           Test.QuickCheck                            (Arbitrary (..),
                                                             choose, listOf,
                                                             property)

import           Servant.API.ContentTypes()
import           Servant.API.Internal.Test.ComprehensiveAPI()
import           Servant.Foreign

import           Servant.PY.Internal
import           Servant.PY.Requests
import           Servant.PY

customOptions :: CommonGeneratorOptions
customOptions = defCommonGeneratorOptions
 { urlPrefix = "urlForRequesting:9000"
 , returnMode = DangerMode
 }

spec :: Spec
spec = describe "Servant.PY.Internal" internalSpec

shouldContain :: Text -> Text -> Expectation
a `shouldContain` b  = shouldSatisfy a (T.isInfixOf b)

shouldNotContain :: Text -> Text -> Expectation
a `shouldNotContain` b  = shouldNotSatisfy a (T.isInfixOf b)

newtype ASCII = ASCII {getASCII :: T.Text} deriving (Show)

instance Arbitrary ASCII where
   -- Our arbitrary instance is generating only ASCII, since the language-ecmascript's lexer
   -- is currently (October 2016) still a bit naïve
   arbitrary = fmap (ASCII . T.pack) $ listOf $ choose (minBound, '\127')
   shrink xs = (ASCII . T.pack) <$> shrink (T.unpack $ getASCII xs)


type OctetStreamTestApi = "upload-octet-stream"
                          :> ReqBody '[OctetStream] B.ByteString
                          :> Post '[JSON] SomeJson
octetStreamTestApi :: Proxy OctetStreamTestApi
octetStreamTestApi = Proxy

data SomeJson = SomeJson
 { uvalue       :: !T.Text
 , pvalue       :: !T.Text
 , otherMissing :: Maybe T.Text
 } deriving (Eq, Show, Generic)
instance ToJSON SomeJson


internalSpec :: Spec
internalSpec = describe "Internal" $ do
  describe "indenter" $ do
    it "should only indent using whitespace" $
      property $ \n -> indenter n indent == mconcat (replicate n (T.pack " "))


  describe "pyForAPI" $ do
    it "should work with OctetStream" $ do
      pyForAPI octetStreamTestApi requests
