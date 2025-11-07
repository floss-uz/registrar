{-# LANGUAGE OverloadedStrings #-}

module Registrar.TelegramAuth where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.UTF8 qualified as BSU
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Control.Concurrent.STM (TVar, readTVarIO)
import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (trace)
import Registrar.ClientTypes
import Registrar.Prelude (Generic, Text, Type)
import Registrar.Types (TelegramAuth (..))

class B16DecRes a where b16DecRes :: a -> BS.ByteString
instance B16DecRes (Either String BS.ByteString) where
  b16DecRes = either error (\x -> x)
instance B16DecRes (BS.ByteString, BS.ByteString) where
  b16DecRes = fst

b16decode :: BS.ByteString -> BS.ByteString
b16decode = b16DecRes . B16.decode

hmacEqualsEncoded :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool
hmacEqualsEncoded key msg hex = SHA256.hmac key msg == b16decode hex

mkayload :: Map.Map String String -> String
mkayload =
  List.intercalate "\n"
    . map (\(k, v) -> k ++ "=" ++ v)
    . List.sortOn fst
    . Map.toList
    . Map.delete "hash"

verify :: String -> Map.Map String String -> Bool
verify botToken inputMap =
  case Map.lookup "hash" inputMap of
    Nothing -> False
    Just providedHash ->
      let payloadStr = mkayload inputMap
          secretKey = SHA256.hash (BSU.fromString botToken)
       in hmacEqualsEncoded secretKey (BSU.fromString payloadStr) (BSU.fromString providedHash)

authToMap :: TelegramAuth -> Map.Map String String
authToMap ta =
  (Map.insert "id" (show ta.id))
    . (Map.insert "auth_date" (show ta.auth_date))
    . (justAddToMap "first_name" ta.first_name)
    . (justAddToMap "last_name" ta.last_name)
    . (justAddToMap "photo_url" ta.photo_url)
    . (justAddToMap "username" ta.username)
    $ (Map.insert "hash" ta.hash) Map.empty
 where
  justAddToMap :: String -> Maybe a -> Map.Map String a -> Map.Map String a
  justAddToMap k v m = case v of
    Just v -> Map.insert k v m
    Nothing -> m

verifyAuth :: Text -> TelegramAuth -> AuthResp
verifyAuth tk ta = MkAuthResp $ verify (T.unpack tk) $ authToMap ta
