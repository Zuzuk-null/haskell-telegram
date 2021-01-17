{-# LANGUAGE OverloadedStrings #-}

module Telegram.Commands where
import Telegram.Types
import Data.Either
import Data.Aeson
import Control.Monad
import Data.Function

import qualified Data.ByteString.Lazy.Internal as LBS
import Network.HTTP.Simple
import Data.Functor

method :: (Param a, FromJSON b) => String -> String -> [a] -> IO (Either String b)
method name token params = 
    parseRequest ("https://api.telegram.org/bot" ++ token ++ '/':name ++ toHttp params) 
    >>= httpLBS 
    <&> getResponseBody
    <&> (eitherDecode >=> getTelegramResponse)

getMe :: Bot -> IO User
getMe bot = either error id <$> getMeEither bot 

getMeEither :: Bot -> IO (Either String User)
getMeEither bot = method "getMe" (bot&token) ([] :: [NoneParam])

getUpdatesParams :: [GetUpdatesParam] -> Bot -> IO (Either String [Update]) 
getUpdatesParams params bot = method "getUpdates" (bot&token) params

getUpdates :: Bot -> IO (Either String [Update]) 
getUpdates = getUpdatesParams ([] :: [GetUpdatesParam])

sendMessageParams :: [SendMessageParam] -> Bot -> IO Message
sendMessageParams params bot =
    method "sendMessage" (bot&token) params 
    <&> either error id

sendMessage :: String -> String -> Bot -> IO Message
sendMessage chatId text = sendMessageParams [ChatId chatId, Text text] 
        
toHttp :: Param a => [a] -> String
toHttp [] = ""
toHttp (a:as) = '?':show a ++ Prelude.concat (('&':) . show <$> as)