{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.ByteString
import Data.Aeson
import Data.Functor
import Network.HTTP.Simple
import Data.Function
import Control.Monad
import Data.Aeson.Encoding.Internal
import Control.Exception
import Data.Maybe
import Data.Either
import Control.Applicative

main :: IO ()
main = do
    let bot = Bot "1077579435:AAGx0QNkCU3PGV8TzsARjYhrDdf-1p28518"
    
    updates <- bot&getMe
    mes <- bot&sendMessage "728729538" "lolo"
    
    print mes
    
getMe :: Bot -> IO User
getMe bot = either error id <$> getMeEither bot 

getMeEither :: Bot -> IO (Either String User)
getMeEither bot = (eitherDecode >=> getTelegramResponse) <$> method "getMe" (bot&token) ([] :: [NoneParam])

data NoneParam = NoneParam deriving (Show, Param)

data Update = NewMessage Message
            | InlineQuery
            | ChosenInlineResult
            | CallbackCuery

getUpdatesParams :: [GetUpdatesParam] -> Bot -> IO LBS.ByteString 
getUpdatesParams params bot = method "getUpdates" (bot&token) params

getUpdates :: Bot -> IO LBS.ByteString 
getUpdates = getUpdatesParams ([] :: [GetUpdatesParam])



class (Show a) => Param a where
    toHttpParam :: a -> String
    toHttpParam a = '?':show a 

data SendMessageParam = ChatId String
                | Text String
                | ParseMode String 
                | DisableWebPagePreview Bool 
                | DisableNotification Bool 
                | ReplyToMessageId Int 
                | ReplyMarkup -- TODO

data GetUpdatesParam = Offset Int
                     | Limit Int
                     | Timeout Int

instance Show GetUpdatesParam where
    show (Offset a) = "?offset=" ++ show a
    show (Limit a) = "?limit=" ++ show a
    show (Timeout a) = "?timeout=" ++ show a

instance Param GetUpdatesParam

instance Show SendMessageParam where
    show (ChatId a) = "chat_id=" ++ a
    show (Text a) = "text=" ++ a
    show (ParseMode a) = "parse_mode=" ++ a
    show (DisableWebPagePreview a) = "disable_web_page_preview=" ++ show a
    show (DisableNotification a) = "disable_notification=" ++ show a
    show (ReplyToMessageId a) = "reply_to_message_id=" ++ show a 
    show ReplyMarkup = error "ReplyMarkup: not implemented!" -- TODO

instance Param SendMessageParam

sendMessageParams :: [SendMessageParam] -> Bot -> IO Message
sendMessageParams params bot =
    method "sendMessage" (bot&token) params 
    <&> (eitherDecode >=> getTelegramResponse)
    <&> either error id

sendMessage :: String -> String -> Bot -> IO Message
sendMessage chatId text = sendMessageParams [ChatId chatId, Text text]

newtype TelegramResponse a = TelegramResponse { getTelegramResponse :: Either String a }

instance (FromJSON a) => FromJSON (TelegramResponse a) where
    parseJSON (Object obj) = 
        TelegramResponse 
        <$> (Right <$> obj .: "result" 
             <|> Left <$> ((++) <$> obj .: "error_code" <*> obj .: "description"))

data User = User { userId :: Int
                 , isBot :: Bool
                 , firstName :: String 
                 , lastName :: String
                 , username :: String 
                 } deriving Show 

data Chat = Chat { chatId :: Int 
                 , chatType :: ChatType
                 , title :: Maybe String
                 , chatName :: Maybe String
                 , chatFirstName :: Maybe String
                 , chatLastName :: Maybe String
                 } deriving Show

data ChatType = Private
              | Group
              | SuperGroup
              | Channel deriving Show

data Message = Message { messageId :: Int
                       , from :: Maybe User
                       , date :: Int
                       , chat :: Chat
                       , forwardFrom :: Maybe User
                       , forwardDate :: Maybe Int
                       , replyToMessage :: Maybe Message
                       , messageText :: Maybe String
                       --, entities :: Maybe [Entity]
                       --, audio :: Maybe Audio
                       --, document :: Maybe Document
                       --, photo :: Maybe [PhotoSize]
                       --, sticker :: Maybe Sticker
                       --, video :: Maybe Video
                       --, voice :: Voice
                       , caption :: Maybe String
                       --, contact :: Contact
                       --, location :: Location
                       --, venue :: Venue
                       , newChatMember :: Maybe User
                       , leftChatMember :: Maybe User
                       , newChatTitle :: Maybe String
                       --, newChatPhoto :: Maybe [PhotoSize]
                       , deleteChatPhoto :: Bool
                       , groupChatCreated :: Bool
                       , supergroupChatCreated :: Bool
                       , channelChatCreated :: Bool
                       , migrateToChatId :: Maybe Int
                       , migrate_from_chat_id :: Maybe Int
                       , pinnedMessage :: Maybe Message
                       } deriving Show

instance FromJSON Message where
    parseJSON (Object mes) = Message
        <$> mes .: "message_id"
        <*> mes .:? "from"
        <*> mes .: "date"
        <*> mes .: "chat"
        <*> mes .:? "forward_from"
        <*> mes .:? "forward_date"
        <*> mes .:? "reply_to_message"
        <*> mes .:? "message_text"
        <*> mes .:? "caption"
        <*> mes .:? "new_chat_member"
        <*> mes .:? "left_chat_member"
        <*> mes .:? "new_chat_title"
        <*> (fromMaybe False <$> mes .:? "delete_chat_photo")
        <*> (fromMaybe False <$> mes .:? "group_chat_created")
        <*> (fromMaybe False <$> mes .:? "supergroup_chat_created")
        <*> (fromMaybe False <$> mes .:? "channel_chat_created")
        <*> mes .:? "migrate_to_chat_id"
        <*> mes .:? "migrate_from_chat_id"
        <*> mes .:? "pinned_message"

instance Read ChatType where
    readsPrec 10 "private" = [(Private, "")]
    readsPrec 10 "group" = [(Group, "")]
    readsPrec 10 "supergroup" = [(SuperGroup, "")]
    readsPrec 10 "channel" = [(Channel, "")]
    readsPrec _ _ = []
 
instance FromJSON Chat where
    parseJSON (Object chat) = Chat
        <$> chat .: "id"
        <*> (read <$> chat .: "chatType")
        <*> chat .:? "title"
        <*> chat .:? "name"
        <*> chat .:? "first_name"
        <*> chat .:? "last_name"

instance FromJSON User where
    parseJSON (Object user) = User 
        <$> user .: "id"
        <*> user .: "is_bot"
        <*> user .: "first_name"
        <*> (fromMaybe "" <$> user .:? "last_name")
        <*> (fromMaybe "" <$> user .:? "username")

newtype Bot = Bot { token :: String } 

method :: Param a => String -> String -> [a] -> IO LBS.ByteString 
method name token params = 
    parseRequest ("https://api.telegram.org/bot" ++ token ++ '/':name ++ toHttp params) 
    >>= httpLBS 
    <&> getResponseBody 
        
toHttp :: Param a => [a] -> String
toHttp [] = ""
toHttp (a:as) = '?':show a ++ Prelude.concat (('&':) . show <$> as)