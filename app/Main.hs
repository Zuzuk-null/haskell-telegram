
module Main where
import Telegram.Types
import Telegram.Commands
import Data.Function
import Data.Either
import Data.Functor

main :: IO ()
main = do
    let bot = Bot "Token Example"
    
    bot&getUpdates >>= processUpdates f . fromRight [] 
    main
    where f a = return ()

processUpdates :: (Update -> IO ()) -> [Update] -> IO ()
processUpdates f = foldr ((>>) . f) (return ())