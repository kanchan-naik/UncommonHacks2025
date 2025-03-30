{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Yesod
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal (unpackBytes)
import System.Process (callProcess)
import Data.Text hiding (head, concat, words)
import Network.HTTP.Client
import Network.Mime (defaultMimeLookup)
import Text.JSON.Generic
import System.Directory

data MakeupParam = MakeupParam {
    present :: Int,
    color   :: [Char]
} deriving (Data, Typeable)

instance Show MakeupParam where
  show mp = (show $ present mp) ++ " " ++ (color mp) ++ " "

data MakeupRequest = MakeupRequest {
  foundation :: MakeupParam,
  lipstick   :: MakeupParam,
  eyeshadow   :: MakeupParam,
  blush      :: MakeupParam,
  concealer  :: MakeupParam
} deriving (Data, Typeable)

instance Show MakeupRequest where 
  show mr = (show $ foundation mr) ++ (show $ lipstick mr) ++ (show $ eyeshadow mr)
    ++ (show $ blush mr) ++ (show $ concealer mr) 

data Server = Server

mkYesod "Server" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod Server

getHomeR :: Handler ()
getHomeR = redirect ("https://www.usenix.org/system/files/1401_08-12_mickens.pdf" :: Text)

postHomeR :: Handler TypedContent
postHomeR = do
    cwd <- liftIO $ getCurrentDirectory
    _ <- liftIO $ putStrLn cwd
    let scriptPath = "/Users/kanchannaik/2024-2025/UncommonHacks2025/UncommonHacks2025/haskell-app/app/test.py"
    let filePath = "app/curr.png "
    let interpreter = "/Users/kanchannaik/miniconda3/bin/python3"

    -- get post request body
    (params, body) <- runRequestBody
    rawBody <- fileSourceByteString (snd $ head body)

    let imBytes = unpackBytes rawBody

    -- write it to a file
    _ <- liftIO $ BL.writeFile filePath (BL.pack imBytes)

    let json = decodeJSON (unpack $ snd $ head params) :: MakeupRequest

    let args = [filePath, show json]

    -- Call the Python script correctly
    _ <- liftIO $ putStrLn (concat args)
    _ <- liftIO $ callProcess interpreter (scriptPath : args)

    let newfilePath = "test/final_makeup_result.png"
    pngData <- liftIO $ BL.readFile newfilePath
    let mimeType = defaultMimeLookup $ pack newfilePath
    sendResponse (mimeType, toContent pngData)


main :: IO ()
main = warp 3000 Server