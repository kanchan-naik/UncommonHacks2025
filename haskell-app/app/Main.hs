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
  eyeliner   :: MakeupParam
  -- blush      :: MakeupParam,
  -- concealer  :: MakeupParam
} deriving (Data, Typeable)

instance Show MakeupRequest where 
  show mr = (show $ foundation mr) ++ (show $ lipstick mr) ++ (show $ eyeliner mr)
    ++ show (MakeupParam 0 "l") ++ show (MakeupParam 0 "l")

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
    let scriptPath = "app/test.py"
    let filePath = "app/curr.png"
    let interpreter = "/home/bwaldman/UncommonHacks2025/.conda/bin/python "

    -- get post request body
    (params, body) <- runRequestBody
    rawBody <- fileSourceByteString (snd $ head body)

    let imBytes = unpackBytes rawBody

    -- write it to a file
    _ <- liftIO $ BL.writeFile filePath (BL.pack imBytes)

    let json = decodeJSON (unpack $ snd $ head params) :: MakeupRequest

    let args = words (filePath ++ " " ++ show json)

    _ <- liftIO $ callProcess (interpreter ++ scriptPath) args

    pngData <- liftIO $ BL.readFile filePath
    let mimeType = defaultMimeLookup $ pack filePath
    sendResponse (mimeType, toContent pngData)

main :: IO ()
main = warp 3000 Server