{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal (unpackBytes)
import System.Process (readProcess)
import Data.Text hiding (head)
import Network.HTTP.Client
import Network.Mime (defaultMimeLookup)

data Server = Server

mkYesod "Server" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod Server

getHomeR :: Handler ()
getHomeR = redirect ("https://www.usenix.org/system/files/1401_08-12_mickens.pdf" :: Text)

postHomeR :: Handler TypedContent
postHomeR = do
    let scriptPath = "app/beep.py"
    let filePath = "app/curr.png"

    -- get post request body
    (_, body) <- runRequestBody
    rawBody <- fileSourceByteString (snd $ head body)

    -- write it to a file
    _ <- liftIO $ BL.writeFile filePath (BL.pack $ unpackBytes rawBody)

    -- run the processing script
    _ <- liftIO $ readProcess "python3" (scriptPath : [filePath]) ""

    -- send response
    pngData <- liftIO $ BL.readFile filePath
    let mimeType = defaultMimeLookup $ pack filePath
    sendResponse (mimeType, toContent pngData)

main :: IO ()
main = warp 3000 Server