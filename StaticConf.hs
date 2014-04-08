{-# LANGUAGE OverloadedStrings #-}
import           Web.Scotty
import qualified Web.Scotty

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Codec.Archive.Zip

import qualified Data.Binary
import           Data.List
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid

import qualified Database.Redis                       as Redis

import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse

import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import qualified Text.Blaze.Html5                     as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes          as A

import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy                 as B

import           System.Directory                     (removeFile)
import           System.FilePath                      ((</>))
import qualified System.IO.Strict
import           System.Random

uploads :: String
uploads = "uploads"

dKeyHash :: BS.ByteString
dKeyHash = "delete_keys"

main :: IO ()
main = scotty 3000 $ do
    rconn <- liftIO $ Redis.connect Redis.defaultConnectInfo -- Connect to the redis db

    middleware logStdoutDev
    middleware $ staticPolicy (only [("styles.css", "styles.css")]) -- Serve styles.css at /styles.css
    middleware $ staticPolicy (noDots >-> addBase uploads)          -- Serve anything else at / from uploads

    get "/" $ html (renderHtml $ template Nothing)

    post "/" $ do
        fs <- files
        notice <- if length fs == 1 then liftIO (processFile rconn $ head fs) else return badNumberNotice
        html . renderHtml $ template (Just notice)

    get "/:name/delete" $ do
        name <- param "name"
        key <- param "key"
        dbKey <- liftIO $ Redis.runRedis rconn (Redis.hget dKeyHash name)

        case dbKey of
            Right Nothing -> return ()
            Right (Just correctKey) -> when (key == correctKey) $ liftIO (do
                removeFile $ uploads </> BS.unpack name
                Redis.runRedis rconn $ Redis.hdel dKeyHash [name]
                return ())
            Left _ -> return ()

        redirect "/"

-- What to do with the zip file
processFile :: Redis.Connection -> Web.Scotty.File -> IO H.Html
processFile rconn (_, fi) = do
    lastStr <- System.IO.Strict.readFile "last"
    let content = fileContent fi
        num = show . succ $ (read lastStr :: Integer)
        name = num ++ ".zip"
        uploadName = BS.unpack $ fileName fi

    if ".zip" `isSuffixOf` uploadName && checkZip content
        then (do
            B.writeFile (uploads </> name) content -- Write the file
            writeFile "last" num                   -- Update the last number
            gen <- newStdGen                       -- Create a RandomGen
            let key = mkDeleteKey gen              -- Create a deletion key

            Redis.runRedis rconn $ Redis.hset dKeyHash (BS.pack name) (BS.pack key) -- Add it to the db

            return $ uploadedNotice name key)
        else return badFormatNotice

checkZip :: B.ByteString -> Bool
checkZip zip = case Data.Binary.decodeOrFail zip of
    Left _ -> False
    Right (_, _, a) -> let files = filesInArchive a in not (null files) && all (isPrefixOf "config/") files

-- Keys
keyChars :: String
keyChars = '_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

mkDeleteKey :: RandomGen g => g -> String
mkDeleteKey = take 12 . mkKey

mkKey :: RandomGen g => g -> String
mkKey gen = map (keyChars !!) nums
    where nums = randomRs (0, length keyChars - 1) gen

-- View
template :: Maybe H.Html -> H.Html
template n = do
    H.docTypeHtml $ do
        H.head $ do
            H.title "StaticConf"
            H.link H.! href "styles.css" H.! rel "stylesheet" H.! type_ "text/css"
            H.meta H.! name "viewport" H.! content "width=device-width"
        H.body $ H.div H.! class_ "container" $ do
            H.h1 $ do
                "Static"
                H.span H.! class_ "logo" $ "Conf"
            H.form H.! enctype "multipart/form-data" H.! method "post" H.! action "" $ H.fieldset $ do
                H.legend "Upload Config"
                H.input H.! type_ "file" H.! name "config"
                H.input H.! type_ "submit" H.! value "Submit"
            H.div H.! class_ "content" $ do
                H.p H.! A.id "info" $ do
                    "Static"
                    H.span H.! class_ "logo" $ "Conf "
                    "is a simple, easy to use Minecraft config host, specializing in configuration zip packs in the "
                    H.a H.! href "http://mml.stephenmac.com/" $ "PackAssembler"
                    " format. For security, we currently only support this format, which is just a zipped \"config/\" folder. A common structure would look like:"
                H.ul $ do
                    H.li "config/"
                    H.li "config/SomeConfig.cfg"
                    H.li "config/somedir/SomeOtherConfig.txt"
                    H.li "config/somedir/BecauseINeedTwoConfigs.txt"
                fromMaybe (return ()) n

-- Types of notices
uploadedNotice :: String -> String -> H.Html
uploadedNotice loc key = notice $ do
    "Your file has been uploaded to: "
    H.a H.! href (H.toValue loc) $ H.toHtml loc
    "! To delete it, go to "
    let deleteUrl = loc ++ "/delete?key=" ++ key
    H.a H.! href (H.toValue deleteUrl) $ H.toHtml deleteUrl
    "."

badNumberNotice :: H.Html
badNumberNotice = notice "Please submit one file at a time!"

badFormatNotice :: H.Html
badFormatNotice = notice "Your file does not meet our specification. Please see above."

notice :: H.Html -> H.Html
notice = H.p H.! A.id "notice"
