{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

-- | A no-hassle way to feed data into Google fusion tables.

import qualified Network.Curl as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad 
import System.Info (os)
import System.Process (system, rawSystem)
import System.Exit    (ExitCode(..))
import System.Directory (doesFileExist, doesDirectoryExist, getAppUserDataDirectory)
import System.FilePath ((</>),(<.>))
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                              OAuth2Client(..), OAuth2Tokens(..), getCachedTokens)
import Network.Google (makeRequest, doRequest)
import Network.HTTP.Conduit (simpleHttp)

import Text.JSON
import Text.JSON.Pretty (pp_value)
import Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)
-- import Text.PrettyPrint.HughesPJ (render)

import Network.HTTP as H

import Network.Google.FusionTables as FT

import Data.Aeson
import Data.Aeson.TH

data Test =
  Test
  { jskind :: String
  , jscolumnId :: Int
  , jsname :: String
  , jstype :: String
  } deriving Show

test :: String
test =
  "{\"kind\": \"fusiontables#column\", \"columnId\": 13,"++
   "\"name\": \"median-H256M\", \"type\": \"NUMBER\"}  "

$(deriveJSON (drop 2) ''Test)


cid    = "679570803037.apps.googleusercontent.com"
secret = "FnLhDezKFlHL46XLbCf7Ik1L"

main = do
  let client = OAuth2Client { clientId = cid, clientSecret = secret }
  toks <- getCachedTokens client
  putStrLn$"YAY, toks: "++show toks

  let accessTok = B.pack$ accessToken toks
#if 0
  tables <- FT.listTables accessTok

  let Ok metas = FT.parseTables tables 

  putStrLn$ "User has "++ show (length metas) ++ " fusion tables.  IDs are:"
  forM_ metas $ \ (TableMetadata {tab_tableId}) -> do
    putStrLn$"   TableID: "++ show tab_tableId
    

  -- This is the one we want to insert into:
  let theTab = "1w5bdSTnbT9qG3VNERHejAwPMfBLgr-V5a8qKkXo"
  Ok cols <- fmap parseColumns $ FT.listColumns accessTok theTab

  putStrLn $ "There are "++show (length cols) ++ " columns in the table"
  insertRows accessTok theTab ["Text","Num2","Number"]
    [ ["programattic1","012","034"]
    , ["programattic2","056","078"]
    ]
--"(Text, Num2, Number)"
  putStrLn $ "Done with insert!!"
#endif

  ------------------------------------------------------------------
--  {"columns": [{"name": "foo", "type": "NUMBER"},{"name": "bar", "type": "NUMBER"}], "isExportable": true, "name": "NEWTAB"}

  -- print =<< C.curlPost postStr []
#if 0  
  body <- readFile "../body.txt"
  let url = "https://www.googleapis.com/fusiontables/v1/tables"
      req = H.postRequestWithBody url "application/json" body
  putStrLn$ "CREATING TABLE...\n "++ show req
  H.simpleHTTP req
#endif


--  createTable accessTok "NEWTAB" [("foo",NUMBER), ("bar",NUMBER)]
  createTable accessTok "NEWTAB" [("foob",STRING)]

  putStrLn $ "Done with CREATE table"

  return ()

  -- let JSObject ob = tables
  --     Just (JSArray allTables) = lookup "items" $ fromJSObject ob
  -- return ls


-- -- | Provide a hassle-free way to cache tokens in the user's home directory.
-- -- cacheTokens :: OAuth2Client -> OAuth2Tokens -> IO 
-- -- cacheTokens = undefined

-- main = do
--   -- Ask for permission to read/write your fusion tables:
--   let client = OAuth2Client { clientId = cid, clientSecret = secret }
--       permissionUrl = formUrl client ["https://www.googleapis.com/auth/fusiontables"]

--   b <- doesFileExist file
--   unless b $ do 
--       putStrLn$ "Load this URL: "++show permissionUrl
--       case os of
--         "linux"  -> rawSystem "gnome-open" [permissionUrl]
--         "darwin" -> rawSystem "open"       [permissionUrl]
--         _        -> return ExitSuccess
--       putStrLn "Please paste the verification code: "
--       authcode <- getLine
--       tokens   <- exchangeCode client authcode
--       putStrLn$ "Received access token: "++show (accessToken tokens)
--       tokens2  <- refreshTokens client tokens
--       putStrLn$ "As a test, refreshed token: "++show (accessToken tokens2)
--       writeFile file (show tokens2)
--   accessTok <- fmap (accessToken . read) (readFile file)
  
--   putStrLn "As a test, list the users tables:"
--   response <- simpleHttp ("https://www.googleapis.com/fusiontables/v1/tables?access_token="++accessTok)
--   putStrLn$ unlines$ take 50$ lines$ BL.unpack response




-- -- listTables
-- -- withToken


