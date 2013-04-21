{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, TemplateHaskell #-}

import           Data.Aeson
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (Parser, parse)
import qualified Data.Aeson.Generic as G
import           Data.Text     (Text)
import           Data.Typeable (Typeable)
import           Data.Data     (Data)
import           Data.Attoparsec.Number (Number(I,D))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as H
import           Data.Maybe (fromJust)
import           Control.Monad (mzero)
import           Control.Applicative ((<$>), (<*>))

data Person = Person
     { name :: Text
     , age  :: Int
     } deriving (Show, Data, Typeable)


-- MANUAL instances:
#if 0
instance FromJSON Person where
     parseJSON (Object v) = Person <$>
                            v .: "name" <*>
                            v .: "age"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero

instance ToJSON Person where
     toJSON (Person name age) = object ["name" .= name, "age" .= age]

mydecode x   = decode x
myencode x   = encode x
myfromJSON x = fromJSON x

-- Generic instances:
#elif 0
    
mydecode x   = G.decode x
myencode x   = G.encode x
myfromJSON x = G.fromJSON x
-- myfromJSON x = fromJSON x

-- Compile time template-haskell instances:
#else
$(deriveJSON (drop 0) ''Person)
mydecode x   = decode x
myencode x   = encode x
myfromJSON x = fromJSON x
#endif

convertPerson :: Value -> Person
convertPerson val =
  case val of
    Object hsh ->
      case H.lookup "name" hsh of
        Nothing -> error$ "convertPerson: Did not have an 'name' field: "++show hsh        
        Just (String nametext) ->
          case H.lookup "age" hsh of
            Nothing -> error$ "convertPerson: Did not have an 'age' field: "++show hsh
            Just (Number (I ageint)) -> Person nametext (fromIntegral ageint)
            Just ageval -> error$ "convertPerson: expected Integer for 'age' field, got: "++show ageval
        Just nameval -> error$ "convertPerson: expected Text for 'name' field, got: "++show nameval
    oth -> error$ "convertPerson: Not an object: "++show oth

--------------------------------------------------------------------------------
    
t0 = myencode (Person {name = "Joe", age = 12})

good1 :: Maybe Person
good1 = mydecode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person

good2 :: Person
good2 = convertPerson (fromJust (decode "{\"name\":\"Joe\",\"age\":12}"))

bad_string :: B.ByteString
bad_string = "{\"name\":\"Joe\",\"age\":\"blah\"}"

bad_json :: Value
bad_json = fromJust (decode bad_string :: Maybe Value)

-- bad1 :: Either String Person
-- bad1 = eitherDecode bad_string
-- bad1 = parse parseJSON bad_json -- Equivalent.

bad1 :: Result Person
bad1 = myfromJSON bad_json

bad2 :: Person
bad2 = convertPerson bad_json
