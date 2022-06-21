module YamlParser where


import Data.Aeson as DAeson
import Data.HashMap.Lazy as DHL
import Data.List as DL
import Data.Maybe as DMaybe
import Data.Text as DText
import Data.Yaml as DYaml
import Control.Applicative as CA
import GHC.Generics 


{-Custom YAML input file Datatype and related functions.-}

data FFConfig = FFConfig { outputpath         :: Text
                         , numrows            :: Int
                         , numcols            :: Int
                         } deriving (Eq,Show,Read)

instance FromJSON FFConfig where
  parseJSON (Object v) = parseFFConfig v
  parseJSON _          = CA.empty

parseFFConfig v = FFConfig
  <$> v .: "Output_Path"
  <*> v .: "Number_of_Rows"
  <*> v .: "Number_of_Columns" 

{--------------------------------------------------------}
