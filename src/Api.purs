module Api
    ( queryWord
    ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut as A
import Data.Array ((!!))
import Data.Either (Either(Left), either)
import Data.Maybe (maybe)
import Network.HTTP.Affjax as AX

type DbEntries =
    { segmentations :: String
    , lexicon       :: String
    , prefixes      :: String
    , suffixes      :: String
    }

decodeDbEntries :: A.Json -> Either String DbEntries
decodeDbEntries json = do
    tuple <- A.decodeJson json
    seg <- maybe (Left "Segmentations not found.") A.decodeJson $ tuple !! 0
    lex <- maybe (Left "Lexicon not found."      ) A.decodeJson $ tuple !! 1
    pre <- maybe (Left "Prefixes not found."     ) A.decodeJson $ tuple !! 2
    suf <- maybe (Left "Suffixes not found."     ) A.decodeJson $ tuple !! 3
    pure { segmentations : show $ seg :: A.Json
         , lexicon       : show $ lex :: A.Json
         , prefixes      : show $ pre :: A.Json
         , suffixes      : show $ suf :: A.Json
         }

queryWord :: forall eff. String -> Aff (ajax :: AX.AJAX | eff) DbEntries
queryWord word = do
    res <- AX.post url $ "\"" <> word <> "\""
    either (throwError <<< error) pure $ decodeDbEntries res.response
  where
    url = "http://localhost:3000/api/query/word"

