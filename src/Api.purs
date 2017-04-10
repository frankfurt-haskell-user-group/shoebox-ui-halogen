module Api
    ( Morpheme(..)
    , queryWord
    ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, Json, (.?), decodeJson)
import Data.Array ((!!))
import Data.Either (Either(Left), either)
import Data.Maybe (maybe)
import Network.HTTP.Affjax (AJAX, post)

type DbEntries =
    { segmentations :: Array (Array Morpheme)
    , lexicon       :: Array String
    , prefixes      :: Array String
    , suffixes      :: Array String
    }

data Morpheme = MorphemeLexicon String
              | MorphemePrefix  String
              | MorphemeSuffix  String

instance decodeMorpheme :: DecodeJson Morpheme where
    decodeJson json = do
        obj <- decodeJson json
        (construct =<< obj .? "tag") <*> obj .? "contents"
      where
        construct "MorphemeLex"    = pure MorphemeLexicon
        construct "MorphemePrefix" = pure MorphemePrefix
        construct "MorphemeSuffix" = pure MorphemeSuffix
        construct _                = Left "unknown tag"

decodeDbEntries :: Json -> Either String DbEntries
decodeDbEntries json = do
    tuple <- decodeJson json
    seg <- maybe (Left "Segmentations not found.") decodeJson $ tuple !! 0
    lex <- maybe (Left "Lexicon not found."      ) decodeJson $ tuple !! 1
    pre <- maybe (Left "Prefixes not found."     ) decodeJson $ tuple !! 2
    suf <- maybe (Left "Suffixes not found."     ) decodeJson $ tuple !! 3
    pure { segmentations : seg
         , lexicon       : lex
         , prefixes      : pre
         , suffixes      : suf
         }

queryWord :: forall eff. String -> Aff (ajax :: AJAX | eff) DbEntries
queryWord word = do
    res <- post url $ "\"" <> word <> "\""
    either (throwError <<< error) pure $ decodeDbEntries res.response
  where
    url = "http://localhost:3000/api/query/word"

