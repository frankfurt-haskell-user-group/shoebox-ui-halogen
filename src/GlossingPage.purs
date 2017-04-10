module GlossingPage
  ( Query
  , ui
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (mempty)
import Data.Traversable (sequence)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)

import Api (Morpheme(..), queryWord)

type State = { word         :: String
             , segmentation :: String
             , gloss        :: String
             }

data Query a = UpdateWord String a
             | QueryWord a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX | eff))
ui = H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
  where
    initialState :: State
    initialState = { word         : ""
                   , segmentation : ""
                   , gloss        : ""
                   }

    render :: State -> H.ComponentHTML Query
    render st = HH.div_
        [ HH.h1_ [ HH.text "Shoebox" ]
        , HH.input
            [ HP.value st.word
            , HE.onValueInput $ HE.input UpdateWord
            ]
        , HH.button
            [ HP.disabled $ st.word == mempty
            , HE.onClick $ HE.input_ QueryWord
            ]
            [ HH.text "Query Word" ]
        , HH.p_ [ HH.text st.segmentation ]
        , HH.p_ [ HH.text st.gloss ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX | eff))
    eval (UpdateWord word next) = do
        H.modify $ _ { word = word }
        pure next
    eval (QueryWord next) = do
        word <- H.gets _.word
        result <- H.liftAff $ queryWord word
        if result.lexicon /= mempty
           then H.modify $ _ { segmentation = showAlt result.lexicon
                             , gloss        = showAlt result.lexicon
                             }
           else do
               glosses <- H.liftAff $ querySegmentations result.segmentations
               H.modify $ _ { segmentation = showSegmentations result.segmentations
                            , gloss        = showGlosses glosses
                            }
        pure next
      where
        showAlt :: Array String -> String
        showAlt = intercalate "/"

        showSeq :: Array String -> String
        showSeq = intercalate "-"

        showSegmentations :: Array (Array Morpheme) -> String
        showSegmentations = showAlt <<< map (showSeq <<< map showMorpheme)

        showMorpheme :: Morpheme -> String
        showMorpheme (MorphemeLexicon lex) = lex
        showMorpheme (MorphemePrefix  pre) = pre
        showMorpheme (MorphemeSuffix  suf) = suf

        showGlosses :: Array (Array (Array String)) -> String
        showGlosses = showAlt <<< map showSeq <<< (map $ map showAlt)

        querySegmentations :: Array (Array Morpheme)
                           -> Aff (ajax :: AJAX | eff) (Array (Array (Array String)))
        querySegmentations = sequence <<< map querySegmentation

        querySegmentation :: Array Morpheme
                          -> Aff (ajax :: AJAX | eff) (Array (Array String))
        querySegmentation = sequence <<< map queryGlosses

        queryGlosses :: Morpheme -> Aff (ajax :: AJAX | eff) (Array String)
        queryGlosses (MorphemeLexicon lex) = _.lexicon  <$> queryWord lex
        queryGlosses (MorphemePrefix  pre) = _.prefixes <$> queryWord pre
        queryGlosses (MorphemeSuffix  suf) = _.suffixes <$> queryWord suf

