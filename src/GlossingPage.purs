module GlossingPage
  ( Query
  , ui
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Nothing))
import Data.String (null)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)

import Api (queryWord)

type State = { word          :: String
             , segmentations :: String
             , lexicon       :: String
             , prefixes      :: String
             , suffixes      :: String
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
    initialState = { word          : ""
                   , segmentations : ""
                   , lexicon       : ""
                   , prefixes      : ""
                   , suffixes      : ""
                   }

    render :: State -> H.ComponentHTML Query
    render st = HH.div_
        [ HH.h1_ [ HH.text "Shoebox" ]
        , HH.input
            [ HP.value st.word
            , HE.onValueInput $ HE.input UpdateWord
            ]
        , HH.button
            [ HP.disabled $ null st.word
            , HE.onClick $ HE.input_ QueryWord
            ]
            [ HH.text "Query Word" ]
        , HH.p_ [ HH.text st.segmentations ]
        , HH.p_ [ HH.text st.lexicon ]
        , HH.p_ [ HH.text st.prefixes ]
        , HH.p_ [ HH.text st.suffixes ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX | eff))
    eval (UpdateWord word next) = do
        H.modify $ _ { word = word }
        pure next
    eval (QueryWord next) = do
        word <- H.gets _.word
        result <- H.liftAff $ queryWord word
        H.modify $ _ { segmentations = result.segmentations
                     , lexicon       = result.lexicon
                     , prefixes      = result.prefixes
                     , suffixes      = result.suffixes
                     }
        pure next

