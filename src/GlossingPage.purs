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
import Network.HTTP.Affjax as AX

type State = { word   :: String
             , result :: String
             }

data Query a = UpdateWord String a
             | QueryWord a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui = H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
  where
    initialState :: State
    initialState = { word   : ""
                   , result : ""
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
        , HH.p_ [ HH.text st.result ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
    eval (UpdateWord word next) = do
        H.modify $ _ { word = word }
        pure next
    eval (QueryWord next) = do
        word <- H.gets _.word
        response <- H.liftAff $ AX.post url $ "\"" <> word <> "\""
        H.modify $ _ { result = response.response }
        pure next
      where
        url = "http://localhost:3000/api/query/word"

