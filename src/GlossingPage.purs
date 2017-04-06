module GlossingPage
  ( Query
  , ui
  ) where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Data.String (null)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = String

data Query a = UpdateText String a

ui :: forall m. H.Component HH.HTML Query Unit Void m
ui = H.component
    { initialState : const ""
    , render
    , eval
    , receiver : const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render text = HH.div_
        [ HH.h1_ [ HH.text "Shoebox" ]
        , HH.input
            [ HP.value text
            , HE.onValueInput $ HE.input UpdateText
            ]
        , HH.button
            [ HP.disabled $ null text ]
            [ HH.text "Query Word" ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (UpdateText text next) = do
        H.put text
        pure next

