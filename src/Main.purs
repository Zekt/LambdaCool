module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

data State = Number

data Action = Def

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
    where
  initialState _ = 10.0

  render state =
    HH.div []
           [ HH.div []
                    [ HH.textarea []  ]
           ]
  
  handleAction = case _ of _ -> H.modify_ \s -> s
