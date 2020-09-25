module Main where

import Prelude

import Effect (Effect)
import CSS as CSS
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
      HH.div [] [
        HH.div [ HC.style containerStyle ]
               [ HH.div []
                        [ HH.textarea [ HC.style textareaStyle ] ]
               ]
      ]
  
  handleAction = case _ of _ -> H.modify_ \s -> s

containerStyle = do CSS.border CSS.solid (CSS.px 1.0) CSS.gray
textareaStyle = do CSS.textWhitespace CSS.whitespaceNoWrap
