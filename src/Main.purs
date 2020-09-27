module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

import CSS as CSS
import CSS.Common as CSS.Common
import CSS.TextAlign as CSS.TextAlign
import CSS.Overflow as CSS.Overflow

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.HTMLInputElement (validity) as DOM
import DOM.HTML.Types
  (ValidityState, htmlDocumentToNonElementParentNode, readHTMLInputElement) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.NonElementParentNode (getElementById) as DOM

cssRule :: String -> String -> CSS.CSS
cssRule x y = CSS.rule (CSS.Property (CSS.Key $ CSS.Plain x) (CSS.Value $ CSS.Plain y))


getElementById
    :: forall a eff
     . (Foreign -> F a)
    -> String
    -> Eff (dom :: DOM | eff) (Maybe a)
getElementById reader elementId =
    DOM.window
    >>= DOM.document
    <#> DOM.htmlDocumentToNonElementParentNode
    >>= DOM.getElementById (wrap elementId)
    <#> (_ >>= runReader reader)

data State = String

data Action = Update String

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
      initialState _ = ""

  render state =
  --let parsedState = parse state
    HH.div [] [
      HH.div [ HC.style containerStyle ]
      [ HH.div []
               [ HH.textarea [ HE.onKeyDown \_ -> Just (Update s)
               , HC.style textareaStyle ] ]
      , HH.span [] [ HH.text state ]
      ]
    ]

  handleAction = case _ of
                      Update s -> do getElement
                                     H.modify_ \_ -> s

containerStyle = do CSS.width $ CSS.Size $ CSS.Value $ CSS.Plain "100%"
    CSS.height $ CSS.Size $ CSS.Value $ CSS.Plain "100%"
    CSS.display CSS.flex
    CSS.justifyContent CSS.Common.center
    CSS.alignItems CSS.Common.center
    CSS.TextAlign.textAlign CSS.TextAlign.center
    CSS.Overflow.overflow CSS.Overflow.hidden


textareaStyle = do CSS.position CSS.absolute
    cssRule "opacity" "1"
    --cssRule "pointer-events" "none"
    --CSS.TextAlign.textAlign CSS.TextAlign.center
    CSS.textWhitespace CSS.whitespaceNoWrap

