module Main where

import Prelude

import Node.Optlicative.Internal (charList)
import Data.String.CodeUnits (toCharArray, fromCharArray)

import Data.Maybe (Maybe(..), fromMaybe)
--import Data.List
import Data.Array
import Data.Array.NonEmpty as NE
import Data.Tuple
import Data.Either
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (class MonadEffect)

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

--import Web.HTML (window) as DOM
import Web.HTML.HTMLTextAreaElement (value, fromElement)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Web.HTML (window)
import Web.DOM.NonElementParentNode (getElementById)
--
--import Foreign as Foreign
--import Foreign.Generic.Class as Foreign 

data Parser a = Tuple (Either String a) String

satisfy :: (Char -> Boolean) -> String -> Parser Char
satisfy f str = let nonEmptyStr = NE.fromArray (toCharArray str)
                 in case nonEmptyStr of
                         Nothing -> Tuple (Left "end of string") ""
                         (Just strArr) -> let c  = NE.head strArr
                                              cs = fromCharArray (NE.tail strArr)
                                           in if f c then Tuple (Right c) cs
                                                     else Tuple (Left "do not satisfy") cs

char :: Char -> String -> Parser Char
char c = satisfy (eq c)
                                           
--parse :: (String -> Boolean) -> String -> Parser String
--parse f "" = Tuple (Left "No parsable string.") ""
--
--parse :: forall a. Target a -> String -> Parser a


cssRule :: String -> String -> CSS.CSS
cssRule x y = CSS.rule (CSS.Property (CSS.Key $ CSS.Plain x) (CSS.Value $ CSS.Plain y))

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
                   [ HH.textarea [ HP.id_ "textArea"
                                 , HE.onKeyUp \s -> Just (Update "123")
                                 , HC.style textareaStyle ] ]
          , HH.span [ HC.style spanStyle ] [ HH.text state ]
          ]
        ]

      handleAction :: forall output m. MonadEffect m =>
                        Action -> H.HalogenM String Action () output m Unit
      handleAction = case _ of
                       Update s ->
                         do str <-
                              H.liftEffect $
                                do doc  <- window >>= document
                                   elem <- getElementById "textArea" (toNonElementParentNode doc)
                                   fromMaybe (pure "Error: element not found") (value <$> (elem >>= fromElement))
                            H.modify_ \_ -> str

containerStyle = do CSS.width $ CSS.Size $ CSS.Value $ CSS.Plain "100%"
                    CSS.height $ CSS.Size $ CSS.Value $ CSS.Plain "100%"
                    CSS.display CSS.flex
                    CSS.justifyContent CSS.Common.center
                    CSS.alignItems CSS.Common.center
                    CSS.TextAlign.textAlign CSS.TextAlign.center
                    CSS.Overflow.overflow CSS.Overflow.hidden


textareaStyle = do --CSS.position CSS.absolute
                   cssRule "opacity" "1"
                   --cssRule "pointer-events" "none"
                   --CSS.TextAlign.textAlign CSS.TextAlign.center
                   CSS.textWhitespace CSS.whitespaceNoWrap

spanStyle = do CSS.position CSS.absolute
