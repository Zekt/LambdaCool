module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Node.Optlicative.Internal (charList)
import Data.String.CodeUnits (toCharArray, fromCharArray)

import Data.Maybe (Maybe(..), fromMaybe)
--import Data.List
import Data.Array
import Data.Array.NonEmpty as NEA
import Data.NonEmpty as NE
--import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
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

--newtype Error a = Error (Either String a)
data Parsed a = Tuple (Either String a) String

data Parser a = P (String -> Parsed a)

parse :: forall a. Parser a -> String -> Parsed a
parse (P f) s = f s

derive instance genericParser :: Generic (Parsed a) _
instance showParser :: (Show a) => Show (Parsed a) where
  show = genericShow

instance functorParsed :: Functor Parsed where
  map f (Tuple opt str) = Tuple (map f opt) str

instance functorParser :: Functor Parser where
  map f (P g) = P $ map f <<< g

--instance applyParsed :: Apply Parsed where
--  apply (Tuple fopt str) fa = Tuple (apply fopt fa) str
--
instance applyParser :: Apply Parser where
  apply (P f) (P g) =
    P $ \str ->
          case f str of
               Tuple (Left err) _    -> Tuple (Left err) str
               Tuple (Right ff) left1 ->
                 case g left1 of
                      Tuple (Left err) _ -> Tuple (Left err) str
                      Tuple (Right x) left2 -> Tuple (Right (ff x)) left2

instance bindParser :: Bind Parser where
  bind (P p) f =
    P $ \str ->
          case p str of
               Tuple (Left err) _   -> Tuple (Left err) str
               Tuple (Right a) left ->
                 case f a of
                      P q -> q left

--instance showError :: (Show a) => Show (Error a) where
--  show (Error (Right a))  = "Right (" + show a + ")"
--  show (Error (Left str)) = "Left \"" + str + "\""

--derive instance showParser :: (Show a) => Show (Parser a)

--instance showParser :: (Show a) => Show (Parser a) where
--  show (Tuple eit str) = "("+str+")"

satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = P $ \str ->
                  let nonEmptyStr = NEA.fromArray (toCharArray str)
                   in case nonEmptyStr of
                           Nothing -> Tuple (Left "end of string") ""
                           Just strArr ->
                             case NEA.toNonEmpty strArr of
                                  NE.NonEmpty c cs ->
                                    if f c then Tuple (Right c) (fromCharArray cs)
                                           else Tuple (Left "do not satisfy") (fromCharArray cs)

char :: Char -> Parser Char
char c = satisfy (eq c)

consChar :: Char -> String -> String
consChar c str = fromCharArray $ cons c (toCharArray str)

--parseWith :: forall a b c. Parser a -> (String -> Parser b) -> (a -> b -> c) -> Parser c
--parseWith (Tuple (Left err) str) f1 f2 = Tuple (Left err) str
--parseWith (Tuple (Right a) str) f1 f2 = map (f2 a) (f1 str)

string :: String -> Parser String
string str1 = let nonEmptyStr = NEA.fromArray (toCharArray str1)
               in case nonEmptyStr of
                       Nothing -> P $ Tuple (Right "")
                       Just strArr ->
                         case NEA.toNonEmpty strArr of
                              NE.NonEmpty c cs ->
                                bind (char c) $ \c -> map (consChar c) (string $ fromCharArray cs)
                              

--parse :: (String -> Parser a) -> String -> Parser a
--parse f str = 
                                           
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
