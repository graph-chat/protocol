{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatTerminal.Core where

import Control.Concurrent.STM
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Simplex.Chat.Markdown
import Styled
import System.Console.ANSI.Types
import System.Terminal hiding (insertChars)
import Types

data ActiveTo = ActiveNone | ActiveC Contact | ActiveG Group
  deriving (Eq)

data ChatTerminal = ChatTerminal
  { inputQ :: TBQueue String,
    outputQ :: TBQueue [StyledString],
    activeTo :: TVar ActiveTo,
    termMode :: TermMode,
    termState :: TVar TerminalState,
    termSize :: Size,
    nextMessageRow :: TVar Int,
    termLock :: TMVar ()
  }

data TerminalState = TerminalState
  { inputPrompt :: String,
    inputString :: String,
    inputPosition :: Int,
    previousInput :: String
  }

inputHeight :: TerminalState -> ChatTerminal -> Int
inputHeight ts ct = length (inputPrompt ts <> inputString ts) `div` width (termSize ct) + 1

positionRowColumn :: Int -> Int -> Position
positionRowColumn wid pos =
  let row = pos `div` wid
      col = pos - row * wid
   in Position {row, col}

updateTermState :: ActiveTo -> Int -> (Key, Modifiers) -> TerminalState -> TerminalState
updateTermState ac tw (key, ms) ts@TerminalState {inputString = s, inputPosition = p} = case key of
  CharKey c
    | ms == mempty || ms == shiftKey -> insertCharsWithContact [c]
    | ms == altKey && c == 'b' -> setPosition prevWordPos
    | ms == altKey && c == 'f' -> setPosition nextWordPos
    | otherwise -> ts
  TabKey -> insertCharsWithContact "    "
  BackspaceKey -> backDeleteChar
  DeleteKey -> deleteChar
  HomeKey -> setPosition 0
  EndKey -> setPosition $ length s
  ArrowKey d -> case d of
    Leftwards -> setPosition leftPos
    Rightwards -> setPosition rightPos
    Upwards
      | ms == mempty && null s -> let s' = previousInput ts in ts' (s', length s')
      | ms == mempty -> let p' = p - tw in if p' > 0 then setPosition p' else ts
      | otherwise -> ts
    Downwards
      | ms == mempty -> let p' = p + tw in if p' <= length s then setPosition p' else ts
      | otherwise -> ts
  _ -> ts
  where
    insertCharsWithContact cs
      | null s && cs /= "@" && cs /= "#" && cs /= "/" =
        insertChars $ contactPrefix <> cs
      | otherwise = insertChars cs
    insertChars = ts' . if p >= length s then append else insert
    append cs = let s' = s <> cs in (s', length s')
    insert cs = let (b, a) = splitAt p s in (b <> cs <> a, p + length cs)
    contactPrefix = case ac of
      ActiveNone -> ""
      ActiveC (Contact c) -> "@" <> B.unpack c <> " "
      ActiveG (Group g) -> "#" <> B.unpack g <> " "
    backDeleteChar
      | p == 0 || null s = ts
      | p >= length s = ts' (init s, length s - 1)
      | otherwise = let (b, a) = splitAt p s in ts' (init b <> a, p - 1)
    deleteChar
      | p >= length s || null s = ts
      | p == 0 = ts' (tail s, 0)
      | otherwise = let (b, a) = splitAt p s in ts' (b <> tail a, p)
    leftPos
      | ms == mempty = max 0 (p - 1)
      | ms == shiftKey = 0
      | ms == ctrlKey = prevWordPos
      | ms == altKey = prevWordPos
      | otherwise = p
    rightPos
      | ms == mempty = min (length s) (p + 1)
      | ms == shiftKey = length s
      | ms == ctrlKey = nextWordPos
      | ms == altKey = nextWordPos
      | otherwise = p
    setPosition p' = ts' (s, p')
    prevWordPos
      | p == 0 || null s = p
      | otherwise =
        let before = take p s
            beforeWord = dropWhileEnd (/= ' ') $ dropWhileEnd (== ' ') before
         in max 0 $ p - length before + length beforeWord
    nextWordPos
      | p >= length s || null s = p
      | otherwise =
        let after = drop p s
            afterWord = dropWhile (/= ' ') $ dropWhile (== ' ') after
         in min (length s) $ p + length after - length afterWord
    ts' (s', p') = ts {inputString = s', inputPosition = p'}

styleMessage :: String -> String -> StyledString
styleMessage time msg = do
  case msg of
    "" -> ""
    s@('@' : _) -> sentMessage s
    s@('#' : _) -> sentMessage s
    s -> markdown s
  where
    sentMessage :: String -> StyledString
    sentMessage s =
      let (c, rest) = span (/= ' ') s
       in styleTime time <> " " <> styled (Colored Cyan) c <> markdown rest
    markdown :: String -> StyledString
    markdown = styleMarkdownText . T.pack

styleTime :: String -> StyledString
styleTime = Styled [SetColor Foreground Vivid Black]

safeDecodeUtf8 :: ByteString -> Text
safeDecodeUtf8 = decodeUtf8With onError
  where
    onError _ _ = Just '?'

ttyContact :: Contact -> StyledString
ttyContact (Contact a) = styled (Colored Green) a

ttyFromContact :: Contact -> StyledString
ttyFromContact (Contact a) = styled (Colored Yellow) $ a <> "> "

ttyGroup :: Group -> StyledString
ttyGroup (Group g) = styled (Colored Blue) $ "#" <> g

ttyFromGroup :: Group -> Contact -> StyledString
ttyFromGroup (Group g) (Contact a) = styled (Colored Yellow) $ "#" <> g <> " " <> a <> "> "
