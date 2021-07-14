{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Help where

import Data.List (intersperse)
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import System.Console.ANSI.Types

chatHelpInfo :: [StyledString]
chatHelpInfo =
  map
    styleMarkdown
    [ highlight "Using Simplex chat prototype.",
      "Follow these steps to set up a connection:",
      "",
      Markdown (Colored Green) "Step 1: " <> highlight "/add" <> " -- Alice adds a contact.",
      indent <> "Alice should send the invitation printed by the /add command",
      indent <> "to her contact, Bob, out-of-band, via any trusted channel.",
      "",
      Markdown (Colored Green) "Step 2: " <> highlight "/connect <invitation>" <> " -- Bob accepts the invitation.",
      indent <> "Bob should use the invitation he received out-of-band.",
      "",
      Markdown (Colored Green) "Step 3: " <> "Bob and Alice are notified that the connection is set up,",
      indent <> "both can now send messages:",
      indent <> highlight "@bob Hello, Bob!" <> " -- Alice messages Bob (assuming Bob has display name 'bob').",
      indent <> highlight "@alice Hey, Alice!" <> " -- Bob replies to Alice.",
      "",
      Markdown (Colored Green) "Other commands:",
      indent <> highlight "/delete <name>" <> " -- deletes contact and all messages with them.",
      indent <> highlight "/markdown" <> " -- prints the supported markdown syntax.",
      "",
      "The commands may be abbreviated to a single letter: " <> listCommands ["/a", "/c", "/d", "/h", "/m"]
    ]
  where
    listCommands = mconcat . intersperse ", " . map highlight
    highlight = Markdown (Colored Cyan)
    indent = "        "

markdownInfo :: [StyledString]
markdownInfo =
  map
    styleMarkdown
    [ "Markdown:",
      "  *bold*          - " <> Markdown Bold "bold text",
      "  _italic_        - " <> Markdown Italic "italic text" <> " (shown as underlined)",
      "  +underlined+    - " <> Markdown Underline "underlined text",
      "  ~strikethrough~ - " <> Markdown StrikeThrough "strikethrough text" <> " (shown as inverse)",
      "  `code snippet`  - " <> Markdown Snippet "a + b // no *markdown* here",
      "  !1 text!        - " <> red "red text" <> " (1-6: red, green, blue, yellow, cyan, magenta)",
      "  #secret#        - " <> Markdown Secret "secret text" <> " (can be copy-pasted)"
    ]
  where
    red = Markdown (Colored Red)
