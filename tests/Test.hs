import ChatTests
import MarkdownTests
import ProtocolTests
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    describe "SimpleX chat markdown" markdownTests
    describe "SimpleX chat protocol" protocolTests
    xdescribe "SimpleX chat client" chatTests
