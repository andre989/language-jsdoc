import Test.Hspec
import Language.JSDoc.Parser
import Language.JSDoc.Parser.Parser

input = unlines
    [   "/**"
    ,   " * Substitute %n parameters in a string"
    ,   " * @param {String} string The source string to substitue %n occurences in"
    ,   " * @param {Array|String} params The parameters to use for the substitution. Index 0 will replace %1, index 1, %2"
    ,   " * and so on. If a string is passed, only %1 will be replaced"
    ,   " * @return {String} The final string, with %n occurences replaced with their equivalent"
    ,   " */"
    ]

main :: IO ()
main = hspec $ do
  describe "Language.JSDoc.Parser" $ do
    context "when given a public jsdoc declaration" $ do
        it "returns false for private AST property" $ do
            let (Right ast) = parse input
            private ast `shouldBe` True