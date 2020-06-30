import LexerTests
import PPrintTests
import ParserTests
import TemplateTests

main :: IO ()
main = do pprintTests
          lexerTests
          parserTests
          templateTests
