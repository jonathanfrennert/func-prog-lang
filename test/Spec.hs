import LexerTests
import PPrintTests
import ParserTests

main :: IO ()
main = do pprintTests
          lexerTests
          parserTests
