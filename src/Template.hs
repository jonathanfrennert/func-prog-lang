module Template where

import TemplateBase
import Parser
import Syntax

runProg :: String -> String
runProg = showResults.eval.compile.parse

-- | produces the initial state of the template instantiation machine
compile :: CoreProgram -> TiState
compile = undefined

-- | Perform repeated state transitions until a final state is reached
eval :: TiState -> [TiState]
eval = undefined

showResults :: [TiState] -> String
showResults = undefined
