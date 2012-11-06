import Brainfuck.Parser
import Brainfuck.Compiler
import Brainfuck.Interpreter

import Control.Applicative
import Control.Monad (join)

import qualified Data.Stream as S
import Data.Char (chr, ord)

import System.Environment (getArgs)

main :: IO ()
main = do
  as <- getArgs
  case as of
    -- STDIN is program
    [ ] -> getContents >>= run noInput

    -- STDIN is input
    [f] -> join $ run <$> getInput <*> readFile f

    -- Malformed command line
    _ -> putStrLn "Usage: brainfuck [program]"

run :: Input -> String -> IO ()
run i = putStr . map (chr . fromIntegral) . interpret i . compile . parse

-- EOF is represented as 0
getInput :: IO Input
getInput = fmap f getContents
  where f s = S.fromList (map (fromIntegral . ord) s ++ repeat 0)

noInput :: Input
noInput = S.repeat 0
