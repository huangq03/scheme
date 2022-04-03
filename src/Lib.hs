module Lib
    ( someFunc
    ) where
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import System.Environment
import System.IO
import Parsers.LispVal
import Control.Monad.Except
import Parsers.MainParsers
import Parsers.MiscParsers (spaces)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred' prompt action = do
   result <- prompt
   if pred' result
      then return ()
      else action result >> until_ pred' prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

someFunc :: IO ()
someFunc = do args <- getArgs
              case length args of
                   0 -> runRepl
                   1 -> runOne $ args !! 0
                   _  -> putStrLn "Program takes only 0 or 1 argument"
