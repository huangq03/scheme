module Parsers.MainParsers
( parseExpr
, eval
, Env
, primitiveBindings
, readExpr
, readExprList
, bindVars
)
where
import Control.Monad
import Control.Monad.Except
import Text.Parsec.String
import Text.Parsec hiding (spaces)
import GHC.IO.IOMode
import System.IO
import Parsers.LispVal
import Parsers.MiscParsers
import Parsers.AtomParser (parseAtom)
import Parsers.StringParser (parseString)
import Parsers.NumberParser (parseNumber)
import Parsers.Helpers

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head' <- endBy parseExpr spaces
  tail' <- char '.' >> spaces >> parseExpr
  return $ DottedList head' tail'

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc' IOFunc) ioPrimitives
                                               ++ map (makeFunc' PrimitiveFunc) primitives)
    where makeFunc' constructor (var, func) = (var, constructor func)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc badForm = throwError $ NotFunction "Unsupported function type" (show badForm)

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ badVar= throwError $ Default $ "Bad var for makePort" ++ show badVar

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool True

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc badVar      = throwError $ Default $ "Bad var for readProc" ++ show badVar

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = (liftIO $ hPrint port obj) >> (return $ Bool True)
writeProc badVar = throwError $ Default $ "Bad var for writeProc" ++ show badVar

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents badVar            = throwError $ Default $ "Bad var for readContents" ++ show badVar

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll badVar            = throwError $ Default $ "Bad var for readAll" ++ show badVar

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom id') = getVar env id'
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred', conseq, alt]) =
     do result <- eval env pred'
        case result of
             Bool False -> eval env alt
             _          -> eval env conseq
eval env (List[Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List[Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params') : body')) =
     makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) =
     makeVarArgs varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
     makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
     makeVarArgs varargs env params' body'
eval env (List [Atom "load", String filename]) = do
     load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
     func <-  eval env function
     argVars <- mapM (eval env) args
     apply func argVars
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params' varargs body' closure') args =
      if num params' /= num args && varargs == Nothing
         then throwError $ NumArgs (num params') args
         else (liftIO $ bindVars closure' $ zip params' args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params') args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body'
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env

apply (IOFunc func) args = func args
apply nonFunc _ = throwError $ NotFunction "Unsupported function type" (show nonFunc)
