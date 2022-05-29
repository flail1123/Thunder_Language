module Main where
import System.IO (hPutStrLn, stderr, getContents)
import System.Environment ( getArgs)
import Interpreter
import TypeChecker
import Types
import ParGrammar


parse:: String -> IO () 
parse text = do 
    case pProgram(myLexer text) of 
        (Left s) -> do
            hPutStrLn stderr s
        (Right program) -> do 
            case (runTypeChecker program ) of 
                Left e -> do
                    putStrLn $ "Type checker error in line " ++ show (getTypeCheckerExceptionLine e) ++ ", column " ++ show (getTypeCheckerExceptionColumn e) ++ "!"
                    putStrLn $ show e 
                Right _ -> do 
                    interpreterResult <- runInterpreter program 
                    case interpreterResult of 
                        Left e2 -> do
                            putStrLn $ "Interpreter error in line " ++ show (getRuntimeExceptionLine e2) ++ ", column " ++ show (getRuntimeExceptionColumn e2) ++ "!"
                            putStrLn $ show e2 
                        Right _ ->  do 
                            putStr ""

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            text <- getContents
            parse text
        [file] -> do
            text <- readFile file 
            parse text