import Four

import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

gives :: String -> Expr
gives s = 
    case ret of
        Left _  -> undefined
        Right n -> n
    where 
        ret = parse expr "" s

givesText :: String -> String
givesText s = 
    case ret of
        Left _  -> undefined
        Right n -> show n
    where 
        ret = parse expr "" s

lang :: LanguageDef st
lang = emptyDef{ Token.identStart      = letter
               , Token.identLetter     = alphaNum
               , Token.opStart         = oneOf "&|-="
               , Token.opLetter        = oneOf "&|-="
               , Token.reservedOpNames = ["~"]
               , Token.reservedNames   = ["T","F"]
               , Token.commentLine     = "%"
               }

lexer = Token.makeTokenParser lang

identifier     = Token.identifier lexer
keyword        = Token.reserved lexer
op             = Token.reservedOp lexer
roundBrackets  = Token.parens lexer
whiteSpace     = Token.whiteSpace lexer

exprParser :: Parser Expr
exprParser = whiteSpace >> expr

expr :: Parser Expr
expr = form <|> atom 

atom = do
  s <- identifier 
  return $ Atom s

neg = do
  op "~"
  e <- ft
  return $ Neg e

top = keyword "T" >> return Top
bot = keyword "F" >> return Bot

ft = roundBrackets expr <|> atom <|> top <|> bot 

form = 
  buildExpressionParser [ [Infix (op "&" >> return AndT) AssocLeft] 
    , [Infix (op "|" >> return OrT) AssocLeft] 
    , [Infix (op "->" >> return Mat) AssocLeft] 
    , [Infix (op "=>" >> return Sup) AssocLeft] ] next 
      where next = roundBrackets expr <|> atom <|> top <|> bot <|> neg

main :: IO ()
main = interact (unlines . prettyConcat . (map gives) . lines)
--main = interact (unlines . (map (show . gives)) . lines)
--main = interact (unlines $ prettyConcat $ models $ (map gives) $ lines)
--main = interact (unlines . (map givesText) . lines)
