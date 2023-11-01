{-# OPTIONS_HADDOCK hide #-}

module Four where

import Prelude hiding (lookup)

import Control.Monad (replicateM)
import Data.Map (Map, fromList, toList, lookup)
import Data.Maybe (fromMaybe)
import Data.List (group, sort)

import Text.ParserCombinators.Parsec
    ((<|>), char, choice, eof, letter, parse, spaces, string, try)

import Text.ParserCombinators.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Pos (SourceName)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import qualified Data.Text as T


type Sym = String

data Val = F | N | B | T
         deriving (Eq)

instance Show Val where
  show T = "T"
  show B = "B"
  show N = "N"
  show F = "F"

inverse :: Val -> Val
inverse T = F
inverse F = T
inverse B = B
inverse N = N

data Expr = Atom Sym
          | Neg  Expr
          | AndT Expr Expr
          | OrT  Expr Expr
          | Imp  Expr Expr
          | Sup  Expr Expr
          | Mat  Expr Expr
          | Top
          | Bot
          deriving (Eq)

sbc symbol e e' =
  '(' : show e ++ " " ++ symbol ++ " " ++ show e' ++ ")"

instance Show Expr where
  show (Atom s)    = s
  show (Neg e)     = '-' : show e
  show (AndT e e') = sbc "&" e e' 
  show (OrT e e')  = sbc "|" e e'
  show (Mat e e')  = sbc "->" e e'
  show (Imp e e')  = sbc ">>" e e'
  show (Sup e e')  = sbc "=>" e e'
  show Top         = "TOP"
  show Bot         = "BOT"

type Mapping = Map Sym Val

designated :: (Val -> Val) -> Val -> Bool 
designated f v = f v `elem`[B,T]

valid :: Expr -> Mapping -> Bool
valid (Atom s)       m = maybe False (designated id) (lookup s m)
valid (Neg (Atom s)) m = maybe False (designated inverse) (lookup s m)
valid (AndT e e')    m = valid e m && valid e' m
valid (OrT e e')     m = valid e m || valid e' m
valid (Mat e e')     m = valid (OrT (Neg e) e') m
valid (Imp e e')     m = valid (AndT (Sup e e') (Sup (Neg e') (Neg e))) m
valid (Sup e e')     m = not (valid e m) || valid e' m
valid Top            _ = True
valid Bot            _ = False

variables :: Expr -> [Sym]
variables e = let vars_ (Atom v)    vs = v : vs
                  vars_ Top         _ = []
                  vars_ Bot         _ = []
                  vars_ (Neg  e)    vs = vars_ e vs
                  vars_ (AndT e e') vs = vars_ e vs ++ vars_ e' vs
                  vars_ (OrT  e e') vs = vars_ e vs ++ vars_ e' vs
                  vars_ (Mat  e e') vs = vars_ e vs ++ vars_ e' vs
                  vars_ (Imp  e e') vs = vars_ e vs ++ vars_ e' vs
                  vars_ (Sup  e e') vs = vars_ e vs ++ vars_ e' vs
              in  map head . group . sort $ vars_ e []
 
assignments :: Expr -> [Mapping]
assignments e = let vs = variables e
                    ps = replicateM (length vs) [T,B,F,N]
                in  map (fromList . zip vs) ps


andify :: [Expr] -> Expr
andify = foldl AndT Top

prettyPrint :: Mapping -> IO ()
prettyPrint m = do 
  mapM_ (putStr . (\x -> fst x ++ show (snd x) ++ " ")) (toList m)
  putStrLn ""

--
models ctx = filter (valid f) (assignments f)
  where f = andify ctx
--

infix 1 |=
(|=) :: [Expr] -> [Expr] -> Bool
(|=) = entails

entails s d = and $ [valid (andify d) x | x <- ms] 
  where ms = models s

infix 1 &
(&) :: Expr -> Expr -> Expr
(&) = AndT

infix 1 %
(%) :: Expr -> Expr -> Expr
(%) = OrT

infix 1 %->
(%->) :: Expr -> Expr -> Expr
(%->) = Mat

infix 1 %>>
(%>>) :: Expr -> Expr -> Expr
(%>>) = Imp

infix 1 %=>
(%=>) :: Expr -> Expr -> Expr
(%=>) = Sup


a1 = Atom "fly(tweety)"
a2 = Atom "bird(tweety)"
a3 = Atom "penguin(tweety)"
tweetyDilemma = [
  Mat a2 a1,
  Sup a3 a2,
  Sup a3 (Neg a1),
  a2,
  a3
  ]
entailed4 = [Neg a1, a2, a3]

a4 = Atom "quaker(Nixon)"
a5 = Atom "republican(Nixon)"
a6 = Atom "dove(Nixon)"
a7 = Atom "hawk(Nixon)"
nixonDiamond = [
  a4, a5,
  a4 %-> a6,
  a5 %-> a7,
  a6 %=> Neg a7,
  a7 %=> Neg a6,
  a7 % a6
  ]
t1 = nixonDiamond |= [Neg (Atom "hawk(Nixon)") % Neg (Atom "dove(Nixon)")]



--ctxtw = AndT (AndT (AndT (AndT a2 a3) twf1) twf2) twf3
--xstw = assignments ctxtw
--satstw = filter (valid ctxtw) xstw

a = Atom "a"
b = Atom "b"
c = Atom "c"
f1 = AndT a b
f2 = OrT (Neg c) f1
ctx = AndT f1 f2
xs = assignments ctx

sats = filter (valid ctx) xs
xs' = assignments (Neg ctx)
sats' = filter (valid (Neg ctx)) xs'


-- parser
parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr = parse statement

statement :: GenParser Char st Expr
statement = do spaces
               x <- try binary <|> expr
               spaces
               eof
               return x

expr :: GenParser Char st Expr
expr = choice [binaryP, negation, variable]

              
variable :: GenParser Char st Expr
variable = do c <- letter
              return $ Atom [c]

negation :: GenParser Char st Expr
negation = do char '-'
              spaces
              Neg <$> expr

binaryP :: GenParser Char st Expr
binaryP = do char '('
             spaces
             x <- binary
             spaces
             char ')'
             return x

binary :: GenParser Char st Expr
binary = do x1 <- expr
            spaces
            s  <- choice $ map string ["&", "|", "->", ">>", "=>"]
            spaces
            x2 <- expr
            return $ connective s x1 x2
  where
    connective c = case c of
      "&"   -> AndT
      "|"   -> OrT
      "->"  -> Mat
      ">>"  -> Imp
      "=>"  -> Sup
      _     -> error "Impossible case"

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ';'  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

solve ctx = do 
  putStrLn <$> readFile "test.four"
  mapM_ prettyPrint $ models ctx
