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
designated f v = f v `elem` [B,T]

valid :: Expr -> Mapping -> Bool
valid (Atom s)       m = maybe False (designated id) (lookup s m)
valid (Neg (Atom s)) m = maybe False (designated inverse) (lookup s m)
valid (Neg e)        m = not (valid e m)
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

prettyModel xs = concat (map (\x -> fst x ++ show (snd x) ++ " ") xs)
prettyConcat ctx = map f $ zip [1..] $ map toList $ models ctx
  where f = (\x -> show (fst x) ++ " " ++ (prettyModel $ snd x))
prettyConcatK ctx = map f $ zip [1..] $ map toList $ kmodels ctx
  where f = (\x -> show (fst x) ++ " " ++ (prettyModel $ snd x))

--
models ctx = filter (valid f) (assignments f)
  where f = andify ctx

modelCount ctx = length . models $ ctx 

kval N = 0 
kval T = 1 
kval F = 1 
kval B = 2 

kval' m = sum $ map (\x -> kval $ snd x) $ toList m

kmodels ctx = 
  let 
    xs = map (\x -> (kval' x, x)) $ models ctx
    v  = foldr1 min $ map fst $ xs
  in 
    map snd $ filter (\x -> (fst x) == v) $ xs
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

(~) :: Expr -> Expr
(~) = Neg
