module PicoParser where

import Data.Char
import Library.Parser
import PicoTypes
import Text.Read
import Prelude hiding (EQ, GT, LT, (<$>), (<*>), (<|>))

picoC :: String -> PicoC
picoC s = fst $ head $ filter ((== "") . snd) (pPicoC s)

pPicoC :: Parser PicoC
pPicoC = PicoC <$> oneOrMore pInst

pInst :: Parser Inst
pInst = pIf <|> pReturn <|> pWhile <|> pAtrib <|> pPrint

pAtrib :: Parser Inst
pAtrib = f <$> pNames <*> symbol' '=' <*> pExp <*> symbol' ';'
  where
    f x _ z _ = Attrib x z

pCBlock :: Parser CBlock
pCBlock = enclosedBy (symbol' '{') (zeroOrMore pInst) (symbol' '}')

pWhile :: Parser Inst
pWhile = f <$> token' "while" <*> symbol' '(' <*> pExp <*> symbol' ')'<*> pCBlock
  where
    f _ _ x _ y = While x y

pIf :: Parser Inst
pIf = f <$> token' "if" <*> symbol' '(' <*> pExp <*> symbol' ')' <*> token' "then" <*> pCBlock <*> token' "else" <*> pCBlock
   <|> g <$> token' "if" <*> symbol' '(' <*> pExp <*> symbol' ')' <*> token' "then" <*> pCBlock
  where
    f _ _ x _ _ y _ z = ITE x y z
    g _ _ x _ _ y = ITE x y []

pReturn :: Parser Inst
pReturn = f <$> token' "return" <*> pExp <*> symbol' ';'
  where
    f _ x _ = Return x

pPrint :: Parser Inst
pPrint = f <$> token' "print" <*> symbol' '(' <*> oneOrMore (satisfy' isAscii) <*> symbol' ')' <*> symbol' ';'
    where
        f _ _ x _ _ = Print x

pExp = pExp5

pExp5 :: Parser Exp
pExp5 = f <$> pExp4 <*> token' "||" <*> pExp5
     <|> pExp4
  where
    f x _ = OR x

pExp4 :: Parser Exp
pExp4 = f <$> pExp3 <*> token' "&&" <*> pExp4
     <|> pExp3
  where
    f x _ = AND x

pExp3 :: Parser Exp
pExp3 = f <$> pExp2 <*> token' "==" <*> pExp3
        <|> g <$> pExp2 <*> token' "!=" <*> pExp3
        <|> pExp2
  where
    f x _ = EQ x
    g x _ = DIFF x

pExp2 :: Parser Exp
pExp2 = f <$> pExp1 <*> token' "<" <*> pExp2
     <|> g <$> pExp1 <*> token' ">" <*> pExp2
     <|> h <$> pExp1 <*> token' ">=" <*> pExp2
     <|> i <$> pExp1 <*> token' "<=" <*> pExp2
     <|> pExp1
  where
    f x _ = LT x
    g x _ = GT x
    h x _ = GET x
    i x _ = LET x

pExp1 :: Parser Exp
pExp1 = f <$> pExp0 <*> symbol' '+' <*> pExp1
     <|> g <$> pExp0 <*> symbol' '-' <*> pExp1
     <|> pExp0
  where
    f x _ = Add x
    g x _ = Sub x

pExp0 :: Parser Exp
pExp0 = f <$> pFactor <*> symbol' '*' <*> pExp0
     <|> g <$> pFactor <*> symbol' '/' <*> pExp0
     <|> pFactor
  where
    f x _ = Mult x
    g x _ = Div x

pFactor = f <$> pInt
       <|> g <$> pNames
       <|> h <$> token' "TRUE"
       <|> i <$> token' "FALSE"
       <|> j <$> symbol' '-' <*> pFactor
       <|> k <$> symbol' '!' <*> pFactor
       <|> l <$> symbol' '(' <*> pExp <*> symbol' ')'
  where
    f s = Const $ case readMaybe s :: Maybe Int of
        Just x -> x
        Nothing -> error "Not a number"
    g = Var
    h _ = TRUE
    i _ = FALSE
    j _ = Neg
    k _ = Not
    l _ x _ = x

