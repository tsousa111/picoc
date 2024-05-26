module Library.Parser where

import Data.Char
import Prelude hiding ((<$>), (<*>), (<|>))

infixl 2 <|>

infixl 3 <*>

type Parser r = String -> [(r, String)]

-- symbol :: Char -> String -> [(Char, String)]

symbol' s = const <$> symbol s <*> spaces
symbol :: Char -> Parser Char
symbol _ [] = []
symbol s (h : t)
  | s == h = [(h, t)]
  | otherwise = []

-- satisfy :: (Char -> Bool) -> String -> [(Char, String)]
satisfy' p = const <$> satisfy p <*> spaces
satisfy :: (Char -> Bool) -> Parser Char
satisfy _ [] = []
satisfy p (h : t)
  | p h = [(h, t)]
  | otherwise = []

token' t = const <$> token t <*> spaces
token :: [Char] -> String -> [([Char], String)]
token t inp
  | t == take (length t) inp = [(t, drop (length t) inp)]
  | otherwise = []

succeed :: a -> String -> [(a, String)]
succeed r inp = [(r, inp)]

(<|>) :: Parser a -> Parser a -> Parser a
(p <|> q) inp = p inp ++ q inp

-- (<*>) :: Parser a -> Parser b -> Parser (a,b)
-- (p <*> q) inp = [ ((x,y), rst') | (x, rst) <- p inp, (y, rst') <- q rst]

(<*>) :: Parser (a -> r) -> Parser a -> Parser r
(p <*> q) inp = [(f r, rst') | (f, rst) <- p inp, (r, rst') <- q rst]

(<$>) :: (a -> r) -> Parser a -> Parser r
(f <$> p) inp = [(f r, rst) | (r, rst) <- p inp]

{-
 Examples:

As -> a
    | a A

pAs = f <$> symbol 'a'
   <|> g <$> symbol 'a' <*> pAs
  where
    f x = 1
    g x y = 1 + y

pA = f <$> symbol 'a' <*> symbol 'b' <*> symbol 'c' <*> symbol 'd'
  where
    f x y z w = y

pHaskellLists = enclosedBy (symbol '[') (separatedBy pInt (symbol ',')) (symbol ']')
-}

pDigitos = f <$> satisfy isDigit
        <|> g <$> satisfy isDigit <*> pDigitos
         where
           f x = [x]
           g x y = x : y

oneOrMore p = f <$> p
           <|> g <$> p <*> oneOrMore p
           where
                f x = [x]
                g x y = x : y

zeroOrMore p = succeed []
           <|> g <$> p <*> zeroOrMore p
           where
                g x y = x : y

pString = f <$> symbol '"' <*> zeroOrMore (satisfy (/= '"')) <*> symbol '"' <*> spaces
    where f _ y _ _ = y

optional p = f <$> p
          <|> succeed []
    where f x = [x]

pInt = f <$> optional sign <*> oneOrMore (satisfy isDigit) <*> spaces
  where
    f x y _ = x ++ y

sign = symbol '+'
     <|> symbol '-'

separatedBy p s = f <$> p
               <|> g <$> p <*> s <*> separatedBy p s
            where
               f x = [x]
               g x _ z = x : z

followedBy p s = succeed []
               <|> g <$> p <*> s <*> followedBy p s
            where
               g x _ z = x : z

enclosedBy l p r = f <$> l <*> p <*> r
               where
                 f _ y _ = y

spaces = zeroOrMore (satisfy isSpace)

pNames = f <$> satisfy isLower <*> zeroOrMore (satisfy isAlphaNum) <*> spaces
    where
        f a b _ = a : b
