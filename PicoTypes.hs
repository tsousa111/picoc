{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module PicoTypes where

import Data.Data
import Library.StrategicData
import Prelude hiding (EQ, GT, LT, (<$>), (<*>), (<|>))

instance StrategicData PicoC
instance (StrategicData a) => StrategicData [a]

data PicoC = PicoC [Inst]
    deriving (Data, Eq)

data Inst
    = Attrib !String !Exp
    | While !Exp !CBlock
    | ITE !Exp !CBlock !CBlock
    | Return !Exp
    | Print !String
    deriving (Show, Data, Eq)

type CBlock = [Inst]
data Exp
    = Const !Int
    | TRUE
    | FALSE
    | Var !String
    | Add !Exp !Exp
    | Sub !Exp !Exp
    | Mult !Exp !Exp
    | Div !Exp !Exp
    | Neg !Exp
    | AND !Exp !Exp
    | OR !Exp !Exp
    | LET !Exp !Exp
    | GET !Exp !Exp
    | DIFF !Exp !Exp
    | LT !Exp !Exp
    | GT !Exp !Exp
    | EQ !Exp !Exp
    | Not !Exp
    deriving (Show, Data, Eq)
