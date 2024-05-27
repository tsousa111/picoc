{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Generator where

import PicoTypes
import PicoUnparser
import Test.QuickCheck
import Prelude hiding (EQ, GT, LT)

instance Arbitrary PicoC where
    arbitrary = genPicoC

genPicoC :: Gen PicoC
genPicoC = do
    x <- vectorOf 5 genInst
    return (PicoC (x ++ [Return (Const 0)]))

genInst :: Gen Inst
genInst = frequency [(90,genAttrib),(5,genWhile),(5,genITE)]

genAttrib :: Gen Inst
genAttrib = do
  exp <- genExpArit 1
  var <- genVar
  return (Attrib var exp)

genWhile :: Gen Inst
genWhile = do
  expBool <- genBooleanExp 1
  blocoC <- genCBlock
  return (While expBool blocoC)

genITE :: Gen Inst
genITE = do
  expBool <- genBooleanExp 1
  blocoC <- genCBlock
  blocoC2 <- genCBlock
  return (ITE expBool blocoC blocoC2)

genCBlock :: Gen CBlock
genCBlock = vectorOf 1 genInst

genExpArit :: Int -> Gen Exp
genExpArit n = frequency [(5,genConst),(n*5,genAdd n),(n*5,genMult n),(n*5,genDiv n),(n*5,genSub n),(n*5,genNeg n)]

genMult :: Int -> Gen Exp
genMult n = do
    x <- genExpArit (div n 5)
    y <- genExpArit (div n 5)
    return (Mult x y)

genAdd :: Int -> Gen Exp
genAdd n = do
    x <- genExpArit (div n 5)
    y <- genExpArit (div n 5)
    return (Add x y)

genDiv :: Int -> Gen Exp
genDiv n = do
  x <- genExpArit (div n 5)
  y <- genExpArit (div n 5)
  return (Div x y)

genSub :: Int -> Gen Exp
genSub n = do
  x <- genExpArit (div n 5)
  y <- genExpArit (div n 5)
  return (Sub x y)

genNeg :: Int -> Gen Exp
genNeg n = do
  x <- genExpArit (div n 5)
  return (Neg x)

genConst :: Gen Exp
genConst = do
    x <- arbitrary
    return (Const (abs x))


genBooleanExp :: Int -> Gen Exp
genBooleanExp n = frequency [(5*n,genGT n),(5*n,genLT n),(5*n,genEQ n),(5*n,genDIFF n),(5*n,genAND n),(5*n,genOR n),(5*n,genNot n),(5,genBool)]

genBool :: Gen Exp
genBool = elements [TRUE,FALSE]

genGT :: Int -> Gen Exp
genGT n = do
  x <- arbitrary
  y <- arbitrary
  return (GT x y)

genLT :: Int -> Gen Exp
genLT n = do
  x <- arbitrary
  y <- arbitrary
  return (LT x y)

genEQ :: Int -> Gen Exp
genEQ n = do
  x <- arbitrary
  y <- arbitrary
  return (EQ x y)

genDIFF :: Int -> Gen Exp
genDIFF n = do
  x <- arbitrary
  y <- arbitrary
  return (DIFF x y)

genAND :: Int -> Gen Exp
genAND n = do
  x <- genBooleanExp (div n 6)
  y <- genBooleanExp (div n 6)
  return (AND x y)

genOR :: Int -> Gen Exp
genOR n = do
  x <- genBooleanExp (div n 6)
  y <- genBooleanExp (div n 6)
  return (OR x y)

genNot :: Int -> Gen Exp
genNot n = do
  x <- genBooleanExp (div n 6)
  return (Not x)


genVar :: Gen String
genVar = vectorOf 1 $ elements ['a'..'z']

instance Arbitrary Exp where
    arbitrary :: Gen Exp
    arbitrary = sized genExpArit

