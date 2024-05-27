module Runner where

import Generator
import PicoParser
import PicoTypes
import PicoUnparser
import Props
import Refactor
import TestSuite

-- Programa 1 - Subtração entre 2 números positivos, -1 em caso de erro de input (demora muito)
prog1 = picoC "if((a >= 0) && (b >= 0))then{if(a>b)then{c = a - b;}else{c = b - a;}}else{c = -1;}return c;"

inputs_prog1_1 = [("a", 1), ("b", 10)]
inputs_prog1_2 = [("a", 10), ("b", 10)]
inputs_prog1_3 = [("a", -1), ("b", 10)]

testSuiteProg1 = [(inputs_prog1_1, 9), (inputs_prog1_2, 0), (inputs_prog1_3, -1)]
runTestSuiteProg1 = runTestSuite prog1 testSuiteProg1

runMutationSuiteProg1 s = runMutationSuite prog1 testSuiteProg1 s

-- Programa 2 - Conta arbitrária
prog2 = picoC "if (a>10)then {a = a * 10;}else{a = a - 10;}return a;"

inputsprog_2_1 = [("a", 20)]
inputsprog_2_2 = [("a", 0)]
inputsprog_2_3 = [("a", 3)]

testSuiteProg2 = [(inputsprog_2_1, 200), (inputsprog_2_2, -10), (inputsprog_2_3, -7)]
runTestSuiteProg2 = runTestSuite prog2 testSuiteProg2

runMutationSuiteProg2 s = runMutationSuite prog2 testSuiteProg2 s

-- Programa 3 - Maior de 3 números, resultado em m
prog3 = picoC "if (a > b) then { if (a > c) then { m = a; } else { m = c; } } else { if (b > c) then { m = b; } else { m = c; } } return m;"

inputsprog_3_1 = [("a", 2), ("b", 0), ("c", 5)]
inputsprog_3_2 = [("a", 1), ("b", 6), ("c", 3)]
inputsprog_3_3 = [("a", 5), ("b", 3), ("c", 1)]

testSuiteProg3 = [(inputsprog_3_1, 5), (inputsprog_3_2, 6), (inputsprog_3_3, 5)]
runTestSuiteProg3 = runTestSuite prog3 testSuiteProg3

runMutationSuiteProg3 s = runMutationSuite prog3 testSuiteProg3 s
