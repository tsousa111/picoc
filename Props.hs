module Props where
import PicoTypes

import PicoParser
import PicoUnparser
import Refactor

-- NOTE: Check if a PicoC code is valid
prop_valid :: PicoC -> Bool
prop_valid p = p == picoC (unparse p)

-- NOTE: Check if refactor is idempotent
prop_refactor_idempotent :: PicoC -> Bool
prop_refactor_idempotent p = refactor p == refactor (refactor p)

-- NOTE: Check if optimazation is idempotent
prop_optimization_idempotent :: PicoC -> Bool
prop_optimization_idempotent p = arithmeticOpt p == arithmeticOpt (arithmeticOpt p)
