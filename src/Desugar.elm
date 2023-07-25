module Desugar exposing (desugarProgram)

import AST.Backend as B
import AST.Frontend as F
import Error exposing (DesugarError)


desugarProgram : F.Program -> Result DesugarError B.Program
desugarProgram program =
    Debug.todo "desugar program"
