module Examples
    ( module Examples
    , module Examples.Contrived
    , module Examples.ContrivedNFAs
    , module Examples.Fifo
    , module Examples.NonResidual
    , module Examples.Residual
    , module Examples.RunningExample
    , module Examples.Stack
    ) where

import Examples.Contrived
import Examples.ContrivedNFAs
import Examples.Fifo
import Examples.NonResidual
import Examples.Residual
import Examples.RunningExample
import Examples.Stack
import NLambda (Atom)
import Teacher (Teacher, teacherWithTarget)

-- Wrapping it in a teacher
exampleTeacher :: Teacher Atom
exampleTeacher = teacherWithTarget example4
