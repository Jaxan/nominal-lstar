module Examples
    ( module Examples
    , module Examples.Contrived
    , module Examples.ContrivedNFAs
    , module Examples.Fifo
    , module Examples.Stack
    ) where

import           Examples.Contrived
import           Examples.ContrivedNFAs
import           Examples.Fifo
import           Examples.Stack
import           NLambda            (Atom)
import           Teacher            (teacherWithTarget, Teacher)

-- Wrapping it in a teacher
exampleTeacher :: Teacher Atom
exampleTeacher = teacherWithTarget example4
