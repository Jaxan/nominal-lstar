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
import           Teacher            (TeacherWithTarget (..))

-- Wrapping it in a teacher
exampleTeacher :: TeacherWithTarget Atom
exampleTeacher = TeacherWithTarget example4
