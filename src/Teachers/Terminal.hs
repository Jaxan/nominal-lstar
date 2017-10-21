module Teachers.Terminal where

import NLambda

import Control.Monad
import Data.IORef
import Data.List (intersperse, concat)
import Prelude hiding (filter, map, and, sum)
import System.Console.Haskeline
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- Posing a membership query to the terminal and waits for used to input a formula
ioMembership :: (Show i, NominalType i, Contextual i) => Set [i] -> Set ([i], Formula)
ioMembership queries = unsafePerformIO $ do
    cache <- readIORef mqCache
    let cachedAnswers = filter (\(a, f) -> a `member` queries) cache
    let newQueries = simplify $ queries \\ map fst cache
    let representedInputs = toList . mapFilter id . setOrbitsRepresentatives $ newQueries
    putStrLn "\n# Membership Queries:"
    putStrLn "# Please answer each query with \"True\" or \"False\" (\"^D\" for quit)"
    answers <- forM representedInputs $ \input -> do
        putStr "Q: "
        print input
        let loop = do
                x <- fmap readMaybe <$> getInputLine "A: "
                case x of
                    Nothing -> error "Bye bye, have a good day!"
                    Just Nothing -> do
                        outputStrLn $ "Unable to parse, try again"
                        loop
                    Just (Just f) -> return f
        answer <- runInputT defaultSettings loop
        return $ orbit [] (input, fromBool answer)
    let answersAsSet = simplify . sum . fromList $ answers
    writeIORef mqCache (simplify $ cache `union` answersAsSet)
    return (simplify $ cachedAnswers `union` answersAsSet)
    where
        -- We use a cache, so that questions will not be repeated.
        -- It is a bit hacky, as the Teacher interface does not allow state...
        {-# NOINLINE mqCache #-}
        mqCache = unsafePerformIO $ newIORef empty


-- Same as above, but with a machine-readable format
ioMembership2 :: (Show i, NominalType i, Contextual i) => Set [i] -> Set ([i], Formula)
ioMembership2 queries = unsafePerformIO $ do
    cache <- readIORef mqCache
    let cachedAnswers = filter (\(a, f) -> a `member` queries) cache
    let newQueries = simplify $ queries \\ map fst cache
    let representedInputs = toList . mapFilter id . setOrbitsRepresentatives $ newQueries
    answers <- forM representedInputs $ \input -> do
        let str = Data.List.concat . intersperse " " . fmap show $ input
        putStrLn $ "MQ \"" ++ str ++ "\""
        let askit = do
                x <- getInputLine ""
                case x of
                    Just "Y" -> return True
                    Just "N" -> return False
                    _ -> error "Unable to parse, or quit. Bye!"
        answer <- runInputT defaultSettings askit
        return $ orbit [] (input, fromBool answer)
    let answersAsSet = simplify . sum . fromList $ answers
    writeIORef mqCache (simplify $ cache `union` answersAsSet)
    return (simplify $ cachedAnswers `union` answersAsSet)
    where
        -- We use a cache, so that questions will not be repeated.
        -- It is a bit hacky, as the Teacher interface does not allow state...
        {-# NOINLINE mqCache #-}
        mqCache = unsafePerformIO $ newIORef empty


-- Poses a query to the terminal, waiting for the user to provide a counter example
-- TODO: extend to any alphabet type (hard because of parsing)
ioEquivalent :: (Show q, NominalType q, Show i, Read i, NominalType i) => Automaton q i -> Maybe (Set [i])
ioEquivalent hypothesis = unsafePerformIO $ do
    putStrLn "\n# Is the following automaton correct?"
    putStr "# "
    print hypothesis
    putStrLn "# \"^D\" for equivalent, \"[...]\" for a counter example (eg \"[0,1,0]\")"
    let loop = do
            x <- fmap readMaybe <$> getInputLine "> "
            case x of
                Nothing ->  do
                    outputStrLn $ "Ok, we're done"
                    return Nothing
                Just Nothing -> do
                    outputStrLn $ "Unable to parse (88), try again"
                    loop
                Just (Just f) -> return (Just f)
    answer <- runInputT defaultSettings loop
    return (orbit [] <$> answer)

-- Same as above but in different format.
ioEquivalent2 :: (Show q, NominalType q, Show i, Read i, NominalType i) => Automaton q i -> Maybe (Set [i])
ioEquivalent2 hypothesis = unsafePerformIO $ do
    putStrLn "EQ\n\"Is the following automaton correct?"
    print hypothesis
    putStrLn "\""
    let loop = do
            x <- getInputLine ""
            case x of
                Just "Y" -> return Nothing
                Just ('N' : ' ' : ce) -> return (Just (readCE ce))
                _ -> error "Unable to parse (104), or quit. Bye!"
    answer <- runInputT defaultSettings loop
    return (orbit [] <$> answer)
    where
        readCE [] = []
        readCE (' ' : xs) = readCE xs
        readCE xs = case reads xs of
            [(a, str)] -> a : readCE str
            _ -> error "Unable to parse (113)"
