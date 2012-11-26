-- | Simple in-prcess execution of GHC API calls, for testing, to easily
-- see stdout, stderr, exception, etc.. Uses "GhcRun" directly, in the same
-- process, without the RPC layer.
module Main where

import System.Environment
import qualified Data.List as List

import Common
import GhcRun

-- A program, for debugging and experiments, that type-checks
-- a file and runs its main function.
-- It interfaces with the GHC API code in-process, as opposed
-- to out-of-process that the IdeSession API uses.

defOpts :: [String]
defOpts = [ "-hide-all-packages"
          , "-package parallel", "-package base", "-package old-time" ]

main :: IO ()
main = do
  args <- getArgs
  target <-
    case args of
      [f] -> return f
      []  -> return "test/MainModule/ParFib.hs"
      _   -> fail "usage: in-process [file]"

  putStrLn ""
  runOrErrs <- checkModule [target] (optsToDynFlags defOpts)
                           True (Just ("Main", "main")) 2
                           putStrLn putStrLn
  putStrLn $ "\nRun results:\n"
    ++ case runOrErrs of
      Left (Left ident) -> ident
      Left (Right ex)   -> showExWithClass ex
      Right errs -> List.intercalate "\n" (map formatSourceError errs) ++ "\n"