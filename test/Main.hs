{-# language ImplicitParams #-}

module Main (main) where

import Control.Exception
import Data.IORef
import System.Exit

import Defs

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)

  -- Correct results.
  let ?x = 10 in do
    eq ref "g"        (g 5)            15
    eq ref "f"        (f 5)            31
    eq ref "dead"     (dead 5)         6
    eq ref "deadPass" (deadPass 5)     6
    eq ref "rec1"     (rec1 4)         10
    eq ref "branchT"  (branch True 5)  15
    eq ref "branchF"  (branch False 5) 5
    eq ref "local"    (local 5)        50
    eq ref "poly"     (poly 'a')       ('a', 10)
    eq ref "mixed"    (mixed length "abc") 13
    eq ref "c3"       (c3 1)           33
    eq ref "closure"  (closure 1 2)    13
    let ?y = 100 in eq ref "h" (h 5) 131

  eq ref "localIP" (localIP 1) 17
  eq ref "callSite"        (callSite (* 2) 5)       30
  eq ref "callSiteBranchT" (callSiteBranch (* 2) 5) 30
  eq ref "callSiteBranchF" (callSiteBranch (* 2) 0) 31

  let ?s = "!" in do
    eq ref "str"      (str 7)  "7!"
    eq ref "strTwice" strTwice "1!2!"

  let ?cfg = Cfg 3 "c" in do
    eq ref "cfgUse"  (cfgUse 1)  "c4"
    eq ref "cfgPass" (cfgPass 1) ("c4" ++ "c5" ++ show (Cfg 3 "c"))

  let ?k = (* 2) in do
    eq ref "fnUse"  (fnUse 3)  6
    eq ref "fnPass" (fnPass 3) 14

  -- Strictness: each of these must force the implicit parameter.
  let boom = error "boom" :: Int
  strict ref "g"        (\_ -> let ?x = boom in g 1)
  strict ref "f"        (\_ -> let ?x = boom in f 1)
  strict ref "dead"     (\_ -> let ?x = boom in dead 1)
  strict ref "deadPass" (\_ -> let ?x = boom in deadPass 1)
  strict ref "rec1"     (\_ -> let ?x = boom in rec1 3)
  strict ref "branchF"  (\_ -> let ?x = boom in branch False 1)
  strict ref "local"    (\_ -> let ?x = boom in local 1)
  strict ref "c3"       (\_ -> let ?x = boom in c3 1)
  strict ref "closure"  (\_ -> let ?x = boom in closure 1 2)
  strict ref "cfgUse"   (\_ -> let ?cfg = error "boom" in cfgUse 1)
  strict ref "fnUse"    (\_ -> let ?k = error "boom" in fnUse 1)
  strict ref "callSite" (\_ -> callSite (\_ -> boom) 1)
  strict ref "callSiteBranchF" (\_ -> callSiteBranch (\_ -> boom) 0)

  failures <- readIORef ref
  if failures == 0
    then putStrLn "ALL OK"
    else do putStrLn (show failures ++ " failure(s)")
            exitWith (ExitFailure 1)

eq :: (Eq a, Show a) => IORef Int -> String -> a -> a -> IO ()
eq ref name got want = do
  r <- tryAny (evaluate (got == want))
  case r of
    Right True  -> ok name
    Right False -> bad ref (name ++ ": got " ++ show got ++ ", want " ++ show want)
    Left e      -> bad ref (name ++ ": exception " ++ show e)

-- | Check that forcing the expression raises the error in the implicit parameter.
strict :: Show a => IORef Int -> String -> (() -> a) -> IO ()
strict ref name k = do
  r <- tryErrorCall (evaluate (k ()))
  case r of
    Left _  -> ok (name ++ " forced")
    Right v -> bad ref (name ++ ": not forced, returned " ++ show v)

ok :: String -> IO ()
ok name = putStrLn ("ok    " ++ name)

bad :: IORef Int -> String -> IO ()
bad ref msg = do
  putStrLn ("FAIL  " ++ msg)
  modifyIORef' ref (+1)

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

tryErrorCall :: IO a -> IO (Either ErrorCall a)
tryErrorCall = try
