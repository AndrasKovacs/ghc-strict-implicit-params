{-# language ImplicitParams, RankNTypes #-}

module Defs where

f :: (?x :: Int) => Int -> Int
f n = g n + g (n + 1)
{-# noinline f #-}

g :: (?x :: Int) => Int -> Int
g n = n + ?x
{-# noinline g #-}

h :: (?x :: Int) => (?y :: Int) => Int -> Int
h n = f n + ?y
{-# noinline h #-}

dead :: (?x :: Int) => Int -> Int
dead n = n + 1
{-# noinline dead #-}

deadPass :: (?x :: Int) => Int -> Int
deadPass n = dead n
{-# noinline deadPass #-}

rec1 :: (?x :: Int) => Int -> Int
rec1 0 = ?x
rec1 n = rec1 (n - 1)
{-# noinline rec1 #-}

branch :: (?x :: Int) => Bool -> Int -> Int
branch True  n = n + ?x
branch False n = n
{-# noinline branch #-}

local :: (?x :: Int) => Int -> Int
local n = go n
  where go k = k * ?x
{-# noinline local #-}

localIP :: Int -> Int
localIP n = let go :: (?z :: Int) => Int -> Int
                go k = k + ?z
                {-# noinline go #-}
            in let ?z = 7 in go n + go (n + 1)
{-# noinline localIP #-}

poly :: (?x :: Int) => a -> (a, Int)
poly a = (a, g 0)
{-# noinline poly #-}

mixed :: (?x :: Int) => forall a. (a -> Int) -> a -> Int
mixed k a = k a + ?x
{-# noinline mixed #-}

str :: (?s :: String) => Int -> String
str n = show n ++ ?s
{-# noinline str #-}

strTwice :: (?s :: String) => String
strTwice = str 1 ++ str 2
{-# noinline strTwice #-}

data Cfg = Cfg { cfgA :: !Int, cfgB :: String } deriving (Eq, Show)

cfgUse :: (?cfg :: Cfg) => Int -> String
cfgUse n = cfgB ?cfg ++ show (cfgA ?cfg + n)
{-# noinline cfgUse #-}

cfgPass :: (?cfg :: Cfg) => Int -> String
cfgPass n = cfgUse n ++ cfgUse (n + 1) ++ show ?cfg
{-# noinline cfgPass #-}

fnUse :: (?k :: Int -> Int) => Int -> Int
fnUse n = ?k n
{-# noinline fnUse #-}

fnPass :: (?k :: Int -> Int) => Int -> Int
fnPass n = fnUse n + fnUse (n + 1)
{-# noinline fnPass #-}

c0 :: (?x :: Int) => Int -> Int
c0 n = n + ?x
{-# noinline c0 #-}

c1 :: (?x :: Int) => Int -> Int
c1 n = c0 n
{-# noinline c1 #-}

c2 :: (?x :: Int) => Int -> Int
c2 n = c1 n
{-# noinline c2 #-}

c3 :: (?x :: Int) => Int -> Int
c3 n = c2 n + c1 n + c0 n
{-# noinline c3 #-}

closure :: (?x :: Int) => Int -> (Int -> Int)
closure n = \k -> k + n + ?x
{-# noinline closure #-}
