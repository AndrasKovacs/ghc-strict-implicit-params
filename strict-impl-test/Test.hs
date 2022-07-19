{-# language ImplicitParams, RankNTypes, ConstraintKinds #-}
{-# options_ghc -fplugin StrictImplParams #-}

module Test where

type Foo = (?foo :: Int)
type Bar = (?bar :: Int)

foo :: Foo => Int -> Bar => Int
foo x = x
{-# noinline foo #-}

-- myseq :: a -> b -> b
-- myseq a b = seq a b
-- {-# inline myseq #-}

-- myfoo :: Int -> Int
-- myfoo x = myseq x x
-- {-# noinline myfoo #-}

-- foo :: (?lvl :: Int) => Int -> Int
-- foo x = x + ?lvl

-- foo :: (?lvl :: Int, ?foo :: Int, ?bar :: Int) => Int -> Int -> (?baz :: Int) => Int
-- foo = \x y -> x + y + ?lvl + ?foo + ?bar + ?baz
-- {-# noinline foo #-}

-- bruh :: Int -> Int -> Int -> Int
-- bruh x y z = x

-- bar :: IP "lvl" Int => Int
-- bar = ?lvl

-- main :: IO ()
-- main = do
--   putStrLn "hello"
