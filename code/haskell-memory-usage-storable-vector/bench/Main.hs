{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main (main) where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Foldable (for_)
import qualified Weigh
import           Weigh (Column(..))


main :: IO ()
main = do
  Weigh.mainWith $ do
    Weigh.setColumns
      [ Case
      , Allocated
      , GCs
      , Live
      , Check
      , Max
      , MaxOS
      ]

    -- Important:
    -- For accurate memory measurement it is critical not to accidentally
    -- share values as pointers, otherwise used memory will show up as too small.
    -- For example:
    -- Avoid functions such as `Data.Vector.replicate` that replicate a given
    -- value; instead use `generate` that make it depend on an index.

    let powersOf10 = [0..7]

    Weigh.wgroup "Data.Vector of singleton Storable Vectors" $ do
      -- In this test we create an outer normal Vector of Storable vectors:
      --   outerVector = [
      --     [ (index 0) pointer to -> StorableVector=[0 :: Int]
      --     , (index 1) pointer to -> StorableVector=[1 :: Int]
      --     , (index 2) pointer to -> StorableVector=[2 :: Int]
      --     ...
      --     ]
      -- We can conclude the memory overhead of a Storable Vector (not including
      -- its contents) by looking at the memory usage for this test,
      -- and (assuming 64-bit) subtracting:
      --   * 1 Word each of the outer Vector's entry pointers
      --   * 1 Word for the contents being 1 `Int` in an inner Storable Vector
      -- On my machine I get 80 Bytes memory usage per outer vector entry,
      -- thus 80 - 2*8 = 64 Bytes overhead per Storable Vector.

      let makeNManyStorableVectors n =
            V.generate n $ \i -> -- outer vector we use to allocate `n` many inner vectors
              VS.singleton (i :: Int) -- inner vector, what we want to measure

      for_ powersOf10 $ \(e :: Int) -> do
        let n = 10 ^ e
        Weigh.func ("Storable Vectors " <> " " <> show n) makeNManyStorableVectors n
