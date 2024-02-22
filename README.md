# path-io-effectful

Adaptation of the path-io library for the effectful ecosystem.

## Examples

```haskell
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Effectful
import Effectful.FileSystem.Path.IO
import Path

main :: IO ()
main =
  runEff $ runFileSystem $ do
    createDir [reldir|hoge]
```
