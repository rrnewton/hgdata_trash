-----------------------------------------------------------------------------
--
-- Module      :  Crypto.GnuPG
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------


module Crypto.GnuPG (
  decrypt
, encrypt
) where


import System.Process (runInteractiveProcess)
import System.IO (hClose, hGetContents, hPutStr)


decrypt :: String -> IO String
decrypt input =
  do
    (hIn, hOut, _, _) <- runInteractiveProcess
      "gpg"
      [
        "--decrypt"
      , "--quiet"
      , "--no-mdc-warning"
      , "--batch"
      ]
      Nothing
      Nothing
    hPutStr hIn input
    hClose hIn
    output <- hGetContents hOut
    return output


encrypt :: [String] -> String -> IO String
encrypt recipients input =
  do
    (hIn, hOut, _, _) <- runInteractiveProcess
      "gpg"
      (
        [
          "--encrypt"
        , "--quiet"
        , "--no-mdc-warning"
        , "--batch"
        , "--armor"
        ]
        ++
        concatMap (\r -> ["--recipient", r]) recipients
      )
      Nothing
      Nothing
    hPutStr hIn input
    hClose hIn
    output <- hGetContents hOut
    return output
