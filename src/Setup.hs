#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.PreProcess
import System.FilePath

main = defaultMainWithHooks
        simpleUserHooks{hookedPreProcessors=[("ypp",ppYpp)]}


-- For this files we need to run cpp befor happy
-- Code from http://stackoverflow.com/questions/7370029/any-example-of-a-custom-preprocessor-in-haskell
ppYpp build local =
    PreProcessor {
        platformIndependent = True,
        runPreProcessor =
            mkSimplePreProcessor $ \inFile outFile verbosity -> do
                let yFile = replaceExtension outFile "y"
                runSimplePreProcessor (ppCpp' ["-optP-P"] build local) inFile yFile verbosity
                runSimplePreProcessor (ppHappy build local) yFile outFile verbosity
    }
