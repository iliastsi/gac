#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.PreProcess
import System.FilePath

main = defaultMainWithHooks
        simpleUserHooks{hookedPreProcessors=[("ypp",ppYpp)]}

ppYpp build local =
    PreProcessor {
        platformIndependent = True,
        runPreProcessor =
            mkSimplePreProcessor $ \inFile outFile verbosity -> do
                let yFile = replaceExtension outFile "y"
                runSimplePreProcessor (ppCpp   build local) inFile yFile   verbosity
                runSimplePreProcessor (ppHappy build local) yFile  outFile verbosity
    }
