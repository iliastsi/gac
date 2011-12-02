--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Command-line parser
--
-- This is an abstract command-line parser used by DynFlags
--
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PatternGuards #-}
module CmdLineParser (
    processArgs, OptKind(..),
    CmdLineP(..), getCmdLineState, putCmdLineState,
    Flag(..),

    EwM, addEwMErr, addEwMWarn, getArg, liftEwM, deprecate
  ) where

import Util
import {-# SOURCE #-} Outputable(panic)
import Bag
import SrcLoc

import Data.List


-- -------------------------------------------------------------------
-- The Flag and OptKind types

data Flag m = Flag {
    flagName    :: String,      -- Flag, without the leading "-"
    flagOptKind :: OptKind m    -- What to do if we see it
  }

-- ---------------------------
data OptKind m              -- Suppose the flag is -f
  = NoArg         (EwM m ())              -- -f all by itself
  | HasArg        (String    -> EwM m ()) -- -farg or -f arg
  | SepArg        (String    -> EwM m ()) -- -f arg
  | Prefix        (String    -> EwM m ()) -- -farg
  | OptPrefix     (String    -> EwM m ()) -- -f or -farg (i.e. the arg is optional)
  | OptIntSuffix  (Maybe Int -> EwM m ()) -- -f or -f=n; pass n to fn
  | IntSuffix     (Int       -> EwM m ()) -- -f of -f=n; pass n to fn
  | PassFlag      (String    -> EwM m ()) -- -f; pass "-f" fn
  | AnySuffix     (String    -> EwM m ()) -- -f or -farg; pass entire "-farg" to fn
  | PrefixPred    (String -> Bool) (String -> EwM m ())
  | AnySuffixPred (String -> Bool) (String -> EwM m ())


-- -------------------------------------------------------------------
-- The EwM monad

type Err   = Located String
type Warn  = Located String
type Errs  = Bag Err
type Warns = Bag Warn

-- EwM (short for "errors and warnings monad") is a
-- monad transformer for m tha adds an (err, warn) state
newtype EwM m a = EwM { unEwM :: Located String     -- Current arg
                              -> Errs -> Warns
                              -> m (Errs, Warns, a) }

instance Monad m => Monad (EwM m) where
    (EwM f) >>= k = EwM (\l e w -> do { (e',w',r) <- f l e w
                                      ; unEwM (k r) l e' w' })
    return v = EwM (\_ e w -> return (e, w, v))

setArg :: Located String -> EwM m a -> EwM m a
setArg l (EwM f) = EwM (\_ es ws -> f l es ws)

addEwMErr :: Monad m => String -> EwM m ()
addEwMErr e = EwM (\(L loc _) es ws -> return (es `snocBag` L loc e, ws, ()))

addEwMWarn :: Monad m => String -> EwM m ()
addEwMWarn msg = EwM (\(L loc _) es ws -> return (es, ws `snocBag` L loc w, ()))
    where w = "Warning: " ++ msg

deprecate :: Monad m => String -> EwM m ()
deprecate s = do
    arg <- getArg
    addEwMWarn (arg ++ " is deprecated: " ++ s)

getArg :: Monad m => EwM m String
getArg = EwM (\(L _ arg) es ws -> return (es, ws, arg))

liftEwM :: Monad m => m a -> EwM m a
liftEwM action = EwM (\_ es ws -> do { r <- action; return (es, ws, r) })


-- -------------------------------------------------------------------
-- A state monad for use in the command-line parser
-- (CmdLineP s) typically instantiates the 'm' in (EwM m) and (OptKind m)

newtype CmdLineP s a = CmdLineP { runCmdLine :: s -> (a, s) }

instance Monad (CmdLineP s) where
    return a = CmdLineP $ \s -> (a, s)
    m >>= k  = CmdLineP $ \s ->
                    let (a, s') = runCmdLine m s
                    in runCmdLine (k a) s'

getCmdLineState :: CmdLineP s s
getCmdLineState = CmdLineP $ \s -> (s,s)

putCmdLineState :: s -> CmdLineP s ()
putCmdLineState s = CmdLineP $ \_ -> ((),s)

-- ---------------------------
-- Processing arguments

processArgs :: Monad m
            => [Flag m] -- cmdline parser spec
            -> [Located String]      -- args
            -> m (
                  [Located String], -- spare args
                  [Located String], -- errors
                  [Located String]  -- warnings
                 )
processArgs spec args =
    do { (errs, warns, spare) <- unEwM (process args [])
                                       (panic "processArgs: no arg yet")
                                       emptyBag emptyBag
       ; return (spare, bagToList errs, bagToList warns) }
    where
        -- process :: [Located String] -> [Located String] -> EwM m [Located String]
        process [] spare = return (reverse spare)
        process (locArg@(L _ ('-' : arg)) : args) spare =
            case findArg spec arg of
                 Just (rest, opt_kind) ->
                     case processOneArg opt_kind rest arg args of
                          Left err -> do
                              setArg locArg $ addEwMErr err
                              process args spare
                          Right (action,rest) -> do
                              setArg locArg $ action
                              process rest spare
                 Nothing -> process args (locArg : spare)
        process (arg : args) spare = process args (arg : spare)

processOneArg :: OptKind m -> String -> String -> [Located String]
              -> Either String (EwM m (), [Located String])
processOneArg opt_kind rest arg args =
    let dash_arg   = '-' : arg
        rest_no_eq = dropEq rest
    in
    case opt_kind of
         NoArg a -> Right (a, args)

         HasArg f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                  | otherwise -> case args of
                                    []               -> missingArgErr dash_arg
                                    (L _ arg1:args1) -> Right (f arg1, args1)

         SepArg f -> case args of
                          []               -> unknownFlagErr dash_arg
                          (L _ arg1:args1) -> Right (f arg1, args1)

         Prefix f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                  | otherwise          -> unknownFlagErr dash_arg

         PrefixPred _ f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                        | otherwise          -> unknownFlagErr dash_arg

         PassFlag f | notNull rest -> unknownFlagErr dash_arg
                    | otherwise    -> Right (f dash_arg, args)

         OptIntSuffix f | null rest                     -> Right (f Nothing, args)
                        | Just n <- parseInt rest_no_eq -> Right (f (Just n), args)
                        | otherwise -> Left ("malformed integer argument in " ++ dash_arg)

         IntSuffix f | Just n <- parseInt rest_no_eq -> Right (f n, args)
                     | otherwise -> Left ("malformed integer argument in " ++ dash_arg)

         OptPrefix f       -> Right (f rest_no_eq, args)
         AnySuffix f       -> Right (f dash_arg, args)
         AnySuffixPred _ f -> Right (f dash_arg, args)

findArg :: [Flag m] -> String -> Maybe (String, OptKind m)
findArg spec arg =
    case [ (removeSpaces rest, optKind)
         | flag <- spec,
           let optKind = flagOptKind flag,
           Just rest <- [stripPrefix (flagName flag) arg],
           arg_ok optKind rest arg ]
    of
        []      -> Nothing
        (one:_) -> Just one

arg_ok :: OptKind t -> [Char] -> String -> Bool
arg_ok (NoArg _)            rest _   = null rest
arg_ok (HasArg _)           _    _   = True
arg_ok (SepArg _)           rest _   = null rest
arg_ok (Prefix _)           rest _   = notNull rest
arg_ok (PrefixPred p _)     rest _   = notNull rest && p (dropEq rest)
arg_ok (OptIntSuffix _)     _    _   = True
arg_ok (IntSuffix _)        _    _   = True
arg_ok (OptPrefix _)        _    _   = True
arg_ok (PassFlag _)         rest _   = null rest
arg_ok (AnySuffix _)        _    _   = True
arg_ok (AnySuffixPred p _)  _    arg = p arg

parseInt :: String -> Maybe Int
-- Looks for "433" or "=342", with no trailing gubbins
--      n or =n     => Just n
--      gibberish   => Nothing
parseInt s =
    case reads s of
         ((n,""):_) -> Just n
         _          -> Nothing

dropEq :: String -> String
-- Discards a leading equals sign
dropEq ('=' : s) = s
dropEq s         = s

unknownFlagErr :: String -> Either String a
unknownFlagErr f = Left ("unrecognised flag: " ++ f)

missingArgErr :: String -> Either String a
missingArgErr f = Left ("missing argument for flags: " ++ f)
