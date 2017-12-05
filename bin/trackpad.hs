{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

-- base --------------------------------

import Control.Applicative  ( (<|>), (<*>), pure )
import Control.Monad        ( (>>), forM_, return )
import Data.Function        ( (.), ($) )
import Data.Functor         ( (<$>) )
import Data.List            ( filter )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( (<>) )
import System.IO            ( IO )
import Text.Show            ( Show )

-- data-default ------------------------

import Data.Default  ( def )

-- fluffy ------------------------------

import Fluffy.MonadIO  ( MonadIO, eitherIOThrowT, liftIO, say, warn )
import Fluffy.Nat      ( One, atMostOne )
import Fluffy.Options  ( optParser )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.TH      ( makeLenses )

-- optparse-applicative ----------------

import Options.Applicative  ( InfoMod, Parser, ParserInfo
                            , command, info, progDesc, subparser )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun   ( HasDryRunLevel( dryRunLevel )
                                  , DryRunLevel( DryRunLevel )
                                  , flagDryRun
                                  )
import ProcLib.CommonOpt.Verbose  ( HasVerboseLevel( verboseLevel )
                                  , VerboseLevel( VerboseLevel )
                                  , flagVerbose
                                  )
import ProcLib.Process            ( CmdSpec( CmdSpec ), StdStream( NoStream )
                                  , capture', doProcess, runProcess, system )

-- text --------------------------------

import Data.Text  ( Text, isInfixOf, lines, unpack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Trackpad.Paths  ( xinput )

--------------------------------------------------------------------------------

data Enable = Enable | Disable
  deriving Show

enableText :: Enable -> Text
enableText Enable  = "enable"
enableText Disable = "disable"

enableDesc :: Enable -> InfoMod Enable
enableDesc t = progDesc . unpack $ enableText t <> " trackpad"

enableInfo :: Enable -> ParserInfo Enable
enableInfo e = info (pure e) (enableDesc e)

enableParser :: Enable -> Parser Enable
enableParser e = subparser . command (unpack $ enableText e) $ enableInfo e

data Options = Options { _enable  :: Enable
                       , _verbose :: VerboseLevel One
                       , _dryRun  :: DryRunLevel  One }
  deriving Show

$( makeLenses ''Options )

instance HasDryRunLevel Options One where
  dryRunLevel = dryRun

instance HasVerboseLevel Options One where
  verboseLevel = verbose

parseOpts :: Parser Options
parseOpts = Options <$> (enableParser Enable <|> enableParser Disable)
                    <*> (VerboseLevel <$> atMostOne flagVerbose)
                    <*> (DryRunLevel  <$> atMostOne flagDryRun)


foo :: MonadIO μ => μ ()
foo = do
  liftIO $ say ("hello, world!" :: Text)

main :: IO ()
main = {- runProc_ $ -} do
  opts <- optParser "enable/disable trackpad" parseOpts

  ((o,""),_) <- eitherIOThrowT $ runProcess opts $ capture' def NoStream (CmdSpec xinput [ "--list", "--name-only" ])
  tp <- case filter (isInfixOf "Touchpad") $ lines o of
          []  -> warn "no touchpad found" >> return Nothing
          [t] -> return $ Just t
          ts  -> forM_ ("too many touchpads found:":ts) warn >> return Nothing
  case tp of
    Just x -> doProcess opts $ system (CmdSpec xinput [ enableText (opts ^. enable), x ])
    Nothing -> return ()


-- that's all, folks! ----------------------------------------------------------
