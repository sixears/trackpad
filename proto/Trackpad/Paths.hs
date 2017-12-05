{-# LANGUAGE QuasiQuotes #-}

module Trackpad.Paths where

import Path  ( Abs, File, Path, absfile )

xinput :: Path Abs File
xinput = [absfile|__xinput__/bin/xinput|]
