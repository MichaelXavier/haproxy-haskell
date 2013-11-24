{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.HAProxy.ParsersSpec (spec) where

import System.HAProxy.Parsers
import System.HAProxy.Types
import SpecHelper

import Data.Attoparsec.Text

spec :: Spec 
spec = do
  describe "Deserializeable ShowInfoResp" $ do
    it "parses a full example" $
      let info = parseOnly parser fullInfoStr
      in info `shouldBe` Right fullInfo


fullInfoStr = [q|Name: HAProxy
Version: 1.4.24
Release_date: 2013/06/17
Nbproc: 1
Process_num: 1
Pid: 11142
Uptime: 0d 0h00m36s
Uptime_sec: 36
Memmax_MB: 0
Ulimit-n: 8222
Maxsock: 8222
Maxconn: 4096
Maxpipes: 0
CurrConns: 1
PipesUsed: 0
PipesFree: 0
Tasks: 14
Run_queue: 1
node: Apollo-Minor
description: |]

fullInfo = ShowInfoResp "HAProxy"
                        "1.4.24"
                        (YearMonthDay 2013 06 17 ^. from gregorian)
                        1
                        1
                        "11142"
                        (fromSeconds 36)
                        (MB 0)
                        8222
                        8222
                        4096
                        0
                        1
                        0
                        0
                        14
                        1
                        "Apollo-Minor"
                        ""
