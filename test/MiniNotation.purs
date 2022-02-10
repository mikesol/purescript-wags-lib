module Test.MiniNotation where

import Type.Proxy (Proxy(..))
import WAGS.Lib.Tidal.Tidal (s)
import WAGS.Lib.Tidal.Types (CycleDuration, Voice)

type Test = forall e. CycleDuration -> Voice e

test0 = s (Proxy :: Proxy "") :: Test
test1 = s (Proxy :: Proxy "hh") :: Test
--test2 = s (Proxy :: Proxy "hh]") :: Test
test2 = s (Proxy :: Proxy "hh*2") :: Test
test3 = s (Proxy :: Proxy "hh*23") :: Test
test4 = s (Proxy :: Proxy "[hh hh]") :: Test
test5 = s (Proxy :: Proxy "[<hh> hh]") :: Test
--test6 = s (Proxy :: Proxy "[<hh>> hh]") :: Test
test6 = s (Proxy :: Proxy """<
  [bd bd hh*2 [notes:6, chin*4]]
  [bd bd hh*2 [notes:7, chin*4]]
  [bd bd hh*2 [notes:8, chin*4]]
  [psr:2 hh hh;tgy hh*2]
  [bd bd hh*2 [notes:10, chin*4]]
  [bd bd hh*3 [notes:12, chin*4]]
  [bd bd hh*2 [notes:13, chin*4]]
  [bd [bd,hh:8] hh [notes:14, chin*4]]
  [bd bd hh*2 [notes:1, chin*4]]

  [[bd,hh:4] bd hh*2 [notes:6;tgx, chin*4] bd bd hh*2 [notes:7, chin*4]]
  [bd bd hh*2 [notes:8, chin*4] bd bd hh*2 [notes:9, chin*4]]
  [[bd,hh:5] bd hh*2 [notes:10, chin*4] bd bd hh*2 [notes:12, chin*4]]
  [bd bd hh*2 [notes:13, chin*4] bd bd hh*2 [notes:14, chin*4]]

  [bd bd hh*2 [notes:13, chin*4]]
  [glitch:4 bd hh*2 [notes:14, chin*4]]

  [[bd,hh:1] bd hh*2 [notes:6, chin*4] bd bd hh*2 [notes:7, chin*4]]

  [bd [bd,hh:4] hh*2 [notes:13, chin*4]]
  [bd hh*2 [notes:6, chin*4] bd hh*2 [notes:7, chin*4]]

>  """) :: Test
