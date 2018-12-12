module Rank where

import Prelude

-- | 手札の役を表す
data HandRank = 
  HighCard |
  TwoPair |
  ThreeCard |
  Straight |
  Flush |
  FullHouse |
  FourCard |
  StraightFlush |
  FiveCard |
  RoyalStraightFlush
derive instance eqHandRank :: Eq HandRank
derive instance ordHandRank :: Ord HandRank
