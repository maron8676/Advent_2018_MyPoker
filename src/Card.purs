module Card where

import Prelude

import Data.List (List(..), length, (:))
import Data.Tuple (Tuple(..))

-- | カード１枚を表す
type Card = Tuple Suit CardNum

-- | カードのスート
data Suit = Diamonds | Clubs | Hearts | Spades | StJoker

-- | スートの表示用文字列
instance showSuit :: Show Suit where
  show Diamonds = "Diamonds"
  show Clubs = "Clubs"
  show Hearts = "Hearts"
  show Spades = "Spades"
  show StJoker = ""

-- | スートの同値関係
instance eqSuitInst :: Eq Suit where
  eq _ StJoker = true
  eq StJoker _ = true
  eq a b = show a == show b

-- | スートの順序関係（手札ソートのために定義）
derive instance ordSuit :: Ord Suit

-- | カードの番号（強さ）
-- | OneはAceの別名として使う
data CardNum = 
  One |
  Two |
  Three |
  Four |
  Five |
  Six |
  Seven |
  Eight |
  Nine |
  Ten |
  Jack |
  Queen |
  King |
  Ace |
  Joker 

-- | カードの番号（強さ）の表示用文字列
instance showCardNum :: Show CardNum where
  show One = "A"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"
  show Joker = "JK"
derive instance eqCardNumInst :: Eq CardNum

-- | 番号（強さ）の順序関係（手札ソートのために定義）
derive instance ordCardNum :: Ord CardNum

-- | １つ前の番号（強さ）のカード
pred :: CardNum -> CardNum
pred Ace = King
pred One = Joker
pred Two = Ace
pred Three = Two
pred Four = Three
pred Five = Four
pred Six = Five
pred Seven = Six
pred Eight = Seven
pred Nine = Eight
pred Ten = Nine
pred Jack = Ten
pred Queen = Jack
pred King = Queen
pred Joker = Joker

-- | １つ次の番号（強さ）のカード
succ :: CardNum -> CardNum
succ Ace = Joker
succ One = Two
succ Two = Three
succ Three = Four
succ Four = Five
succ Five = Six
succ Six = Seven
succ Seven = Eight
succ Eight = Nine
succ Nine = Ten
succ Ten = Jack
succ Jack = Queen
succ Queen = King
succ King = Ace
succ Joker = Joker

-- | 第１引数のカードの１つ前の番号（強さ）が第２引数のカードである場合`True`を返す
isPred :: CardNum -> CardNum -> Boolean
isPred _ Joker = true
isPred Joker _ = true
isPred a b = pred a == b

-- | 第１引数のカードの１つ次の番号（強さ）が第２引数のカードである場合`True`を返す
isSucc :: CardNum -> CardNum -> Boolean
isSucc _ Joker = true
isSucc Joker _ = true
isSucc a b = succ a == b

-- | スートの種類数
cardinalitySuit :: Int
cardinalitySuit = length suits

-- | ジョーカー以外のカードの種類数
cardinalityCardNum :: Int
cardinalityCardNum = length normalCardNums

-- | カード全体の種類数
cardinalityCard :: Int
cardinalityCard = length cards

-- | 全てのスートのリスト
suits :: List Suit
suits = (Diamonds : Clubs : Hearts : Spades : Nil)

-- | 全ての番号（強さ）のリスト
normalCardNums :: List CardNum
normalCardNums = (Two : Three : Four : Five : Six : Seven : Eight : Nine : Ten : Jack : Queen : King : Ace : Nil)

-- | ジョーカー以外のカードのリスト
normalCards :: List Card
normalCards = Tuple <$> suits <*> normalCardNums

-- | 全てのカードのリスト
cards :: List Card
cards = (Tuple StJoker Joker) : normalCards

-- | ロイヤルストレートフラッシュ判定用リスト
royalStraight :: List CardNum
royalStraight = (Ten : Jack : Queen : King : Ace : Nil)

-- | ２ペアのサンプル手札（カードのリスト）
sampleTwoPairHand :: List Card
sampleTwoPairHand = ((Tuple Diamonds Three) : (Tuple Clubs Four) : (Tuple Clubs Three) : (Tuple Diamonds Four) : (Tuple Hearts King) : Nil)
