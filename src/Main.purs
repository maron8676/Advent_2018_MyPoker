module Main where

import Prelude

import Card (Card, CardNum(..), Suit(..), cardinalityCardNum, cardinalitySuit, isSucc, normalCardNums, royalStraight, sampleTwoPairHand, suits)
import Data.List (List(..), any, filter, foldl, groupBy, length, nub, singleton, sort, sortBy, transpose, zip, (:))
import Data.List.NonEmpty as NEList
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Console (log)

-- | サンプルの手札が２ペアであることを判定する
main :: Effect Unit
main = do
  log $ show sample1
  (log <<< show <<< isTwoPairRank) sample1

-- | 番号のリストが連続する番号である場合`True`を返す
isSequence :: List CardNum -> Boolean
isSequence ls = any isSequenceSorted $ sortCardNum ls where
  isSequenceSorted Nil = true
  isSequenceSorted (a : Nil) = true
  isSequenceSorted (a : b : c) = isSucc a b && isSequenceSorted (b : c)

-- | カードのリストが連続する番号である場合`True`を返す
isStraightRank :: List Card -> Boolean
isStraightRank = map snd >>> isSequence

-- | カードのリストが10,J,Q,K,Aの組み合わせの場合`True`を返す
isRoyalStraightRank :: List Card -> Boolean
isRoyalStraightRank = map snd >>> sortCardNum >>> (any $ (==) royalStraight)

-- | カードのリストから同じ番号のグループリストを作る
groupCardNum :: List Card -> List (NEList.NonEmptyList CardNum)
groupCardNum = map snd >>> sort >>> (groupBy (==))

-- | 整数のリストの最大値が一定値以上である場合`True`を返す
isBiggerMax :: Int -> List Int -> Boolean
isBiggerMax n = (foldl max 0) >>> ((<=) n)

-- | カードのリストから同じ番号のグループを作り、グループサイズのリストを返す
sizeSameCardNumGroup :: List Card -> List Int
sizeSameCardNumGroup = groupCardNum >>> (map (NEList.toList >>> length))

-- | カードのリストがスリーカードである場合`True`を返す
isThreeCardRank :: List Card -> Boolean
isThreeCardRank = sizeSameCardNumGroup >>> (isBiggerMax 3)

-- | カードのリストがフォーカードである場合`True`を返す
isFourCardRank :: List Card -> Boolean
isFourCardRank = sizeSameCardNumGroup >>> (isBiggerMax 4)

-- | カードのリストがファイブカードである場合`True`を返す
isFiveCardRank :: List Card -> Boolean
isFiveCardRank = sizeSameCardNumGroup >>> (isBiggerMax 5)

-- | カードのリストが２ペアである場合`True`を返す
isTwoPairRank :: List Card -> Boolean
isTwoPairRank = sizeSameCardNumGroup >>> (filter ((<=) 2)) >>> length >>> ((<=) 2)

-- | カードのリストがフルハウスである場合`True`を返す
isFullHouseRank :: List Card -> Boolean
isFullHouseRank ls = (isTwoPairRank ls) && (isThreeCardRank ls)

-- | カードのリストがフラッシュである場合`True`を返す
isFlushRank :: List Card -> Boolean
isFlushRank = map fst >>> allEqual

-- | カードのリストがストレートフラッシュである場合`True`を返す
isStraightFlushRank :: List Card -> Boolean
isStraightFlushRank ls = (isStraightRank ls) && (isFlushRank ls)

-- | カードのリストがロイヤルストレートフラッシュである場合`True`を返す
isRoyalStraightFlushRank :: List Card -> Boolean
isRoyalStraightFlushRank ls = (isRoyalStraightRank ls) && (isFlushRank ls)

-- | リストの要素が全て等しい場合`True`を返す
-- | 空リストの場合も`True`を返す
allEqual :: forall a. Eq a => List a -> Boolean
allEqual = nub >>> length >>> ((>=) 1)

-- | 番号のリストをソートした結果としてあり得るリストのリストを返す
-- | エースとジョーカーがある場合はソート結果が２種類以上となる
sortCardNum :: List CardNum -> List (List CardNum)
sortCardNum = map (expAceJokerElm >>> sort)

-- | カードの順序判定関数
cardOrdering :: Card -> Card -> Ordering
cardOrdering = comparing snd

-- | カードのリストをソートした結果としてあり得るリストのリストを返す
-- | エースとジョーカーがある場合はソート結果が２種類以上となる
sortCard :: List Card -> List (List Card)
sortCard ls = sortBy cardOrdering <$> expCard ls

-- | エースと別名の１を入れ替える　他の番号はそのまま返す
flipAce :: CardNum -> CardNum
flipAce Ace = One
flipAce One = Ace
flipAce cardNum = cardNum

-- | エースを考慮して、カードの番号をあり得る可能性に展開する
expAceElm :: CardNum -> List CardNum
expAceElm cardNum = (identity : flipAce: Nil) <*> singleton cardNum

-- | エースを考慮して、カードの番号のリストをあり得る可能性に展開する
-- | 例　(A : 2 : 3 : 4 : 5 : Nil) -> ((A : 2 : 3 : 4 : 5 : Nil) : (1 : 2 : 3 : 4 : 5 : Nil) : Nil)
expAce :: List CardNum -> List (List CardNum)
expAce = map expAceElm >>> transpose >>> nub

-- | ジョーカーを考慮して、カードの番号をあり得る可能性に展開する
expJokerElm :: CardNum -> List CardNum
expJokerElm Joker = normalCardNums
expJokerElm cardNum = replicate cardinalityCardNum cardNum

-- | ジョーカーを考慮して、カードの番号のリストをあり得る可能性に展開する
-- | 例　(Joker : 2 : 3 : 4 : 5 : Nil) -> ((A : 2 : 3 : 4 : 5 : Nil) : (2 : 2 : 3 : 4 : 5 : Nil) : ... : (K : 2 : 3 : 4 : 5 : Nil) : Nil)
expJoker :: List CardNum -> List (List CardNum)
expJoker = map expJokerElm >>> transpose >>> nub

-- | エースとジョーカーを考慮して、カードの番号をあり得る可能性に展開する
expAceJokerElm :: CardNum -> List CardNum
expAceJokerElm = expJokerElm >=> expAceElm

-- | エースとジョーカーを考慮して、カードの番号のリストをあり得る可能性に展開する
expAceJoker :: List CardNum -> List (List CardNum)
expAceJoker = map expAceJokerElm >>> transpose >>> nub

-- | エースとジョーカーを考慮して、スートをあり得る可能性に展開する
expStJokerElm :: Suit -> List Suit
expStJokerElm StJoker = suits
expStJokerElm suit = replicate cardinalitySuit suit

-- | エースとジョーカーを考慮して、カードをあり得る可能性に展開する
expCardElm :: Card -> List Card
expCardElm card = (expSuit >=> expCardNum) card where
  expSuit c = zip (expStJokerElm $ fst c) (replicate cardinalitySuit $ snd c)
  expCardNum c = zip (replicate (cardinalityCardNum * length (Ace : One : Nil)) $ fst c) (expAceJokerElm $ snd c)

-- | エースとジョーカーを考慮して、カードのリストをあり得る可能性に展開する
expCard :: List Card -> List (List Card)
expCard = map expCardElm >>> transpose >>> nub

-- | サンプル手札
sample1 :: List Card
sample1 = sampleTwoPairHand
