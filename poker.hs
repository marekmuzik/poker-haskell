
-- definice datovych typu
data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Show)
data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Eq, Show)
data Card = Card Rank Suit deriving (Eq, Show)
type Hand = [Card]
data Category = RoyalFlush
              | StraightFlush
              | Four
              | FullHouse
              | Flush
              | Straight
              | Three
              | TwoPair
              | Pair
              | HighCard deriving (Eq, Show)

-- barva karty (Card Rank Suit)
getCardSuit :: Card -> Suit
getCardSuit (Card _ y) = y

-- vaha karty (Card Rank Suit)
getCardRank :: Card -> Rank
getCardRank (Card x _) = x

-- cislo vahy karty (Card (Numeric x) Suit)
getCardRankNum :: Card -> Int
getCardRankNum (Card (Numeric y) z) = y

-- preved vahu na int cislo
convertRank :: Card -> Int
convertRank x = 
    let rank = getCardRank x
        in case rank of
                    Jack -> 11
                    Queen -> 12
                    King -> 13
                    Ace -> 14
                    otherwise -> getCardRankNum x

-- preved list karet na list int cisel
getCardRankList :: [Card] -> [Int]
getCardRankList = map convertRank

-- tridici algoritmus
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let lp = filter (< x) xs
                       rp = filter (>= x) xs
                   in quicksort lp ++ [x] ++ quicksort rp

-- preved list karet na list setridenych cisel
getSortedCards :: [Card] -> [Int]
getSortedCards x = quicksort (getCardRankList x) 

-- funkce overi jestli jsou karty postupka
isStraight :: [Int] -> Bool
isStraight [] = True
isStraight (x:y:xs)
    | y /= x + 1      = False
    | length xs == 0  = True
    | otherwise       = isStraight (y:xs)

-- funkce pro overeni postupky zacinajici esem
isStraightAce :: Hand -> Bool
isStraightAce x = let cards = getSortedCards x
                  in if isStraight (take 4 cards) && head cards == 2 && last cards == 14 then True else False  


-- funkce pro overeni jestli jsou karty stejneho znaku
isFlush :: Hand -> Bool
isFlush (x:xs)
    | length (filter (\x -> getCardSuit x == fstSuit) xs) == 4 = True
    | otherwise                                                = False
    where fstSuit = getCardSuit x


-- funkce overi jestli jsou karty stejneho znaku a zaroven kralovska postupka (soucet techto karet musi byt vzdy 60)
isRoyalFlush :: Hand -> Bool
isRoyalFlush x
    | isFlush x && sum(getCardRankList x) == 60 = True
    | otherwise                                 = False 

-- funkce overi jestli jsou karty ve stavu: dvojic, trojic nebo ctveric. Vraci list touples (vaha, typ) napr. (14, "Pair")
pairFinder :: [Int] -> [(Int, String)] -> [(Int, String)]
pairFinder (x:xs) list
    | countListComp xs x == 1 = pairFinder xs ((x,"Pair"):list)
    | countListComp xs x == 2 = pairFinder (drop 1 xs) ((x,"Three"):list)
    | countListComp xs x == 3 = pairFinder (drop 2 xs) ((x,"Four"):list)
    | xs == []                = list
    | otherwise               = pairFinder xs list


-- funkce na hledani poctu stejnych elementu v seznamu
countListComp :: Eq a => [a] -> a -> Int 
countListComp ys find = length xs
    where xs = [xs | xs <- ys, xs == find]

-- priradi category k dvojicim, trojicim a ctvericim
isPair :: Hand -> Int
isPair x
    | length pairs == 0                       = -1 --neni to pair
    | fstElem == "Pair" && length pairs == 1  = 0 --Pair
    | fstElem == "Pair" && sndElem == "Pair"  = 1 --TwoPair
    | fstElem == "Pair" && sndElem == "Three" = 2 --FullHouse
    | fstElem == "Three" && sndElem == "Pair" = 2 --FullHouse
    | fstElem == "Three" && length pairs == 1 = 3 --Three
    | fstElem == "Four" && length pairs == 1  = 4 --Four
    where pairs = pairFinder (getSortedCards x) []
          fstElem = snd(head pairs)
          sndElem = snd(last pairs)

-- hlavni funkce prirazujici kategorii
decide :: Hand -> Category
decide x
    | isRoyalFlush x                             = RoyalFlush
    | isStraight (getSortedCards x) && isFlush x = StraightFlush
    | isStraightAce x && isFlush x               = StraightFlush -- StraightFlush s postupkou zacinajici od esa
    | isStraight (getSortedCards x)              = Straight
    | isFlush x                                  = Flush
    | isStraightAce x                            = Straight --Straight postupka zacinajici od esa
    | isPair x == 0                              = Pair
    | isPair x == 1                              = TwoPair
    | isPair x == 2                              = FullHouse
    | isPair x == 3                              = Three
    | isPair x == 4                              = Four 
    | otherwise                                  = HighCard



--dodatecny ukol: funkce soucet karet
--uvazujeme ze je Eso take "obrazek" cili bodova hodnota 10

convertRank' :: Card -> Int
convertRank' x = 
    let rank = getCardRank x
        in case rank of
                    Jack -> 10
                    Queen -> 10
                    King -> 10
                    Ace -> 10
                    otherwise -> getCardRankNum x

getCardRankList' :: [Card] -> [Int]
getCardRankList' = map convertRank'

sumCards :: Hand -> Int
sumCards x = sum (getCardRankList' x)
