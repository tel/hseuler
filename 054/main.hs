
import Text.ParserCombinators.Parsec


-- Cards

data Suit = Heart | Club | Diamond | Spade deriving (Eq, Show)

instance Ord Suit where
    -- in poker, suit is inconsequential
    compare _ _ = EQ

data Rank = Rint Int | J | Q | K | A deriving (Show, Eq, Ord)

data Card = Card Rank Suit deriving (Show, Eq, Ord)

mkcard r@(Rint n) s | n < 2 || n > 10 = error $ "Invalid rank: " ++ (show r)
                    | otherwise = Card r s
mkcard r s = Card r s

newtype Hand = MkHand [Card] deriving (Eq)



-- Hands

data HandRank = High Card 
              | Pair Card
              | TwoPair Card
              | Triple Card
              | Straight Card -- highest end of the straight
              | Flush -- ALT: maybe the highest flushed card beats high card
              | House Card -- higher set
              | Quad Card
              | StFlush Card
              | Royal
                deriving (Show, Eq, Ord)

measure :: Hand -> (HandRank, Card) -- Rank and high card
measure = undefined


instance Ord Hand where
    compare a b = let (ar, ahigh) = measure a
                      (br, bhigh) = measure b
                  in 
                    if ar == br 
                    then compare ahigh bhigh
                    else compare ar br



-- Reading the file

number :: Parser Int
number = read `fmap` many1 digit

parseSuit = heart <|> club <|> diamond <|> spade
    where heart   = char 'H' >> return Heart
          club    = char 'C' >> return Club
          diamond = char 'D' >> return Diamond
          spade   = char 'S' >> return Spade

parseRank = (Rint `fmap` number) 
            <|> (char 'J' >> return J)
            <|> (char 'Q' >> return Q)
            <|> (char 'K' >> return K)
            <|> (char 'A' >> return A)

parseCard = do many space
               r <- parseRank
               s <- parseSuit
               return (mkcard r s)

parseHand = count 5 parseCard
parseLine = do a <- parseHand
               b <- parseHand
               (char '\n' >> return ()) <|> eof
               return (a,b)

parseFile = many1 parseLine