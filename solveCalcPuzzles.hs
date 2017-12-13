import Test.Hspec
import Data.Char
import Data.List
import Data.Maybe

data Operation = Noop
               | Plus Int
               | Minus Int
               | Times Int
               | DivBy Int
               | Append Int
               | Backspace
               | ShiftLeft
               | ShiftRight
               | SumDigits
               | Reverse
               | Mirror
               | Inv10
               | Replace String String
               | OpMod Operation
               deriving (Show, Eq)

type Context = (Maybe Int, [Operation], [Operation])
type Solution = [Operation]

applyOp :: Operation -> Int -> Maybe Int
applyOp (Plus n)          c = Just (c + n)
applyOp (Minus n)         c = Just (c - n)
applyOp (Times n)         c = Just (c * n)
applyOp (DivBy n)         c | c `mod` n == 0 = Just (c `div` n)
                            | otherwise  = Nothing
applyOp (Append n)        c = Just (append n c)
applyOp (Backspace)       c = Just (c `div` 10)
applyOp (ShiftLeft)       c = Just (shiftLeft c)
applyOp (ShiftRight)      c = Just (shiftRight c)
applyOp (SumDigits)       c = Just (sumDigits c)
applyOp (Reverse)         c = Just (revDigits c)
applyOp (Mirror)          c = Just (mirror c)
applyOp (Inv10)           c = Just (inv10 c)
applyOp (Replace from to) c = Just (read (replace from to (show c)))
applyOp (OpMod _)         c = Just c

apply :: Operation -> Context -> Context

apply _ c@(Nothing, _, _) = c
apply op@(OpMod f) (Just crt, applied, available) = (Just crt, op:applied, map (opMod f) available)
apply op (Just crt, applied, available) = (applyOp op crt, op:applied, available)

opMod :: Operation -> Operation -> Operation
opMod op (Plus n)    = Plus (fromMaybe 0 (applyOp op n))
opMod op (Minus n)   = Minus (fromMaybe 0 (applyOp op n))
opMod op (Times n)   = Times (fromMaybe 0 (applyOp op n))
opMod op (DivBy n)   = DivBy (fromMaybe 0 (applyOp op n))
opMod op (Append n)  = Append (fromMaybe 0 (applyOp op n))
opMod _  o@(OpMod x) = o


shiftLeft :: Int -> Int
shiftLeft n | n < 0     = -shiftLeft(-n)
            | otherwise = read(tail n' ++ [head n'])
            where n' = show n

shiftRight :: Int -> Int
shiftRight n | n < 0     = -shiftRight(-n)
             | otherwise = read(last n' : init n')
             where n' = show n

sumDigits :: Int -> Int
sumDigits n | n < 0     = -sumDigits (-n)
            | otherwise = sum $ (map read) $ map (:[]) $ show n

inv10 :: Int -> Int
inv10 n | n < 0     = -inv10 (-n)
        | otherwise = read $ map invert $ show n
        where invert digit = chr . (+48) $ (`mod` 10) $ 10 - ((ord digit) - 48)


mirror :: Int -> Int
mirror n | n < 0     = -mirror (-n)
         | otherwise = read (n' ++ reverse n')
         where n' = show n

revDigits :: Int -> Int
revDigits n | n < 0     = -revDigits (-n)
            | otherwise = (read . reverse . show) n


append :: Int -> Int -> Int
append x y = read (show y ++ (show x))

replace :: String -> String -> String -> String
replace _ _ [] = []
replace a b l@(x:xs) | a `isPrefixOf` l = b ++ (replace a b $ drop (length a) l)
                     | otherwise        = x : replace a b xs


wormhole :: Int -> Int -> Int -> Int
wormhole from to x | from < l  = base + add
                   | otherwise = x
  where s = show x
        l = length s
        revstr = reverse s
        (a, b) = splitAt from revstr
        base   = read $ reverse a
        add    = (read b) * 10^to




solve :: Int -> Int -> Context -> Maybe Solution
solve _ _ c@(Nothing, _, _) = Nothing
solve _ (-1) _                 = Nothing
solve goal moves c@(Just crt, appliedOps, availableOps)
        | goal == crt = Just appliedOps
        | otherwise   = fromMaybe Nothing $ find isJust solutions
        where solutions = [solve goal (moves -1) (apply op c)
                             | op <- availableOps]

solveGames = do
  -- print $ fmap reverse $ solve 33 4 (Just 17, [], [Times 2, Minus 4, Mirror, ShiftLeft])
  -- print $ fmap reverse $ solve 13 6 (Just 2152, [], [ Replace "25" "12",
  --                                                     Replace "21" "3",
  --                                                     Replace "12" "5",
  --                                                     ShiftRight,
  --                                                     Reverse])
  -- print $ fmap reverse $ solve 121 4 (Just 356, [], [Minus 2, DivBy 3, ShiftRight])
  -- print $ fmap reverse $ solve 5 5 (Just 0, [], [Plus 1])
  -- print $ fmap reverse $ solve 101 5 (Just 0, [], [Append 2, Plus 5, OpMod(Plus 2)])
  -- print $ fmap reverse $ solve 13 7 (Just 15, [], [SumDigits, Mirror, Inv10])
  -- print $ fmap reverse $ solve 33 5 (Just 5, [], [Times 7, Plus 8, Plus (-9), Times 2, Inv10])
  -- print $ fmap reverse $ solve 48 6 (Just 51, [], [Plus 6, Times 3, Inv10, Reverse, Replace "4" "6"])
  -- print $ fmap reverse $ solve 1 6 (Just 0, [], [Plus 5, Times 3, DivBy 6, Inv10, Reverse])
  print $ fmap reverse $ solve 777 5 (Just 369, [], [Inv10, Replace "93" "63",
                                                            Replace "63" "33",
                                                            Replace "36" "93",
                                                            Replace "39" "33"
                                                    ])

tests = hspec $ do
  describe "Calculator solver" $ do

    it "can apply operations" $ do
      applyOp (Plus 10) 5 `shouldBe` (Just 15)

    describe "operations" $ do

      it "can shift numbers" $ do
        shiftLeft 1234 `shouldBe` 2341
        shiftLeft (-1234) `shouldBe` (-2341)
        shiftRight 1234 `shouldBe` 4123
        shiftRight (-1234) `shouldBe` (-4123)

      it "can sum digits" $ do
        sumDigits 456 `shouldBe` 15
        sumDigits (-456) `shouldBe` (-15)

      it "can mirror digits" $ do
        mirror 10 `shouldBe` 1001
        mirror (-10) `shouldBe` (-1001)

      it "can do inv10" $ do
        inv10 (-10) `shouldBe` (-90)

      it "can reverse digits" $ do
        revDigits 1234 `shouldBe` 4321
        revDigits (-1234) `shouldBe` (-4321)

      it "can append digits" $ do
        append 13 100 `shouldBe` 10013

      it "can replace" $
        replace "test" "xyz" "this test is a test" `shouldBe` "this xyz is a xyz"

      it "can do opmods" $
        opMod (Plus 2) (Append 2) `shouldBe` (Append 4)
        opMod (Plus 2) (Plus 5) `shouldBe` (Plus 7)

      it "can apply opmods on existing ops" $
        let (value, done, available) = apply (OpMod (Plus 2)) (Just 1, [], [Append 2])
        value     `shouldBe` Just 1
        done      `shouldBe` [OpMod (Plus 2)]
        available `shouldBe` [Append 4]

    it "implements wormhole transformations" $ do
      -- wormhole moves a char over anoher at a given index using addition
      wormhole 3 0 3213 `shouldBe` 216
      wormhole 3 1 3213 `shouldBe` 243
      -- wormhole does nothing if number not long enough
      wormhole 3 0 123 `shouldBe` 123




main = do
  tests
  solveGames


