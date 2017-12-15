import Test.Hspec

import Text.Regex (mkRegex, subRegex)

data Operation = Plus Int
               | Minus Int
               | Times Int
               | DivBy Int
               | Append Int
               | BackSpace
               | SumDigits
               | Reverse
               | Mirror
               | Inv10
               | RotateLeft
               | RotateRight
               | Replace String String
               | OpMod Operation
               | Store
               | StoreInactive
               | Retrieve Int
  deriving (Eq, Show)

data Transformer = Wormhole Int Int | None
  deriving (Eq, Show)

data GameState = GameState {
                   target       :: Int,
                   value        :: Int,
                   movesLeft    :: Int,
                   steps        :: [Operation],
                   ops          :: [Operation],
                   transformer  :: Transformer
                 }
               | InvalidGameState
  deriving (Eq, Show)


mkGame :: Int -> Int -> Int -> [Operation] -> GameState
mkGame target value movesLeft ops =
  GameState { target      = target,
              value       = value,
              ops         = ops,
              movesLeft   = movesLeft,
              transformer = None,
              steps       = []
            }

append :: Int -> Int -> Int
append suffix value = read $ (show value) ++ (show suffix)

backspace :: Int -> Int
backspace 0 = 0
backspace n | abs n < 10 = 0
            | otherwise  = read $ init $ show n

applyPosNegOp :: (Int -> Int) -> Int -> Int
applyPosNegOp op v | v < 0     = negate $ op (negate v)
                   | otherwise = op v

sumDigits :: Int -> Int
sumDigits = sum . map (read.pure) . show

revDigits :: Int -> Int
revDigits = read . reverse . show

mirror :: Int -> Int
mirror n = read $ str ++ reverse str
  where str = show n

inv10 :: Int -> Int
inv10 = read . map(inv10digit) . show
  where inv10digit c = head $ show $ 10 - (read [c])

rotLeft :: Int -> Int
rotLeft n = read $ xs ++ [x]
  where (x:xs) = show n

rotRight :: Int -> Int
rotRight n = read $ last s : init s
  where s = show n

replace f t = read . repl . show
  where repl s = subRegex (mkRegex f) s t

modOp :: Operation -> Operation -> Operation
modOp operation operand =
  case operand of
       Plus   x -> Plus   (op x)
       Minus  x -> Minus  (op x)
       Times  x -> Times  (op x)
       DivBy  x -> DivBy  (op x)
       Append x -> Append (op x)
       o        -> o
  where op = case operation of
                  Plus y  -> (+y)
                  Minus y -> (+negate y)

apply :: Operation -> GameState -> GameState
apply op g =
  case op of
       Plus  x     -> g' { value = v + x }
       Minus x     -> g' { value = v - x }
       Times x     -> g' { value = v * x }
       DivBy x     -> if v `mod` x == 0
                         then g { value = v `div` x }
                         else InvalidGameState
       Append x    -> g' { value = append x v }
       BackSpace   -> g' { value = backspace v }
       SumDigits   -> g' { value = applyPosNegOp sumDigits v }
       Reverse     -> g' { value = applyPosNegOp revDigits v }
       Mirror      -> g' { value = applyPosNegOp mirror v }
       Inv10       -> g' { value = applyPosNegOp inv10 v }
       RotateLeft  -> g' { value = applyPosNegOp rotLeft v }
       RotateRight -> g' { value = applyPosNegOp rotRight v }
       Replace f t -> g' { value = replace f t v }
       OpMod op    -> g' { ops   = map (modOp op) (ops g) }

  where v = value g
        g' = g { movesLeft = movesLeft g - 1 }




sampleGame = mkGame 0 0 5 []

tests = hspec $ do

  describe "Operations" $ do

    it "can do simple arithmetic" $ do
      let game = sampleGame { value = 5 }
      value (apply (Plus 3) game) `shouldBe` 8
      value (apply (Plus 5) game) `shouldBe` 10
      value (apply (Minus 5) game) `shouldBe` (0)
      value (apply (Minus 7) game) `shouldBe` (-2)
      value (apply (Times 7) game) `shouldBe` 35

    it "divides when possible" $ do
      let game = sampleGame { value = 35 }
      value (apply (DivBy 7) game) `shouldBe` 5
      value (apply (DivBy 5) game) `shouldBe` 7

    it "disallows division when not evenly divisible" $ do
      let game = sampleGame { value = 35 }
      apply (DivBy 6) game `shouldBe` InvalidGameState

    it "can append digits to value" $ do
      let game = sampleGame { value = 35 }
      value (apply (Append 0) game) `shouldBe` 350
      value (apply (Append 10) game) `shouldBe` 3510

    it "can backspace" $ do
      let game = sampleGame { value = 35 }
      value (apply BackSpace game) `shouldBe` 3
      let game = sampleGame { value = 12345 }
      value (apply BackSpace game) `shouldBe` 1234
      let game = sampleGame { value = 3 }
      value (apply BackSpace game) `shouldBe` 0

    it "can sum digits" $ do
      let game = sampleGame { value = 35 }
      value (apply SumDigits game) `shouldBe` 8
      let game = sampleGame { value = 12345 }
      value (apply SumDigits game) `shouldBe` 15
      let game = sampleGame { value = -12345 }
      value (apply SumDigits game) `shouldBe` -15

    it "reverse digits" $ do
      let game = sampleGame { value = 12345 }
      value (apply Reverse game) `shouldBe` 54321
      let game = sampleGame { value = -12345 }
      value (apply Reverse game) `shouldBe` -54321

    it "mirrors digits" $ do
      let game = sampleGame { value = 123 }
      value (apply Mirror game) `shouldBe` 123321
      let game = sampleGame { value = -123 }
      value (apply Mirror game) `shouldBe` -123321

    it "can inv10 digits" $ do
      let game = sampleGame { value = 123 }
      value (apply Inv10 game) `shouldBe` 987
      let game = sampleGame { value = 941 }
      value (apply Inv10 game) `shouldBe` 169
      let game = sampleGame { value = -123 }
      value (apply Inv10 game) `shouldBe` -987

    it "can rotate left" $ do
      let game = sampleGame { value = 123 }
      value (apply RotateLeft game) `shouldBe` 231
      let game = sampleGame { value = 10 }
      value (apply RotateLeft game) `shouldBe` 1

    it "can rotate right" $ do
      let game = sampleGame { value = 123 }
      value (apply RotateRight game) `shouldBe` 312
      let game = sampleGame { value = 10 }
      value (apply RotateRight game) `shouldBe` 1

    describe "replacements" $ do
      it "replaces substrings if found" $ do
        let game = sampleGame { value = 12345 }
        value (apply (Replace "23" "99") game) `shouldBe` 19945
        let game = sampleGame { value = 123123 }
        value (apply (Replace "12" "99") game) `shouldBe` 993993

      it "leaves untouched if not found" $ do
        let game = sampleGame { value = 1234 }
        value (apply (Replace "13" "99") game) `shouldBe` 1234

    describe "Operation Modifier" $ do
      it "modifies Operations that have an Int param" $ do
        let game = sampleGame { ops = [Plus 5, Minus 3, Append 5, Mirror] }
        ops (apply (OpMod $ Plus 3) game) `shouldBe`
                                      [Plus 8, Minus 6, Append 8, Mirror]
        ops (apply (OpMod $ Minus 2) game) `shouldBe`
                                      [Plus 3, Minus 1, Append 3, Mirror]




main = do
  tests
