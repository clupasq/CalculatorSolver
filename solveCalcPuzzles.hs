import Test.Hspec

import Text.Regex (mkRegex, subRegex)
import Data.Maybe (isJust)

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
                   value        :: Int,
                   movesLeft    :: Int,
                   ops          :: [Operation],
                   transformer  :: Transformer
                 }
               | InvalidGameState
  deriving (Eq, Show)


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

opsAfterStoring :: Int -> [Operation] -> [Operation]
opsAfterStoring v = (Retrieve v :) . map inactivateStore . filter nonRetrieve
  where nonRetrieve (Retrieve _) = False
        nonRetrieve           _  = True
        inactivateStore Store    = StoreInactive
        inactivateStore      op  = op

reactivateStore :: [Operation] -> [Operation]
reactivateStore = map reactivate
  where reactivate StoreInactive = Store
        reactivate             x = x

{-
- Wormhole is a transformation applied after each operation.
- It removes the digit at index `from` and adds it over the
- digit at index `to`.
- All indices refer to positions from right to left (e.g.
- index 1 would be the next-to-last digit)
- This transformation gets applied repeatedly, until there is
- no other digit to move at index `from`.
-}
wormhole :: Int -> Int -> Int -> Int
wormhole from to x | from < l  = wormhole from to $ base + add
                   | otherwise = x
  where s = show x
        l = length s
        revstr = reverse s
        prefix = take from revstr
        suffix = drop (from+1) revstr
        digit  = [revstr !! from]
        base   = read $ reverse (prefix++suffix)
        add    = (read digit) * 10^to

transform :: GameState -> GameState
transform InvalidGameState = InvalidGameState
transform g                = case transformer g of
                               (Wormhole f t) -> g { value = wormhole f t (value g) }
                               None           -> g

apply :: Operation -> GameState -> GameState
apply op g =
  transform $ case op of
                Plus  x     -> g' { value = v + x }
                Minus x     -> g' { value = v - x }
                Times x     -> g' { value = v * x }
                DivBy x     -> if v `mod` x == 0
                                  then g' { value = v `div` x }
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
                Store       -> g  { ops   = opsAfterStoring v (ops g)}
                Retrieve x  -> g' { value = append x v, ops = reactivateStore (ops g) }
  where v = value g
        g' = g { movesLeft = movesLeft g - 1 }


possibleOps :: GameState -> [Operation]
possibleOps g = filter (/= StoreInactive) (ops g)

findValid :: [Maybe a] -> Maybe a
findValid (x:xs) | isJust x  = x
                 | otherwise = findValid xs
findValid     [] = Nothing

solve :: Int -> GameState -> [Operation] -> Maybe [Operation]
solve target g formerSteps
  | target == value g = Just $ reverse formerSteps
  | movesLeft g == 0  = Nothing
  | otherwise         = findValid solutions
  where solutions = [ solve target (apply op g) (op:formerSteps) | op <- possibleOps g ]



sampleGame = GameState { value = 0,
                         movesLeft = 5,
                         ops = [],
                         transformer = None }

tests = hspec $ do

  describe "Operations" $ do

    it "can do simple arithmetic" $ do
      let game = sampleGame { value = 5 }
      value (apply (Plus 3) game) `shouldBe` 8
      value (apply (Plus 5) game) `shouldBe` 10
      value (apply (Minus 5) game) `shouldBe` (0)
      value (apply (Minus 7) game) `shouldBe` (-2)
      value (apply (Times 7) game) `shouldBe` 35
      movesLeft (apply (Times 7) game) `shouldBe` 4

    it "divides when possible" $ do
      let game = sampleGame { value = 35 }
      value (apply (DivBy 7) game) `shouldBe` 5
      value (apply (DivBy 5) game) `shouldBe` 7
      movesLeft (apply (DivBy 5) game) `shouldBe` 4

    it "disallows division when not evenly divisible" $ do
      let game = sampleGame { value = 35 }
      apply (DivBy 6) game `shouldBe` InvalidGameState

    it "can append digits to value" $ do
      let game = sampleGame { value = 35 }
      value (apply (Append 0) game) `shouldBe` 350
      value (apply (Append 10) game) `shouldBe` 3510
      movesLeft (apply (Append 10) game) `shouldBe` 4

    it "can backspace" $ do
      let game = sampleGame { value = 35 }
      value (apply BackSpace game) `shouldBe` 3
      let game = sampleGame { value = 12345 }
      value (apply BackSpace game) `shouldBe` 1234
      let game = sampleGame { value = 3 }
      value (apply BackSpace game) `shouldBe` 0
      movesLeft (apply BackSpace game) `shouldBe` 4

    it "can sum digits" $ do
      let game = sampleGame { value = 35 }
      value (apply SumDigits game) `shouldBe` 8
      let game = sampleGame { value = 12345 }
      value (apply SumDigits game) `shouldBe` 15
      let game = sampleGame { value = -12345 }
      value (apply SumDigits game) `shouldBe` -15
      movesLeft (apply SumDigits game) `shouldBe` 4

    it "reverse digits" $ do
      let game = sampleGame { value = 12345 }
      value (apply Reverse game) `shouldBe` 54321
      let game = sampleGame { value = -12345 }
      value (apply Reverse game) `shouldBe` -54321
      movesLeft (apply Reverse game) `shouldBe` 4

    it "mirrors digits" $ do
      let game = sampleGame { value = 123 }
      value (apply Mirror game) `shouldBe` 123321
      let game = sampleGame { value = -123 }
      value (apply Mirror game) `shouldBe` -123321
      movesLeft (apply Mirror game) `shouldBe` 4

    it "can inv10 digits" $ do
      let game = sampleGame { value = 123 }
      value (apply Inv10 game) `shouldBe` 987
      let game = sampleGame { value = 941 }
      value (apply Inv10 game) `shouldBe` 169
      let game = sampleGame { value = -123 }
      value (apply Inv10 game) `shouldBe` -987
      movesLeft (apply Inv10 game) `shouldBe` 4

    it "can rotate left" $ do
      let game = sampleGame { value = 123 }
      value (apply RotateLeft game) `shouldBe` 231
      let game = sampleGame { value = 10 }
      value (apply RotateLeft game) `shouldBe` 1
      movesLeft (apply RotateLeft game) `shouldBe` 4

    it "can rotate right" $ do
      let game = sampleGame { value = 123 }
      value (apply RotateRight game) `shouldBe` 312
      let game = sampleGame { value = 10 }
      value (apply RotateRight game) `shouldBe` 1
      movesLeft (apply RotateRight game) `shouldBe` 4

    describe "replacements" $ do
      it "replaces substrings if found" $ do
        let game = sampleGame { value = 12345 }
        value (apply (Replace "23" "99") game) `shouldBe` 19945
        let game = sampleGame { value = 123123 }
        value (apply (Replace "12" "99") game) `shouldBe` 993993
        movesLeft (apply (Replace "12" "99") game) `shouldBe` 4

      it "leaves untouched if not found" $ do
        let game = sampleGame { value = 1234 }
        value (apply (Replace "13" "99") game) `shouldBe` 1234
        movesLeft (apply (Replace "13" "99") game) `shouldBe` 4

    describe "Operation Modifier" $ do
      it "modifies Operations that have an Int param" $ do
        let game = sampleGame { ops = [Plus 5, Minus 3, Append 5, Mirror] }
        ops (apply (OpMod $ Plus 3) game) `shouldBe`
                                      [Plus 8, Minus 6, Append 8, Mirror]
        ops (apply (OpMod $ Minus 2) game) `shouldBe`
                                      [Plus 3, Minus 1, Append 3, Mirror]
        movesLeft (apply (OpMod $ Minus 2) game) `shouldBe` 4

      it "Leaves other operations unchanged" $ do
        let initialOps = [ Inv10, Reverse, Mirror, OpMod (Plus 3)]
        let game = sampleGame { ops = initialOps }
        ops (apply (OpMod $ Plus 3) game) `shouldBe` initialOps


    describe "Store/Retrieve operations" $ do

      describe "Storing values" $ do

        it "value stays the same after storing" $ do
          let game = sampleGame { ops = [Store], value = 5 }
          value (apply Store game) `shouldBe` 5

        it "no moves were consumed" $ do
          let game = sampleGame { movesLeft = 5 }
          movesLeft (apply Store game) `shouldBe` 5

        it "adds retrieve to the op list with current value" $ do
          let game = sampleGame { value = 123, ops = [] }
          ops (apply Store game) `shouldContain` [Retrieve 123]

        it "previous Retrieve operation should be replaced" $ do
          let game = sampleGame { value = 123, ops = [ Retrieve 10 ] }
          ops (apply Store game) `shouldContain` [Retrieve 123]
          ops (apply Store game) `shouldNotContain` [Retrieve 10]

        it "after Storing, the Store option becomes inactive \
            \ (storing same value twice would be useless)" $ do
          let game = sampleGame { value = 123, ops = [ Store ] }
          ops (apply Store game) `shouldContain` [ StoreInactive ]
          ops (apply Store game) `shouldNotContain` [ Store ]

      describe "Retrieval" $ do

        it "appends retrieved value to the right" $ do
          let game = sampleGame { value = 123 }
          value (apply (Retrieve 45) game) `shouldBe` 12345

        it "reenables Storage" $ do
          let game = sampleGame { ops = [ StoreInactive ] }
          ops (apply (Retrieve 45) game) `shouldContain`    [Store]
          ops (apply (Retrieve 45) game) `shouldNotContain` [StoreInactive]

    describe "Transformations" $ do

      it "applies transforms after each operation" $ do
        let game = sampleGame { value = 12344, transformer = Wormhole 2 0 }
        value (apply (Plus 1) game) `shouldBe` 51


      describe "wormhole" $ do

        it "moves a char over anoher at a given index using addition" $ do
          wormhole 3 0 3213 `shouldBe` 216
          wormhole 3 1 3213 `shouldBe` 243
          wormhole 4 0 30027 `shouldBe` 30

        it "does nothing if number not long enough" $ do
          wormhole 3 0 123 `shouldBe` 123

        it "applies repeatedly until to transformation can be done" $ do
          {-
          -   v ^
          - 12345  -> 1248
          -
          -   v ^
          -  1248  -> 150
          -
          -   v ^
          -   150  -> 51
          -
          - -}
          wormhole 2 0 12345 `shouldBe` 51

    describe "Solving" $ do

      it "detects if no solutions available" $ do
        let game = sampleGame { value = 0,
                                movesLeft = 3,
                                ops = [Plus 1] }
        solve 99 game [] `shouldBe` Nothing


      it "can find simple solutions" $ do
        let game = sampleGame { value = 0,
                                movesLeft = 3,
                                ops = [Plus 1] }
        solve 3 game [] `shouldBe` Just [ Plus 1, Plus 1, Plus 1]

      it "can find more complex solutions" $ do
        let game = sampleGame { value = 5,
                                movesLeft = 5,
                                ops = [Times 7, Plus 8, Minus 9, Times 2, Inv10] }
        solve 33 game [] `shouldBe` Just [Times 7,Plus 8,Plus 8,Minus 9,Minus 9]



solveLevels = do

  let level188 = GameState {
        movesLeft = 5,
        value = 25,
        ops = [Mirror, Store, BackSpace, Append 5],
        transformer = Wormhole 3 1
      }
  print $ solve 822 level188 []

  let level189 = GameState {
        movesLeft = 4,
        value = 45,
        ops = [Plus 10, Mirror, Reverse],
        transformer = Wormhole 3 1
      }
  print $ solve 516 level189 []

  let level190 = GameState {
        movesLeft = 4,
        value = 238,
        ops = [Minus 5, Inv10, RotateRight, Replace "28" "21"],
        transformer = None
      }
  print $ solve 212 level190 []

  let level192 = GameState {
        movesLeft = 6,
        value = 189,
        ops = [Plus 8, Times 4, Append 9, Inv10, Replace "7" "0"],
        transformer = Wormhole 3 0
      }
  print $ solve 500 level192 []

  let level193 = GameState {
        movesLeft = 4,
        value = 234,
        ops = [Append 9, Plus 9, Replace "53" "32"],
        transformer = Wormhole 3 0
      }
  print $ solve 321 level193 []

  -- let level194 = GameState {
  --       movesLeft = 4,
  --       value = 333,
  --       ops = [Append 1, Append 3, DivBy 2, OpMod (Plus 1)],
  --       transformer = Wormhole 3 0
  --     }
  -- print $ solve 123 level194 []

  let level195 = GameState {
        movesLeft = 5,
        value = 613,
        ops = [Append 5, Times 2, Plus 3, Reverse, Inv10],
        transformer = Wormhole 3 0
      }
  print $ solve 777 level195 []

  let level196 = GameState {
        movesLeft = 7,
        value = 60,
        ops = [Plus 5, Times 5, Append 2, Inv10],
        transformer = Wormhole 3 1
      }
  print $ solve 550 level196 []

  let level197 = GameState {
        movesLeft = 5,
        value = 1234,
        ops = [Replace "24" "13",
               Replace "12" "32",
               Replace "23" "32",
               Replace "13" "21",
               Replace "23" "43" ],
        transformer = None
      }
  print $ solve 4321 level197 []

  let level198 = GameState {
        movesLeft = 7,
        value = 4,
        ops = [Plus 6, Append 4, Times 3, Inv10],
        transformer = Wormhole 3 1
      }
  print $ solve 750 level198 []

  -- no solution ?!
  let level199 = GameState {
        movesLeft = 6,
        value = 3002,
        ops = [Append 7, Replace "3" "5", Inv10, RotateRight],
        transformer = Wormhole 4 0
      }
  print $ solve 3507 level199 []

main = do
  tests
  solveLevels
