import           Lib

import           Test.Hspec

n :: Int
n = 0

main :: IO ()
main = hspec $ describe "Lib" $ do
    it "can handle >" $ do
        moveRight (Band [n .. 2] 3 [4 .. 6]) `shouldBe` Band [0 .. 3] 4 [5 .. 6]
        moveRight (Band [] 1 []) `shouldBe` Band [1] n []
    it "can handle <" $ do
        moveLeft (Band [n .. 2] 3 [4 .. 6]) `shouldBe` Band [0 .. 1] 2 [3 .. 6]
        moveLeft (Band [] 1 []) `shouldBe` Band [] n [1]
    it "can handle +" $ do
        increase (Band [] n []) `shouldBe` Band [] 1 []
        increase (Band [n .. 3] 123 [-1 .. 0])
            `shouldBe` Band [0 .. 3] 124 [-1 .. 0]
        increase (Band [n .. 3] (-123) [-1 .. 0])
            `shouldBe` Band [0 .. 3] (-122) [-1 .. 0]
    it "can handle -" $ do
        decrease (Band [] n []) `shouldBe` Band [] (-1) []
        decrease (Band [n .. 3] (-123) [-1 .. 0])
            `shouldBe` Band [0 .. 3] (-124) [-1 .. 0]
