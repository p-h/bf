import           Lib

import           Test.Hspec

n :: Int
n = 0

main :: IO ()
main = hspec $ describe "Lib" $ do
    it "can handle >" $ do
        moveRight (Band [n .. 2] [3 .. 6]) `shouldBe` Band [0 .. 3] [4 .. 6]
        moveRight (Band [] [1]) `shouldBe` Band [1] [n]
    it "can handle <" $ do
        moveLeft (Band [n .. 2] [3 .. 6]) `shouldBe` Band [0 .. 1] [2 .. 6]
        moveLeft (Band [] [1]) `shouldBe` Band [] [n, 1]
    it "can handle +" $ do
        increase (Band [] [n]) `shouldBe` Band [] [1]
        increase (Band [n .. 3] (123 : [-1 .. 0]))
            `shouldBe` Band [0 .. 3] (124 : [-1 .. 0])
        increase (Band [n .. 3] ((-123) : [-1 .. 0]))
            `shouldBe` Band [0 .. 3] ((-122) : [-1 .. 0])
    it "can handle -" $ do
        decrease (Band [] [n]) `shouldBe` Band [] [-1]
        decrease (Band [n .. 3] ((-123) : [-1 .. 0]))
            `shouldBe` Band [0 .. 3] ((-124) : [-1 .. 0])
    it "can progress" $ do
        nextInstruction (Band "" "++") `shouldBe` Band "+" "+"
        nextInstruction (Band "++" ".++") `shouldBe` Band "++." "++"
    it "can parse" $ do
        parse "" `shouldBe` []
        parse ">" `shouldBe` [MoveRight]
        parse "[<>+-.,]"
            `shouldBe` [ Loop
                             [ MoveLeft
                             , MoveRight
                             , Increase
                             , Decrease
                             , Output
                             , Input
                             ]
                       ]
    it "handles brackets" $ do
        splitOnMatchingBracket "bar]baz" `shouldBe` ("bar", "baz")
        splitOnMatchingBracket "foo[bar]baz]boo"
            `shouldBe` ("foo[bar]baz", "boo")

    it "can run simple programs" $ do
        let code = "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.---.+++++++..+++."
            in run code "" `shouldBe` "hello"
        let code = "+++++++++++++++++++++++++++++++++>+++[<.>-]"
            in run code "" `shouldBe` "!!!"
        let code = ",>,>,>,>,<<<<.>.>.>.>."
            inp = "hello"
            in run code inp `shouldBe `"hello"
