module Block1_1Spec
    (dayTestsTree
    ) where

import           Block1_1
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

dayTestsTree :: IO TestTree
dayTestsTree = testSpec "Week day" dayTests

dayTests :: Spec
dayTests = do
  describe "nextDay" $
    it "next day from day" $ do
      nextDay Monday `shouldBe` Tuesday
      nextDay Tuesday `shouldBe` Wednesday
      nextDay Wednesday `shouldBe` Thursday
      nextDay Thursday `shouldBe` Friday
      nextDay Friday `shouldBe` Saturday
      nextDay Saturday `shouldBe` Sunday
      nextDay Sunday `shouldBe` Monday

  describe "afterDays" $
    it "returns the day after the specified number of days passed" $ do
      afterDays Monday 0 `shouldBe` Monday
      afterDays Tuesday 1 `shouldBe` Wednesday
      afterDays Wednesday 2 `shouldBe` Friday
      afterDays Thursday 3 `shouldBe` Sunday
      afterDays Friday 4 `shouldBe` Tuesday
      afterDays Saturday 5 `shouldBe` Thursday
      afterDays Sunday 6 `shouldBe` Saturday
      afterDays Monday 7 `shouldBe` Monday

  describe "isWeekend" $
    it "is the day of the week a day off" $ do
      isWeekend Monday `shouldBe` False
      isWeekend Tuesday `shouldBe` False
      isWeekend Wednesday `shouldBe` False
      isWeekend Thursday `shouldBe` False
      isWeekend Friday `shouldBe` False
      isWeekend Saturday `shouldBe` True
      isWeekend Sunday `shouldBe` True

  describe "daysToParty" $
    it "displays the number of days remaining until Friday" $ do
      daysToParty Monday `shouldBe` 4
      daysToParty Tuesday `shouldBe` 3
      daysToParty Wednesday `shouldBe` 2
      daysToParty Thursday `shouldBe` 1
      daysToParty Friday `shouldBe` 0
      daysToParty Saturday `shouldBe` 6
      daysToParty Sunday `shouldBe` 5