module StateSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import Control.Monad.State
import Prelude hiding (interact)

import qualified State
import State (TurnstileState (..), TurnstileOutput (..), Action (..))

testCoin :: (TurnstileState -> (TurnstileOutput, TurnstileState)) -> Spec
testCoin coin =
  it "opens and thanks" $ do
    coin Locked   @?= (Thank, Unlocked)
    coin Unlocked @?= (Thank, Unlocked)

testPush :: (TurnstileState -> (TurnstileOutput, TurnstileState)) -> Spec
testPush push =
  it "opens only when payed, otherwise tuts" $ do
    push Locked   @?= (Tut,  Locked)
    push Unlocked @?= (Open, Locked)

testInteract :: (TurnstileState -> Action -> (TurnstileOutput, TurnstileState)) -> Spec
testInteract interact =
  it "correctly interacts with actions" $ do
    interact Locked   Coin @?= (Thank, Unlocked)
    interact Unlocked Coin @?= (Thank, Unlocked)
    interact Locked   Push @?= (Tut,   Locked)
    interact Unlocked Push @?= (Open,  Locked)

tests :: Spec
tests = do
  let unwrap m st i = runState (m i) st
  describe "coin"      $ testCoin     $ State.coin
  describe "push"      $ testPush     $ State.push
  describe "interact"  $ testInteract $ State.interact
  describe "coin'"     $ testCoin     $ runState State.coin'
  describe "push'"     $ testPush     $ runState State.push'
  describe "interact'" $ testInteract $ unwrap State.interact'
