module StateSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Control.Monad.State
import Prelude hiding (interact)

import qualified State
import State (TurnstileState (..), TurnstileOutput (..), Action (..))

testCoin :: (TurnstileState -> (TurnstileOutput, TurnstileState)) -> Rubric
testCoin coin = passOrFail $ do
  it "opens and thanks" $ do
    coin Locked   @?= (Thank, Unlocked)
    coin Unlocked @?= (Thank, Unlocked)

testPush :: (TurnstileState -> (TurnstileOutput, TurnstileState)) -> Rubric
testPush push = passOrFail $ do
  it "opens only when payed, otherwise tuts" $ do
    push Locked   @?= (Tut,  Locked)
    push Unlocked @?= (Open, Locked)

testInteract :: (TurnstileState -> Action -> (TurnstileOutput, TurnstileState)) -> Rubric
testInteract interact = passOrFail $ do
  it "correctly interacts with actions" $ do
    interact Locked   Coin @?= (Thank, Unlocked)
    interact Unlocked Coin @?= (Thank, Unlocked)
    interact Locked   Push @?= (Tut,   Locked)
    interact Unlocked Push @?= (Open,  Locked)

tests :: Rubric
tests = distribute $ do
  let unwrap m st i = runState (m i) st
  distributed "coin"      $ testCoin     $ State.coin
  distributed "push"      $ testPush     $ State.push
  distributed "interact"  $ testInteract $ State.interact
  distributed "coin'"     $ testCoin     $ runState State.coin'
  distributed "push'"     $ testPush     $ runState State.push'
  distributed "interact'" $ testInteract $ unwrap State.interact'
