module State
  ( coin
  , push
  , interact
  , coin'
  , push'
  , interact'
  , TurnstileOutput (..)
  , TurnstileState (..)
  , Action (..)
  ) where

import Control.Monad.State
import Prelude hiding (interact)

-- The State monad:
-----------------------------------------------------------
-- Many computations require some internal state to be 
-- tracked. Since Haskell doesn't have stateful computation
-- out of the box, we require a different way to express this.
--
-- `State s a` is a monad that encapsulates a computation 
-- which has some state `s` and computes `a`. To elaborate, 
-- `Reader s` is the `m` in monad `m a`.
--
-- State, as Reader and Writer, is kind of a misnomer for
-- the monad. The monad is in fact not a State, but a
-- computation that modifies the State in some way.
-- (try to come back to this sentence later if this doesn't
-- make sense now!)
--
-- There is probably no need to argue why state would
-- be useful as it is so ubiquitous in programming.
-- Just to name some examples though, games have some state
-- of their world, online shopping has state within their
-- shopping carts, etc.
--
-- Consider the following function
-- fn :: State Bool Int
-- fn = ...
--
-- Here, `fn` is a computation that has some state Bool and
-- returns an Int.
-- 
-- To read from the state, one can use `get`. For example,
-- fn :: State Bool Int
-- fn = do
--   bool <- get
--   ...
--
-- `get` places the state as the output of the monad. e.g.
-- get fn :: State Bool Bool
--
-- Then we can monadically bind the environemnt to actually
-- read it:
-- fn' = get fn >>= \bool -> ...
--
-- We do recommend do notation as it reads quite a bit nicer.
--
-- As an aside, notice how `get` is equivalent to `ask` from 
-- the Reader monad (apart from working on different monads).
--
-- Apart from reading from the state, we also need a way to
-- write back to the state. For this, we have `put` which we
-- can use as follows:
-- fn :: State Bool Int
-- fn = do
--   bool <- get
--   ...
--   put True
--   ...
--
-- We can of course compose and evaluate Stateful operations
-- runState (fn >> fn) False
-----------------------------------------------------------

-- A turnstile is either Locked or Unlocked
data TurnstileState
  = Locked
  | Unlocked
  deriving (Eq, Show)

-- A turnstile will:
-- - Thank you when inserting a coin
-- - Open when pushing while it was Unlocked
-- - Tut when pushing while it was Locked
data TurnstileOutput 
  = Thank
  | Open
  | Tut
  deriving (Eq, Show)

-- You can either push the turnstile
-- or insert a coin into it
data Action
  = Push
  | Coin
  deriving (Eq, Show)

-- Insert a coin into the turnstile, setting the
-- state to Unlocked and the output to Thank
--
-- e.g. coin Locked = (Thank, Unlocked)
coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin = undefined

-- Push the turnstile. It will Open if it was
-- Unlocked and set its state to Locked.
-- If it was Locked, it will Tut.
--
-- e.g. push Locked = (Tut, Locked)
push :: TurnstileState -> (TurnstileOutput, TurnstileState)
push = undefined

-- Interact with the turnstile, either
-- pushing or paying it depending on the
-- passed action
interact :: TurnstileState -> Action -> (TurnstileOutput, TurnstileState)
interact = undefined

-- Insert a coin into the turnstile, setting the
-- state to Unlocked and the output to Thank
--
-- e.g. runState coin' Locked = (Thank, Unlocked)
coin' :: State TurnstileState TurnstileOutput
coin' = undefined

-- Push the turnstile. It will Open if it was
-- Unlocked and set its state to Locked.
-- If it was Locked, it will Tut.
--
-- e.g. runState push' Locked = (Tut, Locked)
push' :: State TurnstileState TurnstileOutput
push' = undefined

-- Interact with the turnstile, either
-- pushing or paying it depending on the
-- passed action
interact' :: Action -> State TurnstileState TurnstileOutput
interact' = undefined

