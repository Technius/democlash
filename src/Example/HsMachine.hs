{-# LANGUAGE ViewPatterns #-}
module Example.HsMachine where

import Prelude
import Data.Function ((&))
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import qualified Data.List as List
import Control.Arrow

type Reg = Int
type Value = Int
type Addr = Int

data Instr
  = IAdd Reg Reg Reg
  | IImm Reg Value
  | ILoad Reg Addr
  | IStore Reg Addr
  | ISkip
  | IJmp Value
  deriving (Show, Eq, Ord)

data Mstate = Mstate
  { m_regs :: Vector Value
  , m_mem :: Vector Value
  }
  deriving (Show, Eq, Ord)

pcReg :: Reg
pcReg = 1

getReg :: Reg -> Mstate -> Value
getReg r state = (m_regs state) ! r

setReg :: Reg -> Value -> Mstate -> Mstate
setReg r v state = state{m_regs = m_regs state // [(r, v)]}

getMem :: Addr -> Mstate -> Value
getMem a state = (m_mem state) ! a

setMem :: Addr -> Value -> Mstate -> Mstate
setMem a v state = state{m_mem = m_mem state // [(a, v)]}

setPc :: (Value -> Value) -> Mstate -> Mstate
setPc f state = state{m_regs = regs // [(pcReg, f (regs ! pcReg))]}
  where regs = m_regs state

step :: Mstate -> Instr -> Mstate
step state instr = case instr of
  IAdd dst r1 r2 ->
    state & setReg dst (regs ! r1 + regs ! r2) & advPc
  IImm dst v ->
    state & setReg dst v & advPc
  ILoad dst addr ->
    state & setReg dst ((m_mem state) ! addr) & advPc
  IStore src addr ->
    state & setMem addr (regs ! src) & advPc
  ISkip -> state & advPc
  IJmp offs -> state & setPc (+offs)
  where
    regs = m_regs state
    advPc = setPc (+1)

initState :: Mstate
initState = Mstate
  { m_regs = Vector.replicate 8 0
  , m_mem = Vector.replicate 16 0 }

execute :: [Instr] -> [Mstate]
execute (Vector.fromList -> program) = initState : go initState
  where
    go state =
      let state' = step state (program ! (m_regs state ! pcReg)) in
        state' : go state'

sampleProgram :: [Instr]
sampleProgram =
  [ -- r2 = 1
    IImm 2 1
  , -- r2 = r2 + r2
    IAdd 2 2 2
  , -- m[0] = r2
    IStore 2 0
  , -- jmp -2
    IJmp (-2)
  ]
