module Example.Project
  ( Instr(..), AluOp(..), Stall(..)
  , MicroOp(..), microOpBase
  , Machine(..), initState
  , step, alu
  , system
  , myProgram
  , topEntity) where

import Clash.Prelude

type Reg = Unsigned 5
type Value = Signed 8
type Addr = Unsigned 8

data Instr
  = IAdd Reg Reg Reg
  | IImm Reg Value
  | ILoad Reg Addr
  | IStore Reg Addr
  | ISkip
  | IJmp Value
  deriving (Show, Generic, ShowX, NFDataX)

data AluOp
  = AAdd
  deriving (Show, Eq, Ord, Generic, NFDataX)

data Stall
  = SRead
  | SNone
  deriving (Show, Generic, NFDataX)

data MicroOp = MicroOp
  { mop_regWrite :: Maybe (Reg, Value)
  , mop_nextPc :: Value
  , mop_stall :: Stall
  , mop_alu :: Maybe (Reg, AluOp, Value, Value)
  , mop_memRead :: Addr
  , mop_memWrite :: Maybe (Addr, Value)
  }
  deriving (Show, Generic, NFDataX)

microOpBase :: Value -> MicroOp
microOpBase nextPc = MicroOp
  { mop_regWrite = Nothing
  , mop_nextPc = nextPc
  , mop_stall = SNone
  , mop_alu = Nothing
  , mop_memRead = 0
  , mop_memWrite = Nothing
  }

data Machine = Machine
  { m_registers :: Vec 32 Value
  , m_stall :: Stall
  }
  deriving (Show, Generic, NFDataX)

pcReg :: Reg
pcReg = 1

step :: Machine -> (Value, Instr) -> (Machine, (Value, Addr, Maybe (Addr, Value)))
step m (readVal, instr) =
  let MicroOp{mop_nextPc=nextPc, mop_memRead=memRead, mop_memWrite=memWrite} = mop
      nextState = m{m_registers = nextRegisters, m_stall = mop_stall mop} in
    (nextState, (nextPc, memRead, memWrite))
  where
    registers = m_registers m
    pc = registers !! pcReg

    -- Decode
    mop = case instr of
      IAdd dst r1 r2 -> regOp AAdd dst (registers !! r1) (registers !! r2)
      IImm dst val -> regOp AAdd dst val (0 :: Value)
      ILoad dst addr -> case m_stall m of
        SNone -> (microOpBase pc) { mop_stall = SRead, mop_memRead = addr }
        SRead -> (microOpBase (pc + 1)) { mop_regWrite = Just (dst, readVal) }
      IStore src addr ->
        (microOpBase (pc + 1)) { mop_memWrite = Just (addr, registers !! src) }
      ISkip -> microOpBase (pc + 1)
      IJmp offs -> microOpBase (pc + offs)
      where
        regOp op dst v1 v2 = (microOpBase (pc + 1)) { mop_alu = Just (dst, op, v1, v2) }

    -- Execute and writeback
    regWrite =
      case () of
        _ | Just (dst, op, v1, v2) <- mop_alu mop ->
            replace dst (alu op v1 v2)
        _ | Just (dst, val) <- mop_regWrite mop ->
            replace dst val
        _ -> id

    nextRegisters
      = replace (0 :: Reg) 0
      . replace pcReg (mop_nextPc mop)
      . regWrite
      $ registers

alu :: AluOp -> Value -> Value -> Value
alu op v1 v2 = case op of
  AAdd -> v1 + v2

initState :: Machine
initState = Machine
  { m_registers = repeat 0
  , m_stall = SNone
  }

system :: (HiddenClockResetEnable dom, KnownNat n)
       => Vec n Instr -- ^ program
       -> (Signal dom Addr -> Signal dom (Maybe (Addr, Value)) -> Signal dom Value) -- ^ RAM read / write
       -> Signal dom (Instr, Value, Value, Maybe (Addr, Value)) -- ^ executed instr, next pc, read / write addr
system prog ram = bundle (instr, nextInstr, readVal, writeReq)
  where
    (nextInstr, readAddr, writeReq) = mealyB step initState (readVal, instr)
    instr = asyncRom prog <$> register 0 nextInstr
    readVal = ram readAddr writeReq

myProgram =
  ISkip :>
  -- r6 = 20
  IImm (6 :: Reg) (20 :: Value) :>
  -- m[10] = r6
  IStore (6 :: Reg) (10 :: Addr) :>
  -- r7 = m[10]
  ILoad (7 :: Reg) (10 :: Addr) :>
  -- m[10] = r7
  IStore (7 :: Reg) (10 :: Addr) :>
  -- r7 = r6 + r7
  IAdd (7 :: Reg) (6 :: Reg) (7 :: Reg) :>
  -- m[10] = r7
  IStore (7 :: Reg) (10 :: Addr) :>
  -- loop forever here
  IJmp 0 :>
  Nil

topEntity :: SystemClockResetEnable
          => Clock System
          -> Reset System
          -> Enable System
          -> Signal System (Instr, Value, Value, Maybe (Addr, Value))
topEntity = exposeClockResetEnable (system myProgram (blockRam (replicate d64 0)))

