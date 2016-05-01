{-# LANGUAGE LambdaCase #-}
module Language where

data Label = User Int | Fresh Int deriving (Eq, Show, Ord)

type Temp = Int

data RelOp = Eq | Neq | Leq | Geq deriving (Eq, Show)
data Op = Add | Sub | Mul | Div deriving (Eq, Show)

-- The explicit "in stack" operands are for use in stores, they help
-- when we have to do spilling. Ideally we'll compile them away into
-- addressings of %esp in real code but for now it's simpler to have
-- that idiom represented explicitly.
data Operand = Temp Temp | Lit Int | InStack Int
             deriving (Eq, Show)

-- We start with a definition of a simple, quasi-assembly like language.
-- I just have jumps, conditional jumps, load, store, and basic math
data Code = Label Label
          | Asgn Temp Operand Op Operand
          | Copy Temp Operand
          | Jmp Label
          | CJmp Operand RelOp Operand Label Label
          | Load Temp Operand
          | Store Operand Operand
          deriving (Eq, Show)

-- | Returns the temp underlying an operand if one exists (eg we're
-- looking at 'Temp'). Otherwise this gives back nothing.
operand2temp :: Operand -> Maybe Temp
operand2temp (Temp t) = Just t
operand2temp (Lit _) = Nothing
operand2temp (InStack _) = Nothing

-- | Substitute one temporary for another in an instruction.
replace :: Temp -> Temp -> Code -> Code
replace new old = \case
  Label l -> Label l
  Asgn t l o r ->
    Asgn (swap new old t) (swapOper l) o (swapOper r)
  Copy t o -> Copy (swap new old t) (swapOper o)
  Jmp l ->  Jmp l
  CJmp l o r t1 t2 ->
    CJmp (swapOper l) o (swapOper r) t1 t2
  Load t o ->  Load (swap new old t) (swapOper o)
  Store to o ->  Store (swapOper to) (swapOper o)
  where swap new old other = if old == other then new else other
        -- A quick shorthand for working with operators
        swapOper other = swap (Temp new) (Temp old) other
