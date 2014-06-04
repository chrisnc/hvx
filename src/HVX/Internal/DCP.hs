{-# LANGUAGE TypeFamilies #-}

module HVX.Internal.DCP
  ( Vex
  , getVex
  , Affine
  , Convex
  , Concave
  , Mon
  , getMon
  , Const
  , Nondec
  , Noninc
  , Nonmon
  , FlipVex
  , FlipMon
  , ApplyVex
  , ApplyMon
  , AddVex
  , AddMon
  ) where

class Vex v where
  getVex :: e v m -> String
data Affine  = Affine  deriving (Show)
data Convex  = Convex  deriving (Show)
data Concave = Concave deriving (Show)
instance Vex Affine  where getVex _ = show Affine
instance Vex Convex  where getVex _ = show Convex
instance Vex Concave where getVex _ = show Concave

class Mon m where
  getMon :: e v m -> String
data Const  = Const  deriving (Show)
data Nondec = Nondec deriving (Show)
data Noninc = Noninc deriving (Show)
data Nonmon = Nonmon deriving (Show)
instance Mon Const  where getMon _ = show Const
instance Mon Nondec where getMon _ = show Nondec
instance Mon Noninc where getMon _ = show Noninc
instance Mon Nonmon where getMon _ = show Nonmon

-- invert convexities
type family FlipVex v where
  FlipVex Convex  = Concave
  FlipVex Concave = Convex
  FlipVex Affine  = Affine

-- invert monotonicities
type family FlipMon m where
  FlipMon Const  = Const
  FlipMon Nondec = Noninc
  FlipMon Noninc = Nondec
  FlipMon Nonmon = Nonmon

-- determines the convexity of a function applied to an expression
-- "newexpr = apply f expr"
type family ApplyVex vf mf ve me where
  ApplyVex vf      Const  ve      me    = Affine
  ApplyVex vf      mf     ve      Const = Affine
  ApplyVex Affine  mf     Affine  me    = Affine
  ApplyVex Affine  Nondec ve      me    = ve
  ApplyVex Affine  Noninc ve      me    = FlipVex ve
  ApplyVex vf      mf     Affine  me    = vf
  ApplyVex v       Nondec v       me    = v
  ApplyVex Convex  Noninc Concave me    = Convex
  ApplyVex Concave Noninc Convex  me    = Concave

-- determines the monotonicity of a function applied to an expression
-- "newexpr = apply f expr"
type family ApplyMon mf me where
  ApplyMon Const  me     = Const
  ApplyMon mf     Const  = Const
  ApplyMon Nondec me     = me
  ApplyMon Noninc me     = FlipMon me
  ApplyMon Nonmon me     = Nonmon

-- determines the convexity of the sum of two expressions
-- "newexpr = e1 +~ e2"
type family AddVex v1 v2 where
  AddVex Affine  v2      = v2
  AddVex v1      Affine  = v1
  AddVex v       v       = v

-- determines the monotonicity of the sum of two expressions
-- "newexpr = e1 +~ e2"
type family AddMon m1 m2 where
  AddMon m1     Const  = m1
  AddMon Const  m2     = m2
  AddMon m      m      = m
  AddMon m1     m2     = Nonmon
