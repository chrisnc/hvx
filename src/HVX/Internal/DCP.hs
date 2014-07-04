{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HVX.Internal.DCP
  ( Vex(..)
  , GetVex(getVex)
  , Mon(..)
  , GetMon(getMon)
  , ValidVex(validVex)
  , FlipVex
  , FlipMon
  , ApplyVex
  , ApplyMon
  , AddVex
  , AddMon
  ) where

class GetVex (v :: Vex) where
  getVex :: e (v :: Vex) (m :: Mon) -> String


-- | convexity
data Vex = Affine | Convex | Concave | Nonvex
instance GetVex Affine  where getVex _ = "Affine"
instance GetVex Convex  where getVex _ = "Convex"
instance GetVex Concave where getVex _ = "Concave"
instance GetVex Nonvex  where getVex _ = "Nonvex"

class ValidVex (v :: Vex) where
    validVex :: e (v :: Vex) (m :: Mon) -> e v m
    validVex = id

instance ValidVex Affine
instance ValidVex Convex
instance ValidVex Concave

class GetMon (m :: Mon) where
  getMon :: e (v :: Vex) (m :: Mon) -> String

-- | monotonicity
data Mon = Const | Nondec | Noninc | Nonmon
instance GetMon Const  where getMon _ = "Const"
instance GetMon Nondec where getMon _ = "Nondec"
instance GetMon Noninc where getMon _ = "Noninc"
instance GetMon Nonmon where getMon _ = "Nonmon"

-- | invert convexities
type family FlipVex v where
  FlipVex Convex  = Concave
  FlipVex Concave = Convex
  FlipVex Affine  = Affine
  FlipVex Nonvex  = Nonvex

-- | invert monotonicities
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
  ApplyVex vf      mf     Affine  me    = vf
  ApplyVex Affine  Nondec ve      me    = ve
  ApplyVex Affine  Noninc ve      me    = FlipVex ve
  ApplyVex v       Nondec v       me    = v
  ApplyVex Convex  Noninc Concave me    = Convex
  ApplyVex Concave Noninc Convex  me    = Concave
  ApplyVex vf      mf     ve      me    = Nonvex

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
  AddVex v1      v2      = Nonvex

-- determines the monotonicity of the sum of two expressions
-- "newexpr = e1 +~ e2"
type family AddMon m1 m2 where
  AddMon m1     Const  = m1
  AddMon Const  m2     = m2
  AddMon m      m      = m
  AddMon m1     m2     = Nonmon
