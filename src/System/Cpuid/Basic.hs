{-|
Module      : System.Cpuid.Basic
Description : Call the CPUID instruction from Haskell.
Copyright   : (c) 2017, Anselm Jonas Scholl
License     : BSD3
Maintainer  : anselm.scholl@tu-harburg.de
Stability   : experimental
Portability : x86

This module gives access to the CPUID instruction on x86 based systems.
The module will still build on other systems but will not provide any useful
functionality instead.
-}
{-# LANGUAGE CPP                      #-}
#ifdef CPUID_SUPPORT
{-# LANGUAGE ForeignFunctionInterface #-}
#endif
{-# LANGUAGE NondecreasingIndentation #-}
module System.Cpuid.Basic (
     -- * CPUID support
     CpuidArg(..)
    ,CpuidResult(..)
    ,cpuidSupported
    ,cpuid
    ,cpuidMaybe
    ,cpuidHighestFunctionParameter

    -- * XGETBV support
    ,XGetBVArg
    ,XGetBVResult(..)
    ,xgetbvSupport
    ,xgetbv
    ,xgetbvMaybe

    -- * Utilities
    ,supportsSSE2
    ,supportsAVX2
    ,supportsAVX512f
    ) where

#ifdef CPUID_SUPPORT
import Control.Applicative
import Prelude
import Foreign
#else
import Data.Word
#endif

-- * CPUID support

-- | Argument for the CPUID instruction.
data CpuidArg = CpuidArg {
     caEAX :: !Word32 -- ^ Value to query for.
    ,caECX :: !Word32 -- ^ Additional value for queries. You can leave this at 0 most of the time.
} deriving (Show, Eq)

-- | Result of a call to the CPUID instruction.
data CpuidResult = CpuidResult {
     crEAX :: !Word32 -- ^ Value of EAX after the CPUID instruction.
    ,crEBX :: !Word32 -- ^ Value of EBX after the CPUID instruction.
    ,crECX :: !Word32 -- ^ Value of ECX after the CPUID instruction.
    ,crEDX :: !Word32 -- ^ Value of EDX after the CPUID instruction.
} deriving (Show, Eq)

-- | True if the platform supports the CPUID instruction (that is, if this code runs on x86 or AMD64).
cpuidSupported :: Bool
#ifdef CPUID_SUPPORT
cpuidSupported = True
#else
cpuidSupported = False
#endif

-- | Call the CPUID instruction. If the instruction is not supported, throw an error.
cpuid :: CpuidArg -> IO CpuidResult
#ifdef CPUID_SUPPORT
cpuid (CpuidArg eax ecx) = allocaBytes 16 $ \ ptr -> do
    call_cpuid eax ecx ptr
    CpuidResult <$>
        peekElemOff ptr 0 <*>
        peekElemOff ptr 1 <*>
        peekElemOff ptr 2 <*>
        peekElemOff ptr 3

foreign import ccall unsafe "call_cpuid" call_cpuid :: Word32 -> Word32 -> Ptr Word32 -> IO ()
#else
cpuid _ = fail "CPUID instruction unsupported on this platform."
#endif

-- | A safe version of 'cpuid' which never throws exceptions if a 'Just' is returned.
cpuidMaybe :: Maybe (CpuidArg -> IO CpuidResult)
cpuidMaybe = if cpuidSupported
    then Just cpuid
    else Nothing

-- | Get the highest supported function parameter (EAX) for the CPUID instruction.
--   Returns 0 if CPUID is unsupported.
cpuidHighestFunctionParameter :: IO Word32
cpuidHighestFunctionParameter = if cpuidSupported
    then crEAX <$> cpuid (CpuidArg 0 0)
    else pure 0

-- * XGETBV support

-- | Argument for the XGETBV instruction.
type XGetBVArg = Word32

-- | Result of a call to the XGETBV instruction.
data XGetBVResult = XGetBVResult {
     xgEAX :: !Word32
    ,xgEDX :: !Word32
} deriving (Show, Eq)

-- | Returns true if the platform supports the XGETBV instruction (that is, if the
--   code runs on x86 or AMD64) AND the processor (or operating system) support it
--   (that is, the OSXSAVE bit is set)
xgetbvSupport :: IO Bool
#ifdef CPUID_SUPPORT
xgetbvSupport = do
    ecx <- crECX <$> cpuid (CpuidArg 1 0)
    -- check bit 26 and 27 (XGETBV support and OSXSAVE support enabled by OS)
    pure $ (ecx .&. 0xC000000) == 0xC000000
#else
xgetbvSupport = pure False
#endif

-- | Call the XGETBV instruction. If the instruction is not supported, throw an error.
--   You also have to check for support of the instruction by the operating system
--   and the processor first (use 'xgetbvSupport').
xgetbv :: XGetBVArg -> IO XGetBVResult
#ifdef CPUID_SUPPORT
xgetbv ecx = do
    edx_eax <- call_xgetbv ecx
    pure XGetBVResult {
         xgEAX = fromIntegral $ edx_eax .&. 0xFFFFFFFF
        ,xgEDX = fromIntegral $ edx_eax `shiftR` 32
    }

foreign import ccall unsafe "call_xgetbv" call_xgetbv :: Word32 -> IO Word64
#else
xgetbv _ = fail "XGETBV instruction unsupported on this platform."
#endif

-- | A safe version of 'xgetbv' which never throws exceptions if a 'Just' is returned.
xgetbvMaybe :: IO (Maybe (XGetBVArg -> IO XGetBVResult))
xgetbvMaybe = do
    support <- xgetbvSupport
    if support
    then pure $ Just xgetbv
    else pure Nothing

-- * Utilities

-- | Check whether the processor indicated support for SSE2. Note that this does
--   not mean the OS will save the XMM upon context switches.
supportsSSE2 :: IO Bool
supportsSSE2 = do
    -- check if we can call the right CPUID instructions
    -- this will return 0 if CPUID is unsupported
    highestFunParam <- cpuidHighestFunctionParameter
    if highestFunParam < 1 then pure False else do
    -- check SSE2 support
    edx <- crEDX <$> cpuid (CpuidArg 1 0)
    pure $ (edx .&. 0x4000000) == 0x4000000

-- | Check whether OS and CPU support the AVX2 instruction set.
supportsAVX2 :: IO Bool
supportsAVX2 = do
    -- check if we can call the right CPUID instructions
    -- this will return 0 if CPUID is unsupported
    highestFunParam <- cpuidHighestFunctionParameter
    if highestFunParam < 7 then pure False else do
    -- check AVX support
    ecx <- crECX <$> cpuid (CpuidArg 1 0)
    if ((ecx .&. 0x10000000) /= 0x10000000) then pure False else do
    -- check AVX2 support
    ebx <- crEBX <$> cpuid (CpuidArg 7 0)
    if ((ebx .&. 0x20) /= 0x20) then pure False else do
    xSupport <- xgetbvSupport
    if not xSupport then pure False else do
    -- specify 0 for XFEATURE_ENABLED_MASK register
    eax <- xgEAX <$> xgetbv 0
    -- check OS has enabled both XMM and YMM state support
    pure $ eax .&. 0x06 == 0x06

-- | Check whether OS and CPU support the AVX512f instruction set.
supportsAVX512f :: IO Bool
supportsAVX512f = do
    avx2 <- supportsAVX2
    if not avx2 then pure False else do
    -- check AVX512f support
    ebx <- crEBX <$> cpuid (CpuidArg 7 0)
    if ((ebx .&. 0x10000) /= 0x10000) then pure False else do
    -- specify 0 for XFEATURE_ENABLED_MASK register
    eax <- xgEAX <$> xgetbv 0
    -- check OS has enabled full ZMM state support
    pure $ eax .&. 0xE0 == 0xE0
