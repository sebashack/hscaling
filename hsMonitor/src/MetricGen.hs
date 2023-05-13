module MetricGen (
    sampleLoad,
    Seed,
    genSeed,
    genRandomVar,
) where

import Control.Monad.Primitive (PrimState)
import Data.Random (RVar, sampleFrom)
import Data.Random.Distribution.Poisson (poisson)
import System.Random.MWC (Gen, create)

type Seed = Gen (PrimState IO)

genSeed :: IO Seed
genSeed = create

genRandomVar :: RVar Int
genRandomVar = poisson (6.5 :: Float)

sampleLoad :: Seed -> RVar Int -> IO Float
sampleLoad seed rvar = do
    v <- sampleFrom seed rvar
    let r = (fromIntegral v :: Float) / 10.0
    return $ if r > 1.0 then 1.0 else r
