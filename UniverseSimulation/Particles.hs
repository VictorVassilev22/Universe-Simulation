module Particles where

import Data.Vector ( fromList, Vector )
import System.Random.MWC.Distributions as MWC ( bernoulli, categorical )
import System.Random.MWC as MWC ( createSystemRandom )

decide :: Double -> IO Bool --chooses if or not to do the action given chance
decide r = do
    rng <- MWC.createSystemRandom
    MWC.bernoulli r rng

decideFrom:: Vector Double -> IO Int --chooses which action to perform based on weights
decideFrom w = do
    rng <- MWC.createSystemRandom
    MWC.categorical w rng

data Particle = Particle
 {chance :: Double, symbol :: IO String,
  decay :: [[Particle]],
  weights :: Vector Double}

--selectors

getChance :: Particle -> Double
getChance (Particle a _ _ _) = a

getSymbol :: Particle -> IO String
getSymbol (Particle _ b _ _) = b

getDecayValues :: Particle -> [[Particle]]
getDecayValues (Particle _ _ c _) = c

getWeights :: Particle -> Vector Double
getWeights (Particle _ _ _ d) = d

getDecay :: Particle -> IO [Particle]
getDecay p  = getElement (getDecayValues p) (decideFrom (getWeights p)) --gets the decay of a particle

getElement :: Functor f => [b] -> f Int -> f b
getElement list = fmap (list !!) --gets element at position index from list

-- particles

higgsBozone :: Particle --the higgs bozone particle
higgsBozone = Particle 0.000433 (return "H") -- passing the % as probability here 0.000433
  [[bottomQuark, bottomAntiQuark],[wBozone, wBozone],[gluon, gluon],[tau, antiTau], [charmQuark, charmAntiQuark],
   [zBozone, zBozone], [photon, photon], [zBozone, photon], [muon, antiMuon], [topQuark, topAntiQuark]]
 (Data.Vector.fromList[64.8, 14.1, 8.82, 7.04, 3.27, 1.59, 0.223, 0.111, 0.0244, 0.0216])

wBozone :: Particle
wBozone = Particle 0.5 (return "W")
  [[positron, neutrino], [antiMuon, neutrino], [antiTau, neutrino]]
  (Data.Vector.fromList[1, 1, 1])

zBozone :: Particle
zBozone = Particle 0.5 (return "Z")
  [[neutrino, antiNeutrino], [electron, positron], [muon, antiMuon], [tau, antiTau], [downQuark, downAntiQuark],
   [strangeQuark, strangeAntiQuark], [bottomQuark, bottomAntiQuark], [upQuark, upAntiQuark], [charmQuark, charmAntiQuark]]
  (Data.Vector.fromList[20.6, 3.4, 3.4, 3.4, 15.2, 15.2, 15.2, 11.8, 11.8])

topQuark :: Particle
topQuark = Particle 0.1295 (return "T")
                        [[wBozone, downQuark], [wBozone, strangeQuark], [wBozone, bottomQuark]]
    (Data.Vector.fromList[1, 1, 1])

topAntiQuark :: Particle
topAntiQuark = Particle 0.1295 (return "!T")
                        [[wBozone, downAntiQuark], [wBozone, strangeAntiQuark], [wBozone, bottomAntiQuark]]
    (Data.Vector.fromList[1, 1, 1])

bottomQuark :: Particle
bottomQuark = Particle 0 (return "b") [] (Data.Vector.fromList[])

bottomAntiQuark :: Particle
bottomAntiQuark = Particle 0 (return "!b") [] (Data.Vector.fromList[])

downQuark :: Particle
downQuark = Particle 0 (return "d") [] (Data.Vector.fromList[])

downAntiQuark :: Particle
downAntiQuark = Particle 0 (return "!d") [] (Data.Vector.fromList[])

strangeQuark :: Particle
strangeQuark = Particle 0 (return "s") [] (Data.Vector.fromList[])

strangeAntiQuark :: Particle
strangeAntiQuark = Particle 0 (return "!s") [] (Data.Vector.fromList[])

charmQuark :: Particle
charmQuark = Particle 0 (return "c") [] (Data.Vector.fromList[])

charmAntiQuark :: Particle
charmAntiQuark = Particle 0 (return "!c") [] (Data.Vector.fromList[])

upQuark :: Particle
upQuark = Particle 0 (return "u") [] (Data.Vector.fromList[])

upAntiQuark :: Particle
upAntiQuark = Particle 0 (return "!u") [] (Data.Vector.fromList[])

gluon :: Particle
gluon = Particle 0 (return "g") [] (Data.Vector.fromList[])

tau :: Particle
tau = Particle 0 (return "t-") [] (Data.Vector.fromList[])

antiTau :: Particle
antiTau = Particle 0 (return "t+") [] (Data.Vector.fromList[])

photon :: Particle
photon = Particle 0 (return "y0") [] (Data.Vector.fromList[])

muon :: Particle
muon = Particle 0 (return "m-") [] (Data.Vector.fromList[])

antiMuon :: Particle
antiMuon = Particle 0 (return "m+") [] (Data.Vector.fromList[])

neutrino :: Particle
neutrino = Particle 0 (return "v-") [] (Data.Vector.fromList[])

antiNeutrino :: Particle
antiNeutrino = Particle 0 (return "v+") [] (Data.Vector.fromList[])

electron :: Particle
electron = Particle 0 (return "e-") [] (Data.Vector.fromList[])

positron :: Particle
positron = Particle 0 (return "e+") [] (Data.Vector.fromList[])