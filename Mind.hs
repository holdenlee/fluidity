{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

{-# LANGUAGE TemplateHaskell #-}

module Mind where
import System.Environment
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import Data.Array
import Data.Tuple
import Data.Graph.Inductive as G
import System.Random
import Control.Lens

import Utilities
import ParseUtilities
import GraphUtils

-- * PART 1: Agents
{-| Agent' in mind w with memory mem and message type mes -}
data Agent' w mem mes = Agent' { -- | unique name
                                 _name :: String,
                                 -- | return an activation level (and make changes to itself) based on a quick calculation. It has to be quick because this is executed by every active agent. It's not allowed to change the world.
                                 _scout :: w -> Agent' w mem mes -> (Double, Agent' w mem mes),
                                 -- | this agent has been chosen: act on the world!
                                 _act :: w -> Agent' w mem mes -> (w, Agent' w mem mes),
                                 -- | internal memory, only meant to be accessed by itself
                                 _memory :: mem,
                                 -- | list of messages it received
                                 _inbox :: [mes]}

instance (Pointed mem) => Pointed (Agent' w mem mes) where
    point = Agent' {_name = "",
                    _scout = (\_ a -> (0, a)),
                    _act = (,),
                    _memory = point, 
                    _inbox = []}

makeLenses ''Agent'

aname :: Lens' (Agent w mes) String
aname = lens (\a -> case a of
                      Agent a' -> _name a')
             (\a nm -> case a of
                         Agent a' -> Agent a'{_name = nm})

ainbox :: Lens' (Agent w mes) [mes]
ainbox = lens (\a -> case a of
                      Agent a' -> _inbox a')
              (\a msgs -> case a of
                            Agent a' -> Agent a'{_inbox = msgs})

instance (Show mem) => Show (Agent' w mem mes) where
    show = show . _memory

{-| The internal memory of an agent is hidden, so that agents with different internal workings can coexist. -}
data Agent w mes = forall mem. (Show mem) => Agent {_agent :: Agent' w mem mes}

--doesn't work because of existentials
makeLenses ''Agent

--Cannot use record selector `_agent' as a function due to escaped type variables
--_get a = case a of Agent a' -> a'

instance Show (Agent w mes) where
    show a =
        case a of Agent a' -> show $ _memory a'
--right now it's just a memory dump

instance Eq (Agent w mes) where
    a1 == a2 =
        case a1 of Agent a1' -> case a2 of Agent a2' -> _name a1' == _name a2'

instance Ord (Agent w mes) where
    compare a1 a2 =
        case a1 of Agent a1' -> case a2 of Agent a2' -> compare (_name a1') (_name a2')

-- * PART 2: Slipnet
data Concept = Concept { _id :: Int,
                         _cname :: String,
                         _activation :: Double,
                         _depth :: Double
                         --hardcoded in right now...
                       } deriving (Show, Eq, Ord)

--edges themselves are concepts!
type Slipnet = G.Gr Concept Concept


-- * PART 3: Mind
{-| Mind having a workspace of type wksp and messages of type mes-}
data Mind wksp mes = Mind {
                            _workspace :: wksp,
                            -- | Temperature: low temperature selects for agents with high activation, while high temperature evens out the activation. High temperature = open-mindedness = less planning. High temperature means more randomness
                            _temp :: Double,
                            -- | formerly "coderack"
                            _agents :: M.Map Int (Agent (Mind wksp mes) mes),
                            -- | list of indices of active agents
                            _active :: [Int],
                            -- | graph of concepts
                            _slipnet :: Slipnet,
                            -- | maps a concept id to the agents that are related to the concept.
                            _followers :: MM.MultiMap Int (Agent (Mind wksp mes) mes),
                            _rng :: StdGen,
                            -- | number of cycles elapsed
                            _time :: Int}
                     --can make an arbitrary RandomGen, but won't bother right now...

makeLenses ''Mind

instance (Pointed wksp) => Pointed (Mind wksp mes) where
    point = Mind {_workspace = point,
                  _temp =50,
                  _agents = M.empty,
                  _active = [],
                  _slipnet = G.empty,
                  _followers = MM.empty,
                  --warning: rng is not set because it can't be.
                  _time = 0}

indexLens :: Int -> Lens' (M.Map Int a) a
indexLens n = lens (M.!n) (flip (M.insert n))

--agentIndex :: Lens' (Mind wksp mes) Int

agentIndex :: Int -> Lens' (Mind wksp mes) (Agent (Mind wksp mes) mes)
agentIndex n = agents . (indexLens n)

getAgent :: Int -> Mind wksp mes -> Agent (Mind wksp mes) mes
getAgent n = (^. (agentIndex n))
--((M.!n) . _agents)

setAgent :: Int -> Agent (Mind wksp mes) mes -> Mind wksp mes -> Mind wksp mes
setAgent n = set (agentIndex n)
--w{_agents = M.insert n a (_agents w)}

runAgent :: Int -> Mind wksp mes -> Mind wksp mes
runAgent n w =
    case (getAgent n w) of
      Agent a' ->
          let
              (w', a2) = (_act a') w a'
          in
            setAgent n (Agent a2) w'

{-| Get a random number in (lo, hi) from mind, and update the rng seed.-}
getRandom :: (Random a) => (a, a) -> Mind wksp mes -> (a, Mind wksp mes)
getRandom (lo, hi) w = 
    let
        (x, newGen) = randomR (lo, hi) (_rng w)
    in
      (x, w{_rng = newGen})
