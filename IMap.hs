module IMap where
import qualified Data.Map.Strict as M

import Utilities

data IMap v = IMap {_map :: M.Map Int v,
                    _curID :: Int}

iEmpty :: IMap v
iEmpty = IMap M.empty 0

iInsert :: v -> IMap v -> IMap v
iInsert val im = IMap (_map im |> M.insert (_curID im) val) (_curID im + 1)

iInsertK :: Int -> v -> IMap v -> IMap v
iInsertK k val im = im{_map = (_map im |> M.insert k val)}

iInserts :: [v] -> IMap v -> IMap v
iInserts vals im = foldIterate iInsert vals im

iDelete :: Int -> IMap v -> IMap v
iDelete k im = im {_map = _map im |> M.delete k}
