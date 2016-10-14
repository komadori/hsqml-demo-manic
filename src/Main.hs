{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, TypeFamilies #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Exception (evaluate)
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Random
import Data.Function
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Graphics.QML
import Graphics.QML.Objects.ParamNames

import Paths_hsqml_demo_manic

data Bearing = North | East | South | West
    deriving (Typeable, Eq, Enum, Bounded, Show, Read)

instance Marshal Bearing where
    type MarshalMode Bearing c d = ModeTo c
    marshaller = toMarshaller (\x -> case x of
        North -> 0 :: Int
        East  -> 90
        South -> 180
        West  -> 270)

data Orientation =
    Horizontal | Vertical deriving (Eq, Enum, Bounded, Show, Read)

data Colour = Green | Yellow | Brown | Blue
    deriving (Typeable, Eq, Enum, Bounded, Show, Read)

instance Marshal Colour where
    type MarshalMode Colour c d = ModeTo c
    marshaller = toMarshaller (\x -> case x of
        Green  -> T.pack "lime" :: Text
        Yellow -> T.pack "yellow"
        Brown  -> T.pack "darkgoldenrod"
        Blue   -> T.pack "blue")

data Tile
    = Start Colour Bearing
    | End Colour Bearing
    | Corner Bearing
    | Straight Orientation
    | Cross
    deriving (Eq, Show, Read)

data NumberedTile = NumberedTile Tile Int deriving Typeable

newtype TileSource = TileSource [NumberedTile] deriving Typeable

instance Marshal NumberedTile where
    type MarshalMode NumberedTile c d = ModeObjBidi NumberedTile c
    marshaller = bidiMarshallerIO (return . fromObjRef) newObjectDC

instance DefaultClass NumberedTile where
    classMembers = [
        defPropertyConst "tile" (\(NumberedTile tile _) -> return tile),
        defPropertyConst "idx" (\(NumberedTile _ idx) -> return idx)]

instance Marshal TileSource where
    type MarshalMode TileSource c d = ModeObjBidi TileSource c
    marshaller = bidiMarshallerIO (return . fromObjRef) newObjectDC

instance DefaultClass TileSource where
    classMembers = [
        defPropertyConst "top" (\(TileSource ts) -> return $ head ts),
        defPropertyConst "next" (\(TileSource ts) ->
            return $ TileSource $ tail ts),
        defMethod "topN" (\(TileSource ts) n ->
            return $ take n ts :: IO [NumberedTile])]

newTileSource :: IO TileSource
newTileSource = do
    g <- newStdGen
    return $ TileSource $ zipWith NumberedTile (randomTiles g) [0..]

randomEnum :: forall g e. (RandomGen g, Enum e, Bounded e) => Rand g e
randomEnum = toEnum <$> getRandomR (fromEnum lo, fromEnum hi)
    where lo = minBound :: e
          hi = maxBound :: e

randomTile :: (RandomGen g) => Rand g Tile
randomTile = join $ fromList [
    (fmap Corner randomEnum, 4),
    (fmap Straight randomEnum, 2),
    (return Cross, 1)]

randomTiles :: (RandomGen g) => g -> [Tile]
randomTiles = unfoldr (Just . runRand randomTile)

instance Marshal Tile where
    type MarshalMode Tile c d = ModeBidi c
    marshaller = bidiMarshaller (read . T.unpack) (T.pack . show)

data Point = Pt !Int !Int deriving (Typeable, Eq, Ord, Show)

instance DefaultClass Point where
    classMembers = [
        defPropertyConst "x" (\(Pt x _) -> return x),
        defPropertyConst "y" (\(Pt _ y) -> return y)]

instance Marshal Point where
    type MarshalMode Point c d = ModeObjBidi Point c
    marshaller = bidiMarshallerIO (return . fromObjRef) newObjectDC 

data Grid = Grid Int Int (Map Point Tile) deriving (Typeable, Show)

place :: Point -> Tile -> Grid -> Grid
place pt tile (Grid w h grid) = Grid w h $ Map.insert pt tile grid

placeIfFree :: Point -> Tile -> Grid -> Grid
placeIfFree pt tile (Grid w h grid) = Grid w h $ Map.alter (\oldTile ->
    if isJust oldTile then oldTile else Just tile) pt grid

data PickedSignal deriving Typeable

instance SignalKeyClass PickedSignal where
    type SignalParams PickedSignal = Point -> IO ()

instance DefaultClass Grid where
    classMembers = [
        defPropertyConst "width" (\(Grid w _ _) -> return w),
        defPropertyConst "height" (\(Grid _ h _) -> return h),
        defMethod "place" (\grid x y tile ->
            return $ place (Pt x y) tile grid :: IO Grid),
        defMethod "plumb" (return . plumbGrid :: Grid -> IO PlumbedGrid),
        defMethod "pick" (\grid tiles currX currY vol -> void $ forkIO $ do
            let (Pt x y) = pickMove (fromObjRef grid) tiles (Pt currX currY) vol
            x' <- evaluate x
            y' <- evaluate y
            let pos = Pt x' y'
            fireSignal (Proxy :: Proxy PickedSignal) grid pos),
        defSignalNamedParams "picked" (Proxy :: Proxy PickedSignal) $
            fstName "pos"]

instance Marshal Grid where
    type MarshalMode Grid c d = ModeObjBidi Grid c
    marshaller = bidiMarshallerIO (return . fromObjRef) newObjectDC 

data PlumbedGrid = PlumbedGrid [Plumb] [Leak] (Map Point Int)
    deriving Typeable

isPlacable :: PlumbedGrid -> Int -> Point -> Bool
isPlacable (PlumbedGrid _ _ indexMap) n pt =
    maybe True (> n) $ Map.lookup pt indexMap

instance DefaultClass PlumbedGrid where
    classMembers = [
        defPropertyConst "plumbing" (\(PlumbedGrid ps _ _) -> return ps),
        defPropertyConst "leaks" (\(PlumbedGrid _ ls _) -> return ls),
        defPropertyConst "maxIndex" (\(PlumbedGrid ps _ _) ->
            return . minimum . filter (>= 0) .
            map (maximum . map (\(Plumb _ i _ _ _) -> i)) $
            groupBy ((==) `on` (\(Plumb c _ _ _ _) -> c)) ps),
        defMethod "isPlacable" (\pGrid n x y ->
            return $ isPlacable pGrid n (Pt x y) :: IO Bool)]

instance Marshal PlumbedGrid where
    type MarshalMode PlumbedGrid c d = ModeObjBidi PlumbedGrid c
    marshaller = bidiMarshallerIO (return . fromObjRef) newObjectDC 

data Plumb = Plumb Colour Int Point (Maybe Bearing) (Maybe Bearing)
    deriving (Typeable, Show)

instance DefaultClass Plumb where
    classMembers = [
        defPropertyConst "colour" (\(Plumb col _ _ _ _) -> return col),
        defPropertyConst "idx" (\(Plumb _ idx _ _ _) -> return idx),
        defPropertyConst "x" (\(Plumb _ _ (Pt x _) _ _) -> return x),
        defPropertyConst "y" (\(Plumb _ _ (Pt _ y) _ _) -> return y),
        defPropertyConst "entryA" (\(Plumb _ _ _ theta _) -> return theta),
        defPropertyConst "exitA" (\(Plumb _ _ _ _ theta) -> return theta)]

instance Marshal Plumb where 
    type MarshalMode Plumb c d = ModeObjBidi Plumb c
    marshaller = bidiMarshallerIO (return . fromObjRef) newObjectDC

newtype Leak = Leak {unLeak :: Plumb}

instance Marshal Leak where 
    type MarshalMode Leak c d = ModeObjBidi Plumb c
    marshaller = bidiMarshaller Leak unLeak

bend :: Bearing -> Bearing
bend North = East
bend East = South
bend South = West
bend West = North

invert :: Bearing -> Bearing
invert = bend . bend

revBend :: Bearing -> Bearing
revBend = bend . bend . bend

nextPos :: Maybe Bearing -> Point -> Point
nextPos (Just North) (Pt x y) = Pt x (y-1)
nextPos (Just East) (Pt x y) = Pt (x+1) y
nextPos (Just South) (Pt x y) = Pt x (y+1)
nextPos (Just West) (Pt x y) = Pt (x-1) y
nextPos Nothing p = p

plumbTile :: (Point -> Maybe Tile) -> PrePlumb -> Maybe Plumb
plumbTile getTile (PrePlumb col idx curr flow) = case getTile curr of
    Just (Start c theta)
        | flow == Nothing && col == c ->
            Just $ Plumb col idx curr Nothing (Just theta) 
    Just (End c theta)
        | flow == Just (invert theta) && col == c ->
            Just $ Plumb col idx curr (Just theta) Nothing
    Just (Straight Horizontal)
        | flow == Just East || flow == Just West ->
            Just $ Plumb col idx curr (fmap invert flow) flow
    Just (Straight Vertical)
        | flow == Just North || flow == Just South ->
            Just $ Plumb col idx curr (fmap invert flow) flow
    Just (Corner theta)
        | flow == Just (invert theta)  ->
            Just $ Plumb col idx curr (Just theta) (Just $ bend theta)
        | flow == Just (revBend theta) ->
            Just $ Plumb col idx curr (Just $ bend theta) (Just theta)
    Just Cross -> Just $ Plumb col idx curr (fmap invert flow) flow
    _ -> Nothing

data Terminal = Success | EmptyTile | BadPipe deriving Show

data PrePlumb = PrePlumb Colour Int Point (Maybe Bearing) deriving Show

unPlumb :: Plumb -> PrePlumb
unPlumb (Plumb col idx curr inflow _) =
    PrePlumb col idx curr (fmap invert inflow)

plumbOn :: Plumb -> PrePlumb
plumbOn (Plumb col idx curr _ flow) =
    PrePlumb col (idx+1) (nextPos flow curr) flow

plumbStep ::
    (Point -> Maybe Tile) -> PrePlumb -> Either Terminal (Plumb, PrePlumb)
plumbStep tileFn pre@(PrePlumb _ _ curr flow) = case tileFn curr of
    Just _  -> case plumbTile tileFn pre of
        Just plumb -> Right (plumb, plumbOn plumb)
        Nothing -> maybe (Left Success) (const $ Left BadPipe) flow
    Nothing -> Left EmptyTile

unfoldrEither :: (b -> Either c (a, b)) -> b -> (c, [a])
unfoldrEither f b  = case f b of
    Right (a,b') -> (a:) <$> unfoldrEither f b'
    Left c       -> (c, [])

plumbStart :: Grid -> Colour -> Point -> Maybe Bearing -> (Terminal, [Plumb])
plumbStart (Grid _ _ grid) col start flow =
    unfoldrEither (plumbStep (`Map.lookup` grid)) $
    PrePlumb col 0 start flow

plumbAllStarts :: Grid -> [[Plumb]]
plumbAllStarts grid = foldStarts (\col curr ps ->
    snd (plumbStart grid col curr Nothing) : ps) [] grid

foldStarts :: (Colour -> Point -> a -> a) -> a -> Grid -> a
foldStarts f s (Grid _ _ gridMap) =
    Map.foldWithKey (\curr tile a -> case tile of
        Start col _ -> f col curr a
        _           -> a) s gridMap

plumbLeak :: [Plumb] -> Maybe Leak
plumbLeak ps =
    case last ps of
        Plumb col idx end _ (Just flow) ->
            Just $ Leak $ Plumb col (idx+1) end Nothing $ Just flow
        _ -> Nothing

stripPlumbed :: [Plumb] -> Grid -> Grid
stripPlumbed ps (Grid w h grid) =
    Grid w h $ foldl' (flip $ \(Plumb _ _ p theta _) ->
        Map.update (f theta) p) grid ps
    where f (Just North) Cross = Just $ Straight Horizontal
          f (Just East)  Cross = Just $ Straight Vertical
          f (Just South) Cross = Just $ Straight Horizontal
          f (Just West)  Cross = Just $ Straight Vertical
          f _ _ = Nothing

spareParts :: Grid -> [Plumb]
spareParts (Grid _ _ grid) =
    Map.foldWithKey sparePart [] grid

sparePart :: Point -> Tile -> [Plumb] -> [Plumb]
sparePart p (Start col theta) xs = Plumb col (-1) p Nothing (Just theta) : xs
sparePart p (End col theta) xs = Plumb col (-1) p (Just theta) Nothing : xs
sparePart p (Corner theta) xs =
    Plumb Green (-1) p (Just theta) (Just $ bend theta) : xs
sparePart p (Straight Horizontal) xs =
    Plumb Green (-1) p (Just East) (Just West) : xs
sparePart p (Straight Vertical) xs =
    Plumb Green (-1) p (Just North) (Just South) : xs
sparePart p Cross xs =
    Plumb Green (-1) p (Just East) (Just West) :
    Plumb Green (-1) p (Just North) (Just South) : xs

buildIndexMap :: [Plumb] -> Map Point Int
buildIndexMap = foldr (\plumb gridMap -> case plumb of
    Plumb _ i p _ (Just _) -> Map.alter (Just . maybe i (min i)) p gridMap
    Plumb _ _ p _ Nothing  -> Map.insert p (-1) gridMap) Map.empty

plumbGrid :: Grid -> PlumbedGrid
plumbGrid grid =
    let plumbs = plumbAllStarts grid
        leaks = mapMaybe plumbLeak plumbs
        indexMap = buildIndexMap $ plumb ++ filter (\(Plumb _ _ _ _ flow) ->
            isNothing flow) spares
        plumb = concat plumbs
        spares = spareParts $ stripPlumbed plumb grid
    in PlumbedGrid (plumb ++ spares) leaks indexMap

nextPosFollowPlumb :: Grid -> Colour -> Point -> Maybe Bearing -> [PrePlumb]
nextPosFollowPlumb grid@(Grid w h _) col start flow =
    dropOutOfBounds $ case reverse <$> plumbStart grid col start flow of
        (EmptyTile, []) -> [PrePlumb col 0 start flow]
        (EmptyTile, ps) -> plumbOn (head ps) : map unPlumb ps
        (Success, ps) -> map unPlumb ps
        _ -> []
    where dropOutOfBounds pps@(PrePlumb _ _ (Pt x y) _:_) =
              if x >= 0 && y >=0 && x < w && y < h then pps else []
          dropOutOfBounds [] = []

neighbours :: Grid -> Colour -> Point -> [[PrePlumb]]
neighbours grid@(Grid _ _ gridMap) col start =
    filter (not . null) $ case Map.lookup start gridMap of
        Just _  -> [nextPosFollowPlumb grid col start Nothing]
        Nothing -> map ((\flow ->
            nextPosFollowPlumb grid col (nextPos flow start) flow) . Just)
            [North,East,South,West]

findCosts ::
    Grid -> Colour -> Point -> Map Point (Int, [PrePlumb])
findCosts grid col start =
    fst $ fromJust $ find (null . snd) $
    iterate explore (Map.empty, [(start, [], -1)])
    where explore (cache, open) =
              foldr update (cache, []) open 
          update (curr, pps, cost') (cache, open) =
              case fromMaybe (maxBound, []) $ Map.lookup curr cache of
                  (cost, _) | cost' < cost ->
                      (Map.insert curr (cost', pps) cache,
                       map (\pps'@(PrePlumb _ _ curr' _:_) ->
                           (curr', pps' ++ pps, cost' + 1))
                           (neighbours grid col curr) ++ open)
                  _ -> (cache, open)

findGoal ::
    Grid -> Map Point (Int, a) -> Colour -> Maybe (Point, Int, a)
findGoal (Grid _ _ gridMap) costs col =
    fmap (\((a,b),c) -> (a,c,b)) $
    foldr (\(curr,(cost,x)) prevBest -> case prevBest of
        Just best | cost >= snd best -> prevBest
        _                            -> Just ((curr,x),cost)) Nothing $
    Map.toList $ Map.mapMaybeWithKey (\curr tile ->
        case tile of
            End col' _ | col' == col -> Map.lookup curr costs
            _                        -> Nothing) gridMap

findPath :: Grid -> Colour -> Point -> Maybe ([Plumb], Int)
findPath grid col start =
    let costs = findCosts grid col start
        goal = findGoal grid costs col
    in fmap (\(_,cost,pps) -> (plumbPre $ reverse pps,cost)) goal

plumbPre :: [PrePlumb] -> [Plumb]
plumbPre pps = zipWith3 f pps (tail pps) [0..]
    where f (PrePlumb col _ curr flow) (PrePlumb _ _ _ nextFlow) idx =
              Plumb col idx curr (fmap invert flow) nextFlow

gridHeuristic :: Int -> Grid -> Int
gridHeuristic vol grid = snd $ foldStarts (\col start (grid',heur) ->
    let plumbedLen = length $ snd $ plumbStart grid' col start Nothing
        (path, pathLen) = fromMaybe ([], 1000) $ findPath grid' col start
        remain = min (plumbedLen - vol) 4
        bonus = 9*remain - (remain*remain)
        grid'' = foldr placePlumb grid' path
    in (grid'', heur + 2*bonus + 4*plumbedLen - pathLen)) (grid, 0) grid

pickMove :: Grid -> TileSource -> Point -> Int -> Point
pickMove grid (TileSource (NumberedTile tile _:_)) start vol =
    fst $ head $ sortMoves start vol $ searchMoves grid vol tile

searchMoves :: Grid -> Int -> Tile -> [(Point, Grid)]
searchMoves grid@(Grid w h _) vol tile =
    map (\pt -> (pt, place pt tile grid)) $
    filter (isPlacable pGrid vol) $ liftM2 Pt [0..w-1] [0..h-1]
    where pGrid = plumbGrid grid

sortMoves :: Point -> Int -> [(Point, Grid)] -> [(Point, Grid)]
sortMoves start vol =
    map fst . sortBy (flip compare `on` snd) .
    map (\x -> (,) x $ (+ (distance start (fst x) / (-100.0))) $ fromIntegral $ gridHeuristic vol $ snd x)

distance :: Point -> Point -> Double
distance (Pt x1 y1) (Pt x2 y2) =
    sqrt $ (x*x)+(y*y)
    where x = fromIntegral (x1-x2)
          y = fromIntegral (y1-y2)

placePlumb :: Plumb -> Grid -> Grid
placePlumb (Plumb _ _ curr (Just inflow) (Just flow))
    | inflow == bend flow = placeIfFree curr $ Corner flow
    | inflow == revBend flow = placeIfFree curr $ Corner inflow
    | otherwise = placeIfFree curr Cross
placePlumb _ = id

debugMain :: Grid -> IO ()
debugMain myGrid = do
    clazz <- newClass [
        defMethod' "newTileSource" (\_ -> newObjectDC =<< newTileSource),
        defMethod' "newGrid" (\_ -> newObjectDC myGrid),
        defMethod' "sparePart" (\_ tile ->
            return $ sparePart (Pt 0 0) tile [] :: IO [Plumb])]
    object <- newObject clazz ()
    -- setQtArgs "hsqml-manic" ["-qmljsdebugger=port:9999,block"]
    qml <- getDataFileName "Main.qml"
    runEngineLoop $ defaultEngineConfig {
        initialDocument = fileDocument qml,
        contextObject = Just $ anyObjRef object
    }

newGrid :: Grid
newGrid = Grid 8 8 $ Map.fromList [
    (Pt 4 0,Start Yellow South),
    (Pt 3 7,End Yellow North),
    (Pt 0 3, Start Brown East),
    (Pt 7 4, End Brown West)]

main :: IO ()
main = debugMain newGrid
