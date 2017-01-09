{-# LANGUAGE GADTs, Rank2Types, DataKinds, TypeOperators #-}

module Lib.Utility.Mnist.IO
(
    MnistTrainingSize, MnistTestSize, MnistData, TrainingData,
    getMnistData
) where

import Network.HTTP as HTTP
import Data.ByteString.Lazy as Lazy (ByteString, readFile, writeFile, empty)
import Data.ByteString.Lazy.Char8 as Char8 (pack)
import Codec.Compression.GZip as GZip
import qualified Data.IDX as IDX
import Control.Monad.Trans.Maybe
import Data.Vector.Unboxed (Vector, toList)
import qualified Numeric.LinearAlgebra as HMatrix
import qualified Numeric.LinearAlgebra.Static as Static
import System.IO.Error
import qualified Data.Serialize as Serialize
import Control.Arrow
import Control.Applicative
import GHC.TypeLits
import GHC.Generics (Generic)
import Data.Proxy

type TrainingData n = (Static.L n 784, Static.L n 10)

type MnistTrainingSize = 60000
type MnistTestSize = 10000
type MnistData = (TrainingData MnistTrainingSize, TrainingData MnistTestSize)

type SerializedMnistData = ([(Int, [Int])], [(Int, [Int])])
type LabeledIntData = [(Int, Vector Int)]

getMnistData :: MaybeT IO MnistData
getMnistData = do
    loadResult <- loadMnistData
    (tr, te) <- case loadResult of
        Just x -> return x
        Nothing -> do
            () <- MaybeT (Just <$> putStrLn "Try fetch for load fail")
            x <- fetchMnistData
            () <- saveMnistData x
            MaybeT $ return $ parseMatrix $ fromLabeledIntData2Matrix x
    return (tr, te)

loadMnistData :: MaybeT IO (Maybe MnistData)
loadMnistData = MaybeT $ do
    putStrLn $ "Loading from " ++ fpath
    matrixMaybe <- HMatrix.loadMatrix' fpath -- 70000><785
    let loadResult = matrixMaybe >>= parseMatrix
    return $ Just loadResult

-- toOneHot :: HMatrix.Vector Double -> HMatrix.Matrix Double
toOneHot :: forall n. KnownNat n => Static.L n 1 -> Static.L n 10
toOneHot label = Static.build f
    where m0 = Static.extract $ (Static.konst 0 :: Static.L 10 10)
          f r c = if c == (atIndex r 0) then 1.0 else 0
          atIndex r c = Static.unwrap label `HMatrix.atIndex` (truncate r, truncate c)

parseMatrix :: HMatrix.Matrix Double -> Maybe MnistData
parseMatrix matrix = do
        matrix' <- Static.create matrix :: Maybe (Static.L (MnistTrainingSize + MnistTestSize) 785)
        let (label, image) = Static.splitCols matrix'
        let (trL, teL) = Static.splitRows label :: (Static.L MnistTrainingSize 1, Static.L MnistTestSize 1)
        let (trD, teD) = Static.splitRows $ image / 255
        return ((trD, toOneHot trL), (teD, toOneHot teL))

fromLabeledIntData2Matrix :: (LabeledIntData, LabeledIntData) -> HMatrix.Matrix Double
fromLabeledIntData2Matrix (tr, te) = matrix
    where transform = map (second toList)
          tr' = transform tr
          te' = transform te
          join = map (uncurry (:))
          matrix = HMatrix.fromLists $ map (map realToFrac) $ join tr' ++ join te'

saveMnistData :: (LabeledIntData, LabeledIntData) -> MaybeT IO ()
saveMnistData labeledIntData = MaybeT $ do
    putStrLn $ "Saving to " ++ fpath
    let matrix = fromLabeledIntData2Matrix labeledIntData
    let saveAction = HMatrix.saveMatrix fpath "%lf" matrix
    () <- catchIOError saveAction (\_ -> do putStrLn "SaveError"; return ())
    return $ Just ()

fetchMnistData :: MaybeT IO (LabeledIntData, LabeledIntData)
fetchMnistData = do
    (Data_ trD) <- fetchIDX trainImagesPath decodeIDXData
    (Labels_ trL) <- fetchIDX trainLabelsPath decodeIDXLabels
    (Data_ teD) <- fetchIDX testImagesPath decodeIDXData
    (Labels_ teL) <- fetchIDX testLabelsPath decodeIDXLabels
    MaybeT $ return $ do
        tr <- IDX.labeledIntData trL trD
        te <- IDX.labeledIntData teL teD
        return (tr, te)

data IDXW = Data_ IDX.IDXData | Labels_ IDX.IDXLabels

decodeIDXData :: ByteString -> Maybe IDXW
decodeIDXData = fmap Data_ . IDX.decodeIDX
decodeIDXLabels :: ByteString -> Maybe IDXW
decodeIDXLabels = fmap Labels_ . IDX.decodeIDXLabels
fetchIDX :: String -> (ByteString -> Maybe IDXW) -> MaybeT IO IDXW
fetchIDX path decodeF = MaybeT $ do
    let url = toUrl path
    putStrLn $ "Downloading from " ++ url
    idxBytes <- getFileFromUrl url
    let idx = decodeF idxBytes
    return idx

baseUrl = "http://yann.lecun.com/exdb/mnist/"
trainImagesPath = "train-images-idx3-ubyte.gz"
trainLabelsPath = "train-labels-idx1-ubyte.gz"
testImagesPath = "t10k-images-idx3-ubyte.gz"
testLabelsPath = "t10k-labels-idx1-ubyte.gz"
fpath = "mnist.pickle"
toUrl path = baseUrl ++ path

getFileFromUrl :: String -> IO ByteString
getFileFromUrl url = do
    response <- simpleHTTP (getRequest url)
    body <- getResponseBody response
    let bytes = Char8.pack body
    return $ GZip.decompress bytes
