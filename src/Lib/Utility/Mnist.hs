module Lib.Utility.Mnist
(
    miniBatch, getMnistData, trSize, teSize,
    module Control.Arrow,
    module Control.Applicative,
    module Control.Monad.Trans.Maybe
) where

import Network.HTTP as HTTP
import Data.ByteString.Lazy as Lazy (ByteString, readFile, writeFile, empty)
import Data.ByteString.Lazy.Char8 as Char8 (pack)
import Codec.Compression.GZip as GZip
import qualified Data.IDX as IDX
import Control.Monad.Trans.Maybe
import Data.Vector.Unboxed (Vector, toList)
import qualified Numeric.LinearAlgebra as HMatrix
import System.IO.Error
import qualified Data.Serialize as Serialize
import Control.Arrow
import Control.Applicative
import System.Random
import Lib.Utility.NeuralNet (TrainingData)

miniBatch :: Int -> Int -> TrainingData -> MaybeT IO (HMatrix.Matrix Double, HMatrix.Matrix Double)
miniBatch size allSize (images, labels) = do
    gen <- MaybeT $ fmap Just getStdGen
    let rs = HMatrix.idxs $ take size $ randomRs (0, allSize - 1) gen
    let extr = (HMatrix.Pos rs, HMatrix.All)
    return (images HMatrix.?? extr, labels HMatrix.?? extr)

toOneHot :: HMatrix.Vector Double -> HMatrix.Matrix Double
toOneHot labelV = HMatrix.fromRows $ map (HMatrix.fromList . f) labels
    where f :: Int -> [Double]
          f num = take 10 $ replicate num 0 ++ (1 : repeat 0)
          labels = map truncate $ HMatrix.toList labelV

toTrainingData :: HMatrix.Vector Double -> HMatrix.Matrix Double -> TrainingData
toTrainingData labels images = (images, toOneHot labels)

getMnistData :: MaybeT IO (TrainingData, TrainingData)
getMnistData = do
    loadResult <- loadMnistData
    (trL, trD, teL, teD) <- case loadResult of
        Just x -> return x
        Nothing -> do
            () <- MaybeT (Just <$> putStrLn "Try fetch for load fail")
            x <- fetchMnistData
            () <- saveMnistData x
            MaybeT $ return $ parseMatrix $ fromLabeledIntData2Matrix x
    return (toTrainingData trL trD, toTrainingData teL teD)

type SerializedMnistData = ([(Int, [Int])], [(Int, [Int])])
type LabeledIntData = [(Int, Vector Int)]
type MnistData = (HMatrix.Vector Double, HMatrix.Matrix Double, HMatrix.Vector Double, HMatrix.Matrix Double)
loadMnistData :: MaybeT IO (Maybe MnistData)
loadMnistData = MaybeT $ do
    putStrLn $ "Loading from " ++ fpath
    matrixMaybe <- HMatrix.loadMatrix' fpath -- 70000><785
    let loadResult = matrixMaybe >>= parseMatrix
    return $ Just loadResult

saveMnistData :: (LabeledIntData, LabeledIntData) -> MaybeT IO ()
saveMnistData labeledIntData = MaybeT $ do
    putStrLn $ "Saving to " ++ fpath
    let matrix = fromLabeledIntData2Matrix labeledIntData
    let saveAction = HMatrix.saveMatrix fpath "%lf" matrix
    () <- catchIOError saveAction (\_ -> do putStrLn "SaveError"; return ())
    return $ Just ()

fromLabeledIntData2Matrix :: (LabeledIntData, LabeledIntData) -> HMatrix.Matrix Double
fromLabeledIntData2Matrix (tr, te) = matrix
    where transform = map (second toList)
          tr' = transform tr
          te' = transform te
          join = map (uncurry (:))
          matrix = HMatrix.fromLists $ map (map realToFrac) $ join tr' ++ join te'

trSize = 60000 :: Int
teSize = 10000 :: Int
parseMatrix :: HMatrix.Matrix Double -> Maybe MnistData
parseMatrix matrix | HMatrix.rows matrix == (trSize + teSize) = Just (trL, trD, teL, teD)
    where label = HMatrix.takeColumns 1 matrix -- 70000><1
          image = HMatrix.dropColumns 1 matrix -- 70000><784
          trL = HMatrix.flatten $ HMatrix.takeRows trSize label -- 60000
          teL = HMatrix.flatten $ HMatrix.dropRows trSize label -- 10000
          trD = HMatrix.takeRows trSize image -- 60000><784
          teD = HMatrix.dropRows trSize image -- 10000><784
parseMatrix matrix = Nothing

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

