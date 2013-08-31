import Data.Time.Calendar (Day(..),fromGregorian,toGregorian)
import Data.Time.Clock (getCurrentTime,utctDay)
import Yahoo
import Classifier

main = do
    let startDate = fromGregorian 2008 01 01
    today <- getCurrentTime >>= return . utctDay
    mquotes <- getHistoricalQuote "MSFT" startDate today Daily
    case mquotes of
        Nothing -> error "no historical"
        Just l -> return $ onlyAdjCloses l


onlyAdjCloses :: [HistoricalQuote] -> [(Day,Double)]
onlyAdjCloses = map (\q -> (date q, adjclose q))

-- LastPrice, Balance, Quantity
type OrderState = (Double, Double, Double)

sharesPerOrder = 100.0
pctDrop = 0.95

processQuote :: OrderState -> (Day,Double) -> OrderState 
processQuote (px,bal,qty) (_,currentPx)
    | qty == 0                  = (currentPx,newBalance, shares)
    | newBalance < 0            = (px,bal,qty)
    | currentPx/px <= pctDrop   = (currentPx, newBalance, qty + shares) 
    | otherwise                 = (px,bal, qty)
    where cost = currentPx * sharesPerOrder
          shares = sharesPerOrder
          newBalance = bal - cost

