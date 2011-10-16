module Main where 

import Control.Monad (msum)
import Data.Char (toLower)
import Happstack.Server (Method(GET, POST), methodM, nullConf, simpleHTTP, dir, ok, path)

main :: IO ()
main = simpleHTTP nullConf $ msum
       [ do methodM GET 
            ok $ "You did a GET request.\n"
       , do methodM POST
            ok $ "You did a POST request.\n"
       , dir "foo" $ do methodM GET
                        ok $ "You did a GET request on /foo\n"
       ]
