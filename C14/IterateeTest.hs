import qualified Data.Iteratee as I
import Data.Iteratee.Iteratee (($=),(=$),mapChunks, enumPure1Chunk)
import Data.Iteratee.IO
import qualified Data.ByteString as DB
import Data.ByteString (ByteString)
import Data.ListLike hiding (map)
import Control.Monad
import Data.Char

-- alternative :: I.Iteratee
drop1keep1 :: (I.Nullable s, ListLike s el, Monad m) =>  I.Iteratee s m el
drop1keep1 = I.drop 1 >> I.head

alternatives :: (I.Nullable s, ListLike s el, Monad m) =>  I.Iteratee s m [el]
alternatives = replicateM 5 drop1keep1

-- > I.enumPure1Chunk [1..10] alternatives >>= I.run

byteCounter :: Monad m => I.Iteratee ByteString m Int
byteCounter = I.length

countBytes = do
    i' <- (enumFile 8192 "Wreq.hs" >=> enumFile 8192 "Wreq.hs") byteCounter
    result <- I.run i'
    print result

-- > (I.enumPure1Chunk [1..100] $= I.take 10) alternatives >>= I.run

