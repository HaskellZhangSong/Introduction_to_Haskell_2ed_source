import Data.Proxy


-- 无法携带kind信息
foo :: Proxy []
foo = Proxy
