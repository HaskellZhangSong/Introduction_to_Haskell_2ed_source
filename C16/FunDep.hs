{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
class Collection e ce | ce -> e where
    empty :: ce
    insert :: e -> ce -> ce
    member :: e -> ce -> Bool
