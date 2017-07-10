import Data.Tree
import Data.Tree.Zipper
import Data.Function
import Data.Maybe

directory :: Tree String
directory = Node "Home" 
                 [Node "Picture" [Node "travel" [] , Node "family" []], 
                  Node "Video" [Node "Fast and Furious" [], Node "True Lies" []], 
                  Node "Music" [Node "My love" [], Node "Destiny" []]] 
 
