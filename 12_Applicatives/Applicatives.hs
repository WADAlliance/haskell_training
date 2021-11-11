main = print "ore"

data Ore a b   = Leftt a | Rightt b 

instance (Show a, Show b) => Show (Ore a b) where
  show (Leftt x) = show x
  show (Rightt x) = show x


ore :: Ore [Char] b
ore = Leftt "test"