import Foo hiding (foo)
import qualified Foo

import Data.ByteString.Char8

foo = "Hello"

main = Prelude.putStrLn $ unpack (Data.ByteString.Char8.pack foo) <> Foo.foo
