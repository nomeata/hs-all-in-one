import Foo hiding (foo)
import qualified Foo

foo = "Hello"

main = putStrLn $ foo <> Foo.foo
