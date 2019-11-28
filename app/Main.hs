import qualified SortMonoid (run)
import qualified Folds (run)
import qualified ContT (run)

main :: IO ()
main = ContT.run
