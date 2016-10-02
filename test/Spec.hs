import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck               as QC

import qualified Docker.Event.Emitter.Internal.Tests
import qualified Docker.Event.Emitter.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Docker.Event.Emitter.Internal.Tests.tests
    , Docker.Event.Emitter.Tests.tests
    ]
