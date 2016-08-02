import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import qualified Docker.Event.Emitter.Internal.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Docker.Event.Emitter.Internal.Tests.tests
    ]
