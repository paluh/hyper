module Hyper.Node.ServerSpec where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Data.Newtype (unwrap)
import Data.String (length, take)
import Hyper.Node.Server (writeString)
import Node.Buffer (BUFFER, Buffer, toString)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (Writable)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

type WriteDelay = Int

-- | In memory buffer writer. This type is provided so we can
-- | access to it's internal state through `streamBuffer`.
data WritableStream (eff ∷ # Effect)

toWritable ∷ ∀ eff. WritableStream eff → Writable () eff
toWritable = unsafeCoerce

foreign import memoryWritableStreamImpl ∷ ∀ eff. EffFn1 eff WriteDelay (WritableStream eff)

memoryWritableStream ∷ ∀ eff. WriteDelay → Eff eff (WritableStream eff)
memoryWritableStream = runEffFn1 memoryWritableStreamImpl

foreign import streamBufferImpl ∷ ∀ eff. EffFn1 eff (WritableStream eff) Buffer

streamBuffer ∷ ∀ eff. WritableStream eff → Eff eff Buffer
streamBuffer = runEffFn1 streamBufferImpl

buildTestString :: forall eff. Int -> Aff eff String
buildTestString totalLength = do
  let
    str = go "a" 1
  length str `shouldEqual` totalLength
  pure str
 where
  go str len
    | len * 2 < totalLength = go (str <> str) (len * 2)
    | otherwise = str <> take (totalLength - len) str

testWriter :: forall eff. String -> Aff ( buffer :: BUFFER | eff) Unit
testWriter str = do
  output ← liftEff $ memoryWritableStream 10
  let writer = unwrap $ writeString UTF8 str
  writer (toWritable output)
  buffer ← liftEff $ streamBuffer output
  str' ← liftEff $ toString UTF8 buffer
  str' `shouldEqual` str

spec :: forall eff. Spec (buffer ∷ BUFFER | eff) Unit
spec =
  describe "Hyper.Node.Server" do
    describe "writeString" do
      it "handles empty string" do
        testWriter ""

      it "handles short (< 16 KB) string" do
        str ← buildTestString 3000
        testWriter str

      it "handles long (> 64 KB) string" do
        str ← buildTestString 65000
        testWriter str
