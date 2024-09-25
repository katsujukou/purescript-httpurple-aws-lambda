module HTTPurple.AWS.Lambda.Streaming where

import Prelude

import Effect (Effect)
import Node.HTTP.Types (ServerResponse)
import Node.Stream (Writable)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ResponseStream :: Type

toWritable :: forall r. ResponseStream -> Writable r
toWritable = unsafeCoerce

foreign import withMetadata :: forall r. ResponseStream -> { | r } -> Effect ResponseStream

toServerResponse :: ResponseStream -> ServerResponse
toServerResponse = unsafeCoerce
