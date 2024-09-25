module HTTPurple.AWS.Lambda.Streaming where

import Effect (Effect)
import Foreign.Object (Object)
import Node.HTTP.Types (ServerResponse)
import Node.Stream (Writable)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ResponseStream :: Type

toWritable :: forall r. ResponseStream -> Writable r
toWritable = unsafeCoerce

type StreamingResponseMetadata =
  { statusCode :: Int
  , headers :: Object String
  }

foreign import withMetadata :: ResponseStream -> StreamingResponseMetadata -> Effect ResponseStream

toServerResponse :: ResponseStream -> ServerResponse
toServerResponse = unsafeCoerce
