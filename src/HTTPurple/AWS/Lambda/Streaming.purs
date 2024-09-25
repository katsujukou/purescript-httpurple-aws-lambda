module HTTPurple.AWS.Lambda.Streaming where

import Prelude

import Effect (Effect)
import Node.HTTP.Types (ServerResponse)
import Node.Stream (Writable)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ResponseStream :: Type

toWritable :: forall r. ResponseStream -> Writable r
toWritable = unsafeCoerce

foreign import setStatusCode :: Int -> ResponseStream -> Effect Unit

foreign import setHeader :: String -> String -> ResponseStream -> Effect Unit

toServerResponse :: ResponseStream -> ServerResponse
toServerResponse = unsafeCoerce
