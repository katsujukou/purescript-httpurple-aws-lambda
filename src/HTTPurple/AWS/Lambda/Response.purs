module HTTPurple.AWS.Lambda.Response where

import Prelude

import Data.Array (foldl)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import HTTPurple as HTTPurple
import HTTPurple.Headers as Headers
import Node.Buffer.Class as Buffer
import Node.Encoding (Encoding(..))
import Node.EventEmitter as EE
import Node.Stream (dataH, endH)
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

extractBody :: HTTPurple.Response -> Aff (Maybe String)
extractBody { writeBody } = makeAff \done -> do
  stream <- liftEffect Stream.newPassThrough

  -- Consuming body written to stream
  chunks <- liftEffect $ Ref.new []
  stream # EE.on_ dataH \chunk -> do
    Ref.modify_ (\buf -> Array.snoc buf chunk) chunks
  stream # EE.on_ endH do
    body <- Buffer.toString UTF8 =<< Buffer.concat =<< Ref.read chunks
    done $ Right (if body == "" then Nothing else Just body)

  launchAff_ $ writeBody (unsafeCoerce stream)
  pure nonCanceler

foldHeaders :: Headers.ResponseHeaders -> Maybe (Object String)
foldHeaders (Headers.ResponseHeaders headers)
  | Map.isEmpty headers = Nothing
  | otherwise = headers
      # Map.toUnfoldable
      # foldl (\prev (Tuple k v) -> Object.insert (unwrap k) (Str.joinWith ";" v) prev) Object.empty
      # Just