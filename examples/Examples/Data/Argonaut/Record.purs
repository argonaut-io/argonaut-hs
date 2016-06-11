module Examples.Data.Argonaut.Record where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, Json, encodeJson, fromArray, decodeJson, jsonEmptyObject, (~>), (:=), (.?))
import Data.Either (Either)
import Data.Traversable (traverse)

newtype BlogPost = BlogPost
  { id :: Int
  , title :: String
  , categories :: String
  , content :: String
  }

instance decodeJsonBlogPost :: DecodeJson BlogPost where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    title <- obj .? "title"
    categories <- obj .? "categories"
    content <- obj .? "content"
    pure $ BlogPost { id, title, categories, content }

instance encodeJson :: EncodeJson BlogPost where
  encodeJson (BlogPost post)
     = "id" := post.id
    ~> "title" := post.title
    ~> "categories" := post.categories
    ~> "content" := post.content
    ~> jsonEmptyObject

type BlogPostArray = Array BlogPost

decodeBlogPostArray :: Json -> Either String BlogPostArray
decodeBlogPostArray json = decodeJson json >>= traverse decodeJson

encodeBlogPostArray :: BlogPostArray -> Json
encodeBlogPostArray bpa = fromArray $ encodeJson <$> bpa
