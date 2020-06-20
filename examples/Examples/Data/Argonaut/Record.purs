module Examples.Data.Argonaut.Record where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson, fromArray, jsonEmptyObject, (.!=), (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)

newtype BlogPost = BlogPost
  { id :: Int
  , title :: String
  , content :: String
  , publishDate :: Maybe String
  , categories :: String
  }

instance decodeJsonBlogPost :: DecodeJson BlogPost where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    title <- obj .: "title"
    content <- obj .: "content"
    publishDate <- obj .:? "publish_date"
    categories <- obj .:? "categories" .!= ""
    pure $ BlogPost { id, title, content, publishDate, categories }

instance encodeJsonBlogPost :: EncodeJson BlogPost where
  encodeJson (BlogPost post)
     = "id" := post.id
    ~> "title" := post.title
    ~> "content" := post.content
    ~> "publish_date" :=? post.publishDate
    ~>? "categories" := post.categories
    ~> jsonEmptyObject

type BlogPostArray = Array BlogPost

decodeBlogPostArray :: Json -> Either JsonDecodeError BlogPostArray
decodeBlogPostArray json = decodeJson json >>= traverse decodeJson

encodeBlogPostArray :: BlogPostArray -> Json
encodeBlogPostArray bpa = fromArray $ encodeJson <$> bpa
