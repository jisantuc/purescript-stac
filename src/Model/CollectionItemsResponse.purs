module Model.CollectionItemsResponse where

import Model.Item (Item)
import Model.Link (Link)

-- | This type is an alias for a GeoJSON FeatureCollection without the type
-- | field, since it's a constant anyway and I'm not going to consume it
-- | anywhere. Any geojson-consuming methods on items responses will be
-- | passed off with the appropriate type field included, for example,
-- | to turf.
type CollectionItemsResponse
  = { features :: Array Item
    , links :: Array Link
    }
