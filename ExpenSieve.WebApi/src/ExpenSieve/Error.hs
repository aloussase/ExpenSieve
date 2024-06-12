module ExpenSieve.Error where

import           Data.Text (Text)

data Error = NotFound Text | Conflict Text | Unexpected Text
  deriving stock Show
