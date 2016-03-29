
module Import.Layout (layout) where

import Import.Base
import {-# SOURCE #-} Foundation

layout :: WidgetT App IO () -> HandlerT App IO Html
