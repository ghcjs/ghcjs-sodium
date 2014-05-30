{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Alder.Html.Elements
    ( div
    , span
    ) where

import           Alder.Html.Internal

-- | The HTML @div@ element.
div :: Html -> Html
div = Parent "div"

-- | The HTML @span@ element.
span :: Html -> Html
span = Parent "span"
