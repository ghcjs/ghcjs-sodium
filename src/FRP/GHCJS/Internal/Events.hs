-- | HTML events.
module FRP.GHCJS.Internal.Events
    ( Event(..)
    ) where

import qualified GHCJS.DOM.Event as DOM

-- | HTML event types.
class Event e where
    -- | Extract relevant information out of a 'DOM.Event'.
    extractEvent :: DOM.Event -> IO e
