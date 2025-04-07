module Mouse where
-- depuis Valeraaz
import SDL
import Data.Int
import Linear.Affine

-- on pourrait eventuellement  la fonction depuis SDL.Input.Mouse :
-- getMouseLocation :: MonadIO m => m (Point V2 CInt)
getMouseLocation :: Event -> Maybe (V2 Int)
getMouseLocation event =
    case eventPayload event of
        MouseButtonEvent mouseButton ->
            let P coordinates = fmap fromIntegral (mouseButtonEventPos mouseButton) in
                Just coordinates
        _ -> Nothing