-- | Pure packet handling for the Stream Deck MK2 (USB product ID 0x0080).
module StreamDeck.Protocol
    ( Key, keyAt, keyIndex, keyPosition
    , KeyEvent (..), KeyState, idleKeyState, decodeKeyReport, keyEvents
    , imageReports
    ) where

import qualified Data.ByteString as BS

-- | A valid key, numbered left-to-right, top-to-bottom.
newtype Key = Key Int deriving (Eq, Ord, Show)

-- | Zero-based row (0..2) and column (0..4).
keyAt :: Int -> Int -> Either String Key
keyAt row column
    | row >= 0 && row < 3 && column >= 0 && column < 5 =
        Right (Key (row * 5 + column))
    | otherwise = Left "MK2 key position must be within rows 0..2 and columns 0..4"

keyIndex :: Key -> Int
keyIndex (Key index) = index

keyPosition :: Key -> (Int, Int)
keyPosition (Key index) = index `divMod` 5

data KeyEvent = Pressed Key | Released Key deriving (Eq, Show)

newtype KeyState = KeyState [Bool] deriving (Eq, Show)

idleKeyState :: KeyState
idleKeyState = KeyState (replicate 15 False)

-- | Decode the 4-byte header and 15 key states. Trailing HID padding is allowed.
decodeKeyReport :: BS.ByteString -> Either String KeyState
decodeKeyReport report
    | BS.length report < 19 = Left "Truncated MK2 key report"
    | BS.take 4 report /= BS.pack [0x01, 0x00, 0x0f, 0x00] =
        Left "Unexpected MK2 key report header"
    | BS.any (> 1) states = Left "Invalid MK2 key state (expected 0 or 1)"
    | otherwise = Right (KeyState (map (== 1) (BS.unpack states)))
  where
    states = BS.take 15 (BS.drop 4 report)

-- | Emit transitions only, including simultaneous changes in key order.
keyEvents :: KeyState -> KeyState -> [KeyEvent]
keyEvents (KeyState previous) (KeyState current) =
    [ if down then Pressed (Key index) else Released (Key index)
    | (index, (wasDown, down)) <- zip [0..] (zip previous current)
    , wasDown /= down
    ]

-- | Split an encoded, device-oriented JPEG into padded output reports.
imageReports :: Key -> BS.ByteString -> Either String [BS.ByteString]
imageReports key image
    | BS.null image = Left "Key image must not be empty"
    | BS.length image > 1016 * 65536 = Left "Key image exceeds the MK2 page limit"
    | otherwise = Right (pages 0 image)
  where
    pages :: Int -> BS.ByteString -> [BS.ByteString]
    pages page remaining
        | BS.null remaining = []
        | otherwise =
            let (payload, rest) = BS.splitAt 1016 remaining
                size = BS.length payload
                header = BS.pack
                    [ 0x02, 0x07, fromIntegral (keyIndex key)
                    , if BS.null rest then 1 else 0
                    , fromIntegral size, fromIntegral (size `div` 256)
                    , fromIntegral page, fromIntegral (page `div` 256)
                    ]
                report = header <> payload <> BS.replicate (1016 - size) 0
            in report : pages (page + 1) rest
