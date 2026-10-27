module Main where

import StreamDeck
import qualified StreamDeck.FontAwesome as FA
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    fonts <- FA.loadFontAwesome
    local <- loadLocalClockZone
    london <- loadClockZone "Europe/London"
    sydney <- loadClockZone "Australia/Sydney"
    weatherKey <- position 0 0
    clockKey <- position 0 1
    commandsKey <- position 0 4
    backKey <- position 2 4
    pragueKey <- position 0 0
    londonKey <- position 0 1
    sydneyKey <- position 0 2
    launcherKey <- position 0 0
    messageKey <- position 0 1
    let icon glyph = either fail pure (FA.renderAwesomeIcon fonts defaultIconStyle glyph)
        label text = either fail pure (renderText (FA.solidFont fonts) defaultIconStyle text)
        clock zone color = refreshEvery 1 $
            clockImage zone (FA.solidFont fonts) defaultIconStyle { foreground = color, padding = 4 } "%H%M"
    weatherIcon <- icon FA.cloudSun
    commandsIcon <- icon FA.terminal
    backIcon <- icon (FA.AwesomeIcon FA.Solid '\xf060')
    pragueIcon <- label "PRAGUE"
    londonIcon <- label "LONDON"
    sydneyIcon <- label "SYDNEY"
    messageIcon <- icon (FA.AwesomeIcon FA.Solid '\xf0f3')
    putStrLn "Top-left: weather page. Top-right: commands page. Bottom-right arrow: return to main."
    putStrLn "Clock at (0,1): click to cycle LOCAL (green), LONDON (yellow), SYDNEY (blue)."
    putStrLn "Clock format is HHMM. Its selected zone is remembered when you return to main."
    putStrLn "Command buttons: launcher (Super+d) and bell (message popup via zenity)."
    putStrLn "Ctrl-C exits."
    runPages "main"
        [ Page "main"
            [ button "weather" weatherKey weatherIcon (SwitchPage "weather")
            , statefulButton "clock" clockKey "local"
                [ state "local" (clock local (RGB 30 215 96))
                    (Sequence [NextState, RunIO (putStrLn "Clock: London")])
                , state "london" (clock london (RGB 255 210 40))
                    (Sequence [NextState, RunIO (putStrLn "Clock: Sydney")])
                , state "sydney" (clock sydney (RGB 60 170 255))
                    (Sequence [NextState, RunIO (putStrLn "Clock: local")])
                ]
            , button "commands" commandsKey commandsIcon (SwitchPage "commands")
            ]
        , Page "weather"
            [ button "prague" pragueKey pragueIcon (RunIO (putStrLn "Weather city: Prague"))
            , button "london" londonKey londonIcon (RunIO (putStrLn "Weather city: London"))
            , button "sydney" sydneyKey sydneyIcon (RunIO (putStrLn "Weather city: Sydney"))
            , button "back" backKey backIcon (SwitchPage "main")
            ]
        , Page "commands"
            [ button "launcher" launcherKey commandsIcon (SendKeys (super 'd'))
            , button "message" messageKey messageIcon
                (Execute "zenity"
                    ["--info", "--title=Stream Deck", "--text=Hello from your Stream Deck!", "--no-wrap"])
            , button "back" backKey backIcon (SwitchPage "main")
            ]
        ]

position :: Int -> Int -> IO Key
position row column = either fail pure (keyAt row column)
