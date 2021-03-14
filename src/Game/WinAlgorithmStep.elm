module Game.WinAlgorithmStep exposing (WinAlgorithmStep, steps)

import Binary exposing (Bits)
import Coordinate exposing (Coordinate)


type alias WinAlgorithmStep =
    { coordinate : Coordinate
    , win : Bits
    , turnOff : Bits
    }


b =
    Binary.fromIntegers


{-| Each step is visiting the given coordinate. We check for the player's mark.
If the mark is not found then we turn off win path possibilities using a binary
`and` and the turnOff value. If the mark is found then we check for a win from
the current possibilities and the win value, again using `and`.
-}
steps : List WinAlgorithmStep
steps =
    [ { coordinate = ( 0, 0 ), win = b [ 0 ], turnOff = b [ 1, 0, 1, 1, 0, 1, 1 ] }
    , { coordinate = ( 1, 1 ), win = b [ 0 ], turnOff = b [ 1, 0, 1, 1, 0, 1 ] }
    , { coordinate = ( 2, 2 ), win = b [ 1, 0, 0, 0, 0, 0, 0, 0 ], turnOff = b [ 1, 1, 1, 0, 1, 1, 0 ] }
    , { coordinate = ( 2, 0 ), win = b [ 0 ], turnOff = b [ 1, 0, 0, 1, 1, 1, 1, 0 ] }
    , { coordinate = ( 0, 2 ), win = b [ 1, 0, 0, 0, 0, 0, 0 ], turnOff = b [ 1, 0, 1, 1, 0, 0, 1, 1 ] }
    , { coordinate = ( 1, 0 ), win = b [ 1, 0, 0, 0, 0, 0 ], turnOff = b [ 1, 1, 0, 1, 1, 1, 0, 1 ] }
    , { coordinate = ( 0, 1 ), win = b [ 1, 0, 0 ], turnOff = b [ 1, 1, 1, 0, 1, 0, 1, 1 ] }
    , { coordinate = ( 2, 1 ), win = b [ 1, 0, 0, 0, 1 ], turnOff = b [ 1, 1, 1, 0, 1, 1, 1, 0 ] }
    , { coordinate = ( 1, 2 ), win = b [ 1, 0, 1, 0 ], turnOff = b [ 1, 1, 1, 1, 0, 1, 0, 1 ] }
    ]
