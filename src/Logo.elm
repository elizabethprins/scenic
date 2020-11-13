module Logo exposing (view)

import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Svg msg
view =
    svg
        [ width "301", height "218", viewBox "0 0 301 218" ]
        [ defs []
            [ radialGradient
                [ id "gradient"
                , gradientUnits "userSpaceOnUse"
                , cx "30%"
                , cy "0%"
                , r "330"
                ]
                [ stop
                    [ offset "0%", stopColor "#FFF0FF" ]
                    []
                , stop
                    [ offset "10%", stopColor "#FFD4FF" ]
                    []
                , stop
                    [ offset "20%", stopColor "#E8B7FF" ]
                    []
                , stop
                    [ offset "30%", stopColor "#C89CFF" ]
                    []
                , stop
                    [ offset "40%", stopColor "#A781FF" ]
                    []
                , stop
                    [ offset "50%", stopColor "#8566FF" ]
                    []
                , stop
                    [ offset "60%", stopColor "#614DFF" ]
                    []
                , stop
                    [ offset "70%", stopColor "#3333FF" ]
                    []
                ]
            ]
        , Svg.path
            [ fill "url(#gradient)"
            , d "M191.911.456l-55.393 111.125 54.02 106.347h109.846L191.911.456zM0 217.928h145.66L72.83 71.917 0 217.928z"
            ]
            []
        ]
