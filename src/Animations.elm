module Animations exposing
    ( Msg(..)
    , State
    , animatedMenu
    , animatedNav
    , animatedPage
    , animatedSplash
    , animatedSplashLogo
    , init
    , menu
    , resetRemoval
    , setRemoval
    , subscriptions
    , toNewPage
    , update
    , viewUpForRemoval
    )

import Animator
import Animator.Css
import Data exposing (ExpenseOrBandmate(..))
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Page(..))
import String.Interpolate exposing (interpolate)
import Time



-- SUBSCRIPTIONS


subscriptions : State -> Sub Msg
subscriptions state =
    animator |> Animator.toSubscription Tick state


animator : Animator.Animator State
animator =
    Animator.animator
        |> Animator.Css.watching .page
            (\newPage state ->
                { state | page = newPage }
            )
        |> Animator.Css.watching .previousPage
            (\newPage state ->
                { state | previousPage = newPage }
            )
        |> Animator.Css.watching .showMenu
            (\toggleMenu state ->
                { state | showMenu = toggleMenu }
            )
        |> Animator.Css.watching .upForRemoval
            (\item state ->
                { state | upForRemoval = item }
            )



-- MODEL


type alias State =
    { page : Animator.Timeline Page
    , previousPage : Animator.Timeline Page
    , showMenu : Animator.Timeline Bool
    , upForRemoval : Animator.Timeline (Maybe ExpenseOrBandmate)
    }


init : Page -> State
init page =
    { page = Animator.init page
    , previousPage = Animator.init NotFound
    , showMenu = Animator.init False
    , upForRemoval = Animator.init Nothing
    }



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> State -> State
update msg state =
    case msg of
        Tick newTime ->
            Animator.update newTime animator state



-- ANIMATIONS


menu : State -> State
menu state =
    { state
        | showMenu =
            state.showMenu
                |> Animator.go Animator.quickly (not (Animator.current state.showMenu))
    }


setRemoval : State -> ExpenseOrBandmate -> State
setRemoval state expenseOrBandmate =
    { state
        | upForRemoval =
            state.upForRemoval
                |> Animator.go Animator.quickly (Just expenseOrBandmate)
    }


resetRemoval : State -> State
resetRemoval state =
    { state
        | upForRemoval =
            state.upForRemoval
                |> Animator.go Animator.immediately Nothing
    }


toNewPage : Page -> State -> State
toNewPage page state =
    let
        previousPage =
            if List.member page Route.mainPages then
                NotFound

            else if Animator.current state.previousPage == page then
                page

            else
                Animator.current state.page
    in
    { state
        | page = state.page |> Animator.go Animator.verySlowly page
        , previousPage = state.previousPage |> Animator.go Animator.verySlowly previousPage
        , showMenu =
            state.showMenu
                |> Animator.go Animator.quickly False
    }



-- VIEW: ANIMATED WRAPPERS


animatedPage : State -> Page -> List (Html msg) -> Html msg
animatedPage state page =
    Animator.Css.div state.page
        (toPageAnimation page (Animator.current state.previousPage))
        [ class "main__content"
        , classList [ ( "is-open", Animator.current state.page == page ) ]
        ]


animatedSplash : State -> Page -> List (Html msg) -> Html msg
animatedSplash state page =
    let
        previousPage =
            Animator.current state.previousPage
    in
    Animator.Css.div state.page
        [ Animator.Css.opacity <|
            \currentPage ->
                if currentPage == page then
                    Animator.at 1

                else
                    Animator.at 0
        , Animator.Css.style "transform"
            (\x -> interpolate "translateX({0}%)" [ String.fromFloat x ])
            (\currentPage ->
                if currentPage == page || not (List.member previousPage Route.splashPages) then
                    Animator.at 0

                else if currentPage /= previousPage && previousPage /= NotFound then
                    Animator.at -100

                else
                    Animator.at 100
            )
        ]
        [ class "splash__inner" ]


animatedSplashLogo : State -> List (Html msg) -> Html msg
animatedSplashLogo state =
    Animator.Css.div state.page
        [ Animator.Css.transformWith
            { defaultTransformOptions | origin = Animator.Css.offset 0 -140 }
            (\currentPage ->
                case currentPage of
                    Splash ->
                        Animator.Css.scale 1

                    Signin ->
                        Animator.Css.scale 1

                    _ ->
                        Animator.Css.scale 0.6
            )
        ]
        [ class "splash__logo" ]


animatedNav : State -> List (Attribute msg) -> List (Html msg) -> Html msg
animatedNav state =
    Animator.Css.div state.page
        [ Animator.Css.opacity <|
            \currentPage ->
                if not (List.member currentPage Route.splashPages) then
                    Animator.at 1

                else
                    Animator.at 0
        , Animator.Css.style "transform"
            (\x -> interpolate "translateX({0}%)" [ String.fromFloat x ])
            (\currentPage ->
                if currentPage /= Splash then
                    Animator.at 0

                else
                    Animator.at -100
            )
        ]


animatedMenu : State -> List (Attribute msg) -> List (Html msg) -> Html msg
animatedMenu state =
    Animator.Css.div state.showMenu
        [ Animator.Css.opacity <|
            \showMenu ->
                if showMenu then
                    Animator.at 1

                else
                    Animator.at 0
        , Animator.Css.style "transform"
            (\x -> interpolate "translateX({0}%)" [ String.fromFloat x ])
            (\showMenu ->
                if showMenu then
                    Animator.at 0

                else
                    Animator.at -100
            )
        ]


viewUpForRemoval :
    Animator.Timeline (Maybe ExpenseOrBandmate)
    -> Dict String ExpenseOrBandmate
    -> (ExpenseOrBandmate -> List (Html msg))
    -> Html msg
viewUpForRemoval upForRemoval expensesOrBandmates content =
    let
        toContent c =
            List.filter (\a -> Animator.current upForRemoval == Just a) (Dict.values expensesOrBandmates)
                |> List.head
                |> Maybe.map c
                |> Maybe.withDefault []
    in
    animatedToast upForRemoval (toContent content)


animatedToast : Animator.Timeline (Maybe a) -> List (Html msg) -> Html msg
animatedToast maybeA =
    Animator.Css.div maybeA
        [ Animator.Css.opacity <|
            \a ->
                case a of
                    Just _ ->
                        Animator.at 1

                    Nothing ->
                        Animator.at 0
        , Animator.Css.transformWith
            { defaultTransformOptions | origin = Animator.Css.offset 0 100 }
            (\a ->
                case a of
                    Just _ ->
                        Animator.Css.scale 1

                    Nothing ->
                        Animator.Css.scale 0
            )
        ]
        [ class "toast" ]



-- ATTRIBUTE STATES


pageAnimation : Page -> Page -> List (Animator.Css.Attribute Page)
pageAnimation page previousPage =
    [ Animator.Css.opacity <|
        \currentPage ->
            if currentPage == page then
                Animator.at 1

            else
                Animator.at 0
    , Animator.Css.style "transform"
        (\x -> interpolate "translateX({0}%)" [ String.fromFloat x ])
        (\currentPage ->
            if currentPage == page then
                Animator.at 0

            else
                Animator.at -100
        )
    ]


crumbAnimation : Page -> Page -> List (Animator.Css.Attribute Page)
crumbAnimation page previousPage =
    [ Animator.Css.opacity <|
        \currentPage ->
            if currentPage == page then
                Animator.at 1

            else
                Animator.at 0
    , Animator.Css.style "transform"
        (\x -> interpolate "translateX({0}%)" [ String.fromFloat x ])
        (\currentPage ->
            if currentPage == page then
                Animator.at 0

            else if currentPage /= previousPage && previousPage /= NotFound then
                Animator.at -100

            else
                Animator.at 100
        )
    ]


splashAnimation : Page -> Page -> List (Animator.Css.Attribute Page)
splashAnimation page previousPage =
    [ Animator.Css.opacity <|
        \currentPage ->
            if List.member currentPage Route.splashPages then
                Animator.at 1

            else
                Animator.at 0
    , Animator.Css.transformWith
        { defaultTransformOptions | origin = Animator.Css.offset 0 -200 }
        (\currentPage ->
            if List.member currentPage Route.splashPages then
                Animator.Css.scale 1

            else
                Animator.Css.scale 0
        )
    ]



-- HELPERS


toPageAnimation : Page -> Page -> List (Animator.Css.Attribute Page)
toPageAnimation page =
    if List.member page Route.splashPages then
        splashAnimation page

    else if List.member page Route.mainPages then
        pageAnimation page

    else
        crumbAnimation page


defaultTransformOptions : Animator.Css.TransformOptions
defaultTransformOptions =
    { rotationAxis =
        { x = 0
        , y = 0
        , z = 1
        }
    , origin = Animator.Css.offset 0 0
    }
