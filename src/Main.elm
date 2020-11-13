module Main exposing (Model, Msg(..), init, main, update, view)

import Animations
import Animator
import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Color
import Data exposing (Bandmate, Expense, ExpenseOrBandmate(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onFocus, onInput, onSubmit)
import Json.Decode as Decode
import Logo
import Material.Icons as Filled exposing (lock, lock_open)
import Material.Icons.Types exposing (Coloring(..))
import QR
import Route exposing (Page(..))
import String.Interpolate exposing (interpolate)
import Task
import Time
import Url exposing (Url)



-- MAIN


main : Program Int Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map AnimationsMsg (Animations.subscriptions model.animations)



-- MODEL


type alias Model =
    { navKey : Navigation.Key
    , today : Date
    , userConfig : UserConfig
    , you : Bandmate
    , paymentConfirmed : Bool
    , animations : Animations.State
    , bandmates : Dict String Bandmate
    , expenses : Dict String Expense
    , fee : Float
    , showAddBandmate : Bool
    , newBandmateName : String
    , newBandmateSplit : String
    , activeHistoryItem : Maybe ( Date, Dict String Expense )
    , newExpense : Maybe Expense
    , address : Data.Address
    }


type InputType
    = Text
    | Number
    | Password


type alias Total =
    { includingFee : Float
    , excludingFee : Float
    }


type alias UserConfig =
    { username : String
    , password : String
    , email : String
    , phone : String
    }


init : Int -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init now url navKey =
    let
        you =
            Dict.get "Scott Pilgrim" Data.bandmates
                |> Maybe.withDefault Data.bandmateEmpty

        zone =
            -- NYC time: GMT-5
            Time.customZone (-5 * 60) []

        today =
            Date.fromPosix zone (Time.millisToPosix now)

        fee =
            0.0025
    in
    ( { navKey = navKey
      , today = today
      , userConfig = UserConfig "" "" "" ""
      , you = you
      , paymentConfirmed = False
      , animations = Animations.init (Route.toPage url)
      , bandmates = Data.bandmates
      , expenses = Data.expenses
      , fee = fee
      , showAddBandmate = False
      , newBandmateName = ""
      , newBandmateSplit = ""
      , activeHistoryItem = Nothing
      , newExpense = Nothing
      , address = Data.addressEmpty
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | FillFakeLogin
    | EmptyFakeLogin
    | AnimationsMsg Animations.Msg
    | ClickedLink Browser.UrlRequest
    | UrlChanged Url
    | ToggleMenu
    | SaveBandmateSplit String String
    | ToggleAddBandmate
    | SaveNewBandmate
    | OnNameInput String
    | OnSplitInput String
    | ToggleSplitLock String
    | SplitEvenly
    | SetToRemove ExpenseOrBandmate
    | Remove (Maybe ExpenseOrBandmate)
    | ConfirmPayment
    | ActivateHistoryItem ( Date, Dict String Expense )
    | SetNewExpense Expense
    | OnExpenseTitleInput String
    | OnExpenseAmountInput String
    | PopulateAddressFields Bool
    | PayBy (Maybe String)
    | SubmitExpense
    | FocusOnNext Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FillFakeLogin ->
            let
                userConfig =
                    { username = "scottpilgrim"
                    , password = "supersecret"
                    , email = "sonic@boom.com"
                    , phone = "123 456 7890"
                    }
            in
            ( { model | userConfig = userConfig }
            , Cmd.none
            )

        EmptyFakeLogin ->
            ( { model | userConfig = UserConfig "" "" "" "" }
            , Cmd.none
            )

        AnimationsMsg msg_ ->
            ( { model | animations = Animations.update msg_ model.animations }
            , Cmd.none
            )

        ClickedLink request ->
            case request of
                Browser.Internal url ->
                    ( { model | animations = Animations.toNewPage (Route.toPage url) model.animations }
                    , Navigation.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Navigation.load url
                    )

        UrlChanged url ->
            ( { model | animations = Animations.toNewPage (Route.toPage url) model.animations }
            , Cmd.none
            )

        ToggleMenu ->
            ( { model | animations = Animations.menu model.animations }
            , Cmd.none
            )

        SaveBandmateSplit bandmateId split ->
            let
                newSplit =
                    Maybe.withDefault 0 (String.toFloat split)
            in
            ( { model
                | bandmates =
                    Dict.update bandmateId
                        (Maybe.map (\x -> { x | split = newSplit }))
                        model.bandmates
              }
            , Cmd.none
            )

        ToggleAddBandmate ->
            ( { model | showAddBandmate = not model.showAddBandmate }, Cmd.none )

        SaveNewBandmate ->
            ( { model
                | showAddBandmate = False
                , bandmates =
                    Dict.insert model.newBandmateName
                        (Bandmate model.newBandmateName
                            (Maybe.withDefault 0 (String.toFloat model.newBandmateSplit))
                            False
                            ""
                        )
                        model.bandmates
              }
            , Cmd.none
            )

        OnNameInput input ->
            ( { model | newBandmateName = input }, Cmd.none )

        OnSplitInput input ->
            ( { model | newBandmateSplit = input }, Cmd.none )

        ToggleSplitLock bandmateId ->
            ( { model
                | bandmates =
                    Dict.update bandmateId
                        (Maybe.map (\x -> { x | locked = not x.locked }))
                        model.bandmates
              }
            , Cmd.none
            )

        SplitEvenly ->
            let
                ( locked, unlocked ) =
                    Dict.partition (\id rm -> rm.locked == True) model.bandmates

                leftoverShare =
                    100 - toTotalSplits locked

                evenSplit =
                    leftoverShare / toFloat (Dict.size unlocked)

                newUnlocked =
                    Dict.map (\id rm -> { rm | split = evenSplit }) unlocked
            in
            ( { model
                | bandmates = Dict.union locked newUnlocked
              }
            , Cmd.none
            )

        SetToRemove expenseOrBandmate ->
            ( { model | animations = Animations.setRemoval model.animations expenseOrBandmate }
            , Cmd.none
            )

        Remove maybeExpenseOrBandmate ->
            let
                ( newExpenses, newBandmates ) =
                    case maybeExpenseOrBandmate of
                        Just (IsExpense expense) ->
                            ( Dict.remove expense.title model.expenses, model.bandmates )

                        Just (IsBandmate bandmate) ->
                            ( model.expenses, Dict.remove bandmate.name model.bandmates )

                        Nothing ->
                            ( model.expenses, model.bandmates )
            in
            ( { model
                | animations = Animations.resetRemoval model.animations
                , expenses = newExpenses
                , bandmates = newBandmates
              }
            , Cmd.none
            )

        ConfirmPayment ->
            ( { model | paymentConfirmed = True }, Cmd.none )

        ActivateHistoryItem clicked ->
            ( { model | activeHistoryItem = Just clicked }, Cmd.none )

        SetNewExpense expense ->
            ( { model | newExpense = Just expense }, Cmd.none )

        OnExpenseTitleInput input ->
            ( mapNewExpense model (\a -> { a | title = input })
            , Cmd.none
            )

        OnExpenseAmountInput input ->
            ( mapNewExpense model
                (\a -> { a | amount = Maybe.withDefault 0 (String.toFloat input) })
            , Cmd.none
            )

        PopulateAddressFields isPopulated ->
            let
                newAddress =
                    if isPopulated then
                        Data.addressFilled

                    else
                        Data.addressEmpty
            in
            ( { model | address = newAddress }, Cmd.none )

        PayBy maybeBandmate ->
            let
                bandmate =
                    Maybe.andThen (\bm -> Dict.get bm model.bandmates) maybeBandmate
                        |> Maybe.withDefault Data.bandmateEmpty
            in
            ( mapNewExpense model
                (\a -> { a | payedBy = bandmate })
            , Cmd.none
            )

        SubmitExpense ->
            let
                newExpenses =
                    case model.newExpense of
                        Just expense ->
                            Dict.insert expense.title expense model.expenses

                        Nothing ->
                            model.expenses
            in
            ( { model | expenses = newExpenses, address = Data.addressEmpty, newExpense = Nothing }
            , Cmd.none
            )

        FocusOnNext index input ->
            let
                cmd =
                    case String.toInt input of
                        Just i ->
                            Task.attempt (\_ -> NoOp) <|
                                Browser.Dom.focus (String.fromInt <| index + 1)

                        Nothing ->
                            Cmd.none
            in
            ( model, cmd )


mapNewExpense : Model -> (Expense -> Expense) -> Model
mapNewExpense model f =
    let
        new =
            case model.newExpense of
                Just expense ->
                    Just (f expense)

                Nothing ->
                    model.newExpense
    in
    { model | newExpense = new }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        view_ page viewContent =
            Animations.animatedPage model.animations page [ viewContent model ]
    in
    { title = "Scenic"
    , body =
        [ main_ [ class "main" ]
            [ viewNav model
            , div [ class "page" ]
                [ view_ Splash viewSplash
                , view_ Signup viewSignup
                , view_ Signin viewSignin
                , view_ AddressInfo viewAddressInfo
                , view_ CodeScan viewCodeScan
                , view_ ChooseNotifications viewChooseNotifications
                , view_ Overview viewOverview
                , view_ Charge viewCharge
                , view_ ThankYou viewThankYou
                , view_ AreYouSure viewAreYouSure
                , view_ Bandmates viewOverviewBandmates_
                , view_ ManageBandmates viewManageBandmates
                , view_ Expenses viewOverviewExpenses_
                , view_ ManageExpenses viewManageExpenses
                , view_ PickCategory viewPickCategory
                , view_ FillExpenseForm viewFillExpenseForm
                , view_ History viewHistory
                , view_ HistoryDetail viewHistoryDetail
                , view_ Support viewSupport
                , view_ Settings viewSettings
                , view_ SettingsText viewSettingsPage
                , view_ SupportText viewSupportPage
                ]
            ]
        ]
    }



-- VIEW NAV


viewNav : Model -> Html Msg
viewNav model =
    let
        icon =
            if Animator.current model.animations.showMenu then
                Filled.clear 35 (Color colorBlue)

            else
                Filled.account_circle 35 (Color colorBlue)
    in
    Animations.animatedNav model.animations
        [ class "nav"
        , classList [ ( "is-open", Animator.current model.animations.showMenu ) ]
        ]
        [ button [ class "nav-toggle", onClick ToggleMenu ]
            [ icon ]
        , h2 [] [ a [ href <| Route.toUrl Overview ] [ text "scenic" ] ]
        , viewMenu model
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    Animations.animatedMenu model.animations
        [ class "nav__menu" ]
        [ div []
            [ Filled.account_circle 75 (Color colorBlueBlack)
            , h3 [] [ text model.you.name ]
            , p [] [ text model.you.email ]
            ]
        , ul [ class "nav__menu__list" ] <|
            List.map (viewMenuItem (Animator.current model.animations.page))
                [ ( "Home", Overview, Filled.home )
                , ( "Expenses", Expenses, Filled.insert_chart )
                , ( "Bandmates", Bandmates, Filled.account_box )
                , ( "History", History, Filled.favorite )
                , ( "Support", Support, Filled.shopping_basket )
                , ( "Settings", Settings, Filled.settings )
                , ( "Logout", Splash, Filled.power_settings_new )
                ]
        ]


viewMenuItem : Page -> ( String, Page, Material.Icons.Types.Icon msg ) -> Html msg
viewMenuItem currentPage ( title, page, icon ) =
    let
        ( className, coloring, arrow ) =
            if currentPage == page then
                ( "is-active"
                , Color colorBlue
                , Filled.keyboard_arrow_left
                )

            else
                ( ""
                , Color <| colorGrey
                , Filled.keyboard_arrow_right
                )
    in
    li [ class className ]
        [ a [ href <| Route.toUrl page ]
            [ icon 25 coloring
            , text title
            , arrow 25 coloring
            ]
        ]



-- VIEW SPLASH PAGES


viewSplash : Model -> Html Msg
viewSplash model =
    div [ class "splash" ]
        [ Animations.animatedSplashLogo model.animations
            [ Logo.view
            , h1 [] [ text "scenic" ]
            , h2 [] [ text "trying out transitions" ]
            ]
        , Animations.animatedSplash model.animations
            Splash
            [ p []
                [ text """
                    Let's try some animated page transitions!
                    Starting off with a typical signin flow."""
                ]
            , div [ class "splash__buttons" ]
                [ a [ class "btn", href <| Route.toUrl Signup ]
                    [ span []
                        [ text "Sign up manually" ]
                    ]
                , a [ class "btn btn--soft", href <| Route.toUrl CodeScan ]
                    [ span [] [ text "Join through invite" ] ]
                , p [] [ text "Already a member?" ]
                , a [ class "btn btn--clear", href <| Route.toUrl Signin ]
                    [ span [] [ text "Sign in" ] ]
                ]
            ]
        ]


viewSignup : Model -> Html Msg
viewSignup ({ userConfig } as model) =
    Animations.animatedSplash model.animations
        Signup
        [ p [] [ text "Create your user profile" ]
        , Html.form
            [ class "fake-form" ]
            [ viewInputFake Text "username" userConfig.username [ onFocus FillFakeLogin ]
            , viewInputFake Text "email" userConfig.email []
            , viewInputFake Text "phone" userConfig.phone []
            , viewInputFake Password "choose password" userConfig.password []
            , viewInputFake Password "confirm password" userConfig.password []
            , a
                [ class "btn"
                , href <| Route.toUrl AddressInfo
                , onClick EmptyFakeLogin
                ]
                [ span [] [ text "Next" ] ]
            ]
        ]


viewSignin : Model -> Html Msg
viewSignin ({ userConfig } as model) =
    Animations.animatedSplash model.animations
        Signin
        [ Html.form
            [ class "fake-form" ]
            [ viewInputFake Text "username" userConfig.username [ onFocus FillFakeLogin ]
            , viewInputFake Password "password" userConfig.password []
            , a
                [ class "btn"
                , href <| Route.toUrl Overview
                , onClick EmptyFakeLogin
                ]
                [ span [] [ text "sign in" ] ]
            ]
        ]


viewCodeScan : Model -> Html Msg
viewCodeScan model =
    Animations.animatedSplash model.animations
        CodeScan
        [ p [] [ text "Do you have an invite? Fill in a six digit code below or scan a QR code." ]
        , div [ class "splash__buttons enter-code" ]
            [ div [ class "splash__enter-code" ] <|
                List.indexedMap
                    (\i _ ->
                        viewInputFake Number
                            (String.fromInt i)
                            ""
                            [ onInput (FocusOnNext i) ]
                    )
                    (List.range 0 5)
            , QR.view
            , a [ class "btn", href <| Route.toUrl Overview ] [ span [] [ text "Scan code" ] ]
            ]
        ]


viewAddressInfo : Model -> Html Msg
viewAddressInfo ({ address } as model) =
    Animations.animatedSplash model.animations
        AddressInfo
        [ p [] [ text "Perhaps some address info" ]
        , Html.form
            [ class "fake-form" ]
            [ viewInputFake Text "address" address.address [ onFocus (PopulateAddressFields True) ]
            , viewInputFake Text "city" address.city []
            , viewInputFake Text "state" address.state []
            , viewInputFake Text "zip" address.zip []
            , a
                [ class "btn"
                , href <| Route.toUrl ChooseNotifications
                , onClick (PopulateAddressFields False)
                ]
                [ span [] [ text "Next" ] ]
            ]
        ]


viewChooseNotifications : Model -> Html Msg
viewChooseNotifications model =
    let
        button c s =
            a [ class (interpolate "btn {0}" [ c ]), href <| Route.toUrl Overview ]
                [ span [] [ text s ] ]
    in
    Animations.animatedSplash model.animations
        ChooseNotifications
        [ p [] [ text "Turn notifications on to be reminded of important stuff" ]
        , div [ class "splash__buttons spaced" ]
            [ button "btn" "Send me all notifications"
            , button "btn--soft" "Just a few notifications"
            , button "btn--clear" "No notifications"
            ]
        ]



-- VIEW OVERVIEW


viewOverview : Model -> Html Msg
viewOverview model =
    let
        url =
            if toTotalSplits model.bandmates == 100 then
                Charge

            else
                ManageBandmates

        total =
            toTotal model.expenses model.fee
    in
    div [ class "overview" ]
        [ viewIntro model.you
        , viewOverviewTotal total model.fee "Current total ☀️"
        , viewOverviewBandmates model.animations.page model.bandmates total
        , viewOverviewExpenses model.animations.page model.expenses
        , div []
            [ a [ class "btn btn--large", href <| Route.toUrl url ] [ text "Split expenses" ] ]
        ]


viewIntro : Bandmate -> Html Msg
viewIntro bandmate =
    div [ class "overview__welcome" ]
        [ h2 [] [ text "Welcome to an app!" ]
        , p []
            [ text """
                Let's try out some animated page transitions
                with this example app. This example is a kind of "wiebetaaltwat" for
                Scott Pilgrim and his band. You can add expenses and bandmates
                through the menu on the left, and the page transitions shall be animated."""
            ]
        ]


viewOverviewTotal : Total -> Float -> String -> Html Msg
viewOverviewTotal total fee title =
    div [ class "overview__total" ]
        [ h1 [] [ text title ]
        , p []
            [ text "$"
            , text <| FormatNumber.format usLocale total.excludingFee
            ]
        , p []
            [ text "Transaction fee:"
            , span []
                [ text "$"
                , text <| FormatNumber.format usLocale <| total.excludingFee * fee
                ]
            ]
        ]



-- CHARGE


viewCharge : Model -> Html Msg
viewCharge model =
    let
        total =
            toTotal model.expenses model.fee
    in
    div [ class "overview" ]
        [ div [ class "overview__charge" ]
            [ h2 [] [ text "Confirm" ]
            , p [] [ text "The total of expenses is" ]
            , p [ class "overview__charge__amount" ]
                [ text "$"
                , text <| FormatNumber.format usLocale total.includingFee
                ]
            , ul [] <|
                List.map (viewBandmateExpenses model.expenses total.includingFee) <|
                    Dict.values model.bandmates
            , p [] [ text "The app will send payment requests to your bandmates." ]
            , a
                [ class "btn"
                , href <| Route.toUrl ThankYou
                , onClick ConfirmPayment
                ]
                [ span [] [ text "Confirm" ] ]
            , a
                [ class "btn btn--soft"
                , href <| Route.toUrl AreYouSure
                ]
                [ span [] [ text "Cancel" ] ]
            ]
        ]


viewThankYou : Model -> Html Msg
viewThankYou model =
    div [ class "overview" ]
        [ div [ class "overview__charge" ]
            [ h2 [] [ text "All done" ]
            , p [ class "overview__charge__next" ]
                [ text "The app has emailed a payment request to all of your bandmates." ]
            , a
                [ class "btn"
                , href <| Route.toUrl Overview
                ]
                [ span [] [ text "Done" ] ]
            ]
        ]


viewAreYouSure : Model -> Html Msg
viewAreYouSure model =
    div [ class "overview" ]
        [ div [ class "overview__charge" ]
            [ h2 [] [ text "Are you sure?" ]
            , p [ class "overview__charge__next" ]
                [ text "Remember, he who pays the piper calls the tune." ]
            , a
                [ class "btn"
                , href <| Route.toUrl Charge
                ]
                [ span [] [ text "Let's split some expenses!" ] ]
            , a
                [ class "btn btn--soft"
                , href <| Route.toUrl Overview
                ]
                [ span [] [ text "Not right now" ] ]
            ]
        ]



-- VIEW EXPENSES


viewOverviewExpenses_ : Model -> Html Msg
viewOverviewExpenses_ model =
    div [ class "overview" ]
        [ viewOverviewExpenses model.animations.page model.expenses ]


viewOverviewExpenses : Animator.Timeline Page -> Dict String Expense -> Html Msg
viewOverviewExpenses page expenses =
    div [ class "overview__expenses" ]
        [ h2 [] [ text "Expenses 💸" ]
        , ul [] <|
            List.map (viewExpense False) <|
                Dict.values expenses
        , viewIf (Animator.current page == Expenses) <|
            \_ ->
                a [ class "btn", href <| Route.toUrl ManageExpenses ]
                    [ text "Manage expenses" ]
        ]


viewExpense : Bool -> Expense -> Html Msg
viewExpense isEditable expense =
    let
        viewEdit =
            if isEditable then
                button [ onClick (SetToRemove (IsExpense expense)), class "delete-item" ]
                    [ Filled.remove_circle 18 (Color colorBlue) ]

            else
                text ""

        payedBy =
            case expense.payedBy.name of
                "Unknown" ->
                    " (Unknown)"

                _ ->
                    interpolate " ({0}.{1}.)" <|
                        List.map (String.left 1) (String.words expense.payedBy.name)
    in
    li []
        [ viewEdit
        , span [] [ text expense.title, text payedBy ]
        , span []
            [ text "$"
            , text <| FormatNumber.format usLocale <| expense.amount
            ]
        ]


viewManageExpenses : Model -> Html Msg
viewManageExpenses model =
    div [ class "overview" ]
        [ div [ class "overview__expenses" ]
            [ h2 [] [ text "Manage expenses" ]
            , ul [] <|
                List.map (viewExpense True) (Dict.values model.expenses)
            , Animations.viewUpForRemoval model.animations.upForRemoval
                (Dict.map (\k expense -> IsExpense expense) model.expenses)
                viewRemoval
            , a
                [ class "btn btn--clear"
                , href <| Route.toUrl PickCategory
                ]
                [ span [] [ text "Add expense" ] ]
            , a
                [ class "btn"
                , href <| Route.toUrl Expenses
                ]
                [ span [] [ text "Done" ] ]
            ]
        ]


viewRemoval : ExpenseOrBandmate -> List (Html Msg)
viewRemoval expenseOrBandmate =
    let
        title =
            case expenseOrBandmate of
                IsExpense expense ->
                    expense.title

                IsBandmate bandmate ->
                    bandmate.name
    in
    [ p [] [ text <| interpolate "Are you sure you wish to remove {0}?" [ title ] ]
    , button [ onClick (Remove (Just expenseOrBandmate)), class "btn" ]
        [ span [] [ text "Yes, delete" ] ]
    , button [ onClick (Remove Nothing), class "btn btn--soft" ]
        [ span [] [ text "No, cancel" ] ]
    ]



-- ADDING EXPENSES


viewPickCategory : Model -> Html Msg
viewPickCategory model =
    div [ class "overview" ]
        [ div [ class "overview__expenses" ]
            [ h2 [] [ text "Add an expense" ]
            , h3 [] [ text "Choose a category" ]
            , ul [ class "categories" ] <| List.map viewExpenseCategory Data.expenseCategories
            , a
                [ class "btn"
                , href <| Route.toUrl ManageExpenses
                ]
                [ span [] [ text "Back" ] ]
            ]
        ]


viewExpenseCategory : ( String, Data.ExpenseCategory ) -> Html Msg
viewExpenseCategory ( title, expenseCategory ) =
    let
        className =
            interpolate "category cat-{0}"
                [ String.toLower <| String.replace " & " "-" <| String.replace "/" "-" title ]
    in
    li [ class className ]
        [ a
            [ href <| Route.toUrl FillExpenseForm
            , onClick (SetNewExpense (Expense expenseCategory "" 0 Data.bandmateEmpty))
            ]
            [ text title ]
        ]


viewFillExpenseForm : Model -> Html Msg
viewFillExpenseForm model =
    let
        bandmates =
            Dict.values model.bandmates
    in
    div
        [ class "overview" ]
        [ div [ class "overview__expenses" ]
            [ h2 [] [ text "Add expense" ]
            , Html.form [ class "form", onSubmit SubmitExpense ]
                [ viewInput Text "Title" (Maybe.map .title model.newExpense) OnExpenseTitleInput
                , viewInput Number "Amount" Nothing OnExpenseAmountInput
                , viewSelect "Payed by"
                    (Maybe.map .payedBy model.newExpense)
                    bandmates
                    PayBy
                , a
                    [ href <| Route.toUrl Expenses
                    , class "btn"
                    , onClick SubmitExpense
                    ]
                    [ text "Save" ]
                ]
            ]
        ]


viewInputFake : InputType -> String -> String -> List (Attribute Msg) -> Html Msg
viewInputFake inputType placeholderOrId value_ attrMsg =
    let
        typeAttr =
            case inputType of
                Number ->
                    [ type_ "number"
                    , attribute "inputmode" "decimal"
                    , step "1"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "9"
                    , id placeholderOrId
                    ]

                Text ->
                    [ type_ "text", value value_, placeholder placeholderOrId ]

                Password ->
                    [ type_ "password", value value_, placeholder placeholderOrId ]
    in
    input
        (typeAttr
            ++ attrMsg
            ++ [ autocomplete False ]
        )
        []


viewInput : InputType -> String -> Maybe String -> (String -> Msg) -> Html Msg
viewInput inputType title maybeValue toMsg =
    let
        value_ =
            Maybe.withDefault [] (Maybe.map (\a -> [ value a ]) maybeValue)

        name_ =
            String.toLower <| String.replace " " "-" title

        typeAttr =
            case inputType of
                Number ->
                    [ type_ "number"
                    , step "0.01"
                    , attribute "inputmode" "decimal"
                    ]

                Text ->
                    [ type_ "text" ]

                Password ->
                    [ type_ "password" ]
    in
    div []
        [ label [ for name_ ] [ text title ]
        , input
            ([ placeholder title
             , name name_
             , id name_
             , onInput toMsg
             , autocomplete False
             ]
                ++ value_
                ++ typeAttr
            )
            []
        ]


viewSelect : String -> Maybe Bandmate -> List Bandmate -> (Maybe String -> msg) -> Html msg
viewSelect fieldTitle payingBandmate bandmates toMsg =
    let
        isEmpty s =
            if String.isEmpty s then
                Decode.succeed Nothing

            else
                Decode.fail "Not empty"

        decoder =
            Decode.oneOf
                [ Decode.at [ "target", "value" ] Decode.string
                    |> Decode.andThen isEmpty
                , Decode.at [ "target", "value" ] Decode.string
                    |> Decode.map Just
                ]
    in
    div [ class "select" ]
        [ label [ for "select" ] [ text fieldTitle ]
        , select
            [ id "select", on "change" (Decode.map toMsg decoder) ]
            (viewDefaultOption payingBandmate :: List.map (viewOption payingBandmate) bandmates)
        , Filled.expand_more 22 (Color colorBlueBlack)
        ]


viewDefaultOption : Maybe Bandmate -> Html msg
viewDefaultOption payingBandmate =
    option
        [ attribute "value" ""
        , selected (payingBandmate == Nothing)
        ]
        [ text "Select" ]


viewOption : Maybe Bandmate -> Bandmate -> Html msg
viewOption payingBandmate bandmate =
    option
        [ value bandmate.name
        , selected (payingBandmate == Just bandmate)
        ]
        [ text bandmate.name ]



-- VIEW BANDMATES


viewOverviewBandmates_ : Model -> Html Msg
viewOverviewBandmates_ model =
    div [ class "overview" ]
        [ viewOverviewBandmates model.animations.page
            model.bandmates
            (toTotal model.expenses model.fee)
        ]


viewOverviewBandmates : Animator.Timeline Page -> Dict String Bandmate -> Total -> Html Msg
viewOverviewBandmates page bandmates total =
    div [ class "overview__bandmates" ]
        [ h2 [] [ text "Bandmates 🎸" ]
        , ul [] <|
            List.map (viewBandmate total.includingFee) <|
                Dict.values bandmates
        , viewIf (Animator.current page == Bandmates) <|
            \_ ->
                a [ class "btn", href <| Route.toUrl ManageBandmates ]
                    [ span [] [ text "Manage bandmates" ] ]
        ]


viewBandmate : Float -> Bandmate -> Html Msg
viewBandmate totalIncludingFee bandmate =
    li []
        [ span [] [ text bandmate.name ]
        , span [ class "bandmate-split" ]
            [ text <| String.fromFloat bandmate.split ]
        , span [] [ text "%" ]
        , span [ class "bandmate-divider" ] [ text "|" ]
        , span []
            [ text "$"
            , text <| FormatNumber.format usLocale <| totalIncludingFee / 100 * bandmate.split
            ]
        ]


viewBandmateExpenses : Dict String Expense -> Float -> Bandmate -> Html Msg
viewBandmateExpenses expenses totalIncludingFee bandmate =
    let
        personalSplit =
            totalIncludingFee / 100 * bandmate.split

        personalExpenses =
            toPersonalTotal expenses bandmate

        personalTotal =
            personalSplit - personalExpenses

        format =
            String.replace "$−" "-$"
                << interpolate "${0}"
                << List.singleton
                << FormatNumber.format usLocale
    in
    li [ class "bandmate-personal-calc" ]
        [ span [] [ text bandmate.name ]
        , span []
            [ text <| FormatNumber.format usLocale personalSplit
            , text " - "
            , text <| FormatNumber.format usLocale personalExpenses
            , text " = "
            ]
        , span []
            [ text <| format personalTotal ]
        ]


viewManageBandmates : Model -> Html Msg
viewManageBandmates model =
    div [ class "overview" ]
        [ div [ class "overview__bandmates" ]
            [ h2 [] [ text "Manage bandmates" ]
            , viewCheckSplitsCombined model.bandmates
            , ul [] <|
                List.map (viewBandmateManagerItem model.you) (Dict.toList model.bandmates)
            , Animations.viewUpForRemoval model.animations.upForRemoval
                (Dict.map (\k rm -> IsBandmate rm) model.bandmates)
                viewRemoval
            , viewIf model.showAddBandmate <|
                \_ ->
                    div [ class "add-bandmate" ]
                        [ input
                            [ type_ "text"
                            , onInput OnNameInput
                            , autocomplete False
                            , placeholder "name"
                            , class "add-bandmate__name"
                            ]
                            []
                        , span [ class "split-label" ] [ text "% split:" ]
                        , input
                            [ type_ "number"
                            , step "0.01"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "100"
                            , attribute "inputmode" "decimal"
                            , placeholder "0"
                            , onInput OnSplitInput
                            , class "add-bandmate__split"
                            ]
                            []
                        ]
            , viewManageBandmatesButtons model.showAddBandmate
            , viewButtonDone model.bandmates
            ]
        ]


viewCheckSplitsCombined : Dict String Bandmate -> Html Msg
viewCheckSplitsCombined bandmates =
    if toTotalSplits bandmates == 100 then
        p [ class "split-check valid-split" ] [ text "Total of splits is 100%" ]

    else
        div []
            [ p [ class "split-check invalid-split" ] [ text "Oops! Total of splits should be 100%" ]
            , button
                [ class "btn btn--split"
                , onClick SplitEvenly
                ]
                [ text "Split evenly among unlocked" ]
            ]


viewManageBandmatesButtons : Bool -> Html Msg
viewManageBandmatesButtons showAddBandmate =
    if showAddBandmate then
        div [ class "add-bandmate__btns" ]
            [ button [ class "btn btn--clear", onClick SaveNewBandmate ] [ text "Save" ]
            , button [ class "btn btn--soft", onClick ToggleAddBandmate ] [ text "Cancel" ]
            ]

    else
        button [ class "btn btn--clear", onClick ToggleAddBandmate ] [ text "Add bandmate" ]


viewButtonDone : Dict String Bandmate -> Html Msg
viewButtonDone bandmates =
    let
        ( el, attributes ) =
            if toTotalSplits bandmates == 100 then
                ( a, [ href (Route.toUrl Bandmates) ] )

            else
                ( div, [ class "-disabled" ] )
    in
    el (class "btn" :: attributes)
        [ span [] [ text "Done" ] ]


viewBandmateManagerItem : Bandmate -> ( String, Bandmate ) -> Html Msg
viewBandmateManagerItem you ( id, bandmate ) =
    let
        ( msg, color ) =
            if id == you.name then
                ( NoOp, colorGrey )

            else
                ( SetToRemove (IsBandmate bandmate), colorBlue )
    in
    li []
        [ button [ onClick msg, class "delete-item" ]
            [ Filled.remove_circle 18 (Color color) ]
        , span []
            [ text bandmate.name ]
        , span [ class "split-label" ] [ text "% split:" ]
        , input
            [ type_ "number"
            , step "0.01"
            , Html.Attributes.min "0"
            , Html.Attributes.max "100"
            , attribute "inputmode" "decimal"
            , value <|
                String.fromFloat bandmate.split
            , onInput (SaveBandmateSplit id)
            ]
            []
        , viewLockButton ( id, bandmate )
        ]


viewLockButton : ( String, Bandmate ) -> Html Msg
viewLockButton ( id, bandmate ) =
    let
        lock =
            if bandmate.locked then
                Filled.lock 20 (Color <| colorBlueBlack)

            else
                Filled.lock_open 20 (Color colorGrey)
    in
    button [ onClick (ToggleSplitLock id) ]
        [ lock ]



-- VIEW HISTORY


viewHistory : Model -> Html Msg
viewHistory model =
    let
        history =
            [ ( Date.add Months -1 model.today, Data.expenses )
            , ( Date.add Months -2 model.today, Data.expensesOld )
            , ( Date.add Months -3 model.today, Data.expensesOld )
            ]

        currentHistory =
            if model.paymentConfirmed then
                ( model.today, model.expenses ) :: history

            else
                history
    in
    div [ class "overview" ]
        [ div [ class "overview__payment" ]
            [ h2 [] [ text "History ⏳" ]
            , ul [] <| List.map (viewHistoryItem model.fee) currentHistory
            ]
        ]


viewHistoryItem : Float -> ( Date, Dict String Expense ) -> Html Msg
viewHistoryItem fee ( date, expenses ) =
    li []
        [ a [ href <| Route.toUrl HistoryDetail, onClick (ActivateHistoryItem ( date, expenses )) ]
            [ i [ class "list-icon" ]
                [ Filled.view_headline 18 (Color colorBlue) ]
            , span [] [ text (Date.format "MMMM y" date) ]
            , span []
                [ text "$"
                , text <| FormatNumber.format usLocale (.includingFee <| toTotal expenses fee)
                ]
            ]
        ]


viewHistoryDetail : Model -> Html Msg
viewHistoryDetail model =
    case model.activeHistoryItem of
        Just ( date, expenses ) ->
            viewHistoryDetail_ model date expenses

        Nothing ->
            viewHistory model


viewHistoryDetail_ : Model -> Date -> Dict String Expense -> Html Msg
viewHistoryDetail_ model date expenses =
    let
        total =
            toTotal expenses model.fee
    in
    div [ class "overview history-detail" ]
        [ viewOverviewTotal total model.fee (Date.format "MMMM y" date)
        , viewOverviewExpenses model.animations.page expenses
        , viewOverviewBandmates model.animations.page model.bandmates total
        , a [ class "btn", href <| Route.toUrl History ]
            [ text "Back to history" ]
        ]



-- VIEW SETTINGS


viewSettings : Model -> Html Msg
viewSettings model =
    div [ class "overview" ]
        [ div [ class "overview__settings" ]
            [ h2 [] [ text "Settings 👍" ]
            , viewSettingsList
                [ ( "Edit profile", SettingsText, Filled.person )
                , ( "Privacy", SettingsText, Filled.lock )
                , ( "Notifications", SettingsText, Filled.notifications )
                , ( "Change password", SettingsText, Filled.vpn_key )
                , ( "Rate App", SettingsText, Filled.grade )
                ]
            ]
        ]


viewSettingsList : List ( String, Page, Material.Icons.Types.Icon msg ) -> Html msg
viewSettingsList =
    ul [ class "settings-list" ] << List.map (viewMenuItem Settings)



-- VIEW SUPPORT


viewSupport : Model -> Html Msg
viewSupport model =
    div [ class "overview" ]
        [ div [ class "overview__settings" ]
            [ h2 [] [ text "Support 🔥" ]
            , h3 [] [ text "Find help quickly" ]
            , viewSettingsList
                [ ( "FAQ", SupportText, Filled.search )
                ]
            , h3 [] [ text "Connect with us" ]
            , viewSettingsList
                [ ( "Email US", SupportText, Filled.alternate_email )
                , ( "Chat", SupportText, Filled.question_answer )
                ]
            , h3 [] [ text "Information" ]
            , viewSettingsList
                [ ( "Privacy policy", SupportText, Filled.cloud_done )
                , ( "Legal information", SettingsText, Filled.library_books )
                ]
            ]
        ]


viewSettingsPage : Model -> Html Msg
viewSettingsPage =
    viewSettingsPage_ Settings


viewSupportPage : Model -> Html Msg
viewSupportPage =
    viewSettingsPage_ Support


viewSettingsPage_ : Page -> Model -> Html Msg
viewSettingsPage_ page model =
    let
        backTo =
            case page of
                Support ->
                    "Support"

                Settings ->
                    "Settings"

                _ ->
                    "unknown"
    in
    div [ class "overview text" ]
        [ div [ class "overview__settings" ]
            [ h2 [] [ text <| interpolate "{0} page" [ backTo ] ]
            , p []
                [ text """
                This could be a detail page explaining a little more about
                how the app works, etc.
                """
                ]
            ]
        , a [ class "btn", href <| Route.toUrl page ]
            [ text <| interpolate "Back to {0}" [ String.toLower backTo ] ]
        , a [ class "btn btn--soft", href <| Route.toUrl Overview ]
            [ text "Home" ]
        ]



--HELPERS


viewIf : Bool -> (() -> Html msg) -> Html msg
viewIf bool html =
    if bool then
        html ()

    else
        text ""


toTotal : Dict String Expense -> Float -> Total
toTotal expenses fee =
    let
        total =
            List.foldl (+) 0 <|
                Dict.foldl addAmount [] expenses
    in
    { includingFee = total + (total * fee)
    , excludingFee = total
    }


toPersonalTotal : Dict String Expense -> Bandmate -> Float
toPersonalTotal expenses bandmate =
    let
        personalExpenses =
            Dict.filter (\k v -> v.payedBy == bandmate) expenses
    in
    List.foldl (+) 0 <|
        Dict.foldl addAmount [] personalExpenses


addAmount : String -> Expense -> List Float -> List Float
addAmount _ expense amounts =
    expense.amount :: amounts


toTotalSplits : Dict String Bandmate -> Float
toTotalSplits bandmates =
    List.foldl (+) 0 <|
        Dict.foldl addSplit [] bandmates


addSplit : String -> Bandmate -> List Float -> List Float
addSplit _ bandmate splits =
    bandmate.split :: splits



--COLORS


colorBlue : Color.Color
colorBlue =
    -- #3333FF
    Color.rgb255 51 51 255


colorLilac : Color.Color
colorLilac =
    -- #8566FF
    Color.rgb255 133 102 255


colorGrey : Color.Color
colorGrey =
    -- #A4A3AB
    Color.rgb255 164 163 171


colorBlueBlack : Color.Color
colorBlueBlack =
    -- #1D1D35
    Color.rgb255 29 29 53
