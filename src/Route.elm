module Route exposing
    ( Page(..)
    , mainPages
    , parser
    , splashPages
    , toPage
    , toUrl
    )

import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), s)



-- URL HANDLING


type Page
    = Splash
    | Signup
    | Signin
    | CodeScan
    | AddressInfo
    | ChooseNotifications
    | Overview
    | Charge
    | ThankYou
    | AreYouSure
    | Bandmates
    | ManageBandmates
    | Expenses
    | ManageExpenses
    | PickCategory
    | FillExpenseForm
    | History
    | HistoryDetail
    | Support
    | SupportText
    | Settings
    | SettingsText
    | NotFound


mainPages : List Page
mainPages =
    [ Overview
    , Expenses
    , Bandmates
    , History
    , Support
    , Settings
    ]


splashPages : List Page
splashPages =
    [ Splash
    , Signup
    , Signin
    , CodeScan
    , AddressInfo
    , ChooseNotifications
    ]


toPage : Url -> Page
toPage url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
        |> Maybe.withDefault Splash


parser : Parser.Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Splash Parser.top
        , Parser.map Signup (Parser.s "signup")
        , Parser.map Signin (Parser.s "signin")
        , Parser.map CodeScan (Parser.s "code-scan")
        , Parser.map AddressInfo (Parser.s "address-info")
        , Parser.map ChooseNotifications (Parser.s "notifications")
        , Parser.map Overview (Parser.s "overview")
        , Parser.map Charge (Parser.s "charge")
        , Parser.map ThankYou (Parser.s "charge" </> Parser.s "thank-you")
        , Parser.map AreYouSure (Parser.s "charge" </> Parser.s "are-you-sure")
        , Parser.map Bandmates (Parser.s "bandmates")
        , Parser.map ManageBandmates (Parser.s "bandmates" </> Parser.s "manage")
        , Parser.map Expenses (Parser.s "expenses")
        , Parser.map ManageExpenses (Parser.s "expenses" </> Parser.s "manage")
        , Parser.map PickCategory (Parser.s "expenses" </> Parser.s "manage" </> Parser.s "add")
        , Parser.map FillExpenseForm (Parser.s "expenses" </> Parser.s "manage" </> Parser.s "add" </> Parser.s "form")
        , Parser.map History (Parser.s "payment-history")
        , Parser.map HistoryDetail (Parser.s "payment-history" </> Parser.s "detail")
        , Parser.map Support (Parser.s "support")
        , Parser.map SupportText (Parser.s "support" </> Parser.s "text")
        , Parser.map Settings (Parser.s "settings")
        , Parser.map SettingsText (Parser.s "settings" </> Parser.s "text")
        ]


toUrl : Page -> String
toUrl page =
    case page of
        Splash ->
            Url.Builder.absolute [] []

        Overview ->
            Url.Builder.absolute [ "#", "overview" ] []

        Signup ->
            Url.Builder.absolute [ "#", "signup" ] []

        Signin ->
            Url.Builder.absolute [ "#", "signin" ] []

        CodeScan ->
            Url.Builder.absolute [ "#", "code-scan" ] []

        AddressInfo ->
            Url.Builder.absolute [ "#", "address-info" ] []

        ChooseNotifications ->
            Url.Builder.absolute [ "#", "notifications" ] []

        Charge ->
            Url.Builder.absolute [ "#", "charge" ] []

        ThankYou ->
            Url.Builder.absolute [ "#", "charge", "thank-you" ] []

        AreYouSure ->
            Url.Builder.absolute [ "#", "charge", "are-you-sure" ] []

        Bandmates ->
            Url.Builder.absolute [ "#", "bandmates" ] []

        ManageBandmates ->
            Url.Builder.absolute [ "#", "bandmates", "manage" ] []

        Expenses ->
            Url.Builder.absolute [ "#", "expenses" ] []

        ManageExpenses ->
            Url.Builder.absolute [ "#", "expenses", "manage" ] []

        PickCategory ->
            Url.Builder.absolute [ "#", "expenses", "manage", "add" ] []

        FillExpenseForm ->
            Url.Builder.absolute [ "#", "expenses", "manage", "add", "form" ] []

        History ->
            Url.Builder.absolute [ "#", "payment-history" ] []

        HistoryDetail ->
            Url.Builder.absolute [ "#", "payment-history", "detail" ] []

        Support ->
            Url.Builder.absolute [ "#", "support" ] []

        SupportText ->
            Url.Builder.absolute [ "#", "support", "text" ] []

        Settings ->
            Url.Builder.absolute [ "#", "settings" ] []

        SettingsText ->
            Url.Builder.absolute [ "#", "settings", "text" ] []

        NotFound ->
            Url.Builder.absolute [ "notfound" ] []
