module Data exposing
    ( Address
    , Bandmate
    , Expense
    , ExpenseCategory(..)
    , ExpenseOrBandmate(..)
    , addressEmpty
    , addressFilled
    , bandmateEmpty
    , bandmates
    , expenseCategories
    , expenseEmpty
    , expenses
    , expensesOld
    )

import Dict exposing (Dict)


type ExpenseOrBandmate
    = IsExpense Expense
    | IsBandmate Bandmate


type alias Bandmate =
    { name : String
    , split : Float
    , locked : Bool
    , email : String
    }


type alias Address =
    { address : String
    , city : String
    , state : String
    , zip : String
    }


addressEmpty : Address
addressEmpty =
    Address "" "" "" ""


addressFilled : Address
addressFilled =
    Address
        "90 Bedford Street"
        "New York"
        "New York"
        "10014"


bandmateEmpty : Bandmate
bandmateEmpty =
    { name = "Unknown"
    , split = 0
    , locked = False
    , email = ""
    }


type alias Expense =
    { category : ExpenseCategory
    , title : String
    , amount : Float
    , payedBy : Bandmate
    }


expenseEmpty : Expense
expenseEmpty =
    { category = Other
    , title = ""
    , amount = 0
    , payedBy = bandmateEmpty
    }


type ExpenseCategory
    = Transportation
    | Accommodation
    | FoodAndDrink
    | Studio
    | Gear
    | Other


expenseCategories : List ( String, ExpenseCategory )
expenseCategories =
    [ ( "Transport", Transportation )
    , ( "Sleeping", Accommodation )
    , ( "Food & drink", FoodAndDrink )
    , ( "Studio/rent", Studio )
    , ( "Gear", Gear )
    , ( "Other", Other )
    ]


bandmates : Dict String Bandmate
bandmates =
    Dict.fromList <|
        List.map (\( x, y ) -> ( x, Bandmate x y False "sonic@boom.com" ))
            [ ( "Stephen Stills", 25 )
            , ( "Scott Pilgrim", 25 )
            , ( "Kim Pine", 20 )
            , ( "Young Neil", 30 )
            ]


expenses : Dict String Expense
expenses =
    Dict.fromList <|
        List.map
            (\( a, b, c ) ->
                ( a, Expense Other a b c )
            )
            [ ( "Studio", 250, get "Stephen Stills" )
            , ( "Mic stand", 30, get "Scott Pilgrim" )
            , ( "Car rental", 78.96, get "Stephen Stills" )
            , ( "Drinks at Lee's", 123.3, get "Stephen Stills" )
            , ( "Pizza Pizza", 58, get "Young Neil" )
            ]


expensesOld : Dict String Expense
expensesOld =
    Dict.fromList <|
        List.map
            (\( a, b, c ) ->
                ( a, Expense Other a b c )
            )
            [ ( "Studio", 250, get "Stephen Stills" )
            , ( "Mini Korg", 230, get "Stephen Stills" )
            , ( "Car rental", 53.92, get "Stephen Stills" )
            , ( "Lee's Palace", 70, get "Stephen Stills" )
            , ( "Pizza Pizza", 32, get "Stephen Stills" )
            ]


get : String -> Bandmate
get name =
    Dict.get name bandmates
        |> Maybe.withDefault bandmateEmpty
