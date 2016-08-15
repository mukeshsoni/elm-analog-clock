import Html exposing (Html)
import Html.App as Html
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

-- main =
--   Html.program
--     { init = init
--     , view = view "blue"
--     , update = update
--     , subscriptions = subscriptions
--     }



-- MODEL

type TimeZone = 
    IST | PST | UTC | WAT

type alias Model = 
    { timeZone: TimeZone
    , currentTime: Time
    }


init : TimeZone -> (Model, Cmd Msg)
init timeZone =
  ({timeZone = timeZone, currentTime = 0}, Cmd.none)


timezomeAdjustments tz = 
    case tz of
        PST -> -(7 * 60 * 60 * 1000)
        UTC -> 0
        WAT -> 1 * 60 * 60 * 1000
        IST -> 5.5 * 60 * 60 * 1000
        

city tz = 
    case tz of
        IST -> "New Delhi"
        PST -> "Los Angeles"
        UTC -> "London"
        WAT -> "Cameroon"
    
-- UPDATE


type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({model | currentTime = newTime}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

hoursCorrectionForMinutes model = 
    -- degrees (toFloat ((floor (Time.inMinutes model)) % 60) / 2)
    (toFloat ((floor (Time.inMinutes model)) % 60)) / 2

hourHand model = 
    let
        angle =
          degrees (toFloat (((floor (Time.inHours model)) % 12) * 30) + (hoursCorrectionForMinutes model) - 90)
    
        handX =
          toString (50 + 25 * cos angle)
    
        handY =
          toString (50 + 25 * sin angle)
    in
        line [ strokeWidth "2px", x1 "50", y1 "50", x2 handX, y2 handY, stroke "black" ] []

minuteHand model = 
    let
        angle =
          degrees (toFloat ((floor (Time.inMinutes model) % 60) * 6) - 90)
    
        handX =
          toString (50 + 33 * cos angle)
    
        handY =
          toString (50 + 33 * sin angle)
    in
        line [ strokeWidth "3px", x1 "50", y1 "50", x2 handX, y2 handY, stroke "black" ] []

secondHand model = 
    let
        angle =
          turns (Time.inMinutes model)
    
        handX =
          toString (50 + 40 * cos angle)
    
        handY =
          toString (50 + 40 * sin angle)
    in
        line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []

        
-- VIEW
view clockColor model = 
    Html.div [] 
        [ Html.div 
            [Html.Attributes.style [("color", "white"), ("text-align", "center")]] 
            [text (city model.timeZone)]
        , clock clockColor model
        ]

getMilliseconds : Model -> Float
getMilliseconds model =
    model.currentTime + (timezomeAdjustments model.timeZone)
    
-- view : Model -> Html Msg
clock clockColor model =
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill clockColor ] []
        , secondHand (getMilliseconds model)
        , minuteHand (getMilliseconds model)
        , hourHand (getMilliseconds model)
      ]


