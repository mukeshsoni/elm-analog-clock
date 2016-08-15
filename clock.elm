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

type TimeZone = IST | PST | UTC

type alias Model = 
    { timeZone: TimeZone
    , currentTime: Time
    }


init : TimeZone -> (Model, Cmd Msg)
init timeZone =
  ({timeZone = timeZone, currentTime = 0}, Cmd.none)


timezomeAdjustments = 
    [ (IST, 5.5 * 60 * 60 * 1000)
    , (PST, -(7 * 60 * 60 * 1000))
    , (UTC, 0)
    ]
    
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
        [ Html.div [Html.Attributes.style [("color", "white"), ("text-align", "center")]] [text (toString model.timeZone)]
        , clock clockColor model
        ]

getAdjustment a =
    case a of
        Nothing -> 0
        Just (_, adjustment) -> adjustment
        
getMilliseconds : Model -> Float
getMilliseconds model =
    let 
        tzAdjustment = List.head (List.filter (\(tz, a) -> tz == model.timeZone) timezomeAdjustments)
        adjustment = getAdjustment tzAdjustment
    in
        model.currentTime + adjustment
        
-- view : Model -> Html Msg
clock clockColor model =
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill clockColor ] []
        , secondHand (getMilliseconds model)
        , minuteHand (getMilliseconds model)
        , hourHand (getMilliseconds model)
      ]


