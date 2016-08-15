import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



-- main =
--   Html.program
--     { init = init
--     , view = view
--     , update = update
--     , subscriptions = subscriptions
--     }



-- MODEL


type alias Model = Time


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)



-- UPDATE


type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)



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
        line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "red" ] []

minuteHand model = 
    let
        angle =
          degrees (toFloat ((floor (Time.inMinutes model) % 60) * 6) - 90)
    
        handX =
          toString (50 + 33 * cos angle)
    
        handY =
          toString (50 + 33 * sin angle)
    in
        line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "white" ] []

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
        [ text "the clock"
        , clock clockColor model
        ]

-- view : Model -> Html Msg
clock clockColor model =
  let
    angle =
      turns (Time.inMinutes model)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    svg [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill clockColor ] []
      , line [ x1 "50", y1 "5", x2 "50.00", y2 "10", stroke "#023963" ] []
      , line [ x1 "72.50", y1 "11.03", x2 "70.00", y2 "15.36", stroke "#023963" ] []
      , line [ x1 "88.97", y1 "27.50", x2 "84.64", y2 "30.00", stroke "#023963" ] []
      , line [ x1 "95.00", y1 "50.00", x2 "90.00", y2 "50.00", stroke "#023963" ] []
      , line [ x1 "88.97", y1 "72.50", x2 "84.64", y2 "70.00", stroke "#023963" ] []
      , line [ x1 "72.50", y1 "88.97", x2 "70.00", y2 "84.64", stroke "#023963" ] []
      , line [ x1 "50.00", y1 "95.00", x2 "50.00", y2 "90.00", stroke "#023963" ] []
      , line [ x1 "27.50", y1 "88.97", x2 "30.00", y2 "84.64", stroke "#023963" ] []
      , line [ x1 "11.03", y1 "72.50", x2 "15.36", y2 "70.00", stroke "#023963" ] []
      , line [ x1 "5.000", y1 "50.00", x2 "10.00", y2 "50.00", stroke "#023963" ] []
      , line [ x1 "11.03", y1 "27.50", x2 "15.36", y2 "30.00", stroke "#023963" ] []
      , line [ x1 "27.50", y1 "11.03", x2 "30.00", y2 "15.36", stroke "#023963" ] []
      , secondHand model
      , minuteHand model
      , hourHand model
      ]
