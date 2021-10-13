module Main exposing (..)

import Angle exposing (Angle)
import Assets
import Axis2d
import Browser
import Circle2d exposing (Circle2d)
import Direction2d exposing (Direction2d)
import Html as H exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Unitless)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Vector2d exposing (Vector2d)



-- Model, Msg,  init & update


type alias Model =
    { rayAngle : Float
    }


type Msg
    = SetRayAngle String


init : ( Model, Cmd Msg )
init =
    ( { rayAngle = 30 }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRayAngle angleString ->
            ( angleString
                |> String.toFloat
                |> Maybe.map
                    (\newRayAngle ->
                        { model | rayAngle = newRayAngle }
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )



-- Type aliases for concise signatures


type alias LineSegment =
    LineSegment2d Unitless ()


type alias Point =
    Point2d Unitless ()


type alias Circle =
    Circle2d Unitless ()



-- Objects


type MirroredSide
    = Left
    | Right


type alias Wall =
    { mirrored : Maybe MirroredSide
    , lineSegment : LineSegment
    }


{-| The window on the box
-}
boxWindow : LineSegment
boxWindow =
    let
        observerCenter =
            observer
                |> Circle2d.centerPoint
                |> Point2d.toUnitless

        observerRadius =
            observer |> Circle2d.radius |> Quantity.unwrap

        observerStartX =
            observerCenter.x - observerRadius

        observerEndX =
            observerCenter.x + observerRadius

        margin =
            0.07
    in
    LineSegment2d.from (Point2d.unitless (observerStartX - margin) 1) (Point2d.unitless (observerEndX + margin) 1)


box : List Wall
box =
    [ { mirrored = Nothing, lineSegment = LineSegment2d.from (Point2d.unitless 0 0) (Point2d.unitless 1 0) }
    , { mirrored = Just Right, lineSegment = LineSegment2d.from (Point2d.unitless 1 0) (Point2d.unitless 1 1) }
    , { mirrored = Nothing, lineSegment = LineSegment2d.from (Point2d.unitless 0 1) (Point2d.unitless (startPointXY boxWindow).x 1) }
    , { mirrored = Nothing, lineSegment = LineSegment2d.from (Point2d.unitless (endPointXY boxWindow).x 1) (Point2d.unitless 1 1) }
    , { mirrored = Just Left, lineSegment = LineSegment2d.from (Point2d.unitless 0 1) (Point2d.unitless 0 0) }
    ]


objectPosition : Point
objectPosition =
    Point2d.unitless 0.5 0.3


observer : Circle
observer =
    let
        x =
            0.5

        y =
            1.07

        radius =
            0.07
    in
    Circle2d.atPoint (Point2d.unitless x y) (Quantity.float radius)


{-| Represents a reflection.
It tracks where the reflection occurred and how many times the ray bounced till this reflection was reached.
-}
type alias Reflection =
    { side : MirroredSide
    , point : Point
    , bounces : Int
    }


{-| Represents what the observer sees.
If reflection is Nothing, the image is seen directly, otherwise it's seen through a reflection.
-}
type Image
    = Image (Maybe Reflection)


view : Model -> Html Msg
view model =
    let
        { trace, image } =
            traceRay (Angle.degrees <| model.rayAngle)
    in
    div [ HA.class "Applet" ]
        [ H.div []
            [ H.div [ HA.class "Canvas" ]
                [ Svg.svg
                    [ SA.width "500"
                    , SA.height "500"
                    , SA.viewBox "0 0 1.2 1.2"
                    ]
                    [ Svg.rect [ SA.x "0", SA.y "0", SA.width "1.2", SA.height "1.2", SA.fill "rgba(0, 0, 0, 0.05)" ] [] -- Background
                    , Svg.g [ SA.transform "translate(0.2, 0.2) scale(0.8)" ]
                        [ box
                            |> List.map viewWall
                            |> Svg.g []
                        , viewObject
                        , trace
                            |> viewRay
                        , viewObserver { active = image /= Nothing }
                        , image
                            |> Maybe.andThen (\(Image maybeReflection) -> maybeReflection)
                            |> maybeView viewMirrorImage
                        ]
                    ]
                ]
            , viewAngleSlider model.rayAngle
            ]
        ]



-- Helpers for tracking the path of the ray.


traceRay : Angle -> { trace : List Point, image : Maybe Image }
traceRay angle =
    let
        trajectory =
            Vector2d.rTheta (Quantity.float 5) angle
                |> LineSegment2d.fromPointAndVector objectPosition
    in
    traceRayRecursively trajectory
        { prevReflection = Nothing
        , trace = [ LineSegment2d.startPoint trajectory ]
        }


traceRayRecursively :
    LineSegment
    -> { prevReflection : Maybe Reflection, trace : List Point }
    -> { trace : List Point, image : Maybe Image }
traceRayRecursively trajectory { prevReflection, trace } =
    let
        maybePrevReflectionSide =
            prevReflection |> Maybe.map .side

        -- Check if we've hit a wall
        maybeWallHit : Maybe WallHit
        maybeWallHit =
            box
                |> List.filter (.mirrored >> (/=) maybePrevReflectionSide)
                |> List.foldr
                    (\wall acc_ ->
                        case acc_ of
                            Just p ->
                                Just p

                            Nothing ->
                                LineSegment2d.intersectionPoint trajectory wall.lineSegment
                                    |> Maybe.map
                                        (hitWall wall
                                            trajectory
                                            prevReflection
                                        )
                    )
                    Nothing

        -- Stop tracking reflections when the ray hits a point equal to a previous one within a certain tolerance
        didLoop point =
            List.any (Point2d.equalWithin (Quantity.float 0.01) point) trace

        missedObserver point =
            { trace = trace ++ [ point ]
            , image = Nothing
            }

        hitObserver point =
            { trace = trace ++ [ point ]
            , image = Just <| Image prevReflection
            }
    in
    case maybeWallHit of
        Just (Reflected newTrajectory reflection) ->
            if didLoop reflection.point then
                missedObserver reflection.point

            else
                traceRayRecursively newTrajectory
                    { prevReflection = Just reflection
                    , trace =
                        trace
                            ++ [ reflection.point
                               ]
                    }

        Just (NonReflected point) ->
            missedObserver point

        Nothing ->
            -- Check if the ray has reached the observer
            case lineCircleIntersection trajectory observer of
                point :: _ ->
                    hitObserver point

                [] ->
                    missedObserver <| LineSegment2d.endPoint trajectory


type WallHit
    = Reflected LineSegment Reflection
    | NonReflected Point


hitWall : Wall -> LineSegment -> Maybe Reflection -> Point -> WallHit
hitWall wall trajectory prevReflection point =
    let
        mirroredTrajectory =
            trajectory
                |> LineSegment2d.vector
                |> Vector2d.reverse
                |> LineSegment2d.fromPointAndVector point
                |> LineSegment2d.mirrorAcross (Axis2d.through point Direction2d.x)

        prevBounces =
            prevReflection
                |> Maybe.map .bounces
                |> Maybe.withDefault 0

        reflect side =
            Reflected mirroredTrajectory
                { side = side
                , point = point
                , bounces = prevBounces + 1
                }
    in
    wall.mirrored
        |> Maybe.map reflect
        |> Maybe.withDefault (NonReflected point)



-- View helpers


colors =
    { mirror = "#2C6FEF"
    , eye = "#1DAB89"
    , ray = "#F2B135"
    }


viewObserver : { active : Bool } -> Svg msg
viewObserver { active } =
    let
        { x, y } =
            observer
                |> Circle2d.centerPoint
                |> Point2d.toUnitless

        radius =
            Circle2d.radius observer
                |> Quantity.unwrap

        retina =
            Svg.circle
                [ SA.r <| String.fromFloat (radius * 0.5)
                , SA.fill "black"
                ]
                []
    in
    Svg.g
        [ SA.transform <| translate x y
        ]
        [ Svg.circle
            [ SA.r <| String.fromFloat radius
            , SA.fill <|
                if active then
                    colors.eye

                else
                    "gray"
            ]
            []
        , retina
        ]


viewWall : Wall -> Svg msg
viewWall { mirrored, lineSegment } =
    viewLineSegment
        [ SA.stroke <|
            if mirrored /= Nothing then
                colors.mirror

            else
                "rgb(0, 0, 0)"
        ]
        lineSegment


viewLineSegment : List (Svg.Attribute msg) -> LineSegment -> Svg msg
viewLineSegment attrs lineSegment =
    let
        start =
            pointToString <| LineSegment2d.startPoint lineSegment

        end =
            pointToString <| LineSegment2d.endPoint lineSegment
    in
    Svg.line
        ([ SA.x1 start.x
         , SA.y1 start.y
         , SA.x2 end.x
         , SA.y2 end.y
         , SA.stroke "black"
         , SA.strokeWidth "0.01"
         ]
            ++ attrs
        )
        []


viewObject : Svg msg
viewObject =
    let
        { x, y } =
            objectPosition
                |> Point2d.toUnitless
    in
    Svg.g [ SA.transform (translate x y) ]
        [ Assets.cat
        ]


viewRay : List Point -> Svg msg
viewRay points =
    let
        points_ =
            points
                |> List.map Point2d.toUnitless
                |> List.map (\{ x, y } -> [ x, y ] |> List.map String.fromFloat |> String.join ",")
                |> String.join " "
    in
    Svg.polyline
        [ SA.points points_
        , SA.stroke colors.ray
        , SA.strokeWidth "0.003"
        , SA.fill "none"
        ]
        []


viewMirrorImage : Reflection -> Svg msg
viewMirrorImage { side, point, bounces } =
    let
        { x, y } =
            point
                |> Point2d.toUnitless

        s =
            0.2
    in
    Svg.g
        []
        [ Svg.rect
            [ SA.width <| String.fromFloat s
            , SA.height <| String.fromFloat s
            , SA.fill colors.mirror
            , SA.opacity "0.1"
            , SA.transform <| translate (x - s / 2) (y - s / 2)
            ]
            []
        , Svg.g
            [ SA.transform <| translate x y ++ " scale(0.4)"
            , SA.opacity "0.8"
            ]
            [ Svg.g
                [ SA.transform <|
                    let
                        scaleFactor =
                            max (1 - toFloat bounces * 0.07) 0.1

                        scaleFactorX =
                            if modBy 2 bounces == 1 then
                                -1 * scaleFactor

                            else
                                scaleFactor
                    in
                    scale scaleFactorX scaleFactor
                ]
                [ Assets.cat
                ]
            ]
        ]


viewAngleSlider : Float -> Html Msg
viewAngleSlider value =
    H.label [ HA.class "AngleSlider" ]
        [ H.div []
            [ H.text "Ray angle"
            ]
        , H.input
            [ HA.type_ "range"
            , HA.min "0"
            , HA.max "359"
            , HA.step "0.05"
            , HA.value <| String.fromFloat value
            , HE.onInput SetRayAngle
            , HA.class "AngleSlider__Input"
            ]
            []
        ]


maybeView : (a -> Html msg) -> Maybe a -> Html msg
maybeView viewFn maybeVal =
    maybeVal
        |> Maybe.map viewFn
        |> Maybe.withDefault (H.text "")



-- Other helpers


{-| Gives the intersection points of a line segment and a circle
-}
lineCircleIntersection : LineSegment -> Circle -> List Point
lineCircleIntersection line circle =
    let
        circleCenter =
            circle
                |> Circle2d.centerPoint
                |> Point2d.toUnitless

        circleRadius =
            circle
                |> Circle2d.radius
                |> Quantity.unwrap

        lineStart =
            startPointXY line

        lineEnd =
            endPointXY line

        slope =
            (lineEnd.y - lineStart.y) / (lineEnd.x - lineStart.x)

        yInt =
            lineStart.y - slope * lineStart.x

        a =
            1 + slope ^ 2

        b =
            2 * (slope * (yInt - circleCenter.y) - circleCenter.x)

        c =
            circleCenter.x ^ 2 + (yInt - circleCenter.y) ^ 2 - circleRadius ^ 2

        determinant =
            b ^ 2 - 4 * a * c

        point x =
            Point2d.unitless x (x * slope + yInt)
    in
    if determinant < 0 then
        []

    else if determinant == 0 then
        [ point <| (-b + sqrt determinant) / (2 * a) ]

    else if determinant > 0 then
        [ point <| (-b + sqrt determinant) / (2 * a)
        , point <| (-b - sqrt determinant) / (2 * a)
        ]

    else
        []


startPointXY : LineSegment -> { x : Float, y : Float }
startPointXY =
    LineSegment2d.startPoint >> Point2d.toUnitless


endPointXY : LineSegment -> { x : Float, y : Float }
endPointXY =
    LineSegment2d.endPoint >> Point2d.toUnitless


pointToString : Point -> { x : String, y : String }
pointToString point =
    let
        { x, y } =
            point
                |> Point2d.toUnitless
    in
    { x = String.fromFloat x
    , y = String.fromFloat y
    }


translate : Float -> Float -> String
translate x y =
    "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


scale : Float -> Float -> String
scale x y =
    "scale(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"



--


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
