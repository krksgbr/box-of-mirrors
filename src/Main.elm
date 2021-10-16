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
import Process
import Quantity exposing (Unitless)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task
import Vector2d exposing (Vector2d)


{-| The two views of the interactive.
In ReflectionExplorer the user searches for reflections,
in ImageDistanceDetails they learn about mirror-images' apparent distance.
-}
type View
    = ReflectionExplorer TraceResult
    | ImageDistanceDetails ImageDistanceFocus TraceResult MirroredSide


{-| Determines what ray segments to highlight when in ImageDistanceDetails.
-}
type ImageDistanceFocus
    = MirroredSegments
    | OriginalSegments



-- Type aliases for more concise signatures


type alias LineSegment =
    LineSegment2d Unitless ()


type alias Point =
    Point2d Unitless ()


type alias Circle =
    Circle2d Unitless ()


{-| Represents a reflection.
It tracks where the reflection occurred and how many times the ray bounced.
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


{-| Produced by tracing the trajectory of the ray through the box.
-}
type alias TraceResult =
    { trace : List LineSegment
    , image : Maybe Image
    }


{-| Walls of our box that are mirrored.
-}
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



--


type alias Model =
    { rayAngle : Float
    , view : View
    }


type Msg
    = SetRayAngle String
    | InitReflectionExplorer TraceResult
    | InitImageDistanceExplanation TraceResult MirroredSide
    | SwitchFocus


init : ( Model, Cmd Msg )
init =
    let
        rayAngle =
            30
    in
    ( { view = ReflectionExplorer (traceRay rayAngle)
      , rayAngle = rayAngle
      }
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
                        { model
                            | rayAngle = newRayAngle
                            , view = ReflectionExplorer (traceRay newRayAngle)
                        }
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        InitReflectionExplorer traceResult ->
            ( { model | view = ReflectionExplorer traceResult }, Cmd.none )

        InitImageDistanceExplanation trace reflectionSide ->
            ( { model | view = ImageDistanceDetails MirroredSegments trace reflectionSide }
            , switchFocus
            )

        SwitchFocus ->
            case model.view of
                ImageDistanceDetails focus trace reflectionSide ->
                    let
                        newFocus =
                            case focus of
                                MirroredSegments ->
                                    OriginalSegments

                                OriginalSegments ->
                                    MirroredSegments
                    in
                    ( { model | view = ImageDistanceDetails newFocus trace reflectionSide }
                    , switchFocus
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ HA.class "Applet" ] <|
        case model.view of
            ReflectionExplorer traceResult ->
                let
                    maybeReflection =
                        traceResult.image
                            |> Maybe.andThen (\(Image maybeReflection_) -> maybeReflection_)
                in
                [ H.div [ HA.class "Canvas Canvas--Explore" ]
                    [ Svg.svg
                        [ SA.viewBox "0 0 1.6 1.3"
                        ]
                        [ viewScene [ SA.transform <| translate 0.3 0.05 ]
                            [ traceResult.trace
                                |> viewRay (always [ SA.stroke colors.ray ])
                            , viewObserver { active = traceResult.image /= Nothing }
                            , maybeReflection
                                |> maybeView
                                    (\reflection ->
                                        viewMirrorImage (InitImageDistanceExplanation traceResult reflection.side) reflection
                                    )
                            ]
                        ]
                    ]
                , H.div [ HA.class "BottomModule" ]
                    [ H.div [ HA.class "BottomModule__Control" ] [ viewAngleSlider model.rayAngle ]
                    , H.div [ HA.class "BottomModule__Text" ]
                        [ case maybeReflection of
                            Nothing ->
                                H.p []
                                    [ H.text "Explore reflections by looking at rays leaving the cat from various angles. "
                                    , H.br [] []
                                    , H.text "Use the slider to search for ray at a specific angle. "
                                    ]

                            Just _ ->
                                H.p []
                                    [ H.b []
                                        [ H.text "You have found a reflection! "
                                        ]
                                    , H.br [] []
                                    , H.text "What do you notice about the mirror image?   "
                                    , H.br [] []
                                    , H.text "Under what condition does the cat appear mirrored and when does it not? "
                                    , H.br [] []
                                    , H.text "How does the size of the image change? "
                                    , H.br [] []
                                    , H.br [] []
                                    , H.em []
                                        [ H.text "Click the image to learn more about mirror images. "
                                        ]
                                    ]
                        ]
                    ]
                ]

            ImageDistanceDetails focus ({ trace } as traceResult) side ->
                let
                    numRaySegments =
                        List.length trace

                    viewSvg sceneIndex =
                        Svg.svg
                            [ SA.viewBox "0 0 1 1.2"
                            , SA.class "ImageDistance-Scene"
                            , SA.class <|
                                if modBy 2 sceneIndex == 1 then
                                    "ImageDistance-Scene--Flipped"

                                else
                                    ""
                            , SA.class <|
                                if sceneIndex == 0 then
                                    "ImageDistance-Scene--Original"

                                else
                                    "ImageDistance-Scene--Mirrored"
                            , SA.class <|
                                if sceneIndex == numRaySegments - 1 then
                                    "ImageDistance-Scene--WithImage"

                                else
                                    ""
                            , SA.class <|
                                if focus == MirroredSegments then
                                    "Focus-MirroredSegments"

                                else
                                    "Focus-OriginalSegments"
                            ]
                            [ viewScene []
                                [ trace
                                    |> viewRay
                                        (\raySegmentIndex ->
                                            let
                                                highLightRay =
                                                    [ SA.class "RaySegment--Highlighted"
                                                    , SA.strokeWidth "0.005"
                                                    ]
                                            in
                                            if focus == OriginalSegments && sceneIndex == 0 then
                                                highLightRay

                                            else if focus == MirroredSegments && (numRaySegments - sceneIndex) == raySegmentIndex + 1 then
                                                highLightRay

                                            else
                                                []
                                        )
                                , viewObserver { active = sceneIndex == 0 }
                                ]
                            ]
                in
                [ List.range 0 (numRaySegments - 1)
                    |> List.map viewSvg
                    |> (if side == Left then
                            List.reverse

                        else
                            identity
                       )
                    |> H.div [ HA.class "Canvas Canvas--ImageDistance" ]
                , H.div [ HA.class "BottomModule" ]
                    [ H.div [ HA.class "BottomModule__Control" ]
                        [ H.button [ HA.class "Button", HE.onClick <| InitReflectionExplorer traceResult ]
                            [ H.text "Go back"
                            ]
                        ]
                    , H.div [ HA.class "BottomModule__Text" ]
                        [ H.p []
                            [ H.text "Mirror images appear as if they were \"behind\" the mirror. "
                            , H.text "The distance at which they appear equals to the total distance the ray had to travel before it entered one's eye. "
                            , H.text "An interesting way of seeing this is to imagine the entire box being mirrored as many times as the ray bounced. "
                            , H.br [] []
                            , H.text "Compare the highlighted trajectories of the ray. What do you find in common between them? "
                            ]
                        ]
                    ]
                ]



-- Helpers for tracking the path of the ray.


traceRay : Float -> TraceResult
traceRay angle =
    let
        trajectory =
            Vector2d.rTheta (Quantity.float 5) (Angle.degrees angle)
                |> LineSegment2d.fromPointAndVector objectPosition
    in
    traceRayRecursively trajectory
        { prevReflection = Nothing
        , trace = [ LineSegment2d.startPoint trajectory ]
        }


traceRayRecursively :
    LineSegment
    -> { prevReflection : Maybe Reflection, trace : List Point }
    -> TraceResult
traceRayRecursively trajectory { prevReflection, trace } =
    let
        notLastMirroredWall { mirrored } =
            mirrored == Nothing || mirrored /= Maybe.map .side prevReflection

        -- Check if we've hit a wall
        maybeWallHit : Maybe WallHit
        maybeWallHit =
            box
                |> List.filter notLastMirroredWall
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
            { trace = pointsToSegments <| trace ++ [ point ]
            , image = Nothing
            }

        hitObserver point =
            { trace = pointsToSegments <| trace ++ [ point ]
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


pointsToSegments : List Point -> List LineSegment
pointsToSegments points =
    List.map2 LineSegment2d.from points (List.tail points |> Maybe.withDefault [])


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


viewScene : List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
viewScene attrs contents =
    Svg.g attrs <|
        (box
            |> List.map viewWall
            |> Svg.g []
        )
            :: viewObject
            :: contents


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
    in
    Svg.g
        [ SA.transform <| translate x y
        , SA.class "Observer"
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
        , Svg.circle
            [ SA.r <| String.fromFloat (radius * 0.5)
            , SA.fill "black"
            ]
            []
        ]


viewWall : Wall -> Svg msg
viewWall { mirrored, lineSegment } =
    viewLineSegment
        [ SA.stroke <|
            if mirrored /= Nothing then
                colors.mirror

            else
                "rgb(0, 0, 0)"
        , SA.strokeWidth "0.015"
        , SA.class "Wall"
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
    Svg.g [ SA.transform (translate x y), SA.class "Object" ]
        [ Assets.cat
        ]


viewRay : (Int -> List (Svg.Attribute msg)) -> List LineSegment -> Svg msg
viewRay getAttrs lineSegments =
    lineSegments
        |> List.indexedMap
            (\i ->
                viewLineSegment
                    ([ SA.strokeWidth "0.003"
                     , SA.class "RaySegment"
                     , SA.stroke colors.ray
                     ]
                        ++ getAttrs i
                    )
            )
        |> Svg.g []


viewMirrorImage : msg -> Reflection -> Svg msg
viewMirrorImage onClick { side, point, bounces } =
    let
        { x, y } =
            point
                |> Point2d.toUnitless

        s =
            0.2

        triangleWidth =
            0.025

        arrow attrs =
            Svg.g attrs
                [ Svg.polygon
                    [ [ [ 0, 0 ]
                      , [ triangleWidth, triangleWidth ]
                      , [ 0, triangleWidth * 2 ]
                      ]
                        |> List.map (List.map String.fromFloat >> String.join " ")
                        |> String.join ", "
                        |> SA.points
                    , SA.transform <| translate 0 -triangleWidth
                    , SA.fill "black"
                    , SA.opacity "0.4"
                    ]
                    []
                ]
    in
    Svg.g
        [ SA.transform <|
            translate
                (if side == Left then
                    x - s - triangleWidth

                 else
                    x + triangleWidth
                )
                (y - s / 2)
        , SA.class "MirrorImage"
        , HE.onClick onClick
        ]
        [ Svg.rect
            [ SA.width <| String.fromFloat s
            , SA.height <| String.fromFloat s
            , SA.fill colors.mirror
            , SA.strokeWidth "0.003"
            , SA.stroke "black"
            , SA.class "MirrorImage__Frame"
            ]
            []
        , Svg.g
            [ SA.transform <| translate (s / 2) (s / 2) ++ " scale(0.4)"
            , SA.opacity "0.8"
            ]
            [ Svg.g
                [ SA.transform <|
                    let
                        scaleFactor =
                            max (1 - toFloat bounces * 0.05) 0.1

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
        , arrow
            [ SA.transform <|
                if side == Left then
                    translate s (s / 2)

                else
                    translate 0 (s / 2) ++ " " ++ scale -1 1
            ]
        ]


viewAngleSlider : Float -> Html Msg
viewAngleSlider value =
    H.label [ HA.class "AngleSlider" ]
        [ H.div []
            [ H.text <| "Ray angle"
            ]
        , H.input
            [ HA.type_ "range"
            , HA.min "0"
            , HA.max "360"
            , HA.step "0.025"
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


switchFocus : Cmd Msg
switchFocus =
    Process.sleep 1500
        |> Task.perform (always SwitchFocus)


{-| Gives the intersection points of a line segment and a circle.
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
