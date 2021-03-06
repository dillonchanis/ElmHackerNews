module HackerNews exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, oneOf, field, succeed)
import Helpers exposing (..)


-- MODELS


type AppState
    = ViewingAll
    | Reading
    | Loading


type alias Model =
    { stories : List Story
    , appState : AppState
    , storyIds : List Int
    , alertMessage : Maybe String
    }


type alias Story =
    { by : String
    , descendants : Int
    , id : Int
    , kids : List Int
    , score : Int
    , time : Int
    , title : String
    , url : String
    }


initialModel : Model
initialModel =
    { stories = []
    , appState = ViewingAll
    , storyIds = []
    , alertMessage = Nothing
    }



-- API CONSTANTS


apiGetItem : Int -> String
apiGetItem id =
    "https://hacker-news.firebaseio.com/v0/item/" ++ (toString id) ++ ".json"


apiTopStoriesUrl : String
apiTopStoriesUrl =
    "https://hacker-news.firebaseio.com/v0/topstories.json"



-- UPDATES


type Msg
    = Top (Result Http.Error (List Int))
    | TopStory (Result Http.Error Story)
    | CloseAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Top (Ok topStories) ->
            ( model, Cmd.batch (fetchTopStories topStories) )

        Top (Err error) ->
            ( { model | alertMessage = Just (translateHttpErrorToMessage error) }, Cmd.none )

        TopStory (Ok story) ->
            ( { model | stories = List.append (List.singleton story) model.stories }, Cmd.none )

        TopStory (Err error) ->
            ( { model | alertMessage = Just (translateHttpErrorToMessage error) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )


translateHttpErrorToMessage : Http.Error -> String
translateHttpErrorToMessage error =
    case error of
        Http.NetworkError ->
            "The server is experiencing issues or downtime. Please try again later."

        Http.BadStatus response ->
            (toString response.status)

        Http.BadPayload message _ ->
            "Decoding failure: " ++ message

        _ ->
            (toString error)



-- DECODERS/ENCODERS


storyDecoder : Decoder Story
storyDecoder =
    Decode.map8 Story
        (field "by" Decode.string)
        (Decode.oneOf
            [ (field "descendants" Decode.int)
            , succeed 0
            ]
        )
        (field "id" Decode.int)
        (Decode.oneOf
            [ (field "kids" (Decode.list Decode.int))
            , succeed []
            ]
        )
        (field "score" Decode.int)
        (field "time" Decode.int)
        (field "title" Decode.string)
        (Decode.oneOf
            [ (field "url" Decode.string)
            , succeed ""
            ]
        )



-- COMMANDS


fetchStoryById : Int -> Cmd Msg
fetchStoryById topStoryId =
    storyDecoder
        |> Http.get (apiGetItem topStoryId)
        |> Http.send TopStory


fetchTopStories : List Int -> List (Cmd Msg)
fetchTopStories topStoriesIds =
    List.map fetchStoryById (List.take 25 topStoriesIds)


getTopStories : Cmd Msg
getTopStories =
    (Decode.list Decode.int)
        |> Http.get apiTopStoriesUrl
        |> Http.send Top



-- VIEW


viewHeader : Html msg
viewHeader =
    header []
        [ h1 [ class "h1" ] [ text "Hacker News" ]
        ]


viewFooter : Html msg
viewFooter =
    footer [ class "footer" ]
        [ div [ class "has-text-centered" ]
            [ p [ class "footer-link" ] [ text "Visit ", a [ href "https://news.ycombinator.com/", target "_blank" ] [ text "Hacker News" ] ]
            , p [ class "footer-link" ] [ text "Powered by ", a [ href "http://elm-lang.org/", target "_blank" ] [ text "Elm" ], text "." ]
            , p [ class "footer-link" ] [ a [ href "https://github.com/dillonchanis", class "icon" ] [ i [ class "fa fa-github" ] [] ] ]
            ]
        ]


viewStoryPostInfo : Story -> Html msg
viewStoryPostInfo story =
    div []
        [ p [ class "text-muted" ]
            [ text ((toString story.score) ++ " points by ")
            , a [ href ("https://news.ycombinator.com/user?id=" ++ story.by) ] [ text story.by ]
            , span [ class "text-muted" ] [ text (" | " ++ (toString story.descendants) ++ " comments") ]
            ]
        ]


viewSingleStory : Story -> Html msg
viewSingleStory story =
    li [ class "story-item elevation" ]
        [ h2 [ class "h2" ] [ a [ href ("https://news.ycombinator.com/item?id=" ++ (toString story.id)), target "_blank" ] [ text story.title ] ]
        , viewStoryPostInfo story
        ]


viewStoryList : List Story -> Html msg
viewStoryList stories =
    let
        storyList =
            List.map viewSingleStory stories
    in
        ul [] storyList


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div [ class "container" ]
                [ viewHeader
                , alert CloseAlert model.alertMessage
                , viewStoryList model.stories
                ]
            ]
        , viewFooter
        ]


init : ( Model, Cmd Msg )
init =
    ( initialModel, getTopStories )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
