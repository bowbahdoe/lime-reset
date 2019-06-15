module Orange exposing
    ( a
    , abbr
    , acronym
    , address
    , applet
    , article
    , aside
    , audio
    , b
    , big
    , blockquote
    , canvas
    , caption
    , center
    , cite
    , code
    , dd
    , del
    , details
    , dfn
    , div
    , dl
    , dt
    , em
    , embed
    , fieldset
    , figcaption
    , figure
    , footer
    , form
    , h1
    , h2
    , h3
    , h4
    , h5
    , h6
    , header
    , hgroup
    , i
    , iframe
    , img
    , ins
    , kbd
    , label
    , legend
    , li
    , mark
    , menu
    , nav
    , object
    , ol
    , output
    , p
    , pre
    , q
    , ruby
    , s
    , samp
    , section
    , small
    , span
    , strike
    , strong
    , sub
    , summary
    , sup
    , table
    , tbody
    , td
    , tfoot
    , th
    , thead
    , time
    , tr
    , tt
    , u
    , ul
    , var
    , video
    )

import Css
import Dict exposing (Dict)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes


type alias TagName =
    String


type alias StyleDict =
    Dict TagName (List Css.Style)


mergeTagStyleDicts : List StyleDict -> StyleDict
mergeTagStyleDicts dicts =
    let
        mergeTwo : StyleDict -> StyleDict -> StyleDict
        mergeTwo d1 d2 =
            Dict.merge
                (\tagName style1 newDict -> Dict.insert tagName style1 newDict)
                (\tagName style1 style2 newDict -> Dict.insert tagName (style1 ++ style2) newDict)
                (\tagName style2 newDict -> Dict.insert tagName style2 newDict)
                d1
                d2
                Dict.empty
    in
    List.foldl mergeTwo Dict.empty dicts


makeStyleDict : List TagName -> List Css.Style -> StyleDict
makeStyleDict tagNames style =
    Dict.fromList (List.map (\tagName -> ( tagName, style )) tagNames)


styleDict : StyleDict
styleDict =
    mergeTagStyleDicts
        [ makeStyleDict
            [ "html"
            , "body"
            , "div"
            , "span"
            , "applet"
            , "object"
            , "iframe"
            , "h1"
            , "h2"
            , "h3"
            , "h4"
            , "h5"
            , "h6"
            , "p"
            , "blockquote"
            , "pre"
            , "a"
            , "abbr"
            , "acronym"
            , "address"
            , "big"
            , "cite"
            , "code"
            , "del"
            , "dfn"
            , "em"
            , "img"
            , "ins"
            , "kbd"
            , "q"
            , "s"
            , "samp"
            , "small"
            , "strike"
            , "strong"
            , "sub"
            , "sup"
            , "tt"
            , "var"
            , "b"
            , "u"
            , "i"
            , "center"
            , "dl"
            , "dt"
            , "dd"
            , "ol"
            , "ul"
            , "li"
            , "fieldset"
            , "form"
            , "label"
            , "legend"
            , "table"
            , "caption"
            , "tbody"
            , "tfoot"
            , "thead"
            , "tr"
            , "th"
            , "td"
            , "article"
            , "aside"
            , "canvas"
            , "details"
            , "embed"
            , "figure"
            , "figcaption"
            , "footer"
            , "header"
            , "hgroup"
            , "menu"
            , "nav"
            , "output"
            , "ruby"
            , "section"
            , "summary"
            , "time"
            , "mark"
            , "audio"
            , "video"
            ]
            [ Css.property "margin" "0"
            , Css.property "padding" "0"
            , Css.property "border" "0"
            , Css.fontSize (Css.pct 100)
            , Css.property "font" "inherit"
            , Css.verticalAlign Css.baseline
            ]
        , makeStyleDict
            [ "article"
            , "aside"
            , "details"
            , "figcaption"
            , "figure"
            , "footer"
            , "header"
            , "hgroup"
            , "menu"
            , "nav"
            , "section"
            ]
            [ Css.display Css.block ]
        , makeStyleDict [ "body" ] [ Css.lineHeight (Css.num 1) ]
        , makeStyleDict [ "ol", "ul" ] [ Css.listStyle Css.none ]
        , makeStyleDict
            [ "blockquote"
            , "q"
            ]
            [ Css.property "quotes" "none"
            , Css.before
                [ Css.property "content" "''"
                , Css.property "content" "none"
                ]
            , Css.after
                [ Css.property "content" "''"
                , Css.property "content" "none"
                ]
            ]
        , makeStyleDict [ "table" ]
            [ Css.borderCollapse Css.collapse
            , Css.property "border-spacing" "0"
            ]
        ]


tagStyle : TagName -> Css.Style
tagStyle tagName =
    Css.batch (Maybe.withDefault [] (Dict.get tagName styleDict))


type alias NormalTag msg =
    List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg


makeTag : TagName -> NormalTag msg
makeTag tagName =
    let
        newTag : NormalTag msg
        newTag attrs children =
            Html.node tagName (Attributes.css [ tagStyle tagName ] :: attrs) children
    in
    newTag


embed : NormalTag msg
embed =
    makeTag "embed"


img : NormalTag msg
img =
    makeTag "img"


a : NormalTag msg
a =
    makeTag "a"


abbr : NormalTag msg
abbr =
    makeTag "abbr"


acronym : NormalTag msg
acronym =
    makeTag "acronym"


address : NormalTag msg
address =
    makeTag "address"


applet : NormalTag msg
applet =
    makeTag "applet"


article : NormalTag msg
article =
    makeTag "article"


aside : NormalTag msg
aside =
    makeTag "aside"


audio : NormalTag msg
audio =
    makeTag "audio"


b : NormalTag msg
b =
    makeTag "b"


big : NormalTag msg
big =
    makeTag "big"


blockquote : NormalTag msg
blockquote =
    makeTag "blockquote"


canvas : NormalTag msg
canvas =
    makeTag "canvas"


caption : NormalTag msg
caption =
    makeTag "caption"


center : NormalTag msg
center =
    makeTag "center"


cite : NormalTag msg
cite =
    makeTag "cite"


code : NormalTag msg
code =
    makeTag "code"


dd : NormalTag msg
dd =
    makeTag "dd"


del : NormalTag msg
del =
    makeTag "del"


details : NormalTag msg
details =
    makeTag "details"


dfn : NormalTag msg
dfn =
    makeTag "dfn"


div : NormalTag msg
div =
    makeTag "div"


dl : NormalTag msg
dl =
    makeTag "dl"


dt : NormalTag msg
dt =
    makeTag "dt"


em : NormalTag msg
em =
    makeTag "em"


fieldset : NormalTag msg
fieldset =
    makeTag "fieldset"


figcaption : NormalTag msg
figcaption =
    makeTag "figcaption"


figure : NormalTag msg
figure =
    makeTag "figure"


footer : NormalTag msg
footer =
    makeTag "footer"


form : NormalTag msg
form =
    makeTag "form"


h1 : NormalTag msg
h1 =
    makeTag "h1"


h2 : NormalTag msg
h2 =
    makeTag "h2"


h3 : NormalTag msg
h3 =
    makeTag "h3"


h4 : NormalTag msg
h4 =
    makeTag "h4"


h5 : NormalTag msg
h5 =
    makeTag "h5"


h6 : NormalTag msg
h6 =
    makeTag "h6"


header : NormalTag msg
header =
    makeTag "header"


hgroup : NormalTag msg
hgroup =
    makeTag "hgroup"


i : NormalTag msg
i =
    makeTag "i"


iframe : NormalTag msg
iframe =
    makeTag "iframe"


ins : NormalTag msg
ins =
    makeTag "ins"


kbd : NormalTag msg
kbd =
    makeTag "kbd"


label : NormalTag msg
label =
    makeTag "label"


legend : NormalTag msg
legend =
    makeTag "legend"


li : NormalTag msg
li =
    makeTag "li"


mark : NormalTag msg
mark =
    makeTag "mark"


menu : NormalTag msg
menu =
    makeTag "menu"


nav : NormalTag msg
nav =
    makeTag "nav"


object : NormalTag msg
object =
    makeTag "object"


ol : NormalTag msg
ol =
    makeTag "ol"


output : NormalTag msg
output =
    makeTag "output"


p : NormalTag msg
p =
    makeTag "p"


pre : NormalTag msg
pre =
    makeTag "pre"


q : NormalTag msg
q =
    makeTag "q"


ruby : NormalTag msg
ruby =
    makeTag "ruby"


s : NormalTag msg


s =
    makeTag "s"


samp : NormalTag msg
samp =
    makeTag "samp"


section : NormalTag msg
section =
    makeTag "section"


small : NormalTag msg
small =
    makeTag "small"


span : NormalTag msg
span =
    makeTag "span"


strike : NormalTag msg
strike =
    makeTag "strike"


strong : NormalTag msg
strong =
    makeTag "strong"


sub : NormalTag msg
sub =
    makeTag "sub"


summary : NormalTag msg
summary =
    makeTag "summary"


sup : NormalTag msg
sup =
    makeTag "sup"


table : NormalTag msg
table =
    makeTag "table"


tbody : NormalTag msg
tbody =
    makeTag "tbody"


td : NormalTag msg
td =
    makeTag "td"


tfoot : NormalTag msg
tfoot =
    makeTag "tfoot"


th : NormalTag msg
th =
    makeTag "th"


thead : NormalTag msg
thead =
    makeTag "thead"


time : NormalTag msg
time =
    makeTag "time"


tr : NormalTag msg
tr =
    makeTag "tr"


tt : NormalTag msg
tt =
    makeTag "tt"


u : NormalTag msg
u =
    makeTag "u"


ul : NormalTag msg
ul =
    makeTag "ul"


var : NormalTag msg
var =
    makeTag "var"


video : NormalTag msg
video =
    makeTag "video"
