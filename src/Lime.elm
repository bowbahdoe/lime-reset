module Lime exposing
    ( a
    , abbr
    , acronym
    , address
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


{-| The name of a tag. This module is pretty loose with static typing, so this is
just here to make type signatures easier to read.
-}
type alias TagName =
    String


{-| A mapping from the name of a tag to the list of styles that should be applied
to that tag.
-}
type alias StyleDict =
    Dict TagName (List Css.Style)


makeStyleDict : List ( List TagName, List Css.Style ) -> StyleDict
makeStyleDict stylePairs =
    let
        combineDicts : (a -> a -> a) -> Dict comparable a -> Dict comparable a -> Dict comparable a
        combineDicts mergeValues d1 d2 =
            Dict.merge
                Dict.insert
                (\k v1 v2 -> Dict.insert k (mergeValues v1 v2))
                Dict.insert
                d1
                d2
                Dict.empty

        makeDict : ( List TagName, List Css.Style ) -> StyleDict
        makeDict ( tagNames, styles ) =
            Dict.fromList (List.map (\tagName -> ( tagName, styles )) tagNames)
    in
    List.foldl (combineDicts (++)) Dict.empty (List.map makeDict stylePairs)


{-| The actual meyer reset stylings.
-}
meyerStyles : StyleDict
meyerStyles =
    makeStyleDict
        [ ( [ "html"
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
          , [ Css.property "margin" "0"
            , Css.property "padding" "0"
            , Css.property "border" "0"
            , Css.fontSize (Css.pct 100)
            , Css.property "font" "inherit"
            , Css.verticalAlign Css.baseline
            ]
          )
        , ( [ "article"
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
          , [ Css.display Css.block ]
          )
        , ( [ "body" ], [ Css.lineHeight (Css.num 1) ] )
        , ( [ "ol", "ul" ], [ Css.listStyle Css.none ] )
        , ( [ "blockquote"
            , "q"
            ]
          , [ Css.property "quotes" "none"
            , Css.before
                [ Css.property "content" "''"
                , Css.property "content" "none"
                ]
            , Css.after
                [ Css.property "content" "''"
                , Css.property "content" "none"
                ]
            ]
          )
        , ( [ "table" ]
          , [ Css.borderCollapse Css.collapse
            , Css.property "border-spacing" "0"
            ]
          )
        ]


{-| A "normal" html tag. It takes in a list of attributes and a list of
children in order to produce some html. This type signature is repeated quite a bit,
so it is worth making this alias as a shorthand.
-}
type alias Tag msg =
    List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg


makeTag : TagName -> Tag msg
makeTag tagName =
    Html.styled
        (Html.node tagName)
        (Maybe.withDefault [] (Dict.get tagName meyerStyles))



-- All definitions come from the Mozilla Developer Network.
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/
{- What use would `html` be even?
   html : Tag msg
   html =
     makeTag "html"
-}


embed : Tag msg
embed =
    makeTag "embed"


img : Tag msg
img =
    makeTag "img"


a : Tag msg
a =
    makeTag "a"


abbr : Tag msg
abbr =
    makeTag "abbr"


acronym : Tag msg
acronym =
    makeTag "acronym"


address : Tag msg
address =
    makeTag "address"



{- Java Applets are deader than disco
   applet : Tag msg
   applet =
       makeTag "applet"
-}


article : Tag msg
article =
    makeTag "article"


aside : Tag msg
aside =
    makeTag "aside"


audio : Tag msg
audio =
    makeTag "audio"


b : Tag msg
b =
    makeTag "b"


big : Tag msg
big =
    makeTag "big"


blockquote : Tag msg
blockquote =
    makeTag "blockquote"


canvas : Tag msg
canvas =
    makeTag "canvas"


caption : Tag msg
caption =
    makeTag "caption"


center : Tag msg
center =
    makeTag "center"


cite : Tag msg
cite =
    makeTag "cite"


code : Tag msg
code =
    makeTag "code"


dd : Tag msg
dd =
    makeTag "dd"


del : Tag msg
del =
    makeTag "del"


details : Tag msg
details =
    makeTag "details"


dfn : Tag msg
dfn =
    makeTag "dfn"


div : Tag msg
div =
    makeTag "div"


dl : Tag msg
dl =
    makeTag "dl"


dt : Tag msg
dt =
    makeTag "dt"


em : Tag msg
em =
    makeTag "em"


fieldset : Tag msg
fieldset =
    makeTag "fieldset"


figcaption : Tag msg
figcaption =
    makeTag "figcaption"


figure : Tag msg
figure =
    makeTag "figure"


footer : Tag msg
footer =
    makeTag "footer"


form : Tag msg
form =
    makeTag "form"


h1 : Tag msg
h1 =
    makeTag "h1"


h2 : Tag msg
h2 =
    makeTag "h2"


h3 : Tag msg
h3 =
    makeTag "h3"


h4 : Tag msg
h4 =
    makeTag "h4"


h5 : Tag msg
h5 =
    makeTag "h5"


h6 : Tag msg
h6 =
    makeTag "h6"


header : Tag msg
header =
    makeTag "header"


hgroup : Tag msg
hgroup =
    makeTag "hgroup"


i : Tag msg
i =
    makeTag "i"


iframe : Tag msg
iframe =
    makeTag "iframe"


ins : Tag msg
ins =
    makeTag "ins"


kbd : Tag msg
kbd =
    makeTag "kbd"


label : Tag msg
label =
    makeTag "label"


legend : Tag msg
legend =
    makeTag "legend"


li : Tag msg
li =
    makeTag "li"


mark : Tag msg
mark =
    makeTag "mark"


menu : Tag msg
menu =
    makeTag "menu"


nav : Tag msg
nav =
    makeTag "nav"


object : Tag msg
object =
    makeTag "object"


ol : Tag msg
ol =
    makeTag "ol"


output : Tag msg
output =
    makeTag "output"


p : Tag msg
p =
    makeTag "p"


pre : Tag msg
pre =
    makeTag "pre"


q : Tag msg
q =
    makeTag "q"


ruby : Tag msg
ruby =
    makeTag "ruby"


s : Tag msg
s =
    makeTag "s"


samp : Tag msg
samp =
    makeTag "samp"


section : Tag msg
section =
    makeTag "section"


small : Tag msg
small =
    makeTag "small"


span : Tag msg
span =
    makeTag "span"


strike : Tag msg
strike =
    makeTag "strike"


strong : Tag msg
strong =
    makeTag "strong"


sub : Tag msg
sub =
    makeTag "sub"


summary : Tag msg
summary =
    makeTag "summary"


sup : Tag msg
sup =
    makeTag "sup"


table : Tag msg
table =
    makeTag "table"


tbody : Tag msg
tbody =
    makeTag "tbody"


td : Tag msg
td =
    makeTag "td"


tfoot : Tag msg
tfoot =
    makeTag "tfoot"


th : Tag msg
th =
    makeTag "th"


thead : Tag msg
thead =
    makeTag "thead"


time : Tag msg
time =
    makeTag "time"


tr : Tag msg
tr =
    makeTag "tr"


tt : Tag msg
tt =
    makeTag "tt"


u : Tag msg
u =
    makeTag "u"


ul : Tag msg
ul =
    makeTag "ul"


var : Tag msg
var =
    makeTag "var"


video : Tag msg
video =
    makeTag "video"
