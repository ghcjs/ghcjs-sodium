{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Alder.Html5.Attributes where

import           Data.Text           (Text)

import qualified Alder.Html.Events   as E
import           Alder.Html.Internal

-- This file was automatically generated by scripts/gen_html.py

-- | The @accept@ attribute.
accept :: Text -> Attribute
accept = attribute "accept"

-- | The @accept-charset@ attribute.
acceptCharset :: Text -> Attribute
acceptCharset = attribute "accept-charset"

-- | The @accesskey@ attribute.
accesskey :: Text -> Attribute
accesskey = attribute "accesskey"

-- | The @action@ attribute.
action :: Text -> Attribute
action = attribute "action"

-- | The @alt@ attribute.
alt :: Text -> Attribute
alt = attribute "alt"

-- | The @async@ attribute.
async :: Attribute
async = boolean "async"

-- | The @autocomplete@ attribute.
autocomplete :: Attribute
autocomplete = boolean "autocomplete"

-- | The @autofocus@ attribute.
autofocus :: Attribute
autofocus = boolean "autofocus"

-- | The @autoplay@ attribute.
autoplay :: Attribute
autoplay = boolean "autoplay"

-- | The @challenge@ attribute.
challenge :: Text -> Attribute
challenge = attribute "challenge"

-- | The @charset@ attribute.
charset :: Text -> Attribute
charset = attribute "charset"

-- | The @checked@ attribute.
checked :: Attribute
checked = boolean "checked"

-- | The @cite@ attribute.
cite :: Text -> Attribute
cite = attribute "cite"

-- | The @class@ attribute.
class_ :: Text -> Attribute
class_ = attribute "class"

-- | The @cols@ attribute.
cols :: Text -> Attribute
cols = attribute "cols"

-- | The @colspan@ attribute.
colspan :: Text -> Attribute
colspan = attribute "colspan"

-- | The @content@ attribute.
content :: Text -> Attribute
content = attribute "content"

-- | The @contenteditable@ attribute.
contenteditable :: Attribute
contenteditable = boolean "contenteditable"

-- | The @contextmenu@ attribute.
contextmenu :: Text -> Attribute
contextmenu = attribute "contextmenu"

-- | The @controls@ attribute.
controls :: Attribute
controls = boolean "controls"

-- | The @coords@ attribute.
coords :: Text -> Attribute
coords = attribute "coords"

-- | The @data@ attribute.
data_ :: Text -> Attribute
data_ = attribute "data"

-- | The @datetime@ attribute.
datetime :: Text -> Attribute
datetime = attribute "datetime"

-- | The @defer@ attribute.
defer :: Attribute
defer = boolean "defer"

-- | The @dir@ attribute.
dir :: Text -> Attribute
dir = attribute "dir"

-- | The @disabled@ attribute.
disabled :: Attribute
disabled = boolean "disabled"

-- | The @draggable@ attribute.
draggable :: Text -> Attribute
draggable = attribute "draggable"

-- | The @enctype@ attribute.
enctype :: Text -> Attribute
enctype = attribute "enctype"

-- | The @for@ attribute.
for :: Text -> Attribute
for = attribute "for"

-- | The @form@ attribute.
form :: Text -> Attribute
form = attribute "form"

-- | The @formaction@ attribute.
formaction :: Text -> Attribute
formaction = attribute "formaction"

-- | The @formenctype@ attribute.
formenctype :: Text -> Attribute
formenctype = attribute "formenctype"

-- | The @formmethod@ attribute.
formmethod :: Text -> Attribute
formmethod = attribute "formmethod"

-- | The @formnovalidate@ attribute.
formnovalidate :: Text -> Attribute
formnovalidate = attribute "formnovalidate"

-- | The @formtarget@ attribute.
formtarget :: Text -> Attribute
formtarget = attribute "formtarget"

-- | The @headers@ attribute.
headers :: Text -> Attribute
headers = attribute "headers"

-- | The @height@ attribute.
height :: Text -> Attribute
height = attribute "height"

-- | The @hidden@ attribute.
hidden :: Attribute
hidden = boolean "hidden"

-- | The @high@ attribute.
high :: Text -> Attribute
high = attribute "high"

-- | The @href@ attribute.
href :: Text -> Attribute
href = attribute "href"

-- | The @hreflang@ attribute.
hreflang :: Text -> Attribute
hreflang = attribute "hreflang"

-- | The @http-equiv@ attribute.
httpEquiv :: Text -> Attribute
httpEquiv = attribute "http-equiv"

-- | The @icon@ attribute.
icon :: Text -> Attribute
icon = attribute "icon"

-- | The @id@ attribute.
id :: Text -> Attribute
id = attribute "id"

-- | The @ismap@ attribute.
ismap :: Attribute
ismap = boolean "ismap"

-- | The @item@ attribute.
item :: Text -> Attribute
item = attribute "item"

-- | The @itemprop@ attribute.
itemprop :: Text -> Attribute
itemprop = attribute "itemprop"

-- | The @keytype@ attribute.
keytype :: Text -> Attribute
keytype = attribute "keytype"

-- | The @label@ attribute.
label :: Text -> Attribute
label = attribute "label"

-- | The @lang@ attribute.
lang :: Text -> Attribute
lang = attribute "lang"

-- | The @list@ attribute.
list :: Text -> Attribute
list = attribute "list"

-- | The @loop@ attribute.
loop :: Attribute
loop = boolean "loop"

-- | The @low@ attribute.
low :: Text -> Attribute
low = attribute "low"

-- | The @manifest@ attribute.
manifest :: Text -> Attribute
manifest = attribute "manifest"

-- | The @max@ attribute.
max :: Text -> Attribute
max = attribute "max"

-- | The @maxlength@ attribute.
maxlength :: Text -> Attribute
maxlength = attribute "maxlength"

-- | The @media@ attribute.
media :: Text -> Attribute
media = attribute "media"

-- | The @method@ attribute.
method :: Text -> Attribute
method = attribute "method"

-- | The @min@ attribute.
min :: Text -> Attribute
min = attribute "min"

-- | The @multiple@ attribute.
multiple :: Attribute
multiple = boolean "multiple"

-- | The @name@ attribute.
name :: Text -> Attribute
name = attribute "name"

-- | The @novalidate@ attribute.
novalidate :: Attribute
novalidate = boolean "novalidate"

-- | Set the handler for the @change@ event.
onchange :: Handler f => f E.InputEvent -> Attribute
onchange = onEvent "change"

-- | Set the handler for the @click@ event.
onclick :: Handler f => f E.MouseEvent -> Attribute
onclick = onEvent "click"

-- | Set the handler for the @dblclick@ event.
ondblclick :: Handler f => f E.MouseEvent -> Attribute
ondblclick = onEvent "dblclick"

-- | Set the handler for the @input@ event.
oninput :: Handler f => f E.InputEvent -> Attribute
oninput = onEvent "input"

-- | Set the handler for the @keydown@ event.
onkeydown :: Handler f => f E.KeyboardEvent -> Attribute
onkeydown = onEvent "keydown"

-- | Set the handler for the @keypress@ event.
onkeypress :: Handler f => f E.KeyboardEvent -> Attribute
onkeypress = onEvent "keypress"

-- | Set the handler for the @keyup@ event.
onkeyup :: Handler f => f E.KeyboardEvent -> Attribute
onkeyup = onEvent "keyup"

-- | Set the handler for the @mousedown@ event.
onmousedown :: Handler f => f E.MouseEvent -> Attribute
onmousedown = onEvent "mousedown"

-- | Set the handler for the @mousemove@ event.
onmousemove :: Handler f => f E.MouseEvent -> Attribute
onmousemove = onEvent "mousemove"

-- | Set the handler for the @mouseup@ event.
onmouseup :: Handler f => f E.MouseEvent -> Attribute
onmouseup = onEvent "mouseup"

-- | Set the handler for the @submit@ event.
onsubmit :: Handler f => f E.SubmitEvent -> Attribute
onsubmit = onEvent "submit"

-- | The @open@ attribute.
open :: Attribute
open = boolean "open"

-- | The @optimum@ attribute.
optimum :: Text -> Attribute
optimum = attribute "optimum"

-- | The @pattern@ attribute.
pattern :: Text -> Attribute
pattern = attribute "pattern"

-- | The @ping@ attribute.
ping :: Text -> Attribute
ping = attribute "ping"

-- | The @placeholder@ attribute.
placeholder :: Text -> Attribute
placeholder = attribute "placeholder"

-- | The @preload@ attribute.
preload :: Attribute
preload = boolean "preload"

-- | The @pubdate@ attribute.
pubdate :: Attribute
pubdate = boolean "pubdate"

-- | The @radiogroup@ attribute.
radiogroup :: Text -> Attribute
radiogroup = attribute "radiogroup"

-- | The @readonly@ attribute.
readonly :: Attribute
readonly = boolean "readonly"

-- | The @rel@ attribute.
rel :: Text -> Attribute
rel = attribute "rel"

-- | The @required@ attribute.
required :: Attribute
required = boolean "required"

-- | The @reversed@ attribute.
reversed :: Attribute
reversed = boolean "reversed"

-- | The @rows@ attribute.
rows :: Text -> Attribute
rows = attribute "rows"

-- | The @rowspan@ attribute.
rowspan :: Text -> Attribute
rowspan = attribute "rowspan"

-- | The @sandbox@ attribute.
sandbox :: Text -> Attribute
sandbox = attribute "sandbox"

-- | The @scope@ attribute.
scope :: Text -> Attribute
scope = attribute "scope"

-- | The @scoped@ attribute.
scoped :: Attribute
scoped = boolean "scoped"

-- | The @seamless@ attribute.
seamless :: Attribute
seamless = boolean "seamless"

-- | The @selected@ attribute.
selected :: Text -> Attribute
selected = attribute "selected"

-- | The @shape@ attribute.
shape :: Text -> Attribute
shape = attribute "shape"

-- | The @size@ attribute.
size :: Text -> Attribute
size = attribute "size"

-- | The @sizes@ attribute.
sizes :: Text -> Attribute
sizes = attribute "sizes"

-- | The @span@ attribute.
span :: Text -> Attribute
span = attribute "span"

-- | The @spellcheck@ attribute.
spellcheck :: Attribute
spellcheck = boolean "spellcheck"

-- | The @src@ attribute.
src :: Text -> Attribute
src = attribute "src"

-- | The @srcdoc@ attribute.
srcdoc :: Text -> Attribute
srcdoc = attribute "srcdoc"

-- | The @start@ attribute.
start :: Text -> Attribute
start = attribute "start"

-- | The @step@ attribute.
step :: Text -> Attribute
step = attribute "step"

-- | The @style@ attribute.
style :: Text -> Attribute
style = attribute "style"

-- | The @subject@ attribute.
subject :: Text -> Attribute
subject = attribute "subject"

-- | The @summary@ attribute.
summary :: Text -> Attribute
summary = attribute "summary"

-- | The @tabindex@ attribute.
tabindex :: Text -> Attribute
tabindex = attribute "tabindex"

-- | The @target@ attribute.
target :: Text -> Attribute
target = attribute "target"

-- | The @title@ attribute.
title :: Text -> Attribute
title = attribute "title"

-- | The @type@ attribute.
type_ :: Text -> Attribute
type_ = attribute "type"

-- | The @usemap@ attribute.
usemap :: Text -> Attribute
usemap = attribute "usemap"

-- | The @value@ attribute.
value :: Text -> Attribute
value = attribute "value"

-- | The @width@ attribute.
width :: Text -> Attribute
width = attribute "width"

-- | The @wrap@ attribute.
wrap :: Text -> Attribute
wrap = attribute "wrap"

-- | The @xmlns@ attribute.
xmlns :: Text -> Attribute
xmlns = attribute "xmlns"
