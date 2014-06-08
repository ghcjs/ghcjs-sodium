{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Alder.Html.Attributes where

import           Data.Text           (Text)

import qualified Alder.Html.Events   as E
import           Alder.Html.Internal

-- This file was automatically generated by scripts/gen_html.py

-- | The @accept@ attribute.
accept :: Text -> Attribute
accept = token "accept"

-- | The @accesskey@ attribute.
accessKey :: Text -> Attribute
accessKey = token "accesskey"

-- | The @action@ attribute.
action :: Text -> Attribute
action = token "action"

-- | The @allowfullscreen@ attribute.
allowFullScreen :: Text -> Attribute
allowFullScreen = token "allowfullscreen"

-- | The @allowtransparency@ attribute.
allowTransparency :: Text -> Attribute
allowTransparency = token "allowtransparency"

-- | The @alt@ attribute.
alt :: Text -> Attribute
alt = token "alt"

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

-- | The @cellpadding@ attribute.
cellPadding :: Text -> Attribute
cellPadding = token "cellpadding"

-- | The @cellspacing@ attribute.
cellSpacing :: Text -> Attribute
cellSpacing = token "cellspacing"

-- | The @charset@ attribute.
charset :: Text -> Attribute
charset = token "charset"

-- | The @checked@ attribute.
checked :: Attribute
checked = boolean "checked"

-- | The @classname@ attribute. This will append to the current value of the
-- attribute.
className :: Text -> Attribute
className = tokenSet "classname"

-- | The @colspan@ attribute.
colSpan :: Text -> Attribute
colSpan = token "colspan"

-- | The @cols@ attribute.
cols :: Text -> Attribute
cols = token "cols"

-- | The @content@ attribute.
content :: Text -> Attribute
content = token "content"

-- | The @contenteditable@ attribute.
contentEditable :: Text -> Attribute
contentEditable = token "contenteditable"

-- | The @contextmenu@ attribute.
contextMenu :: Text -> Attribute
contextMenu = token "contextmenu"

-- | The @controls@ attribute.
controls :: Text -> Attribute
controls = token "controls"

-- | The @data@ attribute.
data_ :: Text -> Attribute
data_ = token "data"

-- | The @datetime@ attribute.
dateTime :: Text -> Attribute
dateTime = token "datetime"

-- | The @defer@ attribute.
defer :: Attribute
defer = boolean "defer"

-- | The @dir@ attribute.
dir :: Text -> Attribute
dir = token "dir"

-- | The @disabled@ attribute.
disabled :: Attribute
disabled = boolean "disabled"

-- | The @download@ attribute.
download :: Attribute
download = boolean "download"

-- | The @draggable@ attribute.
draggable :: Text -> Attribute
draggable = token "draggable"

-- | The @enctype@ attribute.
encType :: Text -> Attribute
encType = token "enctype"

-- | The @form@ attribute.
form :: Text -> Attribute
form = token "form"

-- | The @formnovalidate@ attribute.
formNoValidate :: Text -> Attribute
formNoValidate = token "formnovalidate"

-- | The @frameborder@ attribute.
frameBorder :: Text -> Attribute
frameBorder = token "frameborder"

-- | The @height@ attribute.
height :: Text -> Attribute
height = token "height"

-- | The @hidden@ attribute.
hidden :: Attribute
hidden = boolean "hidden"

-- | The @href@ attribute.
href :: Text -> Attribute
href = token "href"

-- | The @htmlfor@ attribute.
htmlFor :: Text -> Attribute
htmlFor = token "htmlfor"

-- | The @icon@ attribute.
icon :: Text -> Attribute
icon = token "icon"

-- | The @id@ attribute.
id :: Text -> Attribute
id = token "id"

-- | The @label@ attribute.
label :: Text -> Attribute
label = token "label"

-- | The @lang@ attribute.
lang :: Text -> Attribute
lang = token "lang"

-- | The @list@ attribute.
list :: Text -> Attribute
list = token "list"

-- | The @loop@ attribute.
loop :: Attribute
loop = boolean "loop"

-- | The @max@ attribute.
max :: Text -> Attribute
max = token "max"

-- | The @maxlength@ attribute.
maxLength :: Text -> Attribute
maxLength = token "maxlength"

-- | The @method@ attribute.
method :: Text -> Attribute
method = token "method"

-- | The @min@ attribute.
min :: Text -> Attribute
min = token "min"

-- | The @multiple@ attribute.
multiple :: Attribute
multiple = boolean "multiple"

-- | The @name@ attribute.
name :: Text -> Attribute
name = token "name"

-- | The @novalidate@ attribute.
noValidate :: Attribute
noValidate = boolean "novalidate"

-- | Set the handler for the @blur@ event.
onBlur :: Handler f => f E.FocusEvent -> Attribute
onBlur = onEvent "blur"

-- | Set the handler for the @click@ event.
onClick :: Handler f => f E.MouseEvent -> Attribute
onClick = onEvent "click"

-- | Set the handler for the @dblclick@ event.
onDoubleClick :: Handler f => f E.MouseEvent -> Attribute
onDoubleClick = onEvent "dblclick"

-- | Set the handler for the @focus@ event.
onFocus :: Handler f => f E.FocusEvent -> Attribute
onFocus = onEvent "focus"

-- | Set the handler for the @input@ event.
onInput :: Handler f => f E.InputEvent -> Attribute
onInput = onEvent "input"

-- | Set the handler for the @keydown@ event.
onKeyDown :: Handler f => f E.KeyboardEvent -> Attribute
onKeyDown = onEvent "keydown"

-- | Set the handler for the @keypress@ event.
onKeyPress :: Handler f => f E.KeyboardEvent -> Attribute
onKeyPress = onEvent "keypress"

-- | Set the handler for the @keyup@ event.
onKeyUp :: Handler f => f E.KeyboardEvent -> Attribute
onKeyUp = onEvent "keyup"

-- | Set the handler for the @mousedown@ event.
onMouseDown :: Handler f => f E.MouseEvent -> Attribute
onMouseDown = onEvent "mousedown"

-- | Set the handler for the @mouseenter@ event.
onMouseEnter :: Handler f => f E.MouseEvent -> Attribute
onMouseEnter = onEvent "mouseenter"

-- | Set the handler for the @mouseleave@ event.
onMouseLeave :: Handler f => f E.MouseEvent -> Attribute
onMouseLeave = onEvent "mouseleave"

-- | Set the handler for the @mousemove@ event.
onMouseMove :: Handler f => f E.MouseEvent -> Attribute
onMouseMove = onEvent "mousemove"

-- | Set the handler for the @mouseup@ event.
onMouseUp :: Handler f => f E.MouseEvent -> Attribute
onMouseUp = onEvent "mouseup"

-- | Set the handler for the @submit@ event.
onSubmit :: Handler f => f E.SubmitEvent -> Attribute
onSubmit = onEvent "submit"

-- | The @pattern@ attribute.
pattern :: Text -> Attribute
pattern = token "pattern"

-- | The @placeholder@ attribute.
placeholder :: Text -> Attribute
placeholder = token "placeholder"

-- | The @poster@ attribute.
poster :: Text -> Attribute
poster = token "poster"

-- | The @preload@ attribute.
preload :: Attribute
preload = boolean "preload"

-- | The @radiogroup@ attribute.
radioGroup :: Text -> Attribute
radioGroup = token "radiogroup"

-- | The @readonly@ attribute.
readOnly :: Attribute
readOnly = boolean "readonly"

-- | The @rel@ attribute.
rel :: Text -> Attribute
rel = token "rel"

-- | The @required@ attribute.
required :: Attribute
required = boolean "required"

-- | The @reversed@ attribute.
reversed :: Attribute
reversed = boolean "reversed"

-- | The @role@ attribute.
role :: Text -> Attribute
role = token "role"

-- | The @rowspan@ attribute.
rowSpan :: Text -> Attribute
rowSpan = token "rowspan"

-- | The @rows@ attribute.
rows :: Text -> Attribute
rows = token "rows"

-- | The @sandbox@ attribute.
sandbox :: Text -> Attribute
sandbox = token "sandbox"

-- | The @scope@ attribute.
scope :: Text -> Attribute
scope = token "scope"

-- | The @scrollleft@ attribute.
scrollLeft :: Text -> Attribute
scrollLeft = token "scrollleft"

-- | The @scrolltop@ attribute.
scrollTop :: Text -> Attribute
scrollTop = token "scrolltop"

-- | The @seamless@ attribute.
seamless :: Attribute
seamless = boolean "seamless"

-- | The @selected@ attribute.
selected :: Text -> Attribute
selected = token "selected"

-- | The @size@ attribute.
size :: Text -> Attribute
size = token "size"

-- | The @span@ attribute.
span :: Text -> Attribute
span = token "span"

-- | The @spellcheck@ attribute.
spellCheck :: Attribute
spellCheck = boolean "spellcheck"

-- | The @src@ attribute.
src :: Text -> Attribute
src = token "src"

-- | The @srcdoc@ attribute.
srcDoc :: Text -> Attribute
srcDoc = token "srcdoc"

-- | The @step@ attribute.
step :: Text -> Attribute
step = token "step"

-- | The @style@ attribute.
style :: Text -> Attribute
style = token "style"

-- | The @tabindex@ attribute.
tabIndex :: Text -> Attribute
tabIndex = token "tabindex"

-- | The @target@ attribute.
target :: Text -> Attribute
target = token "target"

-- | The @title@ attribute.
title :: Text -> Attribute
title = token "title"

-- | The @type@ attribute.
type_ :: Text -> Attribute
type_ = token "type"

-- | The @value@ attribute.
value :: Text -> Attribute
value = token "value"

-- | The @width@ attribute.
width :: Text -> Attribute
width = token "width"

-- | The @wmode@ attribute.
wmode :: Text -> Attribute
wmode = token "wmode"
