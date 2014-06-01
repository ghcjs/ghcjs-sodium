#!/usr/bin/env python

keywords = '''
case class data default deriving do else foreign if import in infix infixl
infixr instance let module newtype of then type where
'''.split()

def camelCase(s):
    words = s.split('-')
    words = [words[0]] + [w.title() for w in words[1:]]
    return ''.join(words)

def unreserved(s):
    if s in keywords:
        return s + '_'
    return s

### Elements

outfile = open('src/Alder/Html/Elements.hs', 'w')

html_tags = '''
a abbr address area article aside audio b base bdi bdo big blockquote body br
button canvas caption cite code col colgroup data datalist dd del details dfn
div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6
head header hr html i iframe img input ins kbd keygen label legend li link
main map mark menu menuitem meta meter nav noscript object ol optgroup option
output p param pre progress q rp rt ruby s samp script section select small
source span strong style sub summary sup table tbody td textarea tfoot th
thead time title tr track u ul var video wbr
'''.split()

void_tags = '''
area base br col command embed hr img input keygen link meta param source
track wbr
'''.split()

header = '''\
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Alder.Html.Elements where

import           Alder.Html.Internal

-- This file was automatically generated by scripts/gen_html.py
'''

html_declaration = '''
-- | The HTML @{0}@ tag.
{1} :: Html -> Html
{1} = Parent "{0}"
'''

void_declaration = '''
-- | The HTML @{0}@ tag.
{1} :: Html
{1} = Leaf "{0}"
'''

outfile.write(header)

for tag in html_tags:
    tag = camelCase(tag)
    if tag in void_tags:
        outfile.write(html_declaration.format(tag, unreserved(tag)))
    else:
        outfile.write(void_declaration.format(tag, unreserved(tag)))

outfile.close()

### Attributes

outfile = open('src/Alder/Html/Attributes.hs', 'w')

attributes = '''
accept accept-charset accesskey action alt async autocomplete autofocus
autoplay challenge charset checked cite class cols colspan content
contenteditable contextmenu controls coords data datetime defer dir disabled
draggable enctype for form formaction formenctype formmethod formnovalidate
formtarget headers height hidden high href hreflang http-equiv icon id ismap
item itemprop keytype label lang list loop low manifest max maxlength media
method min multiple name novalidate open optimum pattern ping placeholder
preload pubdate radiogroup readonly rel required reversed rows rowspan
sandbox scope scoped seamless selected shape size sizes span spellcheck src
srcdoc start step style subject summary tabindex target title type usemap
value width wrap xmlns
'''.split()

booleans = '''
async autocomplete autofocus autoplay checked contenteditable controls
defer disabled download hidden ismap loop multiple novalidate open preload
pubdate readonly required reversed seamless scoped spellcheck
'''.split()

header = '''\
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Alder.Html.Attributes where

import           Data.Text           (Text)

import           Alder.Html.Internal

-- This file was automatically generated by scripts/gen_html.py
'''

attr_declaration = '''
-- | The @{0}@ attribute.
{1} :: Text -> Attribute
{1} = attribute "{0}"
'''

boolean_declaration = '''
-- | The @{0}@ attribute.
{1} :: Attribute
{1} = boolean "{0}"
'''

outfile.write(header)

for attr in attributes:
    attr = camelCase(attr)
    if attr in booleans:
        outfile.write(boolean_declaration.format(attr, unreserved(attr)))
    else:
        outfile.write(attr_declaration.format(attr, unreserved(attr)))

outfile.close()