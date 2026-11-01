# Twemoji Mozilla

TwemojiMozilla.ttf: the emoji of Twemoji as a color font in the COLR/CPAL
format (version 0: layers of outlines, each in a color of the palette),
built by Mozilla for Firefox. TeXmacs draws the emoji with it on every
platform, so that a document looks the same everywhere.

Upstream: https://github.com/mozilla/twemoji-colr (release v0.7.0,
file Twemoji.Mozilla.ttf, sha256
6d90152ee0d29e82fe2a87793af5aa4b7ad13e6538360889e141e81ed299ee8e)
License: the code which builds the font under the Apache License 2.0, the
emoji art from Twemoji (https://github.com/twitter/twemoji) under CC-BY 4.0;
see LICENSE.md

| File | Version | Copyright |
|---|---|---|
| TwemojiMozilla.ttf | 0.7.0 (Emoji 14.0) | Copyright 2016-2018 Mozilla Foundation (code); emoji art Copyright Twitter, Inc and other contributors, CC-BY 4.0 |

The emoji font is chosen in `src/src/Graphics/Fonts/smart_font.cpp`
(`resolve_emoji`, `emoji_font_name`).
