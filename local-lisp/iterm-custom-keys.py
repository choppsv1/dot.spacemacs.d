# -*- coding: utf-8 -*-#
# December 31 2015, Christian Hopps <chopps@gmail.com>
#
# Copyright (c) 2015-2016 by Christian E. Hopps.

# All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
from __future__ import absolute_import, division, unicode_literals, print_function, nested_scopes

import argparse

parser = argparse.ArgumentParser("")
parser.add_argument('-i', "--iterm", action="store_true", help='iterm')
args = parser.parse_args()

# Map elms are for C, M, C-S, M-S, C-M-S

shiftmap = { chr(x): chr(x).upper() for x in range(ord('a'), ord('z') + 1) }
shiftmap['`'] = '~'
shiftmap['1'] = '!'
shiftmap['2'] = '@'
shiftmap['3'] = '#'
shiftmap['4'] = '$'
shiftmap['5'] = '%'
shiftmap['6'] = '^'
shiftmap['7'] = '&'
shiftmap['8'] = '*'
shiftmap['9'] = '('
shiftmap['0'] = ')'
shiftmap['-'] = '_'
shiftmap['='] = '+'
shiftmap['['] = '{'
shiftmap[']'] = '}'
shiftmap['\\'] = '|'
shiftmap[';'] = ':'
shiftmap["'"] = '"'
shiftmap[","] = '<'
shiftmap["."] = '>'
shiftmap["/"] = '?'

# We don't use uppercase alpha for emacs codes so they are
# free to use for movement or function keys

# HELP
# INSERT
# HOME
# END
# PGUP
# PGDN
# RETURN
# BACKSPACE
# TAB
# DELETE

# "Keyboard Map" : {
mapfmt = r'''
                "0x{ucode:x}-0x{ameta:x}" : {{
                    "Comment" : "{emesc}{emkey}",
                    "Text" : "[27;{meta:d};{ascii:d}~",
                    "Action" : 10
                }}{comma} '''
elfmt = '    (define-key function-key-map "\e[27;{meta:d};{ascii:d}~" (kbd "{emesc}{emkey}"))'
elfmt2 = '    (define-key function-key-map "\e[27;{meta:d};{ascii:d}~" [{emesc}{emkey}])'

mapfuncfmt = r'''
                "0x{ucode:x}-0x{ameta:x}" : {{
                    "Comment" : "{emesc}{emkey}",
                    "Text" : "[1;{meta:d}{echar}",
                    "Action" : 10
                }}{comma} '''

elfuncfmt = '    (define-key function-key-map "\e[1;{meta:d}{echar}" [{emesc}{emkey}])'

mapfunc2fmt = r'''
                "0x{ucode:x}-0x{ameta:x}" : {{
                    "Comment" : "{emesc}{emkey}",
                    "Text" : "[{echar};{meta:d}~",
                    "Action" : 10
                }}{comma} '''

elfunc2fmt = '    (define-key function-key-map "\e[{echar};{meta:d}~" [{emesc}{emkey}])'

mapfunckeyfmt = r'''
                "0x{ucode:x}-0x{ameta:x}" : {{
                    "Comment" : "{emesc}{emkey}",
                    "Text" : "O{meta:d}{echar}",
                    "Action" : 10
                }}{comma} '''

elfunckeyfmt = '    (define-key function-key-map "\eO{meta:d}{echar}" [{emesc}{emkey}])'


SHIFT =   1
CONTROL = 2
META =    4

# No alpha in unshifted meta key
nometa = set([chr(x) for x in range(ord('a'), ord('z') + 1)])
nometa |= set([chr(x) for x in range(ord('A'), ord('Z') + 1)])

# No alpha in unshifted control
nocontrol = set([chr(x) for x in range(ord('a'), ord('z') + 1)])
# Add control characters that are normally handled
nocontrol.add('@')
nocontrol.add('[')
nocontrol.add(']')
nocontrol.add('\\')

def lookahead(iterable):
    """Return an element and an indication if it's the last element"""
    i = iter(iterable)
    last = next(i)
    for elm in i:
        yield last, False
        last = elm
    yield last, True


def makeapple (flags):
    return (flags << 1) << 16


def xtermmeta (flags):
    metamap = {
        SHIFT: 2,
        META: 3,
        META | SHIFT: 4,

        CONTROL: 5,
        CONTROL | SHIFT: 6,
        CONTROL | META: 7,
        CONTROL | META | SHIFT: 8
    }
    return metamap[flags]


def doit (iterm=False, darkstr="-dark"):
    if iterm:
        print("""{
    "Profiles": [
        {
        "Name": "xterm-keymap-profile",
        "Guid": "35E25353-C3DB-4DB0-A660-3D8BDF629AA3",
        "Dynamic Profile Parent Name": "default-dark",
        "Keyboard Map" : {""")

    # For alpha characters
    #    for emacs the actual key name is C-a and C-S-a (i.e., not C-A)
    #    for iterm the unicode is the actual key value (a vs A) also with shift key indicated
    #    we will use the unshifted key value in our escape value.
    # For non-alpha:
    #    for emacs the actual key names are C-` and C-~ (i.e., shifted value no S- key)
    #    for iterm the unicode is the actual value as well as above but with the shift key indicated.
    #    we will use the actual value in our escape code as well.

    for (val, sval), last in lookahead(shiftmap.items()):
        for meta, lastm in lookahead((CONTROL, META, CONTROL|META)):
            emesc = ""
            if meta & CONTROL:
                emesc += "C-"
            if meta & META:
                emesc += "M-"

            emkey = val
            # if emkey == '"' or emkey == "\\":
            #     emkey = "\\" + emkey

            xmeta = xtermmeta(meta)

            # Don't enter codes for normal alpha control character
            if meta == CONTROL and val in nocontrol:
                pass
            elif meta == META and val in nometa:
                pass
            else:
                # First handle unshifted version
                ucode = ord(emkey)
                if iterm:
                    print(mapfmt.format(ucode=ord(val),
                                        ameta=makeapple(meta),
                                        meta=xmeta,
                                        ascii=ucode,
                                        emesc=emesc,
                                        emkey=emkey,
                                        comma=","), end="")
                else:
                    if emkey == '"':
                        emkey = '\\"'
                    if emkey == '\\':
                        emkey = '\\\\'
                    print(elfmt.format(meta=xmeta, ascii=ucode, emkey=emkey, emesc=emesc))

            # Now handle shifted value
            meta |= SHIFT
            if val.isalpha():
                emkey = sval
                emesc += 'S-'
            else:
                emkey = sval

            xmeta = xtermmeta(meta)
            if meta == (CONTROL | SHIFT) and sval in nocontrol:
                pass
            elif meta == (META | SHIFT) and sval in nometa:
                pass
            else:
                ucode = ord(emkey)
                if iterm:
                    print(mapfmt.format(ucode=ord(sval),
                                        ameta=makeapple(meta | SHIFT),
                                        meta=xmeta,
                                        ascii=ucode,
                                        emesc=emesc,
                                        emkey=emkey,
                                        comma=","), end="")
                else:
                    if emkey == '"':
                        emkey = '\\"'
                    if emkey == '\\':
                        emkey = '\\\\'
                    print(elfmt.format(meta=xmeta, ascii=ucode, emkey=emkey, emesc=emesc))
                # if last and lastm:
                #     print()

    # =========================
    # Handle Arrow and HOME/END
    # =========================

    funcmap = {
        "up": ("A", 0xf700, elfuncfmt, mapfuncfmt, 0x200000),
        "down": ("B", 0xf701, elfuncfmt, mapfuncfmt, 0x200000),
        "left": ("D", 0xf702, elfuncfmt, mapfuncfmt, 0x200000),
        "right": ("C", 0xf703, elfuncfmt, mapfuncfmt, 0x200000),
        "end": ("F", 0xf72b, elfuncfmt, mapfuncfmt, 0),
        "home": ("H", 0xf729, elfuncfmt, mapfuncfmt, 0),
        "insert": ("2", 0xF727, elfunc2fmt, mapfunc2fmt, 0),
        "delete": ("3", 0xF728, elfunc2fmt, mapfunc2fmt, 0),
        "prior": ("5", 0xf72c, elfunc2fmt, mapfunc2fmt, 0),
        "next": ("6", 0xf72d, elfunc2fmt, mapfunc2fmt, 0),
        "tab": (9, 0x9, elfmt2, mapfmt, 0),
        "return": (13, 0xd, elfmt2, mapfmt, 0),
        "f1": ("P", 0xf704, elfunckeyfmt, mapfunckeyfmt, 0),
        "f2": ("Q", 0xf705, elfunckeyfmt, mapfunckeyfmt, 0),
        "f3": ("R", 0xf706, elfunckeyfmt, mapfunckeyfmt, 0),
        "f4": ("S", 0xf707, elfunckeyfmt, mapfunckeyfmt, 0),
        "f5": ("15", 0xf708, elfunc2fmt, mapfunc2fmt, 0),
        "f6": ("17", 0xf709, elfunc2fmt, mapfunc2fmt, 0),
        "f7": ("18", 0xf70A, elfunc2fmt, mapfunc2fmt, 0),
        "f8": ("19", 0xf70B, elfunc2fmt, mapfunc2fmt, 0),
        "f9": ("20", 0xf70C, elfunc2fmt, mapfunc2fmt, 0),
        "f10": ("21", 0xf70D, elfunc2fmt, mapfunc2fmt, 0),
        "f11": ("23", 0xf70E, elfunc2fmt, mapfunc2fmt, 0),
        "f12": ("24", 0xf70F, elfunc2fmt, mapfunc2fmt, 0),
    }
    xtermfmt = r"\e[27;%d;%d~"
    for (key, val), last in lookahead(funcmap.items()):
        for meta, lastm in lookahead((CONTROL,
                                      SHIFT,
                                      META,
                                      CONTROL | SHIFT,
                                      META | SHIFT,
                                      CONTROL | META,
                                      CONTROL | SHIFT | META)):

            if last and lastm:
                comma = ""
            else:
                comma = ","

            echar = val[0]
            ucode = val[1]
            xmeta = xtermmeta(meta)

            emesc = ""
            if meta & CONTROL:
                emesc += "C-"
            if meta & META:
                emesc += "M-"
            if meta & SHIFT:
                emesc += "S-"

            if iterm:
                # Tab character unicode 19 for backtab (shifted tab)
                if ucode == 9 and (meta & SHIFT):
                    ucode = 0x19
                print(val[3].format(ucode=ucode,
                                    ameta=makeapple(meta) | val[4],
                                    meta=xmeta,
                                    echar=echar,
                                    ascii=echar,
                                    comma=comma,
                                    emkey=emkey,
                                    emesc=emesc), end="")
            else:
                ucode = ord(emkey)
                print(val[2].format(meta=xmeta, ascii=echar, echar=echar, emkey=key, emesc=emesc))

            if last and lastm:
                print()
    if iterm:
        print('            }')
        print('        }')
        print('    ]')
        print('}')
    else:
        print("(provide 'iterm-xterm-extra)")

    # CSI 27 ; METACODE ; KEYCODE ~

# (defun chopps-add-local-keys (&optional frame)
#   (let ((keymap function-key-map))    ; was local-function-key-map

#     ;; ;; These are apparently the xterm defaults (there are others for mod combos)
#     (define-key keymap "\e[1;2A" [S-up])
#     (define-key keymap "\e[1;2B" [S-down])
#     (define-key keymap "\e[1;2C" [S-right])
#     (define-key keymap "\e[1;2D" [S-left])

#     (define-key keymap "\e[1;3A" [M-up])
#     (define-key keymap "\e[1;3B" [M-down])
#     (define-key keymap "\e[1;3C" [M-right])
#     (define-key keymap "\e[1;3D" [M-left])

#     (define-key keymap "\e[1;9A" [M-up])
#     (define-key keymap "\e[1;9B" [M-down])
#     (define-key keymap "\e[1;9C" [M-right])
#     (define-key keymap "\e[1;9D" [M-left])
#  aM
#     (define-key keymap "\e[1;5A" [C-up])
#     (define-key keymap "\e[1;5B" [C-down])
#     (define-key keymap "\e[1;5C" [C-right])
#     (define-key keymap "\e[1;5D" [C-left])

#     (define-key keymap "\e[1;6A" [C-S-up])
#     (define-key keymap "\e[1;6B" [C-S-down])
#     (define-key keymap "\e[1;6C" [C-S-right])
#     (define-key keymap "\e[1;6D" [C-S-left])

#     (define-key keymap "\e[1;4A" [M-S-up])
#     (define-key keymap "\e[1;4B" [M-S-down])
#     (define-key keymap "\e[1;4C" [M-S-right])
#     (define-key keymap "\e[1;4D" [M-S-left])

#     (define-key keymap "\e[1;10A" [M-S-up])
#     (define-key keymap "\e[1;10B" [M-S-down])
#     (define-key keymap "\e[1;10C" [M-S-right])
#     (define-key keymap "\e[1;10D" [M-S-left])

#     (define-key keymap (kbd "ESC \" 2 R") '[S-return])
#     (define-key keymap (kbd "ESC \" 2 T") '[backtab])

#     (define-key keymap (kbd "ESC \" 5 r") '[C-return])
#     (define-key keymap (kbd "ESC \" 5 2") '[C-2])
#     (define-key keymap (kbd "ESC \" 5 ;") '[?\C-\;])
#     (define-key keymap (kbd "ESC \" 5 :") '[?\C-\:])
#     (define-key keymap (kbd "ESC \" 5 ,") '[?\C-\,])
#     (define-key keymap (kbd "ESC \" 5 .") '[?\C-\.])
#     (define-key keymap (kbd "ESC \" 5 >") '[?\C-\>])
#     (define-key keymap (kbd "ESC \" 5 <") '[?\C-\<])
#     (define-key keymap (kbd "ESC \" 5 /") '[?\C-\/])
#     (define-key keymap (kbd "ESC \" 5 ?") '[?\C-\?])
#     (define-key keymap (kbd "ESC \" 5 \'") '[?\C-\'])
#     (define-key keymap (kbd "ESC \" 5 \"") '[?\C-\"])
#     (define-key keymap (kbd "ESC \" 5 |") '[?\C-|])
#     (define-key keymap (kbd "ESC \" 5 \\") '[?\C-\\])

#     (define-key keymap (kbd "ESC \" 5 t") '[C-tab])

#     (define-key keymap (kbd "ESC \" 5 T") '[C-backtab])
#     (define-key keymap (kbd "ESC \" 7 R") '[C-S-return])

#     (define-key keymap (kbd "ESC \" 7 2") '[C-S-2])
# ))

# (chopps-add-local-keys)

# (provide 'iterm-custom-keys)

doit(args.iterm)
