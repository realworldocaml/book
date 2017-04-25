# -*- coding: utf-8 -*-
"""
    pygments.lexers.ml
    ~~~~~~~~~~~~~~~~~~

    Lexers for ML family languages.

    :copyright: Copyright 2006-2017 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, default, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Error

class OcamlLexer(RegexLexer):
    """
    For the OCaml language.

    .. versionadded:: 0.7
    """

    name = 'OCaml'
    aliases = ['ocaml']
    filenames = ['*.ml', '*.mli', '*.mll', '*.mly']
    mimetypes = ['text/x-ocaml']

    keywords = (
        'as', 'assert', 'begin', 'class', 'constraint', 'do', 'done',
        'downto', 'else', 'end', 'exception', 'external', 'false',
        'for', 'fun', 'function', 'functor', 'if', 'in', 'include',
        'inherit', 'initializer', 'lazy', 'let', 'match', 'method',
        'module', 'mutable', 'new', 'object', 'of', 'open', 'private',
        'raise', 'rec', 'sig', 'struct', 'then', 'to', 'true', 'try',
        'type', 'value', 'val', 'virtual', 'when', 'while', 'with'
    )
    keyopts = (
        '!=', '#', '&', '&&', r'\(', r'\)', r'\*', r'\+', ',', '-',
        r'-\.', '->', r'\.', r'\.\.', ':', '::', ':=', ':>', ';', ';;', '<',
        '<-', '=', '>', '>]', r'>\}', r'\?', r'\?\?', r'\[', r'\[<', r'\[>',
        r'\[\|', ']', '_', '`', r'\{', r'\{<', r'\|', r'\|]', r'\}', '~',
        """
        SanderSpies: added the output colors of RWO chapter 7 to ensure that they
        are not mangled by the tokenizer
        """
        r'\[38;5;(4|9|236)m', '[0m'
    )
    operators = r'[!$%&*+\./:<=>?@^|~-]'
    word_operators = ('and', 'asr', 'land', 'lor', 'lsl', 'lxor', 'mod', 'or')
    prefix_syms = r'[!?~]'
    infix_syms = r'[=<>@^|&+\*/$%-]'
    primitives = ('unit', 'int', 'float', 'bool', 'string', 'char', 'list', 'array')

    tokens = {
        'escape-sequence': [
            (r'\\[\\"\'ntbr]', String.Escape),
            (r'\\[0-9]{3}', String.Escape),
            (r'\\x[0-9a-fA-F]{2}', String.Escape),
        ],
        'root': [
            (r'\s+', Text),
            (r'false|true|\(\)|\[\]', Name.Builtin.Pseudo),

            (r'\b([A-Z][\w\']*)(?=\s*\.)', Name.Namespace, 'dotted'),
            (r'\b([A-Z][\w\']*)', Name.Class),
            (r'\(\*(?![)])', Comment, 'comment'),
            (r'\b(%s)\b' % '|'.join(keywords), Keyword),
            (r'(%s)' % '|'.join(keyopts[::-1]), Operator),
            (r'(%s|%s)?%s' % (infix_syms, prefix_syms, operators), Operator),
            (r'\b(%s)\b' % '|'.join(word_operators), Operator.Word),
            (r'\b(%s)\b' % '|'.join(primitives), Keyword.Type),

            (r"[^\W\d][\w']*", Name),

            (r'-?\d[\d_]*(.[\d_]*)?([eE][+\-]?\d[\d_]*)', Number.Float),
            (r'0[xX][\da-fA-F][\da-fA-F_]*', Number.Hex),
            (r'0[oO][0-7][0-7_]*', Number.Oct),
            (r'0[bB][01][01_]*', Number.Bin),
            (r'\d[\d_]*', Number.Integer),

            (r"'(?:(\\[\\\"'ntbr ])|(\\[0-9]{3})|(\\x[0-9a-fA-F]{2}))'",
             String.Char),
            (r"'.'", String.Char),
            (r"'", Keyword),  # a stray quote is another syntax element

            (r'"', String.Double, 'string'),

            (r'[~?][a-z][\w\']*:', Name.Variable),
        ],
        'comment': [
            (r'[^(*)]+', Comment),
            (r'\(\*', Comment, '#push'),
            (r'\*\)', Comment, '#pop'),
            (r'[(*)]', Comment),
        ],
        'string': [
            (r'[^\\"]+', String.Double),
            include('escape-sequence'),
            (r'\\\n', String.Double),
            (r'"', String.Double, '#pop'),
        ],
        'dotted': [
            (r'\s+', Text),
            (r'\.', Punctuation),
            (r'[A-Z][\w\']*(?=\s*\.)', Name.Namespace),
            (r'[A-Z][\w\']*', Name.Class, '#pop'),
            (r'[a-z_][\w\']*', Name, '#pop'),
            default('#pop'),
        ],
    }
