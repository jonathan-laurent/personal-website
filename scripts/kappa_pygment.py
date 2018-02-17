#! python3

import re

from pygments.lexer import RegexLexer
from pygments.token import *

from pygments import highlight
from pygments.formatters import HtmlFormatter

__all__ = ['KappaLexer']

def one_of(words):
    return '|'.join(re.escape(entry) for entry in words)

class KappaLexer(RegexLexer):

    name = 'Kappa'
    aliases = ['kappa']
    filenames = ['*.ka']

    directives = ["agent", "var", "init", "obs"]

    identifier = r"[a-zA-Z_]\w*"

    number = r"\d+(\.\d*)?(E\-?\d+)?"

    blanks = (r'\s+', Text)

    tokens = {
        'root': [
            (r'#.*$', Comment.Single),
            ("^%({}):".format(one_of(directives)), Keyword.Declaration),
            (r'@', Operator),
            (r"'[^']+'", Name.Variable),
            (identifier, Text),
            (r'(\(|\)|,|\[|\]|\{|\}|\.)', Punctuation),
            (r'(!|->|<->|~)', Operator),
            (r'(\||\*|\+|\-|/)', Operator),
            (number, Text), # Literal.Number
            blanks
        ],
    }

import sys

code = sys.stdin.read()
print(highlight(code, KappaLexer(), HtmlFormatter()))