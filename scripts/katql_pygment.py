#! python3

import re

from pygments.lexer import RegexLexer
from pygments.token import *

from pygments import highlight
from pygments.formatters import HtmlFormatter

__all__ = ['KappaTraceQueryLexer']

def one_of(words):
    return '|'.join(re.escape(entry) for entry in words)

class KappaTraceQueryLexer(RegexLexer):

    name = 'Kappa Trace Patterns Language'
    aliases = ['kappa-trace-patterns']
    filenames = ['*.katp']

    keywords = ["match", "last", "first", "after", "before", "when", \
                "with", "and", "do", "return", "query", "every", "seconds"]

    funs = ["time", "state", "nphos", "this", "component", "count", \
            "rule", "int_state"]

    tokens = {
        'root': [
            (r'#.*$', Comment.Single),
            (r'({})'.format(one_of(keywords)), Keyword),
            (r'({})'.format(one_of(funs)), Name.Constant),
            (r"'[^']+'", String),
            (r'(,|;|\.|\|)', Punctuation),
            (r'.', Text)
        ],
    }

import sys

code = sys.stdin.read()
print(highlight(code, KappaTraceQueryLexer(), HtmlFormatter()))
