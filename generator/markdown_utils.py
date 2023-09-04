import re
import textwrap

import markdown


def render_fenced_blocks(txt: str) -> str:
    """
    Matches ::: { #id .class1 .class2 } ::: BODY :::
    """

    def replace(match):
        attrs = match.group(1).split()
        id: str | None = None
        classes: list[str] = []
        for attr in attrs:
            if attr.startswith("#"):
                id = attr[1:]
            elif attr.startswith("."):
                classes.append(attr[1:])
            else:
                assert False
        body = match.group(2)
        # Render the HTML
        attrs_elts = []
        if id is not None:
            attrs_elts.append(f'id="{id}"')
        if classes:
            attrs_elts.append(f'class="{" ".join(classes)}"')
        if "hidden" in classes:
            # Special treatment for hidden blocks
            attrs_elts.append('style="display: none;"')
        attrs_str = " ".join(attrs_elts)
        return f"<div {attrs_str}>{body}</div>"

    pat = r"::: { (.*?) } :::(.*?):::"
    return re.sub(pat, replace, txt, flags=re.DOTALL | re.MULTILINE)


def render_button(txt: str) -> str:
    def replace(match):
        text = match.group(1)
        to_toggle = match.group(2)
        return (
            f'<span class="toggle button" '
            + f"onclick=\"toggle('{to_toggle}')\">{text}</span>"
        )

    pat = r"\[(.*?)\]{.button toggle=\"(.*?)\"}"
    return re.sub(pat, replace, txt, flags=re.DOTALL | re.MULTILINE)


def markdown_extended(txt: str) -> str:
    txt = markdown.markdown(txt)
    txt = render_fenced_blocks(txt)
    txt = render_button(txt)
    return txt


def test_markdown_extended():
    ex = textwrap.dedent(
        """
        Hello [world]{.button toggle="id"}.

        ::: { #id .class1 .class2 .hidden } :::
        This is inside.
        :::

        After.
        """
    )
    # ex = "ABC"
    print(markdown_extended(ex))


def no_p_markdown(non_p_string) -> str:
    """Strip enclosing paragraph marks, <p> ... </p>,
    which markdown() forces, and which interfere with some jinja2 layout
    """
    # https://stackoverflow.com/questions/15555870/suppress-python-markdown-wrapping-text-in-p-p
    return re.sub(
        "(^<P>|</P>$)",
        "",
        markdown.markdown(non_p_string),
        flags=re.IGNORECASE,
    )
