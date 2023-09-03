import os
import re
import shutil
import subprocess
from os.path import join

import jinja2
import markdown
import yaml


SOURCE_DIR = "website"
GENERATED_DIR = "generated"
TEMPLATES_DIR = "templates"
CONTENT_DIR = "content"


def replace_extension(path: str, new_extension: str) -> str:
    """
    Replace the extension of a file with a new one.
    """
    return ".".join(path.split(".")[:-1]) + "." + new_extension


def compile_scss(source_path: str, *, load_path: str) -> None:
    """
    Use SASS to compile a *.scss file into a *.css file.
    """
    res = subprocess.run(
        [
            "sass",
            join(SOURCE_DIR, source_path),
            join(GENERATED_DIR, replace_extension(source_path, "css")),
            "--load-path",
            join(SOURCE_DIR, load_path),
        ]
    )
    assert res.returncode == 0


def copy_directory(dir: str) -> None:
    shutil.copytree(
        join(SOURCE_DIR, dir), join(GENERATED_DIR, dir), dirs_exist_ok=True
    )


def copy_file(path: str) -> None:
    shutil.copy(join(SOURCE_DIR, path), join(GENERATED_DIR, path))


TEMPLATES = jinja2.Environment(
    loader=jinja2.FileSystemLoader([SOURCE_DIR, TEMPLATES_DIR]),
    trim_blocks=True,
    lstrip_blocks=True,
)


def render_template(template_name: str, **kwargs) -> str:
    return TEMPLATES.get_template(template_name).render(**kwargs)


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


def render_page(path: str, content: dict[str, object]) -> None:
    txt = render_template(
        path,
        markdown=markdown.markdown,
        inline_markdown=no_p_markdown,
        **content
    )
    with open(join(GENERATED_DIR, path), "w") as f:
        f.write(txt)


def compile_content() -> dict[str, object]:
    content: dict[str, object] = {}
    for file in os.listdir(CONTENT_DIR):
        name, ext = file.split(".")
        if ext == "md":
            with open(join(CONTENT_DIR, file)) as f:
                html = markdown.markdown(f.read())
                content[name] = html
        elif ext == "yaml":
            content[name] = yaml.safe_load(open(join(CONTENT_DIR, file)))
    return content


def serve() -> None:
    subprocess.run(
        ["python", "-m", "http.server", "8000", "--directory", GENERATED_DIR]
    )


if __name__ == "__main__":
    compile_scss("css/style.scss", load_path="css/style")
    copy_file("css/fonts.css")
    for dir in ["downloads", "fonts", "img", "pdf"]:
        copy_directory(dir)
    content = compile_content()
    render_page("index.html", content)
    serve()
