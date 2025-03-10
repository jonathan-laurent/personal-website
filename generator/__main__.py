import os
import re
import shutil
import subprocess
from os.path import join

import fire
import jinja2
import markdown
import yaml

import generator.markdown_utils as md


SOURCE_DIR = "website"
GENERATED_DIR = "generated"
TEMPLATES_DIR = "templates"
CONTENT_DIR = "content"
IMAGE_FOLDER = "img"


def replace_extension(path: str, new_extension: str) -> str:
    """
    Replace the extension of a file with a new one.
    """
    return ".".join(path.split(".")[:-1]) + "." + new_extension


def add_suffix(path: str, suffix: str) -> str:
    """
    Add a suffix to the name of a file.
    """
    parts = path.split(".")
    parts[-2] += suffix
    return ".".join(parts)


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


def render_page(path: str, content: dict[str, object]) -> None:
    txt = render_template(
        path,
        markdown=md.markdown_extended,
        inline_markdown=md.no_p_markdown,
        **content,
    )
    txt = re.sub(r'href="/', r'href="./', txt)
    txt = re.sub(r'src="/', r'src="./', txt)
    with open(join(GENERATED_DIR, path), "w") as f:
        f.write(txt)


def compile_content(dir: str) -> dict[str, object]:
    content: dict[str, object] = {}
    for file in os.listdir(dir):
        if os.path.isdir(join(dir, file)):
            content |= compile_content(join(dir, file))
        else:
            name, ext = file.split(".")
            if ext == "md":
                with open(join(dir, file)) as f:
                    html = markdown.markdown(f.read())
                    content[name] = html
            elif ext == "yaml":
                content[name] = yaml.safe_load(open(join(dir, file)))
    return content


def serve() -> None:
    subprocess.run(
        ["python", "-m", "http.server", "8000", "--directory", GENERATED_DIR]
    )


def convert_image(name: str) -> None:
    original = join(SOURCE_DIR, IMAGE_FOLDER, name)
    bw_version = join(SOURCE_DIR, IMAGE_FOLDER, add_suffix(name, "-bw"))
    """
    Load the image with PIL and convert it in grayscale.
    """
    from PIL import Image

    with Image.open(original) as img:
        grayscale_img = img.convert("LA").convert("RGBA")
        grayscale_img.save(bw_version)


class Generator:
    def build(self) -> None:
        compile_scss("css/style.scss", load_path="css/style")
        copy_file("css/fonts.css")
        copy_file(".htaccess")
        for dir in ["downloads", "fonts", "img", "pdf"]:
            copy_directory(dir)
        content = compile_content(CONTENT_DIR)
        render_page("index.html", content)
        render_page("publications.html", content)
        render_page("cv.html", content)
        render_page("404.html", content)

    def serve(self) -> None:
        self.build()
        serve()

    def bw(self, *imgs: str) -> None:
        for img in imgs:
            convert_image(img)


if __name__ == "__main__":
    fire.Fire(Generator)
