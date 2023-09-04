# Personal Website

## Setup

```sh
brew install sass/sass/sass  # on MacOS
poetry env use 3.10
poetry install
```

## To generate and visualize

```sh
poetry run python -m generator build
python -m http.server --directory generated
```

## Generate a black and white version of a picture

```sh
poetry run python -m generator bw thumbnails/inhibition.png
```