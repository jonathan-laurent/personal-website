# Personal Website

## Setup

```sh
brew install sass/sass/sass  # on MacOS
poetry env use 3.10
poetry install
```

## To generate and visualize

```sh
poetry run python -m generator
python -m http.server --directory generated
```