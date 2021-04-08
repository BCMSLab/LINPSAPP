[![deploy-shiny](https://github.com/MahShaaban/LINPSAPP/actions/workflows/deploy-shiny.yml/badge.svg)](https://github.com/MahShaaban/LINPSAPP/actions/workflows/deploy-shiny.yml)
[![build-docker](https://github.com/MahShaaban/LINPSAPP/actions/workflows/build-docker.yml/badge.svg)](https://github.com/MahShaaban/LINPSAPP/actions/workflows/build-docker.yml)

# LINPSAPP
LINPSAPP: A web interface for the database of cancer cell-specific perturbations of biological networks

## How to access this app and the database?

The database and the interface are accessible in several ways

1. Through [shinyapps.io](https://www.shinyapps.io/), just click
[here](https://mahshaaban.shinyapps.io/LINPSAPP/).

2. Clone this repo and run the `app.R` file in RStudio

```bash
# build locally
git clone https://github.com/MahShaaban/LINPSAPP
cd LINPSAPP
```

3. Through Docker, pull the image

```bash
# pull from docker hub
docker pull mahshaaban/linpsapp
```

or build locally

```r
# build locally
git clone https://github.com/MahShaaban/LINPSAPP
cd LINPSAPP

docker build -t mahshaaban/linpsapp . 
```

then start a container with the app running on 
[http://0.0.0.0:3838](http://0.0.0.0:3838)

```bash
docker run --rm -p 3838:3838 bcmslab/linpsapp
```

## Related repositories

- To build the database [MahShaaban/LINPS](https://github.com/MahShaaban/LINPS) 