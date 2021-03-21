[![deploy-shiny](https://github.com/MahShaaban/LINPSAPP/actions/workflows/deploy-shiny.yml/badge.svg)](https://github.com/MahShaaban/LINPSAPP/actions/workflows/deploy-shiny.yml)
[![build-docker](https://github.com/MahShaaban/LINPSAPP/actions/workflows/build-docker.yml/badge.svg)](https://github.com/MahShaaban/LINPSAPP/actions/workflows/build-docker.yml)

# LINPSAPP
LINPSAPP: A web interface for the database of cancer cell-specific perturbations of biological networks

# Docker image

```bash
# pull from docker hub
docker pull mahshaaban/linpsapp

# build locally
git clone https://github.com/MahShaaban/LINPSAPP
cd LINPSAPP

docker build -t mahshaaban/linpsapp . 
```

```bash
docker run --rm -p 3838:3838 bcmslab/linpsapp
```
http://0.0.0.0:3838