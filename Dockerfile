# Source: https://juanitorduz.github.io/dockerize-a-shinyapp/
# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 
  

# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages(c('shiny', 'shinyBS', 'DT'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('RSQLite', 'DBI'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('plotly', 'igraph', 'visNetwork', 'Hmisc'), repos='http://cran.rstudio.com/')"


# copy the app to the image
COPY LINPSAPP.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY www /srv/shiny-server/www

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
# CMD ["/usr/bin/shiny-server.sh"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host = '0.0.0.0', port = 3838)"]
