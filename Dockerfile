FROM rocker/shiny-verse:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    r-cran-v8 \
    libv8-dev \
    net-tools \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libudunits2-0 \
    libudunits2-dev \
    libgdal-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
    
# copy the app to the image
COPY *.Rproj /srv/shiny-server/
COPY *.R /srv/shiny-server/
COPY *.Rmd /srv/shiny-server/
COPY *.json /srv/shiny-server/
COPY *.md /srv/shiny-server/
COPY *.css /srv/shiny-server/
COPY input_data /srv/shiny-server/input_data
COPY www /srv/shiny-server/www

# install R packages required 
RUN R -e "install.packages(c('magrittr', 'rvest', 'stringr', 'stringi', 'readxl', 'dplyr', 'maps', 'ggplot2', 'reshape2', 'ggiraph', 'RColorBrewer','leaflet', 'geojsonio', 'shiny', 'shinydashboard', 'sjmisc', 'lubridate', 'kableExtra', 'gridExtra', 'shinyjs', 'shinyWidgets', 'shinythemes','tidyverse','robotstxt','googlesheets4','gargle', 'rJava'), repos='http://cran.rstudio.com/')"

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

# run app
CMD ["/usr/bin/shiny-server.sh"]
