# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:4.0.3

LABEL author="Fatih Uenal mars.fatih@gmail.com" 

# system libraries of general use
# install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libcurl4-openssl-dev \
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

# copy necessary files
## app folder
COPY /app ./app

## renv.lock file
COPY /renv.lock ./renv.lock

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# remove install files
RUN rm -rf /var/lib/apt/lists/*

# make all app files readable, gives rwe permisssion (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 /app

# expose port (for local deployment only)
EXPOSE 3838 

# set non-root
RUN useradd FUenal
USER FUenal

# run app on container start (use heroku port variable for deployment)
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT')))"]