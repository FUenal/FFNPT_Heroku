# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c('magrittr', 'rvest', 'stringr', 'stringi', 'readxl', 'dplyr', 'maps', 'ggplot2', 'reshape2', 'ggiraph', 'RColorBrewer',
                'leaflet', 'geojsonio', 'shiny', 'shinydashboard', 'sjmisc', 'lubridate', 'kableExtra', 'gridExtra', 'shinyjs',
                'shinyWidgets', 'shinythemes','tidyverse','robotstxt','googlesheets4','gargle')

install_if_missing = function(p) {
        if (p %in% rownames(installed.packages()) == FALSE) {
                install.packages(p)
        }
}

invisible(sapply(my_packages, install_if_missing))