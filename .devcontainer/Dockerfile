# Starting Docker image
ARG VARIANT="latest"
FROM rocker/geospatial:${VARIANT}
RUN /rocker_scripts/install_shiny_server.sh

# install R packages required
# (change it depending on the packages you need)
RUN install2.r --error --skipinstalled -r 'https://packagemanager.posit.co/cran/__linux__/jammy/latest' \
    devtools \
    config \
    golem \
    magrittr \
    htmltools \
    readxl \
    writexl \
    leaflet \
    shinyWidgets \
    shinycssloaders \
    DT \
    ggplot2 \
    shinybusy \
    dplyr \
    plyr \
    scales \
    forcats \
    testthat \
    remotes \
    covr \
    rmarkdown \
    knitr \
    spelling \
    sf \
    shinyjs \
    stringr \
    shinyjqui

RUN Rscript -e "devtools::install_github('USEPA/TADA')"

RUN mkdir /shiny-app

# allow permission
RUN chown shiny:shiny -R /shiny-app

USER shiny

EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/shiny-app', host = '0.0.0.0', port = 3838)"]