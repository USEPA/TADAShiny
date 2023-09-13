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

# allow permission
RUN sudo chown -R shiny:shiny /workspaces/TADAShiny

USER shiny

# run app on container start
CMD ["R", "-e", "shiny::runApp('/workspaces/TADAShiny', host = '0.0.0.0', port = 3838)"]
