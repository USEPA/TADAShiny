# Starting Docker image
ARG VARIANT="latest"
FROM rocker/geospatial:${VARIANT}
RUN /rocker_scripts/install_shiny_server.sh

# install R packages required
# (change it depending on the packages you need)
RUN install2.r --error --skipinstalled -r 'https://cloud.r-project.org' \
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

R -e "devtools::install_github('USEPA/TADA')"

# Copy all files and folders from the project root except .github and .devcontainer into a new app folder
mkdir ./app
for f in $(ls -A | grep -v -E ".github|.devcontainer|CONTRIBUTING.md|README.md|.Rbuildignore"); do 
    cp -r $f ./app; 
done

# allow permission
RUN sudo chown -R shiny:shiny /app

USER shiny

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
