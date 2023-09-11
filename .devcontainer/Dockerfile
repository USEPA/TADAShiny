ARG VARIANT="4.3.1"
FROM rocker/geospatial:${VARIANT}

# RUN /rocker_scripts/install_rstudio.sh

# install R packages required
# (change it depending on the packages you need)
RUN install2.r --error --skipinstalled -r 'https://cloud.r-project.org' \
    pak \
    rjson \
    stringr