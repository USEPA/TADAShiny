#FROM tgagor/centos
FROM centos:7
# minimal docker file to get sp and sf running on ubunty 16.04 image,
# using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable

MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de

RUN yum install -y epel-release
RUN yum install -y libzstd-devel libwebp-devel libtiff-devel
RUN yum install -y udunits2-devel openssl-devel
RUN yum install -y hdf5-devel proj-devel geos-devel sqlite-devel gdal-devel

#https://docs.rstudio.com/resources/install-r/#optional-install-recommended-packages :
# Enable the Extra Packages for Enterprise Linux (EPEL) repository
RUN yum install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm 

# On RHEL 7, enable the Optional repository
#RUN yum install -y subscription-manager
#RUN subscription-manager repos --enable "rhel-*-optional-rpms"

# If running RHEL 7 in a public cloud, such as Amazon EC2, enable the
# Optional repository from Red Hat Update Infrastructure (RHUI) instead
RUN yum install yum-utils
RUN yum-config-manager --enable "rhel-*-optional-rpms"

RUN export R_VERSION=4.1.3
#RUN curl -O https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm
RUN curl -O https://cdn.rstudio.com/r/centos-7/pkgs/R-4.1.3-1-1.x86_64.rpm
#RUN yum install -y R-${R_VERSION}-1-1.x86_64.rpm
RUN yum install -y R-4.1.3-1-1.x86_64.rpm

RUN /opt/R/4.1.3/bin/R --version
RUN ln -s /opt/R/4.1.3/bin/R /usr/local/bin/R
RUN ln -s /opt/R/4.1.3/bin/Rscript /usr/local/bin/Rscript

RUN gdalinfo --version
#RUN R -e 'install.packages("sf", repos = "https://cloud.r-project.org/"); library(sf)'
