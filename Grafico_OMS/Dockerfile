FROM openanalytics/r-base

MAINTAINER Cristian Heredia "cristianvoice@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'lubridate', 'ggplot2', 'tidyr', 'dplyr',
'shinymanager', 'scrypt', 'DT', 'RMySQL', 'htmltools', 'shinythemes'), 
repos='https://cloud.r-project.org/')"

# install dependencies of the euler app
#RUN R -e "install.packages('Rmpfr', repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/
COPY . /root/

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/')"]