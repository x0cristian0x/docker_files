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

# basic shiny functionality
RUN R -e "install.packages( c('shiny','remotes' ,'markdown', 'stringr', 'NLP', 'dplyr', 'ggplot2', 'shinydashboard', 'plotly'), repos='https://cloud.r-project.org/')"

RUN R -e "remotes::install_github('trinker/qdap')"

# copy the app to the image
RUN mkdir /root/predecir_texto

COPY . /root/predecir_texto

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/predecir_texto', host='0.0.0.0', port=3838)"]
