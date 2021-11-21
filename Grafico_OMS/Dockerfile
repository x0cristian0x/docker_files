FROM rocker/r-ver
  
RUN apt-get update \
  && apt-get install -y --no-install-recommends libmariadbclient-dev libssl-dev

MAINTAINER Cristian Heredia "cristianvoice@gmail.com"

# Options download
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site

# Install packages
RUN R -e "install.packages(c('shiny', 'lubridate', 'ggplot2', 'tidyr', 'dplyr', 'shinymanager', 'scrypt', 'DT','DBI','RSQLite' ,'RMySQL', 'htmltools', 'shinythemes', 'remotes'), repos='https://cloud.r-project.org/')"

# Create makedir
RUN mkdir /grafico/
COPY . /grafico/

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/grafico/', host='0.0.0.0', port= 3838)"]
