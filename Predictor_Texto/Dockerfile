FROM rocker/r-ver

LABEL maintainer = "CH cristianvoice@gmail.com"

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    && apt-get clean    

# System dependencies
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    ## for rJava
    default-jdk \
    ## used to build rJava and other packages
    libbz2-dev \
    libicu-dev \
    liblzma-dev \
    ## system dependency of hunspell (devtools)
    libhunspell-dev \
    ## system dependency of hadley/pkgdown
    libmagick++-dev \
    ## poppler to install pdftools to work with .pdf files
    # for topicmodels on which depends textmineR
    gsl-bin \
    libgsl0-dev \
    ## rdf, for redland / linked data
    librdf0-dev \
    ## for V8-based javascript wrappers
    libv8-dev \
    ## system dependency for igraph
    glpk-utils \
    && apt-get clean
   
# System utilities
RUN install2.r --error \
  shiny shinydashboard ggplot2 rJava markdown dplyr stringr plotly NLP \
  && rm -rf /tmp/downloaded_packages/

# NLP
RUN install2.r --error \ 
    # Text Mining
    tm \
    # stopword lists
    stopwords \
    # Word Clouds. Functionality to create pretty word clouds, visualize differences and similarity between documents
    wordcloud \
    # A Word Cloud Geom for 'ggplot2'
    ggwordcloud \
    # helps split text into tokens, supporting shingled n-grams, skip n-grams, words, word stems, sentences, paragraphs,
    # characters, lines, and regular expressions. 
    tokenizers \
    # transcript analysis, Text Mining / Natural Language Processing: frequency counts of sentence types, words,
    # sentences, turns of talk, syllables and other assorted analysis tasks
    qdap \
    #approximate string matching version of R's native 'match' function
    stringdist \
    && rm -rf /tmp/downloaded_packages/

# Create makedir
RUN mkdir /build_zone
COPY . /build_zone
WORKDIR /build_zone

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/build_zone/', host='0.0.0.0', port=3838)"]