FROM r-base:latest

MAINTAINER Winston Chang "winston@rstudio.com"

# Install dependencies and Download and install shiny server
RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    gdebi-core \
    pandoc \
    libxml2-dev \
    libssl-dev \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev && \
    wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'rmarkdown', 'memoise', 'wordcloud', 'dplyr', 'rmarkdown', 'lubridate', 'igraph', 'circlize', 'xts', 'stringr', 'httr', 'rvest', 'xml2', 'tidyverse' ), dependencies=TRUE, repos='https://cran.rstudio.com/')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /srv/shiny-server/*

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
ADD ./WFMU_Explorer/ /srv/shiny-server/

CMD ["/usr/bin/shiny-server.sh"]
