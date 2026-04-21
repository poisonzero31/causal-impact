FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('xts','ggplot2','tidyr','dplyr','forecast','corrplot','scales','withr'), repos='https://cran.rstudio.com/')"

# Install CausalImpact from GitHub
RUN R -e "install.packages('remotes', repos='https://cran.rstudio.com/'); remotes::install_github('google/CausalImpact')"

# Copy app
COPY app.R /srv/shiny-server/causal-impact/app.R

# Copy Shiny Server config
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
