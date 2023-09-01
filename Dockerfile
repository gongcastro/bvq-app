FROM rocker/r-base:latest

LABEL maintainer="gongcastro <gonzalo.garciadecastro@upf.edu>"

# install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

# install shiny
RUN install.r shiny

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

# create Linux group with permissions
RUN addgroup --system app \
    && adduser --system --ingroup app app
RUN chown app:app -R /home/app
USER app

WORKDIR /home/app
COPY app .
COPY ./renv.lock .

# install and restore renv
RUN R -e 'install.packages("renv", repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")'
RUN Rscript -e "options(renv.consent = TRUE); \
	renv::restore(lockfile = '/srv/shiny-server/renv.lock', repos = \
    c(CRAN='https://packagemanager.rstudio.com/all/__linux__/focal/latest'))"

# expose app
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app')"]
