FROM rocker/shiny-verse:latest

WORKDIR /code

# Install system dependencies for sf/mapgl
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Install stable packages from CRAN
RUN install2.r --error \
    ggExtra \
    shiny \
    highcharter \
    arrow \
    leaflet \
    leaflet.extras \
    markdown \
    plotly \
    orca \
    bsicons \
    mapgl \
    sf

# Install development packages from GitHub
RUN installGithub.r \
    rstudio/bslib \
    rstudio/httpuv

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
