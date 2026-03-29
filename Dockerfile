FROM rocker/shiny-verse:latest

RUN useradd -m -u 1000 user

ENV HOME=/home/user \
    PATH=/home/user/.local/bin:$PATH

RUN mkdir -p $HOME/app && chown user:user $HOME/app

WORKDIR $HOME/app

# Install system dependencies for sf/mapgl
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Install stable packages from CRAN
RUN install2.r --error \
    shiny \
    arrow \
    leaflet \
    plotly \
    bsicons \
    mapgl \
    shinyjs \
    sf \
    remotes

# Install development packages from GitHub
RUN installGithub.r \
    rstudio/bslib \
    rstudio/httpuv

COPY --chown=user . $HOME/app

USER user

EXPOSE 7860

CMD ["R", "--quiet", "-e", "shiny::runApp('/home/user/app', host='0.0.0.0', port=7860)"]
