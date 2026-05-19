FROM rocker/r-ver:4.6.0

# 1. System dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libzstd-dev \
    libuv1-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    git \
    procps \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R dependencies from Posit Package Manager (binary packages for speed)
ENV ARROW_R_WITH_ZSTD=libzstd
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest')); \
    install.packages(c('devtools', 'remotes', 'arrow', 'duckdb', 'duckplyr', \
                       'dplyr', 'dbplyr', 'glue', 'ggplot2', 'readr', \
                       'tidyr', 'withr', 'testthat'))"

# 3. Install ModSeqR v1.2.0
RUN R -e "remotes::install_github('Wasatch-Biolabs-Bfx/ModSeqR@performance-improvements', upgrade='never'); \
    if (!requireNamespace('ModSeqR', quietly = TRUE)) stop('ModSeqR installation failed')"

LABEL org.opencontainers.image.title="ModSeqR" \
      org.opencontainers.image.version="1.2.0" \
      org.opencontainers.image.vendor="Wasatch Biolabs" \
      org.opencontainers.image.source="https://github.com/Wasatch-Biolabs-Bfx/ModSeqR"

WORKDIR /data