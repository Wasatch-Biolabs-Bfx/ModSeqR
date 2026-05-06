FROM rocker/r-ver:4.3.2

# 1. System dependencies (Added libuv1-dev for the 'fs' package)
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

# 2. Use Posit Binaries
ENV ARROW_R_WITH_ZSTD=libzstd
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')); \
    install.packages(c('devtools', 'remotes', 'arrow', 'duckdb', 'duckplyr', 'dplyr', 'ggplot2'))"

# 3. Install ModSeqR
RUN R -e "devtools::install_github('Wasatch-Biolabs-Bfx/ModSeqR', upgrade='never'); \
    if (!require('ModSeqR')) { stop('Installation failed!') }"

WORKDIR /