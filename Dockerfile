FROM rocker/shiny-verse:4.3.0

# Workaround for renv cache
RUN mkdir /.cache
RUN chmod 777 /.cache

WORKDIR /code

# Install renv
RUN install2.r --error \
    renv

# Copy application code
COPY . .

# Install dependencies
RUN Rscript -e 'options(renv.config.cache.enabled = FALSE); renv::restore(prompt = FALSE)'

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]