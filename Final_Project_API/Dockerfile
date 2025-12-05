FROM rstudio/plumber
RUN apt-get update -qq && apt-get install -y \ 
libssl-dev \ 
libcurl4-gnutls-dev \ 
libpng-dev \ 
pandoc
RUN R -e "install.packages(c('tidyverse', 'tidymodels', 'plumber', 'rpart', 'ranger'))"
COPY projectAPI.R projectAPI.R
COPY diabetes_binary_health_indicators_BRFSS2015.csv diabetes_binary_health_indicators_BRFSS2015.csv
COPY final_wkf.RDS final_wkf.RDS
COPY final_model.RDS final_model.RDS
EXPOSE 8000
ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('projectAPI.R'); pr$run(host='0.0.0.0', port=8000)"]