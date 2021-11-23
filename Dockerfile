FROM rocker/shiny-verse:4.1.2

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libnss-wrapper \
  gettext-base \
  libudunits2-0 \
  libproj-dev \
  libgdal-dev \
  python3-pip

RUN pip3 install altair

RUN install2.r --error \
  DT \
  ggplot2 \
  RCurl \
  RMariaDB \
  sf \
  stringi \
  yaml \
  shinyAce \
  shinyjs \
  altair \
  tmap \
  httr \
  tmaptools \
  xml2 \
  plotly \
  pacman

COPY app.R /srv/shiny-server/
COPY config.yaml /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# --------------------------------------------------------
#
# copy over the startup script
#
# --------------------------------------------------------
COPY passwd.template /passwd.template
COPY shiny-server.sh /usr/bin/shiny-server.sh

ENV DB_HOST host
ENV DB_PORT 3306
ENV DB_USER user
ENV DB_PASS pass
ENV DB_NAME dbname

RUN chmod a+x /usr/bin/shiny-server.sh && mkdir -p /var/log/shiny-server && chmod a+rwx /var/log/shiny-server && mkdir -p /var/lib/shiny-server/bookmarks/shiny && chmod a+rwx /srv/shiny-server && chmod a+rwx /usr/local/lib/R/site-library
CMD ["/usr/bin/shiny-server.sh"]

