FROM quay.io/hsci/shiny-verse:4.2.3

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libnss-wrapper \
  gettext-base \
  libjq-dev \
  libudunits2-0 \
  libproj-dev \
  libgdal-dev \
  python3-pip

RUN pip3 install altair

RUN install2.r --error \
  DT \
  jqr \
  ggplot2 \
  RCurl \
  RMariaDB \
  sf \
  stringi \
  yaml \
  shinyAce \
  shinyjs \
  shinybrowser \
  altair \
  tmap \
  httr \
  tmaptools \
  xml2 \
  plotly \
  pacman \
  whisker \
  wordcloud2

COPY app.R /srv/shiny-server/
COPY config.yaml /srv/shiny-server/
COPY about.html /srv/shiny-server/
COPY R/ /srv/shiny-server/R
COPY www/ /srv/shiny-server/www
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# --------------------------------------------------------
#
# copy over the startup script
#
# --------------------------------------------------------
COPY passwd.template /passwd.template
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod a+x /usr/bin/shiny-server.sh && mkdir -p /var/log/shiny-server && chmod a+rwx /var/log/shiny-server && mkdir -p /var/lib/shiny-server/bookmarks/shiny && chmod a+rwx /srv/shiny-server && chmod a+rwx /usr/local/lib/R/site-library
CMD ["/usr/bin/shiny-server.sh"]

