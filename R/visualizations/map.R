read_maps <- function() {
  # FIXME `con` should be a parameter of query_db and this function!!!
  # (right now it unnecessarily connects and disconnects many times)
  maps <- query_db("SELECT map_id, name FROM maps;")
  con <- connect_to_db()
  result <- lapply(
    maps$map_id,
    function(i) {
        r <- st_read(con, query=paste0(
            "SELECT ",
            "  pol.pol_id, name AS pol_name, lang, ",
            "  ST_AsBinary(geometry) AS geometry ",
            "FROM polygons pol ",
            "  JOIN map_pol ON pol.pol_id = map_pol.pol_id ",
            "WHERE map_pol.map_id = ", i, " AND geometry IS NOT NULL;"))
        st_crs(r) <- 'urn:ogc:def:crs:EPSG::3067'
        r <- st_make_valid(r)
        r[!st_is_empty(r),]
    }
  )
  dbDisconnect(con)
  names(result) <- maps$name
  result
}

read_place.poly <- function() {
  query_db('SELECT pol_id, place_orig_id AS place_id FROM pol_pl NATURAL JOIN places;')
}

make_map <- function(df, input, maps) {
    breaks <- sapply(unlist(stri_split_fixed(input$map__breaks, ',')), as.numeric)
    if (!('pol_id' %in% names(df))) {
      # if no polygon ID -- add them based on place IDs and regroup
      if ('place_id' %in% names(df)) {
        df <- df %>%
          inner_join(place.poly, by='place_id') %>%
          group_by(pol_id) %>%
          summarize(y = sum(y))
      } else {
        # if no place IDs -- match based on names as last resort
        # FIXME place names instead of polygon names!
        df <- df %>%
          inner_join(maps[[input$map__map]] %>% select(pol_id, pol_name),
                     by=c(place_name = 'pol_name')) %>%
          group_by(pol_id) %>%
          summarize(y = sum(y))
      }
    }
    tm_shape(
      maps[[input$map__map]] %>%
        left_join(df, by='pol_id')
    ) + tm_polygons(col="y", id="pol_name",
                    palette=input$map__palette, style=input$map__style,
                    n = input$map__classes, breaks=breaks,
                    colorNA=if (input$map__palette == 'Greys') { 'white' } else { NA },
                    contrast=if (input$map__palette == 'Greys') { c(0.3, 1) } else { NA },
                    title = input$map__var, textNA="\u2014"
    ) + tm_layout(title = input$map__title,
                  legend.format = list(text.separator='\u2013'))
}

