make_wordcloud <- function(df, input) {
    wordcloud2(df %>%
      filter(y >= input$wordcloud__min_freq) %>%
      mutate(y = as.integer(exp(input$wordcloud__scale*log(y))))
    )
}

