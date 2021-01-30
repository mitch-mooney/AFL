reactable_function <- function(data){
  
  col_order <- c("Rank", "change", "Player",
                 "Rating", "sparkline")
  spark_table <- spark_table[, col_order]
  
  # Icon to indicate trend: unchanged, up, down, or new
  trend_indicator <- function(change = c("Unchanged", "Up", "Down")) {
    value <- match.arg(change)
    label <- switch(change,
                    Unchanged = "Unchanged", Up = "Trending up",
                    Down = "Trending down")
    
    # Add img role and tooltip/label for accessibility
    args <- list(role = "img", title = change)
    
    if (value == "Unchanged") {
      args <- c(args, list("–", style = "color: #666; font-weight: 700"))
    } else if (value == "Up") {
      args <- c(args, list(shiny::icon("caret-up"), style = "color: #1ed760"))
    } else if (value == "Down") {
      args <- c(args, list(shiny::icon("caret-down"), style = "color: #cd1a2b"))
    } else {
      args <- c(args, list(shiny::icon("circle"), style = "color: #2e77d0; font-size: 10px"))
    }
    do.call(span, args)
  }
  
  reactable(spark_table, pagination = FALSE, defaultPageSize = 20, columns = list(
    Rank = colDef(maxWidth = 75, align = "center"),
    change = colDef(
      header = span("", class = "sr-only"),
      sortable = FALSE,
      align = "left",
      width = 40,
      cell = function(change) trend_indicator(change)
    ),
    Player = colDef(name = "Team", maxWidth = 150, align = "center", cell = function(value) {
      img_src <- knitr::image_uri(sprintf("images/%s.png", value))
      image <- img(src = img_src, height = "45px", alt = value)
      tagList(
        div(style = list(display = "inline-block", width = "200px"), image)
      )
    }),
    Rating = colDef(maxWidth = 100, align = "center", format = colFormat(digits = 0)),
    
    sparkline = colDef(name = "2021 Progress", cell = function(value, index) {
      sparkline(spark_table$sparkline[[index]], height = "50px", width = "150px")
    })
    
  ))
}