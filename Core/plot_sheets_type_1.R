library(tidyverse)
library(plotly)



plot_sheet_type_1 <- function(dataset_name){
  g <- eval(as.symbol(dataset_name))
  time_line <- suppressWarnings(as.Date(as.POSIXct(as.numeric(unlist(g[1])), 
                                             origin="1970-01-01")))
  g[, -1] <- sapply(g[, -1], as.numeric)
  
  colset <- color_23
  
  p <- plot_ly()
  for (i in 2:(length(colnames(g)))){
    p <- p %>% add_trace(x = time_line, 
                         y = unlist(g[colnames(g)[i]]), 
                         name = colnames(g)[i], 
                         type = 'scatter', 
                         mode = 'lines',
                         colors= colset) 
  }
  return(p)
}