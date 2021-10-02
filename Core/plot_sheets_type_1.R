library(tidyverse)
library(plotly)

plot_sheet_type_1 <- function(dataset_name){
  g <- eval(as.symbol(dataset_name))
  g[, 2:length(colnames(g))] <- sapply(g[, 2:length(colnames(g))], as.numeric)
  ccc <- suppressWarnings(as.Date(as.POSIXct(as.numeric(unlist(g[1])), 
                                             origin="1970-01-01")))
  p <- plot_ly()
  for (i in 2:length(colnames(g))){
    p <- p %>% add_trace(x = ccc, 
                         y = unlist(g[colnames(g)[i]]), 
                         name = colnames(g)[i], 
                         type = 'scatter', 
                         mode = 'lines') 
  }
  return(p)
}