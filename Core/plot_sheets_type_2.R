library(tidyverse)
library(plotly)


plot_sheet_type_2 <- function(dataset_name){
  g <- eval(as.symbol(dataset_name))
  g[, 3:(length(colnames(g)))] <- sapply(g[, 3:(length(colnames(g)))], 
                                           as.numeric)
  ccc <- as.Date(unlist(g[1]))
  p <- plot_ly()
  for (i in 3:(length(colnames(g)))){
    p <- p %>% add_trace(x = ccc, 
                         y = unlist(g[colnames(g)[i]]), 
                         name = colnames(g)[i], 
                         type = 'scatter', 
                         mode = 'lines',
                         transforms = list(
                           list(type = 'groupby',
                                groups = unlist(g[2])
                                )
                           )
                         )
  }
  return(p)
}