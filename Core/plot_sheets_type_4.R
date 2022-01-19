library(tidyverse)
library(plotly)


plot_sheet_type_4 <- function(dataset_name){
  g <- eval(as.symbol(dataset_name))
  
  time_line <- as.Date(unlist(g[1]))
  g[,3:length(colnames(g))] <- suppressWarnings(sapply(g[,3:length(colnames(g))], 
                                           as.numeric))
  
  p <- plot_ly()
  for (i in 3:length(colnames(g))){
    p <- p %>% add_trace(x = time_line, 
                         y = unlist(g[colnames(g)[i]]), 
                         legendgroup = colnames(g)[i],
                         name = paste(colnames(g)[i],
                                      unlist(g[colnames(g)[2]]),
                                      sep='-'),
                         type = 'scatter', 
                         mode = 'lines',
                         color= (unlist(g[colnames(g)[2]]))
                         )
  }
  return(p)
}