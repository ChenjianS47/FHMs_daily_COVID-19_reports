time_step_type_1 <- function(sheet_name,time_col_pos,data_col_start,data_col_end,file_stats){
  i <- sheet_name
  
  if (file_stats== 'first') {
    assign(paste('time_step',i,sep='_'),0,envir=.GlobalEnv)
  }else{
    i_name <- unlist(colnames(eval(as.symbol(i))))
    
    # Get the colname we select in the config.yml
    time_name <- c(i_name[unlist(time_col_pos[i])])
    data_name <- c(i_name[unlist(data_col_start[i]):unlist(data_col_end[i])])
    
    old_i <- eval(as.symbol(paste('old',i,sep='_')))
    new_i <- eval(as.symbol(i))
    
    len_old_data <- dim(old_i)[1]
    len_new_data <- dim(new_i)[1]
    

    old_time <- suppressWarnings(as.Date(as.POSIXct(as.numeric(unlist(old_i[time_name])[len_old_data]), 
                                                    origin="1970-01-01")))
    new_time <- suppressWarnings(as.Date(as.POSIXct(as.numeric(unlist(new_i[time_name])[len_new_data]), 
                                                    origin="1970-01-01")))
    if (is.na(old_time)==TRUE) {
      len_old_data = len_old_data - 1
      old_time <- suppressWarnings(as.Date(as.POSIXct(as.numeric(unlist(old_i[time_name])[len_old_data]), 
                                                      origin="1970-01-01")))
    }
    
    if (is.na(new_time)==TRUE) {
      len_new_data = len_new_data - 1
      new_time <- suppressWarnings(as.Date(as.POSIXct(as.numeric(unlist(new_i[time_name])[len_new_data]), 
                                                      origin="1970-01-01")))
    }
    
    
    
    time_differ = as.numeric(new_time-old_time)
    
    time_old_data <- unlist(old_i[1:(len_old_data),2])
    time_new_data <- unlist(new_i[1:(len_new_data-time_differ),2])
    
    test <- unlist(list(time_old_data==time_new_data))
    
    idx <-  unlist(list(which(test == FALSE)))
    
    if (length(idx)!=0) {
      assign(paste('time_step',i,sep='_'),idx[1],envir=.GlobalEnv)
    }
  
  
  
  
  
  
  }
  
  
  
  
  
  
  
  
}