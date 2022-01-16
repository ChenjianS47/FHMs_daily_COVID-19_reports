data_differ_type_1 <- function(sheet_name,data_col_start,data_col_end,file_stats,file_add){
  i <- sheet_name
  
  if (file_stats == "first") {
    data_i <- eval(as.symbol(i))
    i_name <- unlist(colnames(eval(as.symbol(i))))
    time_name <- c(i_name[1])
    data_name <- c(i_name[unlist(data_col_start[i]):unlist(data_col_end[i])])
    i_name_data <- paste(colnames(data_i)[-1],'differ',sep='_')
    i_name_data_first <- paste(colnames(data_i)[-1],'first',sep='_')
    new_i_name <- c(time_name,data_name,i_name_data_first,i_name_data)
    new_i <- as.data.frame(matrix(nrow=0,ncol=length(new_i_name)))
    colnames(new_i) <- new_i_name
    assign(paste('series',i,sep='_'),new_i,envir = .GlobalEnv)
  }else{
    data_series <- eval(as.symbol(paste(i,'series',sep='_')))
    
    i_name <- unlist(colnames(eval(as.symbol(i))))
    
    # Get the colname we select in the config.yml
    time_name <- c(i_name[1])
    data_name <- c(i_name[unlist(data_col_start[i]):unlist(data_col_end[i])])
    
    old_i <- eval(as.symbol(unlist(data_series[1])))
    new_i <- eval(as.symbol(i))
    
    len_old_data <- dim(old_i)[1]
    len_new_data <- dim(new_i)[1]
    
    old_time <- suppressWarnings(as.Date(as.POSIXct(as.numeric(unlist(old_i[time_name])[len_old_data]), 
                                                    origin="1970-01-01")))
    set_fix <- 0
    
    if (is.na(old_time)==TRUE) {
      len_old_data = len_old_data - 1
      old_time <- suppressWarnings(as.Date(as.POSIXct(as.numeric(unlist(old_i[time_name])[len_old_data]), 
                                                      origin="1970-01-01")))
      set_fix <- 1
    }
    
    time_old <- unlist(old_i[time_name])[len_old_data]
    
    data_old <- old_i[which(old_i==time_old),]
    
    if (length(which(new_i==time_old)) != 0) {
      data_new <- new_i[which(new_i==time_old),]
    }else{
      data_new <- list(time_old,rep(NA,(length(data_old)-1)))
    }
    
    
    data_fix <- unlist(data_new[-1])-unlist(data_old[-1])
    
    new_data <- eval(as.symbol(paste('series',i,sep='_')))
    
    data_seq_new <- c(unlist(data_new),unlist(data_old[-1]),data_fix)
    
    
    
    data_ma <- matrix(data=data_seq_new,ncol=(length(new_data)))
    colnames(data_ma) <- colnames(new_data)
    

    new_data <- rbind(new_data,data_ma)

    assign(paste('series',i,sep='_'),new_data,envir = .GlobalEnv)
    
    
    date_str <- unlist(strsplit(unlist(strsplit(file_add,split = '_'))[3],split='.xlsx'))

    if (strptime(date_str,"%b %d %Y") ==as.Date(unlist(config['end_time'])) ) {
      data_no_diff <- new_i[which(old_i==time_old):(dim(new_i))[1],]
      for (ss in 1:dim(data_no_diff)[1]) {
        data_na <- c(unlist(data_no_diff[1,]),rep(NA,(length(new_data)-length(data_no_diff))))
        data_na <- as.data.frame(matrix(data_na,ncol=length(data_na)))
        colnames(data_na) <- colnames(new_data)
        new_data <- rbind(new_data,data_na)
      }
      assign(paste('series',i,sep='_'),new_data,envir = .GlobalEnv)
    }
    chr <- c(unlist(data_series[1]))
    rm(list=chr,envir = .GlobalEnv)
    data_series <- data_series[-1]
    assign(paste(i,'series',sep='_'),data_series,envir = .GlobalEnv)
    
  }
}