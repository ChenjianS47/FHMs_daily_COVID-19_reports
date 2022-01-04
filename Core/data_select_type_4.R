data_select_type_4 <- function(sheet_name,file_add,file_stats,sheet_name_t,time_col_pos,config){
  if (file_stats=='first') {
    data_name <- 'final'
  }else if(file_stats=='others'){
    data_name <- 'temp'
  }
  # Get the name of the combined sheet and assign it as name_temp
  name_temp<-gsub(' ','_',names(config[sheet_name]))
  # Get the sheets that need to combined
  combined_sheets <- unlist(config[sheet_name])
  # According to the define in the config, get the main sheet and the 
  # vice sheet
  main_sheet_name <- sheet_name_t[combined_sheets[3]]
  vice_sheet_name <- sheet_name_t[combined_sheets[4]]
  # Get the row length of the main sheet
  main_row_len <- dim(eval(as.symbol(main_sheet_name)))[1]
  # Get the row length of the vice sheet
  vice_row_len <- dim(eval(as.symbol(vice_sheet_name)))[1]
  # Get the row name of the vice sheet,according to the data sets' format,
  # the row name of the vice sheet is usually the first column
  vice_row_name <- unlist(eval(as.symbol(vice_sheet_name))[1])
  # Get the column name of the main sheet
  main_col_name <- unlist(colnames(eval(as.symbol(main_sheet_name))))
  # Get the column name of the vice sheet
  vice_col_name <- unlist(colnames(eval(as.symbol(vice_sheet_name))))
  # Add the name of the columns to the col_name for the later rename of 
  # the relative data frame
  col_name <<- c('Date',main_col_name[1])
  
  # Get the combined columns name
  for (k in 2:length(main_col_name)){
    for (c in 1:length(vice_col_name)){
      if (main_col_name[k]==vice_col_name[c]){
        for (ck in vice_row_name){
          col_name <<- append(col_name, 
                              paste(main_col_name[k],ck,sep='_'))
        }
      }
    }
  }
  
  # Get the same name column
  common_col <- intersect(main_col_name, vice_col_name)
  common_lenth <- length(common_col)
  
  # Filter the main sheet data and vice sheet data
  main_temp <- eval(as.symbol(main_sheet_name))[,common_col]
  vice_temp <- eval(as.symbol(vice_sheet_name))[,common_col]
  
  # Assign the combined data sheet with name final_xxx
  assign(paste(data_name,name_temp,sep='_'),
         matrix(nrow = 0,ncol = length(col_name)),
         envir = .GlobalEnv)
  # Rename the final_xxx in
  ma_temp <- eval(as.symbol(paste(data_name,name_temp,sep='_')))
  colnames(ma_temp) <- unlist(col_name)
  assign(paste(data_name,name_temp,sep='_'),ma_temp, envir = .GlobalEnv)
  # Get the date of the file
  date_str <- unlist(strsplit(
    unlist(strsplit(file_add,config['file_add_and_name']))[2],'.xlsx'))
  # Create a temp data set for store the data
  temp_sets <- matrix(nrow = main_row_len,ncol=2)
  temp_sets[,1] <-rep(format(as.Date(date_str, format='%b %d %Y'),
                             '%Y/%m/%d'),main_row_len)
  temp_sets[,2] <- unlist(eval(as.symbol(main_sheet_name))[,unlist(time_col_pos)[main_sheet_name]])
  # Create temp matrix to store data
  col_temp <- matrix(nrow = main_row_len,ncol=1)
  col_final <- matrix(nrow = main_row_len,ncol=0)
  
  # Assign the data into each colunms and combined it into the final one
  for(k in 1:common_lenth){
    for (d in 1:vice_row_len){
      main_col_temp <- unlist(as.data.frame(main_temp[k]))
      vice_col_temp <- unlist(as.data.frame(vice_temp[k]))
      col_temp[,1] <- main_col_temp*((vice_col_temp)[d]/sum(vice_col_temp))
      col_final <- cbind(col_final,col_temp)
    }
  }
  # Combined the data to the temp_sets
  temp_sets <- cbind(temp_sets,col_final)
  # Rename the temp_sets
  colnames(temp_sets) <- unlist(col_name)
  # Create a temp variable as final_xxx
  temp_final <- eval(as.symbol(paste(data_name,name_temp,sep='_')))
  # Combined data
  temp_final <- rbind(temp_final,temp_sets)
  # Assign the final_xxx with the latest data
  assign(paste(data_name,name_temp,sep='_'),
         as.data.frame(temp_final), 
         envir = .GlobalEnv)
  if (file_stats=='others'){
    temp <- eval(as.symbol(paste('temp',name_temp,sep='_')))
    final <- eval(as.symbol(paste('final',name_temp,sep='_')))
    if (length(colnames(temp)) != length(colnames(final))) {
      row_length <- dim(temp)[1]
      temp_temp <- temp[,1:2]
      for (i_col_name in colnames(final)) {
        if (i_col_name %in% colnames(temp)) {
          temp_temp <- cbind(temp_temp,temp[i_col_name])
        }else{
          y = matrix(nrow = row_length,ncol=1)
          colnames(y) <- i_col_name
          temp_temp <- cbind(temp_temp,y)
        }
      }
      temp <- temp_temp[,-1]
      temp <- temp[,-1]
    }
    colnames(temp) <- colnames(eval(as.symbol(paste('final',name_temp,sep='_'))))
    assign(paste('temp',name_temp,sep='_'),temp,envir = .GlobalEnv)
    assign(paste('final',name_temp,sep='_'),
           rbind(eval(as.symbol(paste('final',name_temp,sep='_'))),
                 eval(as.symbol(paste(data_name,name_temp,sep='_')))),
           envir = .GlobalEnv)
  }
  }