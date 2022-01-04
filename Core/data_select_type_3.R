data_select_type_3 <- function(sheet_name,time_col_pos,data_col_start,data_col_end){
  i <- sheet_name
  # First get the colnames of this sheet
  i_name <- unlist(colnames(eval(as.symbol(i))))
  # Get the colname we select in the config.yml
  
  if (length(colnames(eval(as.symbol(i))))!=unlist(data_col_end[i])) {
    temp_sheet <- eval(as.symbol(i))
    row_len <- dim(temp_sheet)[1]
    temp_col <- rep(2020,row_len)
    temp_sheet <- cbind(temp_col,temp_sheet)
    assign(i,temp_sheet,envir=.GlobalEnv)
    i_name <- unlist(colnames(eval(as.symbol(i))))
  }

  if (unlist(time_col_pos[i])!=0){
    name_temp <- c(i_name[1:2],
                   i_name[unlist(time_col_pos[i])],
                   i_name[unlist(data_col_start[i]):unlist(data_col_end[i])])
  }else{
    name_temp <- c(i_name[1:2],
                   i_name[unlist(data_col_start[i]):unlist(data_col_end[i])])
  }
  
  # Select the subset of the data
  data_temp <- subset(eval(as.symbol(i)),select=name_temp)
  # Assign the selected data to overrun the original one.
  assign(i,data_temp,envir=.GlobalEnv)
}