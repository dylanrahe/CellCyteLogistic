CellCyte_Input<-function(data, metadata, excel=T) {
  ## should probably put some checks here. "excel=F" etc. isn't a good implementation, and the error messages are not useful.
  readydata<-
    if(excel==F){
      suppressMessages(
        read_csv(data, show_col_types = FALSE) %>%
          select(-contains(c("Scan", "Stdev"))) %>%
          rename(!!! setNames(names(.), c("Hour", gsub("Well ", "", .[1, -1])))) %>%
          filter(!row_number() %in% c(1, 2)) %>%
          mutate_if(is.character, as.numeric) %>%
          mutate(across(!1, MassEst)) %>%
          mutate(Hour=Hour/60)
      )
    } else if(excel==T){
      readydata<-suppressMessages(
        read_excel(data) %>%
          select(-contains(c("Scan", "Stdev"))) %>%
          rename(!!! setNames(names(.), c("Hour", gsub("Well ", "", .[1, -1])))) %>%
          filter(!row_number() %in% c(1, 2)) %>%
          mutate_if(is.character, as.numeric) %>%
          mutate(across(!1, MassEst)) %>%
          mutate(Hour=Hour/60) 
      )
    }
  
  if(dim(metadata)[1]!=(length(colnames(readydata))-1)){
    stop("Mismatch between Data columns and Metadata rows: Check if all wells with data are present in metadata")
  }
  
  plotdata<-readydata %>%
    gather(Well, Mass.Est, -Hour) %>%
    inner_join(metadata, by=join_by(Well)) %>%
    group_by(Hour, Line) %>%
    mutate(Outlier = abs(Mass.Est - median(Mass.Est, na.rm=T)) > 1.5 * IQR(Mass.Est, na.rm=T))
  
  mins<-plotdata %>% 
    group_by(Well, Line) %>% 
    filter(Mass.Est==min(Mass.Est, na.rm=T)) %>%
    filter(row_number()==1)
  
  output<-list(data=readydata, metadata=metadata, plotdata=plotdata, mins=mins)
  return(output)
}
