CellCyte_Optim<-function(CC_Input, init=c(K=1200, N0=100, Td=36, T_off=24, Bg=50), BgEst_method="Median", Optim_method="Nelder-Mead", cspr.vol=200, cspr.cell=312){
  data<-CC_Input$data
  metadata<-CC_Input$metadata
  plotdata<-CC_Input$plotdata
  mins<-CC_Input$mins
  
  #This needs to be made more universal, or specified in function
  # Try to make it be specified like "Design" in DEseq2
  
  samples<-unique(metadata$Line)
  
  #Logistic function minimization (RSS):
  
  formula<-function(data, init){
    with(data,
         sum(
           sqrt(
             (
               (init[1]/(1+((init[1]/init[2])-1)*exp(-((log(2)/init[3])*(Hour-init[4])))))-(Mass.Est-init[5])
             )^2
           ), na.rm=T
         )
         
    )
  }
  output_UseAll<-data.frame()
  for(i in samples){
    Bg_est<-median(mins %>% filter(Line==i & !Outlier) %>% pull(Mass.Est), na.rm=T)
    T_est<-median(mins %>% filter(Line==i & !Outlier) %>% pull(Hour), na.rm=T)
    K_est<-max(plotdata %>% filter(Line==i & Hour>24) %>% pull(Mass.Est), na.rm=T)
    subset<-plotdata %>%
      filter(!Outlier & Line==i & Hour>T_est)
    opt<-optim(par=c(K_est-Bg_est, 0, 24, T_est, Bg_est), 
               fn=formula, 
               data=subset, 
               method=Optim_method)
    output_UseAll<-rbind(
      output_UseAll,
      data.frame(
        Sample=i,
        method=ifelse(Optim_method=="Nelder-Mead", "NM", Optim_method), 
        K=unname(opt$par[1]), 
        N0=unname(opt$par[2]), 
        Td=unname(opt$par[3]),
        Bg=unname(opt$par[5]),
        Time_offset=unname(opt$par[4]),
        RSS=unname(opt$value)
      ) %>%
        mutate(r=log(2)/Td) %>%
        mutate(cspr=((2*r*cspr.vol)*1000)/(((K-N0)*log((K/N0)-1))*cspr.cell))
    )
  } 
  CC_Input$fits_UseAll<-output_UseAll
  CC_Input$fits_UseAll$Sample<-factor(CC_Input$fits_UseAll$Sample, levels=unique(metadata$Line))
  
  output_each<-data.frame()
  for(i in CC_Input$metadata %>% pull(Well)){
    Bg_est<-min(CC_Input$data %>% pull(i), na.rm=T)
    K_est<-max(CC_Input$data %>% filter(Hour >24) %>% pull(i), na.rm=T)
    T_est<-CC_Input$mins %>% filter(Well == i) %>% pull(Hour)
    subset<-CC_Input$plotdata %>% filter(Hour > T_est & Well== i)
    
    opt<-optim(par=c(K_est-Bg_est, 0, 24, T_est, Bg_est), 
               fn=formula, 
               data=subset, 
               method="Nelder-Mead")
    
    output_each<-rbind(
      output_each,
      data.frame(
        Well=i,
        K=unname(opt$par[1]), 
        N0=unname(opt$par[2]), 
        Td=unname(opt$par[3]),
        Bg=unname(opt$par[5]),
        Time_offset=unname(opt$par[4]),
        RSS=unname(opt$value)
      ) %>%
        mutate(r=log(2)/Td) %>%
        mutate(cspr=((2*r*cspr.vol)*1000)/(((K-N0)*log((K/N0)-1))*cspr.cell))
    )
  }
  
  output_each <- output_each %>% left_join(CC_Input$metadata, "Well")
  CC_Input$fits_each<-output_each
  CC_Input$fits_each$Line<-factor(CC_Input$fits_each$Line, levels=unique(metadata$Line))
  
  predict<-data.frame(Hour=seq(-50,max(CC_Input$data$Hour)+50,1))
  for(i in output_each$Well){
    opt<-output_each %>%
      filter(Well==i)
    predict[i]<-(opt$K/(1+((opt$K/opt$N0)-1)*exp(-((log(2)/opt$Td)*(predict$Hour-opt$Time_offset)))))+opt$Bg
  }
  for(i in unique(CC_Input$metadata$Line)){
    opt<-CC_Input$fits_UseAll %>%
      filter(Sample==i)
    predict[paste0(i, "_all")]<-(opt$K/(1+((opt$K/opt$N0)-1)*exp(-((log(2)/opt$Td)*(predict$Hour-opt$Time_offset)))))+opt$Bg
  }
  predict<-predict %>% gather(Sample, Predict, -Hour) %>% left_join(CC_Input$metadata, c("Sample"="Well"))
  predict[is.na(predict$Line),]["Line"]<-apply(predict[is.na(predict$Line),]["Sample"], 1, function(x) gsub("_all", "", x))
  
  CC_Input$predict<-predict
  
  logislopes<-numeric()
  for(i in unique(metadata$Line)){
    logislopes<-c(logislopes, LogiSlope(as.numeric(CC_Input$fits_UseAll %>% filter(Sample == i) %>% select(c(K, Td, N0, Time_offset, Bg))))[1])
  }
  for(i in metadata$Well){
    logislopes<-c(logislopes, LogiSlope(as.numeric(CC_Input$fits_each %>% filter(Well == i) %>% select(c(K, Td, N0, Time_offset, Bg))))[1])
  }
  names(logislopes)<-c(unique(metadata$Line),metadata$Well)
  logislopes<-data.frame(Sample=names(logislopes), logislopes=logislopes, row.names=NULL) %>% left_join(metadata, c("Sample"="Well"))
  CC_Input$logislopes<-logislopes
  CC_Input$logislopes$Line <- factor(CC_Input$logislopes$Line, levels=unique(metadata$Line))
  
  
  
  
  return(CC_Input)
}
