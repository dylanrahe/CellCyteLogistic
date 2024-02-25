CellCyte_Optim<-function(CC_Input, init=c(K=1200, N0=100, Td=36, T_off=24, Bg=50), BgEst_method="Median", Optim_method="Nelder-Mead"){
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
           )
         )
         
    )
  }
  output<-data.frame()
  for(i in samples){
    Bg_est<-median(mins %>% filter(Line==i & !Outlier) %>% pull(Mass.Est))
    T_est<-median(mins %>% filter(Line==i & !Outlier) %>% pull(Hour))
    K_est<-max(plotdata %>% filter(Line==i & Hour>24) %>% pull(Mass.Est), na.rm=T)
    subset<-plotdata %>%
      filter(!Outlier & Line==i & Hour>T_est)
    opt<-optim(par=c(K_est-Bg_est, 0, 24, T_est, Bg_est), 
               fn=formula, 
               data=subset, 
               method=Optim_method)
    output<-rbind(
      output,
      data.frame(
        sample=i,
        method=ifelse(Optim_method=="Nelder-Mead", "NM", Optim_method), 
        K=unname(opt$par[1]), 
        N0=unname(opt$par[2]), 
        Td=unname(opt$par[3]),
        Bg=unname(opt$par[5]),
        Time_offset=unname(opt$par[4]),
        RSS=unname(opt$value)
      ) %>%
        mutate(r=log(2)/Td)
    )
  } 
  CC_Input$fits<-output
  return(CC_Input)
}