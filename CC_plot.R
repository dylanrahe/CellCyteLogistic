CC_plot<-function(CC_data, sample, plot.exp=FALSE){
  plotdata<-
    CC_data$plotdata %>%
    filter(Line==sample)
  opt<-
    CC_data$fits_UseAll[CC_data$fits_UseAll$Sample==sample,]
  plot<-
  ggplot() + 
    geom_line(data=data.frame(Hour=seq(0,max(CC_data$data$Hour),1)) %>% 
                mutate(Predict=(opt$K/(1+((opt$K/opt$N0)-1)*exp(-((log(2)/opt$Td)*(Hour-opt$Time_offset)))))+opt$Bg), 
              aes(Hour, Predict), color="firebrick", linewidth=2) +
    geom_point(data=plotdata, aes(Hour, Mass.Est), color="grey70", alpha=0.6, pch=16) +
    geom_point(data=plotdata %>% filter(!Outlier), aes(Hour, Mass.Est), color="grey50", alpha=0.6, pch=16) +
    ylim(c(0,round(max(CC_data$plotdata %>% filter(Line==sample & Hour>24) %>% pull(Mass.Est), na.rm=T)*1.1))) +
    scale_x_continuous(breaks=seq(0, max(CC_data$plotdata$Hour), by=24),
                       limits=c(0, max(CC_data$plotdata$Hour))) +
    labs(title=paste("Sample:", sample, sep=" "), x="Hours", y="Estimated Mass, µg") +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5, face="bold"))
  if(plot.exp==TRUE) {
    plot<-
    plot + geom_line(data=data.frame(Hour=seq(0,max(CC_data$data$Hour),1)) %>% 
                mutate(Predict=opt$N0*exp(((log(2)/opt$Td)*(Hour-opt$Time_offset)))+opt$Bg), 
              aes(Hour, Predict), color="tomato3", linewidth=0.75)
  }
  plot
}
