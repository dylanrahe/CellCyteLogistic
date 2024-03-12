CC_ParamPlot<-function(CC_Input, parameter="K", refGroup=NULL, title=NULL, plot.outliers=TRUE){
  
  fits_each<-if(plot.outliers==T){
    CC_Input$fits_each
  } else {
    CC_Input$fits_each %>% 
      group_by(Line) %>% 
      mutate(Outlier = abs(get(parameter) - median(get(parameter), na.rm=T)) > 1.5 * IQR(CC_Input$fits_each[,parameter], na.rm=T)) %>%
      filter(!Outlier)
  }
  
  fits_UseAll<-if(plot.outliers==T){
    CC_Input$fits_UseAll
  } else {
    CC_Input$fits_UseAll %>% 
      mutate(Outlier = abs(get(parameter) - median(get(parameter), na.rm=T)) > 1.5 * IQR(CC_Input$fits_each[,parameter], na.rm=T)) %>%
      filter(!Outlier)
  }
  
  y_labels<-list(
    K="Carrying Capacity, µg",
    N0="Initial Mass, µg",
    Td="Doubling Time, Hours",
    Bg="X-offset, µg",
    Time_offset="Y-offset, Hours",
    r=bquote('Rate Constant, '~day^-1),
    CSPR="CSPR, nl/cell/day"
  )
  
  title<-if(is.null(title)){
    list(
      K="Carrying Capacity",
      N0="Initial Mass",
      Td="Doubling Time",
      Bg="X-offset",
      Time_offset="Y-offset",
      r="Rate Constant",
      CSPR="CSPR"
    )[parameter]
  } else {
    title
  }
  
  plot<-
    ggplot() +
    geom_boxplot(data=fits_each, aes(Line, get(parameter), color=Line), size=1.5) +
    geom_jitter(data=fits_each, aes(Line, get(parameter), color=Line), width=0.2, size=2) +
    geom_point(data=fits_UseAll, aes(Sample, get(parameter)), size=3, color="grey40") +
    scale_color_manual(values=viridis(12)) +
    ylab(y_labels[parameter]) +
    xlab("Sample") +
    ggtitle(title) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust=1),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          axis.line = element_line(color="black"),
          axis.title.y = element_text(size=9),
          plot.title = element_text(hjust=0.5, face="bold"),
          plot.subtitle = element_text(hjust=0.5))
  if(plot.outliers==F){
    plot<-plot +
      annotate(geom = "text", x = 1, y = range(fits_each[parameter])[2], 
               label = "*Outliers removed from plot", hjust = 0, vjust = 1, size = 2)
  }
  
  if(!is.null(refGroup)){
    if(refGroup %in% CC_Input$metadata$Line) {
      plot <- plot + stat_compare_means(data=CC_Input$fits_each, aes(Line, get(parameter)),method = "anova", label.y = max(fits_each[parameter])*1.02) + # Add global p-value
        stat_compare_means(data=CC_Input$fits_each, aes(Line, get(parameter)),label="p.signif", method = "t.test",
                           ref.group = refGroup, label.y = max(fits_each[parameter])*1.06)
    } else {
      stop("\"refGroup\" is not contained within metadata")
    }
  }
  plot
}
