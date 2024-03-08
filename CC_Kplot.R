CC_Kplot<-function(CC_Input, refGroup=NULL, title="Carrying Capacity", plot.outliers=TRUE){
  
  fits_each<-if(plot.outliers==T){
    CC_Input$fits_each
  } else {
    CC_Input$fits_each %>% 
      group_by(Line) %>% 
      mutate(Outlier = abs(K - median(K, na.rm=T)) > 1.5 * IQR(CC_Input$fits_each$K, na.rm=T)) %>%
      filter(!Outlier)
  }
  
  fits_UseAll<-if(plot.outliers==T){
    CC_Input$fits_UseAll
  } else {
    CC_Input$fits_UseAll %>% 
      mutate(Outlier = abs(K - median(K, na.rm=T)) > 1.5 * IQR(K, na.rm=T)) %>%
      filter(!Outlier)
  }
  
  plot<-
    ggplot() +
    geom_boxplot(data=fits_each, aes(Line, K, color=Line), size=1.5) +
    geom_jitter(data=fits_each, aes(Line, K, color=Line), width=0.2, size=2) +
    geom_point(data=fits_UseAll, aes(Sample, K), size=3, color="grey40") +
    scale_color_manual(values=viridis(12)) +
    ylab("Carrying Capacity, µg") +
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
      annotate(geom = "text", x = 1, y = range(fits_each$K)[2], 
               label = "*Outliers removed from plot", hjust = 0, vjust = 1, size = 2)
  }
  
  if(!is.null(refGroup)){
    if(refGroup %in% CC_Input$metadata$Line) {
      plot <- plot + stat_compare_means(data=CC_Input$fits_each, aes(Line, K),method = "anova", label.y = max(fits_each$K)*1.02) + # Add global p-value
        stat_compare_means(data=CC_Input$fits_each, aes(Line, K),label="p.signif", method = "t.test",
                           ref.group = refGroup, label.y = max(fits_each$K)*1.06)
    } else {
      stop("\"refGroup\" is not contained within metadata")
    }
  }
  plot
}