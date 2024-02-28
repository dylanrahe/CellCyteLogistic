CC_SlopesPlot<-function(CC_Input, refGroup=NULL){
  plot<-
  ggplot() +
    geom_boxplot(data=CC_Input$logislopes[!(CC_Input$logislopes$Sample %in% metadata$Line),], aes(Line, logislopes, color=Line), size=1.5) +
    geom_jitter(data=CC_Input$logislopes[!(CC_Input$logislopes$Sample %in% metadata$Line),], aes(Line, logislopes, color=Line), width=0.2, size=2) +
    geom_point(data=CC_Input$logislopes[CC_Input$logislopes$Sample %in% metadata$Line,], aes(Sample,logislopes), size=3, color="grey40") +
    scale_color_manual(values=viridis(12)) +
    ylab("Growth Rate, µg/hr") +
    xlab("Sample") +
    ggtitle("Growth Rate using Automatic Fits", 
            subtitle = "Slope of tangent line at inflection point of logistic curves") +
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
    
  
  if(!is.null(refGroup)){
    if(refGroup %in% metadata$Line) {
      plot <- plot + stat_compare_means(data=CC_Input$logislopes[!(CC_Input$logislopes$Sample %in% metadata$Line),], aes(Line, logislopes),method = "anova", label.y = max(CC_Input$logislopes$logislopes)*1.02) + # Add global p-value
                     stat_compare_means(data=CC_Input$logislopes[!(CC_Input$logislopes$Sample %in% metadata$Line),], aes(Line, logislopes),label="p.signif", method = "t.test",
                                        ref.group = refGroup, label.y = max(CC_Input$logislopes$logislopes)*1.06)
    } else {
      stop("\"refGroup\" is not contained within metadata")
    }
  }
  plot
}
