



  costQALYQALYPlotting <- function(DT, variationDescription, baseCaseValue, varyingVarTitle){
 
      costQALYPlot <- 
        ggplot(DT, aes(x = as.numeric(get(variationDescription)), 
                                        y = meandiff_cost_qaly_discounted))+
        geom_point()+
        geom_line(size = 1.25)+
        geom_vline(xintercept = baseCaseValue, linetype = 'dashed', colour = 'red',  size = 1.25)+
        labs(y = 'Incremental Cost per QALY', x = varyingVarTitle)+
        scale_y_continuous(labels = scales::comma)+
        theme_classic()+
        theme(axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12),
              axis.title.x = element_text(margin = unit(c(5,0, 0, 0), "mm"), size = 12),
              axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10))
        
  
      QALYPlot <- 
        ggplot(DT, aes(x = as.numeric(get(variationDescription)), 
                                        y = meandiff_lifetime_utility_discounted))+
        geom_point()+
        geom_line()+
        geom_vline(xintercept = baseCaseValue, linetype = 'dashed')+
        labs(y = 'QALY gain (discounted)')+
        theme_classic()+
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 10))
      
      return(list(QALYPlot = QALYPlot, costQALYPlot = costQALYPlot))
  }
 

 
  costQALYQALYPlottingCrossZero <- function(popMeanDeltasPlotting, variationDescription, baseCaseValue){
 
  # Split into above and below zero if appropriate
    
    if((nrow(popMeanDeltasPlotting[meandiff_cost_qaly_discounted > 0]) > 0 &
        nrow(popMeanDeltasPlotting[meandiff_cost_qaly_discounted < 0]) > 0 )){
    
        popMeanDeltasPlotting1 <- popMeanDeltasPlotting[meandiff_cost_qaly_discounted < 0]
        popMeanDeltasPlotting2 <- popMeanDeltasPlotting[meandiff_cost_qaly_discounted >= 0]
    
    } else {
      
        popMeanDeltasPlotting1 <- popMeanDeltasPlotting
        popMeanDeltasPlotting2 <- popMeanDeltasPlotting
    }
    
    
         
      
     costQALYPlot <- 
        ggplot(popMeanDeltasPlotting, aes(x = as.numeric(get(variationDescription)), 
                                        y = meandiff_cost_qaly_discounted))+
        geom_point()+
        geom_line(data = popMeanDeltasPlotting1, aes(x = as.numeric(get(variationDescription)), 
                                        y = meandiff_cost_qaly_discounted),
                  size = 1.25)+
        geom_line(data = popMeanDeltasPlotting2, aes(x = as.numeric(get(variationDescription)), 
                                        y = meandiff_cost_qaly_discounted),
                  size = 1.25)+
        geom_vline(xintercept = baseCaseValue, linetype = 'dashed',
                    colour = 'red',  size = 1.25)+
        geom_hline(yintercept = 0)+
        labs(y = 'Incremental Cost per QALY', x = varyingVarTitle)+
        scale_y_continuous(labels = scales::comma)+
        theme_classic()+
        theme(axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12),
              axis.title.x = element_text(margin = unit(c(5,0, 0, 0), "mm"), size = 12),
              axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10))
        
  
     
  # Split into above and below zero if appropriate
    
      QALYPlot <- 
        ggplot(popMeanDeltasPlotting, aes(x = as.numeric(get(variationDescription)), 
                                        y = meandiff_lifetime_utility_discounted))+
        geom_point()+
        geom_line()+
        geom_vline(xintercept = baseCaseValue, linetype = 'dashed')+
        geom_hline(yintercept = 0)+
        labs(y = 'QALY gain (discounted)')+
        theme_classic()+
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 10))
      
      
      plot <-  QALYPlot / costQALYPlot+
        plot_layout(heights = c(1,2))
      
      return(list(QALYPlot = QALYPlot, costQALYPlot = costQALYPlot))
  
  }
