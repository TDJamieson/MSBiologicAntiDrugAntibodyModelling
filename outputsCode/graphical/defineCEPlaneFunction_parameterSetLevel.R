createIncrementalCostQALYPlot_undiscounted <- function(DT,
                                          CEThreshold = 20000){
    
 
    DT <- copy(DT)
  
    costLimits <- c(ifelse(min(DT[,meandiff_lifetime_cycleCosts]) <0,
                           min(DT[,meandiff_lifetime_cycleCosts])*1.25,
                           0),
                    ifelse(max(DT[,meandiff_lifetime_cycleCosts]) >0,
                           max(DT[,meandiff_lifetime_cycleCosts])*1.25,
                           0))
    QALYLimits <- c(ifelse(min(DT[, meandiff_lifetime_utility]) <0,
                           min(DT[, meandiff_lifetime_utility])*1.25,
                           0),
                    ifelse(max(DT[, meandiff_lifetime_utility]) >0,
                           max(DT[, meandiff_lifetime_utility])*1.25,
                           0))
   
    CEThresholdPlot <- data.frame(y = c(-CEThreshold, CEThreshold), x = c(-1,1))
    
    summaryMeans <- DT[, .(meanDeltaCosts = mean(meandiff_lifetime_cycleCosts),
                           meanDeltaQALYs = mean(meandiff_lifetime_utility),
                           meanCostQALYs = mean(meandiff_lifetime_cycleCosts)/mean(meandiff_lifetime_utility))]

  
    costQALYPlot <- ggplot(DT, 
                           aes(y = meandiff_lifetime_cycleCosts,
                               x = meandiff_lifetime_utility))+
                                
                              geom_point(alpha= 1, size = 2)+
                              #geom_hline(yintercept = 0, colour = 'grey')+
                              #geom_vline(xintercept = 0, colour = 'grey')+
                              geom_line(data = CEThresholdPlot, aes(x=x, y = y), linetype = 'dashed')+
                              geom_point(data = summaryMeans,
                                         aes(x = meanDeltaQALYs,
                                             y = meanDeltaCosts),
                                         colour = 'red',
                                         fill ='red',
                                         shape = 3,
                                         size = 6,
                                         stroke = 1.25)+
                             stat_ellipse(linetype = 'dotted')+
                             geom_point(data = summaryMeans,
                                         aes(x = meanDeltaQALYs,
                                             y = meanDeltaCosts),
                                         colour = 'white',
                                         fill ='white',
                                         size = 2,
                                         stroke = 1)+
                             geom_point(data = summaryMeans,
                                         aes(x = meanDeltaQALYs,
                                             y = meanDeltaCosts),
                                         colour = 'black',
                                         fill ='black',
                                         size = 0.05)+
                              scale_y_continuous(breaks = seq(costLimits[1],costLimits[2], 500), 
                                                 labels = seq(costLimits[1],costLimits[2], 500),
                                                  expand = c(0,0))+
                              scale_x_continuous(breaks = seq(QALYLimits[1],QALYLimits[2], 0.02), 
                                                 labels = seq(QALYLimits[1],QALYLimits[2], 0.02),
                                                 expand = c(0,0))+
                              coord_cartesian(xlim = QALYLimits, ylim = costLimits)+


                              labs(x = 'Incremental Undiscounted QALYs',
                                   y = 'Incremental Undiscounted Costs (\U00a3)',
                                   title = paste0('Incremental Costs Against Incremental QALYs\n','Probabilistic Sensitivity Results'),
                                   subtitle = paste0('Mean difference in cost: \U00a3', 
                                                     format(round(mean(summaryMeans[, meanDeltaCosts]), 2), 
                                                            big.mark = ",",
                                                       scientific = FALSE), 
                                                     '\n',
                                                     'Mean difference in QALYs: ', 
                                                     format(round(mean(summaryMeans[, meanDeltaQALYs]), 2), 
                                                            big.mark = ",",
                                                       scientific = FALSE), 
                                                     ' QALYs', '\n',
                                                     'Cost-effectiveness threshold line at \U00a3', 
                                                     format(CEThreshold, big.mark = ",", scientific = FALSE),
                                                     ' per QALY'))+
      
                              theme_classic()+
                              guides(colour = FALSE)+
                              theme(axis.title.x = element_text(size = rel(1.3), margin = margin(5,0,0,0)))+
                              theme(axis.title.y = element_text(size = rel(1.3), margin = margin(0,5,0,0)))+
                              theme(plot.title = element_text(size = rel(1.4)))

    return(costQALYPlot)

  
    
}



createIncrementalCostQALYPlot_discounted <- function(DT,
                                          CEThreshold = 20000){
    
 
    DT <- copy(DT)
  
    costLimits <- c(ifelse(min(DT[,meandiff_lifetime_cycleCosts_discounted]) <0,
                           min(DT[,meandiff_lifetime_cycleCosts_discounted])*1.25,
                           0),
                    ifelse(max(DT[,meandiff_lifetime_cycleCosts_discounted]) >0,
                           max(DT[,meandiff_lifetime_cycleCosts_discounted])*1.25,
                           0))
    QALYLimits <- c(ifelse(min(DT[, meandiff_lifetime_utility_discounted]) <0,
                           min(DT[, meandiff_lifetime_utility_discounted])*1.25,
                           0),
                    ifelse(max(DT[, meandiff_lifetime_utility_discounted]) >0,
                           max(DT[, meandiff_lifetime_utility_discounted])*1.25,
                           0))
   
    CEThresholdPlot <- data.frame(y = c(-CEThreshold, CEThreshold), x = c(-1,1))
    
    summaryMeans <- DT[, .(meanDeltaCosts = mean(meandiff_lifetime_cycleCosts_discounted),
                           meanDeltaQALYs = mean(meandiff_lifetime_utility_discounted),
                           meanCostQALYs = mean(meandiff_lifetime_cycleCosts_discounted)/mean(meandiff_lifetime_utility_discounted))]

  
    costQALYPlot <- ggplot(DT, 
                           aes(y = meandiff_lifetime_cycleCosts_discounted,
                               x = meandiff_lifetime_utility_discounted))+
                                
                              geom_point(alpha= 1, size = 2)+
                              #geom_hline(yintercept = 0, colour = 'grey')+
                              #geom_vline(xintercept = 0, colour = 'grey')+
                              geom_line(data = CEThresholdPlot, aes(x=x, y = y), linetype = 'dashed')+
                              geom_point(data = summaryMeans,
                                         aes(x = meanDeltaQALYs,
                                             y = meanDeltaCosts),
                                         colour = 'red',
                                         fill ='red',
                                         shape = 3,
                                         size = 6,
                                         stroke = 1.25)+
                             stat_ellipse(linetype = 'dotted')+
                             geom_point(data = summaryMeans,
                                         aes(x = meanDeltaQALYs,
                                             y = meanDeltaCosts),
                                         colour = 'white',
                                         fill ='white',
                                         size = 2,
                                         stroke = 1)+
                             geom_point(data = summaryMeans,
                                         aes(x = meanDeltaQALYs,
                                             y = meanDeltaCosts),
                                         colour = 'black',
                                         fill ='black',
                                         size = 0.05)+
                              scale_y_continuous(breaks = seq(costLimits[1],costLimits[2], 500), 
                                                 labels = seq(costLimits[1],costLimits[2], 500),
                                                  expand = c(0,0))+
                              scale_x_continuous(breaks = seq(QALYLimits[1],QALYLimits[2], 0.01), 
                                                 labels = seq(QALYLimits[1],QALYLimits[2], 0.01),
                                                 expand = c(0,0))+
                              coord_cartesian(xlim = QALYLimits, ylim = costLimits)+


                              labs(x = 'Incremental QALYs',
                                   y = 'Incremental Costs (\U00a3)',
                                   title = paste0('Incremental Costs Against Incremental QALYs\n','Probabilistic Sensitivity Results'),
                                   subtitle = paste0('Mean difference in cost: \U00a3', 
                                                     format(round(mean(summaryMeans[, meanDeltaCosts]), 2), 
                                                            big.mark = ",",
                                                       scientific = FALSE), 
                                                     '\n',
                                                     'Mean difference in QALYs: ', 
                                                     format(round(mean(summaryMeans[, meanDeltaQALYs]), 2), 
                                                            big.mark = ",",
                                                       scientific = FALSE), 
                                                     ' QALYs', '\n',
                                                     'Cost-effectiveness threshold line at \U00a3', 
                                                     format(CEThreshold, big.mark = ",", scientific = FALSE),
                                                     ' per QALY'))+
      
                              theme_classic()+
                              guides(colour = FALSE)+
                              theme(axis.title.x = element_text(size = rel(1.3), margin = margin(5,0,0,0)))+
                              theme(axis.title.y = element_text(size = rel(1.3), margin = margin(0,5,0,0)))+
                              theme(plot.title = element_text(size = rel(1.4)))

    return(costQALYPlot)

  
    
}