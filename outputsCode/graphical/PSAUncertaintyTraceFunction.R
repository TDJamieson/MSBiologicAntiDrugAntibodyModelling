

createPSAUncertaintytrace <- function(limits, DT, varName, varLabel, 
                                      plotTitle, deterministicVal, costPlot = 0){
 
  borders <- eval(limits)
  
  if(costPlot == 1){
    
    deterministicValLabel = paste0('\U00a3', format(deterministicVal, big.mark = ','))
    probabilisticValLabel = paste0('\U00a3', format(DT[, mean(get(varName))], big.mark = ',', digits = 3))
    
  } else {
    
    deterministicValLabel = deterministicVal
    probabilisticValLabel = format(DT[, mean(get(varName))], big.mark = ',', digits = 3)
  
  }
  
  
  
  
  
  plotDT1 <- as.data.table(rbindlist(lapply(borders, function(limit) { 
             
              mean = DT[1:limit, mean(get(varName))]
              sdev = DT[1:limit, sd(get(varName))]
              
              data.table(limit = limit, 
                         ci1 = mean + (sdev*1.96),
                         ci2 = mean - (sdev*1.96),
                         mean = mean)
              })))
  
  plotDT2 <- melt(plotDT1, id.vars = 'limit')

  means <- plotDT2[variable == 'mean', .(value, limit)]

  cis <- plotDT2[variable != 'mean', .(value, limit)]
 
  cis[, topline := max(value), by = limit]

  cis[, bottomline := min(value), by = limit]

  cis[, x1 := limit - 5]

  cis[, x2 := limit + 5]

  cis[means, mean := i.value, on = 'limit']
  
  filler <- copy(cis)
  setorder(filler, limit)
  filler[, nextTopLine := shift(topline,-2)]
  filler[, nextTopLine := nextTopLine - topline]

  filler[, nextBottomLine := shift(bottomline,-2)]
  filler[, nextBottomLine := nextBottomLine - bottomline]
  
  filler[, nextLimit := shift(limit,-2)]
  
  filler <- expand_grid(filler[, .(limit, nextLimit, topline, bottomline, 
                                   nextTopLine, nextBottomLine)],
                        multiplier = seq(0,1, 0.02))
  
  setDT(filler)
  filler[, interTop := (nextTopLine*multiplier) + topline]
  filler[, interBottom := (nextBottomLine * multiplier) + bottomline]
  filler[, limit := ((nextLimit - limit) * multiplier) + limit]
 
    plot <- ggplot(cis, aes(x = limit))+
            geom_line(aes(y = value, group= limit), lwd = 1.05)+
            geom_point(aes(x = limit, group = limit, y = mean), 
                        size = 5, colour = 'black',  shape =18)+
            geom_point(aes(x = limit, group = limit, y = mean), 
                        size = 1, colour = 'white', shape =18)+
            geom_segment(aes(x = x1, xend = x2, y = topline, yend = topline), lwd = 1.025)+
            geom_segment(aes(x = x1, xend = x2, y = bottomline, yend = bottomline), lwd = 1.025)+
            geom_segment(data = filler, aes(x = limit, xend = limit, 
                                            y = interBottom, yend = interTop), 
                         alpha = 0.2, linetype = 'dotted', colour = 'blue')+
            geom_hline(yintercept = deterministicVal, linetype = 'dashed',
                       colour = 'red', lwd = 1.025)+
          geom_hline(yintercept = DT[, mean(get(varName))], linetype = 'dashed',
                     colour = 'black', lwd = 1.025)+
          labs(title = plotTitle,
               # subtitle = paste0('Deterministic mean value: ', deterministicValLabel, '\n',
               #                   'Probabilistic mean value: ', probabilisticValLabel),
               y = varLabel, x = 'No of parameter sets')+
          scale_x_continuous(breaks = seq(0,1000,50), labels = seq(0,1000,50))+
          theme_classic()+
          theme(axis.title.x = element_text(size = rel(2.5), margin = margin(5,0,0,0)),
                axis.text.x = element_text(size = rel(3)))+
          theme(axis.title.y = element_text(size = rel(2.5), margin = margin(0,10,0,5)),
                axis.text.y = element_text(size = rel(3)))+
          theme(plot.title = element_text(size = rel(3)),
                plot.subtitle = element_text(size = rel(2.5)),
                axis.line = element_line(colour = 'black', linewidth = 1.2))

  
}

