#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Relapsing-remitting Multiple Sclerosis Microsimulation Model       ---#
#           Produce outputs from completed runs where a varying                #
#                       variable has been specified                            #
#                                                                              #
#==============================================================================#  
#==============================================================================#



#===============================================================================
#                    Create excel workbook for outcomes                        #
#===============================================================================

    
    outputWkbkFilePath <- paste0(runDirectory, runDescription, '_finalOutputs.xlsx')

    
    
    # Delete if existing
    
      file.remove(outputWkbkFilePath)
    
      
    # Create new and add worksheets
    
      outputsWkbk <- createWorkbook()
      
      addWorksheet(outputsWkbk, 'WholePopMeanOutcomes')
      addWorksheet(outputsWkbk, 'inputParameters')

      
    # Create styles
    
      doubleBorder <- createStyle(border = 'bottom', borderStyle = 'double')
      
      wrapTextStyle <- createStyle(wrapText = TRUE, textDecoration = 'bold')
      
      boldStyle <- createStyle(textDecoration = 'bold')
  
    
#===============================================================================

 
#===============================================================================
#                                Create table                                  #
#===============================================================================
    
  # Identify varying variable name
    
    variationDescription <- popMeanDeltas[,.SD[1]][, runDescription]
    variationDescription <- str_replace(variationDescription, runDescription, '')
    variationDescription <- str_replace(variationDescription, '_vary.*', '')
    variationDescription <- str_replace(variationDescription, '_', '')


  # Identify varying variable values (done for all outcome outputs)
  
    popMeanDeltas[, c(variationDescription) := sub('.*_', '', runDescription)]
    allDeltas[, c(variationDescription) := sub('.*_', '', runDescription)]
    IvCMeanDeltas[, c(variationDescription) := sub('.*_', '', runDescription)]
    
    
  # Save for graphical outputs
    
    popMeanDeltasPlotting <- copy(popMeanDeltas)
    
    
  # Melt
  
    popMeanDeltas <- melt(popMeanDeltas, id.vars = c('parameterSetID', 
                                                     variationDescription))
    
    
  # Merge in tidied names and tidy generally 
  
    popMeanDeltas[popMean_outputsTable, `:=` (Outcome = i.DifferenceOutcome,
                                              Include = i.Include,
                                              Order = i.TableRowOrder),
                  on = .(variable = DifferenceVariable)]

    popMeanDeltas <- popMeanDeltas[Include == 1]

    popMeanDeltas[, `:=`(Order = NULL,
                         Include = NULL,
                         variable = NULL)]
    
    popMeanDeltas[, popDescription := paste0('PSA ', '(', parameterSetID, ') ', '\n',
                                             'Variation: ', get(variationDescription))]
    
 
  # Find mins and maxes
     
    popMeanDeltas[, value := as.numeric(value)]
    popMeanDeltas[is.infinite(value), value := NA_integer_]
    
    popMeanDeltas[, min := min(value, na.rm = TRUE), by = .(Outcome)]
    popMeanDeltas[, max := max(value, na.rm = TRUE), by = .(Outcome)]
    
    popMeanDeltas[, c('min', 'max', 'value') := lapply(.SD, function (col) {
                        as.character(col)}),
                  .SDcols = c('min', 'max', 'value')]
    
 
  # Tidy formatting of numeric fields

    nonOutcomeCols <- c('value', 'min', 'max')
  
    popMeanDeltas[grepl('cost', Outcome, ignore.case = TRUE),
                  c(nonOutcomeCols) := lapply(.SD, function(col) {
                    paste0('\U00a3', format(round(as.numeric(col), 2), 
                                            big.mark = ','))}),
                    .SDcols = nonOutcomeCols]

    popMeanDeltas[!grepl('cost', Outcome, ignore.case = TRUE),
                 c(nonOutcomeCols) := lapply(.SD, function(col) {
                   paste0(round(as.numeric(col), digits = 4))}),
                 .SDcols = nonOutcomeCols]

    popMeanDeltas[, Range := paste0(min, ' - ', max)]
    popMeanDeltas[, c('min', 'max') := NULL]
    
    setcolorder(popMeanDeltas, 'Outcome')

    
  # Re-cast    

    popMeanDeltas <- dcast(popMeanDeltas, Outcome + Range ~ popDescription)

    popMeanDeltas[popMean_outputsTable, Order := i.TableRowOrder, 
                  on = .(Outcome = DifferenceOutcome)]   
    setorder(popMeanDeltas, Order)
    
    popMeanDeltas[, Order := NULL]
    
  
  # Add to excel
  
     writeData(outputsWkbk, 'WholePopMeanOutcomes', popMeanDeltas,
              startCol = 2, startRow = 2, colNames = TRUE)
     
     addStyle(outputsWkbk, 'WholePopMeanOutcomes', doubleBorder,
             rows = 1,
             cols = c(2:(2+ncol(popMeanDeltas)-1)))
     
     addStyle(outputsWkbk, 'WholePopMeanOutcomes', doubleBorder,
             rows = 2,
             cols = c(2:(2+ncol(popMeanDeltas)-1)))
     
     addStyle(outputsWkbk, 'WholePopMeanOutcomes', doubleBorder,
             rows = 2+nrow(popMeanDeltas),
             cols = c(2:(2+ncol(popMeanDeltas)-1)))
     
     setRowHeights(outputsWkbk, "WholePopMeanOutcomes", rows = 2, height = 80)

     setColWidths(outputsWkbk, "WholePopMeanOutcomes", cols = c(1:10000), width = 12)

     addStyle(outputsWkbk, sheet = 'WholePopMeanOutcomes', wrapTextStyle,
               rows = 2, cols = c(1:10000))
                                         
    
#===============================================================================

 
 
#===============================================================================
#               Create combined cost/QALY and QALY plot                        #
#===============================================================================
    
    # Define plotting functions
    
      source(paste0(outputProcessingDirectory, 'graphical/costQALYQALYPlots.R'))
     
     
    # Plot depends on whether cost/QALY crosses zero 
     
      if((nrow(popMeanDeltasPlotting[meandiff_cost_qaly_discounted > 0]) > 0 &
          nrow(popMeanDeltasPlotting[meandiff_cost_qaly_discounted < 0]) > 0 ) | 
         (nrow(popMeanDeltasPlotting[meandiff_lifetime_utility_discounted > 0]) > 0 &
          nrow(popMeanDeltasPlotting[meandiff_lifetime_utility_discounted < 0]) > 0 )){
       
        plot <- 
          costQALYQALYPlottingCrossZero(popMeanDeltasPlotting, variationDescription,
                                         baseCaseValue)
        
         QALYPlot <- plot$QALYPlot
         costQALYPlot <- plot$costQALYPlot
         
        if(grepl('yes', breakForPlot, ignore.case = TRUE) == TRUE) browser()
           
        combinedPlot <- QALYPlot / costQALYPlot+
                plot_layout(heights = c(1,2))
     
         
     } else {
    
       plot <- 
          costQALYQALYPlotting(popMeanDeltasPlotting, variationDescription,
                                        baseCaseValue)
       QALYPlot <- plot$QALYPlot
       costQALYPlot <- plot$costQALYPlot
       
       if(grepl('yes', breakForPlot, ignore.case = TRUE) == TRUE) browser()
      
        combinedPlot <- QALYPlot / costQALYPlot+
                plot_layout(heights = c(1,2))
      
     } 
     
    
     # Add to workbook
   
      addWorksheet(outputsWkbk, 'plot')
    
      print(combinedPlot)
      insertPlot(outputsWkbk, 'plot', width = 12, height = 8, 
                 startRow = 2, startCol = 2)


      
     
#===============================================================================

      

#===============================================================================
#                        Add run input parameters                              #
#===============================================================================
         
         inputs <- readRDS(paste0(runDirectory, 'inputParameters/','inputParameters'))
         inputs <- transpose(inputs, keep.names = 'Input Parameter')
         
          writeData(outputsWkbk, 'inputParameters', inputs,
              startCol = 2, startRow = 2, colNames = TRUE)
        
#===============================================================================   
         
     
     
#===============================================================================
#                               Save Excel                                     #
#===============================================================================

     setColWidths(outputsWkbk, sheet = 'WholePopMeanOutcomes',
                   widths = 'auto', cols = c(1:1000))

     setColWidths(outputsWkbk, sheet = 'inputParameters',
                   widths = 'auto', cols = c(1:20))
    
     saveWorkbook(outputsWkbk, outputWkbkFilePath, overwrite = TRUE)
     
     shell.exec(outputWkbkFilePath)

#===============================================================================