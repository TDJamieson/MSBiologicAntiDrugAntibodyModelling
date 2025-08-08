#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Relapsing-remitting Multiple Sclerosis Microsimulation Model       ---#
#           Produce outputs from completed runs where no varying               #
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
      addWorksheet(outputsWkbk, 'ProfileMeanOutcomes')
      addWorksheet(outputsWkbk, 'inputParameters')
    
    
    # Create styles
    
      doubleBorder <- createStyle(border = 'bottom', borderStyle = 'double')
      
      wrapTextStyle <- createStyle(wrapText = TRUE, textDecoration = 'bold')
      
      boldStyle <- createStyle(textDecoration = 'bold')

    
#===============================================================================

 
  

#===============================================================================
#                      Whole Population Mean Outcomes                          #
#===============================================================================

  # Transpose

    popMeanDeltas <- transpose(popMeanDeltas, keep.names = 'Variable')


  # Add col names with PSA ID if appropriate

    valueCols <- colnames(popMeanDeltas)[!grepl('Variable',
                                                colnames(popMeanDeltas))]


    if(ncol(popMeanDeltas) == 2) {

      setnames(popMeanDeltas, valueCols, 'Difference')


    } else {

      PSAIDs <- popMeanDeltas[Variable == 'parameterSetID', c(..valueCols)]

      setnames(popMeanDeltas, valueCols, paste0('Difference ', '(PSA ', PSAIDs, ')'))

    }
 
  
  # Merge in Outcome descriptions from supporting excel; exclude outcomes not
  # wanted as identified by 'Include' column in spreadsheet and use rowOrder
  # from spreadsheet to set order

    popMeanDeltas[popMean_outputsTable, `:=` (Outcome = i.DifferenceOutcome,
                                              Include = i.Include,
                                              Order = i.TableRowOrder),
                  on = .(Variable = DifferenceVariable)]

    popMeanDeltas <- popMeanDeltas[Include == 1]
    setorder(popMeanDeltas, Order)
    popMeanDeltas[, `:=`(Order = NULL,
                         Include = NULL,
                         Variable = NULL)]


   # Tidy formatting of numeric fields

    nonOutcomeCols <- colnames(popMeanDeltas)[!grepl('Outcome',
                                                     colnames(popMeanDeltas))]

    popMeanDeltas[grepl('cost', Outcome, ignore.case = TRUE),
                  c(nonOutcomeCols) := lapply(.SD, function(col) {
                    paste0('\U00a3', format(round(as.numeric(col), 2), 
                                            big.mark = ','))}),
                    .SDcols = nonOutcomeCols]

    popMeanDeltas[!grepl('cost', Outcome, ignore.case = TRUE),
                 c(nonOutcomeCols) := lapply(.SD, function(col) {
                   paste0(round(as.numeric(col), digits = 4))}),
                 .SDcols = nonOutcomeCols]

    setcolorder(popMeanDeltas, 'Outcome')



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
            rows = nrow(popMeanDeltas)+2,
            cols = c(2:(2+ncol(popMeanDeltas)-1)))
   
   
#===============================================================================



#===============================================================================
#              Intervention Vs Comparator Population Mean Outcomes             #
#===============================================================================

 
  # Transpose and name columns (with PSA ID if appropriate)

    IvCMeanDeltas <- transpose(IvCMeanDeltas, keep.names = 'Variable')

    valueCols <- colnames(IvCMeanDeltas)[!grepl('Variable',
                                                colnames(IvCMeanDeltas))]

    IorCs <- IvCMeanDeltas[Variable == 'interventionCohort', c(..valueCols)]



    if(ncol(IvCMeanDeltas) == 3) {

      cols <- paste0(IorCs)

      setnames(IvCMeanDeltas, valueCols, cols)


    } else {

      PSAIDs <- IvCMeanDeltas[Variable == 'parameterSetID', c(..valueCols)]

      cols <- paste0(IorCs, ' (PSA ', PSAIDs, ')')

      setnames(IvCMeanDeltas, valueCols, cols)

    }
 
    
  # Merge in Outcome descriptions from supporting excel; exclude outcomes not
  # wanted as identified by 'Include' column in spreadsheet and use rowOrder
  # from spreadsheet to set order

    IvCMeanDeltas[popMean_outputsTable, `:=` (Outcome = i.MeanOutcome,
                                              Include = i.Include,
                                              Order = i.TableRowOrder),
                  on = .(Variable = MeanVariable)]

    IvCMeanDeltas <- IvCMeanDeltas[Include == 1]
    setorder(IvCMeanDeltas, Order)

    IvCMeanDeltas[, `:=`(Order = NULL,
                         Include = NULL,
                         Variable = NULL)]


 # Tidy formatting of numeric fields

    nonOutcomeCols <- colnames(IvCMeanDeltas)[!grepl('Outcome',
                                                     colnames(IvCMeanDeltas))]

    IvCMeanDeltas[grepl('cost', Outcome, ignore.case = TRUE),
                  c(nonOutcomeCols) := lapply(.SD, function(col) {
                    paste0('\U00a3', format(round(as.numeric(col), 2), 
                                            big.mark = ','))}),
                    .SDcols = nonOutcomeCols]

    IvCMeanDeltas[!grepl('cost', Outcome, ignore.case = TRUE),
                 c(nonOutcomeCols) := lapply(.SD, function(col) {
                   paste0(round(as.numeric(col), digits = 4))}),
                 .SDcols = nonOutcomeCols]

    setcolorder(IvCMeanDeltas, c('Outcome', 'Intervention', 'Comparator'))

    
  # Add to excel

     writeData(outputsWkbk, 'WholePopMeanOutcomes', IvCMeanDeltas,
               startCol = 6, startRow = 2, colNames = TRUE)
    
     addStyle(outputsWkbk, 'WholePopMeanOutcomes', doubleBorder,
                 rows = 1,
                 cols = c(6:(6+ncol(IvCMeanDeltas)-1)))

     addStyle(outputsWkbk, 'WholePopMeanOutcomes', doubleBorder,
             rows = 2,
             cols = c(6:(6+ncol(IvCMeanDeltas)-1)))

     addStyle(outputsWkbk, 'WholePopMeanOutcomes', doubleBorder,
              rows = nrow(IvCMeanDeltas)+2,
              cols = c(6:(6+ncol(IvCMeanDeltas)-1)))
     
     
#===============================================================================

   
    
#===============================================================================
#                   Merge together IvC and Mean Outcomes                       #
#===============================================================================
 
 
  # Merge
   
    popMeanDeltas[, joinOutcome := tolower(sub('Difference in ', '', Outcome))]
    IvCMeanDeltas[Outcome == 'Cost Per QALY (discounted)', 
                  `:=` (Outcome = 'Incremental Cost per QALY (discounted)',
                        Intervention = '  -  ',
                        Comparator = '  -  ')]
            
    popMeanDeltas[, Outcome := NULL]
    IvCMeanDeltas[, joinOutcome := tolower(Outcome)]
    
    joinedOutcomes <- IvCMeanDeltas[popMeanDeltas, on = .(joinOutcome)]
    joinedOutcomes[, joinOutcome := NULL]

  
  # Add to excel 
  
    writeData(outputsWkbk, 'WholePopMeanOutcomes', joinedOutcomes,
              startCol = 11, startRow = 2, colNames = TRUE)
 
    addStyle(outputsWkbk, 'WholePopMeanOutcomes', doubleBorder,
               rows = 1,
               cols = c(11:(11+ncol(joinedOutcomes)-1)))

    addStyle(outputsWkbk, 'WholePopMeanOutcomes', doubleBorder,
           rows = 2,
           cols = c(11:(11+ncol(joinedOutcomes)-1)))

    addStyle(outputsWkbk, 'WholePopMeanOutcomes', doubleBorder,
            rows = nrow(joinedOutcomes)+2,
            cols = c(11:(11+ncol(joinedOutcomes)-1)))
    

    
#===============================================================================   
    
    
    
#===============================================================================
#                            Individual Profile Outcomes                       #
#===============================================================================
 
    allDeltas[, individualDescription := paste0(ifelse(femaleGender == 1,
                                                      'Female \n',
                                                      'Male \n'),
                                             'EDSS: ', as.integer(onsetEDSS), '\n',
                                             'MS onset: ', as.integer(onsetAge), '\n',
                                             'Age: ', as.integer(startingAge),  '\n',
                                             'PSA ID: ', parameterSetID)]
    
    setorder(allDeltas, onsetEDSS, startingAge, onsetAge)
  
  
     
  # Add mins and maxes to generate a range
    
    numericFields <- colnames(allDeltas %>% select_if(is.numeric))
    
    mins <- do.call(rbind,
                    lapply(numericFields, function(name)
                      {data.table(varDescription = name, Min = min(allDeltas[, get(name)]))})
    )

    maxes <- do.call(rbind,
                    lapply(numericFields, function(name)
                      {data.table(varDescription = name, Max = max(allDeltas[, get(name)]))})
    )


  # Format numbers

    costsCols <- colnames(allDeltas)[grepl('cost', colnames(allDeltas),
                                                    ignore.case = TRUE)]

    allDeltas[, c(costsCols) :=
                           lapply(.SD, function(col)
                               {paste0('\U00a3',
                                       as.character(formatC(as.numeric(col), digits = 2,
                                                     big.mark=",", format = 'f')))}),
                       .SDcols = costsCols]


    numericCols <- names(Filter(is.numeric, allDeltas))

    allDeltas[, c(numericCols) :=
                             lapply(.SD, function(col)
                               {as.character(formatC(as.numeric(col), digits = 4,
                                                     big.mark=",", format = 'f'))}),
                       .SDcols = numericCols]


  # Transpose wide to long

    allDeltas <- data.table::transpose(allDeltas, keep.names = 'Outcome')


  # Merge in min and max and create range

    allDeltas[mins, Min := i.Min, on = .(Outcome = varDescription)]

    allDeltas[maxes, Max := i.Max, on = .(Outcome = varDescription)]

    allDeltas[grepl('cost', Outcome, ignore.case = TRUE),
                       minCosts := paste0('\U00a3', as.character(formatC(as.numeric(Min), digits = 2,
                                                     big.mark=",", format = 'f')))]
    allDeltas[grepl('cost', Outcome, ignore.case = TRUE),
                       maxCosts := paste0('\U00a3', as.character(formatC(as.numeric(Max), digits = 2,
                                                     big.mark=",", format = 'f')))]

    allDeltas[!grepl('cost', Outcome, ignore.case = TRUE),
                       minOther := as.character(formatC(as.numeric(Min), digits = 4,
                                                     big.mark=",", format = 'f'))]
    allDeltas[!grepl('cost', Outcome, ignore.case = TRUE),
                       maxOther := as.character(formatC(as.numeric(Max), digits = 4,
                                                     big.mark=",", format = 'f'))]

    allDeltas[grepl('cost', Outcome, ignore.case = TRUE),
                       Range := paste0(minCosts, ' - ', maxCosts)]
    allDeltas[!grepl('cost', Outcome, ignore.case = TRUE),
                       Range := paste0(minOther, ' - ', maxOther)]

    allDeltas[grepl('NA', Range), Range := ' - ']
    allDeltas[, c('Min', 'minCosts', 'minOther', 'Max',
                           'maxCosts', 'maxOther') := NULL]



  # Merge in variable descriptions and ordering and inclusion

    allDeltas[, Outcome := sub('delta', 'meandiff', Outcome)]
      allDeltas[popMean_outputsTable,
                         `:=` (Outcome = i.DifferenceOutcome,
                                Include = i.Include,
                                Order = i.TableRowOrder),
                  on = .(Outcome = DifferenceVariable)]

      
  # Order and remove fields not marked for inclusion in the input
  # spreadsheet (Keep range)

      allDeltas <-
         allDeltas[Include == 1 | Outcome == 'individualDescription']
       allDeltas[, Include := NULL]
       setorder(allDeltas, Order)

       allDeltas[, Order := NULL]


  # Add description for Range column in row 1

       allDeltas[1, Range := 'Population Range']
       
       setcolorder(allDeltas, c('Outcome', 'Range'))


  # Add to workbook 
  
        writeData(outputsWkbk, 'ProfileMeanOutcomes', allDeltas,
              startCol = 2, startRow = 2, colNames = FALSE)
    
        
        addStyle(outputsWkbk, 'ProfileMeanOutcomes', doubleBorder,
                 rows = 1,
                 cols = c(2:(ncol(allDeltas)+1)))
        
        addStyle(outputsWkbk, 'ProfileMeanOutcomes', doubleBorder,
                 rows = nrow(allDeltas)+1,
                 cols = c(2:(ncol(allDeltas)+1)))
        
        setRowHeights(outputsWkbk, "ProfileMeanOutcomes", rows = 2, height = 80)
        
        setColWidths(outputsWkbk, "ProfileMeanOutcomes", cols = c(1:10000), width = 12)
     
        addStyle(outputsWkbk, sheet = 'ProfileMeanOutcomes', wrapTextStyle,
               rows = 2, cols = c(1:10000))
        
         addStyle(outputsWkbk, 'ProfileMeanOutcomes', doubleBorder,
                 rows = 2,
                 cols = c(2:(ncol(allDeltas)+1)))
    
    
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
                   widths = 'auto', cols = c(1:20))
          
     setColWidths(outputsWkbk, sheet = 'inputParameters',
                   widths = 'auto', cols = c(1:20))
     
     setColWidths(outputsWkbk, sheet = 'ProfileMeanOutcomes',
                   widths = 'auto', cols = c(2:3))

     saveWorkbook(outputsWkbk, outputWkbkFilePath, overwrite = TRUE)

     shell.exec(outputWkbkFilePath)

#===============================================================================