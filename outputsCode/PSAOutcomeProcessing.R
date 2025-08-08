#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Relapsing-remitting Multiple Sclerosis Microsimulation Model       ---#
#                Amalgamate PSA runs and produce outputs                       #
#                                                                              #
#==============================================================================#  
#==============================================================================#
 

#===============================================================================
#                        Define plotting functions                             #
#===============================================================================


    source(paste0(RProjectDirectory, 
                  'outputsCode/Graphical/defineCEPlaneFunction_parameterSetLevel.R'))    

    source(paste0(RProjectDirectory, 
                  'outputsCode/Graphical/PSAUncertaintyTraceFunction.R'))


#==============================================================================#


    outputWkbkFilePath <- 
      'D:/QMULOneDrive/NIHR245601MSADA/modelRuns/ProbabilisticOutputs/BC_PSAOutput.xlsx'


#===============================================================================
#                          Create outputs excel                                #
#===============================================================================

 # Create new and add worksheets
    
      outputsWkbk <- createWorkbook()
      
      addWorksheet(outputsWkbk, 'WholePopMeanOutcomes')
      addWorksheet(outputsWkbk, 'ProfileMeanOutcomes')

    
    # Create styles
    
      doubleBorder <- createStyle(border = 'bottom', borderStyle = 'double')
      
      wrapTextStyle <- createStyle(wrapText = TRUE, textDecoration = 'bold')
      
      boldStyle <- createStyle(textDecoration = 'bold')

#==============================================================================#
      
 
#===============================================================================
#    Read in outcomes from folders identified as containing PSA results        #
#===============================================================================

  # PSA runs need to be held in the 'model runs directory' and have 'PSA'
  # in their name.  The outputs must be in a folder inside each folder called
  # 'outputs_aggregated_CEOutcomes'
  
  
  # AggregatedMeanDifferences

    runFolders <- list.dirs(modelRunsDirectory, recursive = FALSE)
    
    PSAFolders <- runFolders[grepl('PSA', runFolders)]
    
    PSAOutputs <- 
      
      lapply(PSAFolders, function(folder) {
   
        files <- list.files(paste0(folder, '/outputs_aggregated_CEOutcomes/'))
          
        file <- files[grepl('popMeanDeltas', files)]
        
        print(file)
        try(
        readRDS(paste0(folder, '/outputs_aggregated_CEOutcomes/', file))
        )
        
        
      })
  
  
  # Create DT
    
     PSAOutputs <- rbindlist(PSAOutputs)
     
     
  # I vs C outcomes
     
   PSAIvCOutputs <- 
      
      lapply(PSAFolders, function(folder) {
   
        files <- list.files(paste0(folder, '/outputs_aggregated_CEOutcomes/'))
          
        file <- files[grepl('popIvCMeans', files)]
        
        print(file)
        try(
        readRDS(paste0(folder, '/outputs_aggregated_CEOutcomes/', file))
        )
        
        
      })
  
  
  # Create DT
    
     PSAIvCOutputs <- rbindlist(PSAIvCOutputs)
     
     
       
  # All deltas
     
   PSAAllDeltas <- 
      
      lapply(PSAFolders, function(folder) {
   
        files <- list.files(paste0(folder, '/outputs_aggregated_CEOutcomes/'))
          
        file <- files[grepl('allDeltas', files)]
        
        print(file)
        try(
        readRDS(paste0(folder, '/outputs_aggregated_CEOutcomes/', file))
        )
        
        
      })
  
  
  # Create DT
    
     PSAAllDeltas <- rbindlist(PSAAllDeltas)
     
     deltaCols <- colnames(PSAAllDeltas)[grepl('delta', colnames(PSAAllDeltas))]
     PSAAllDeltas <- PSAAllDeltas[, c('seedGroup', 'parameterSetID', 'onsetAge',
                                      'femaleGender', 'onsetEDSS', 'startingAge', 
                                      ..deltaCols)]
  
  
    
#===============================================================================

     
     
#===============================================================================
#               Add test cost passed in as an argument to each run             #
#===============================================================================

  
   PSAOutputs[, `:=` (meandiff_lifetime_TestCosts = testCost * meandiff_lifetime_TestCosts,
                          meandiff_lifetime_TestCosts_Discounted = testCost * meandiff_lifetime_TestCosts_Discounted)]  
      
   PSAOutputs[, `:=`(meandiff_lifetime_cycleCosts = meandiff_lifetime_cycleCosts + meandiff_lifetime_TestCosts,
                         meandiff_lifetime_cycleCosts_discounted = 
                         meandiff_lifetime_cycleCosts_discounted  + meandiff_lifetime_TestCosts_Discounted)]
    
   PSAOutputs[, `:=` (meandiff_cost_qaly = 
                            meandiff_lifetime_cycleCosts/meandiff_lifetime_utility,
                          meandiff_cost_qaly_discounted = 
                            meandiff_lifetime_cycleCosts_discounted/meandiff_lifetime_utility_discounted)]
   
  
   
   PSAIvCOutputs[, `:=` (mean_lifetime_TestCosts = testCost * mean_lifetime_TestCosts,
                          mean_lifetime_TestCosts_Discounted = testCost * mean_lifetime_TestCosts_Discounted)]  
      
   PSAIvCOutputs[, `:=`(mean_lifetime_cycleCosts = mean_lifetime_cycleCosts + mean_lifetime_TestCosts,
                         mean_lifetime_cycleCosts_discounted = 
                         mean_lifetime_cycleCosts_discounted  + mean_lifetime_TestCosts_Discounted)]
    
   PSAIvCOutputs[, `:=` (mean_cost_qaly = 
                            mean_lifetime_cycleCosts/mean_lifetime_utility,
                          mean_cost_qaly_discounted = 
                            mean_lifetime_cycleCosts_discounted/mean_lifetime_utility_discounted)]
   
   
   PSAAllDeltas[, `:=` (delta_lifetime_TestCosts = testCost * delta_lifetime_TestCosts,
                          delta_lifetime_TestCosts_Discounted = testCost * delta_lifetime_TestCosts_Discounted)]  
      
   PSAAllDeltas[, `:=`(delta_lifetime_cycleCosts = delta_lifetime_cycleCosts + delta_lifetime_TestCosts,
                         delta_lifetime_cycleCosts_discounted = 
                         delta_lifetime_cycleCosts_discounted  + delta_lifetime_TestCosts_Discounted)]
    
   PSAAllDeltas[, `:=` (delta_cost_qaly = 
                            delta_lifetime_cycleCosts/delta_lifetime_utility,
                          delta_cost_qaly_discounted = 
                            delta_lifetime_cycleCosts_discounted/delta_lifetime_utility_discounted)]
    
#===============================================================================

   
  
#===============================================================================
#           Generate and save CE plots - CE plane and CI traces                #
#===============================================================================
  
  # Identify no PSA sets to plot
   
    noPSASetsComplete <- max(PSAOutputs[, parameterSetID]) - min(PSAOutputs[, parameterSetID])+1
     
   
  # CE planes - discounted and discounted
  
    costQALYPlot_undiscounted <- createIncrementalCostQALYPlot_undiscounted(PSAOutputs) + theme_classic(base_size = 40)
    
    ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'CEPlane_undiscounted.png'),
           costQALYPlot_undiscounted)
    
     
    costQALYPlot_discounted <- createIncrementalCostQALYPlot_discounted(PSAOutputs) + theme_classic(base_size =18)
    
    ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'CEPlanediscounted.png'),
           costQALYPlot_discounted, width = 10, height = 8)
 
     
  # Uncertainty traces
   
  #----
  
     uncertaintyTrace_discountedQALYs <- createPSAUncertaintytrace(
       limits = seq(25, noPSASetsComplete, 25), 
       DT = PSAOutputs, 
       varName = 'meandiff_lifetime_utility_discounted', 
       varLabel = 'Difference in lifetime utility (QALYs) ', 
       plotTitle = 'Discounted QALYs', 
       deterministicVal = 0.0332,
       costPlot = 0)
     
     ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'PSAuncertainty_discountedQALYs.png'), width = 10, height = 10.1)
     
     
     uncertaintyTrace_undiscountedQALYs <- createPSAUncertaintytrace(
       limits = seq(25, noPSASetsComplete, 25), 
       DT = PSAOutputs, 
       varName = 'meandiff_lifetime_utility', 
       varLabel = 'Difference in lifetime utility (QALYs; undiscounted)', 
       plotTitle = 'Undiscounted QALYs', 
       deterministicVal = 0.0654, 
       costPlot = 0)
     
     ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'PSAuncertainty_undiscountedQALYs.png'), width = 10, height = 10.1)
     
    #----
     
     uncertaintyTrace_discountedCosts <- createPSAUncertaintytrace(
       limits = seq(25, noPSASetsComplete, 25), 
       DT = PSAOutputs, 
       varName = 'meandiff_lifetime_cycleCosts_discounted', 
       varLabel = 'Difference in lifetime costs (£)', 
       plotTitle = 'Discounted Costs', 
       deterministicVal = 1516,
       costPlot = 1)
     
     ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'PSAuncertainty_discountedCosts.png'), width = 10, height = 10.1)
    
      
      uncertaintyTrace_undiscountedCosts <- createPSAUncertaintytrace(
         limits = seq(25, noPSASetsComplete, 25), 
         DT = PSAOutputs, 
         varName = 'meandiff_lifetime_cycleCosts', 
         varLabel = 'Difference in lifetime costs (£; undiscounted)', 
         plotTitle = 'Undiscounted Costs', 
         deterministicVal = 2309,
         costPlot = 1)
     
      ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'PSAuncertainty_undiscountedCosts.png'), width = 10, height = 10.1)

      
    #---- 
     
     uncertaintyTrace_lifeYears <- createPSAUncertaintytrace(
       limits = seq(25, noPSASetsComplete, 25), 
       DT = PSAOutputs, 
       varName = 'meandiff_timeToDeath', 
       varLabel = 'Difference in life-years', 
       plotTitle = 'Life-years', 
       deterministicVal = 0.0228,
       costPlot = 0)
     
     ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'PSAuncertainty_lifeYears.png'), width = 10, height = 10.1)
    
     
    uncertaintyTrace_timeToSPMS <- createPSAUncertaintytrace(
       limits = seq(25, noPSASetsComplete, 25), 
       DT = PSAOutputs, 
       varName = 'meandiff_timeToSPMS', 
       varLabel = 'Difference in time to SPMS', 
       plotTitle = 'RRMS to SPMS transition', 
       deterministicVal = 0.0624,
       costPlot = 0)
     
     ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'PSAuncertainty_timeToSPMS.png'), width = 10, height = 10.1)
     
     
     uncertaintyTrace_relapses <- createPSAUncertaintytrace(
       limits = seq(25, noPSASetsComplete, 25), 
       DT = PSAOutputs, 
       varName = 'meandiff_totalRelapses', 
       varLabel = 'Difference in number of relapses', 
       plotTitle = 'Lifetime relapses', 
       deterministicVal = -0.0202,
       costPlot = 0)
     
     ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'PSAuncertainty_totalRelapses.png'), width = 10, height = 10.1)

     
     
      uncertaintyTrace_ICER <- createPSAUncertaintytrace(
       limits = seq(25, noPSASetsComplete, 25), 
       DT = PSAOutputs, 
       varName = 'meandiff_cost_qaly_discounted', 
       varLabel = 'Cost/QALY', 
       plotTitle = 'Incremental cost-effectiveness ratio', 
       deterministicVal = 45710,
       costPlot = 1)
     
     ggsave(paste0(modelRunsDirectory, '/ProbabilisticOutputs/', 'PSAuncertainty_ICER.png'), width = 10, height = 10.1)

      
    #---- 
     
     plots <- uncertaintyTrace_discountedQALYs + uncertaintyTrace_undiscountedQALYs +
              uncertaintyTrace_discountedCosts + uncertaintyTrace_undiscountedCosts +
              uncertaintyTrace_lifeYears + uncertaintyTrace_timeToSPMS + plot_layout(ncol = 2)
     
  
     
#===============================================================================

     
     
#===============================================================================
#                        Generate tabular output                               #
#===============================================================================
 
   diffFields <- colnames(PSAOutputs)[grepl('meandiff',colnames(PSAOutputs))]
   
                          
  outcomes <-
  as.data.table(rbindlist(

   lapply(diffFields, function(col) {
 
     mean = signif(PSAOutputs[, mean(get(col))],4)
     sd = signif(PSAOutputs[, sd(get(col))],3)
     CI_lower = mean-(1.96*sd)
     CI_higher = mean+(1.96*sd)

     data.table(var = col,
                mean = paste0(mean, ' (', sd, ')'),
                CI_lower = CI_lower,
                CI_higher = CI_higher)
      
   })
   
   ))
  
  
  outcomes[outputTables$popMean_outputsTable, 
           `:=` (Outcome = i.DifferenceOutcome, 
                 include = i.Include),
           on = .(var = DifferenceVariable)]
  
  outcomes <- outcomes[include == 1]
  outcomes[, c('var', 'include') := NULL]

    
  setcolorder(outcomes, 'Outcome')
  setnames(outcomes, c('mean', 'CI_lower', 'CI_higher'), 
              c('Mean (SE)', 'Lower CI', 'Higher CI'))
  
  
  meanFields <- colnames(PSAIvCOutputs)[grepl('mean_',colnames(PSAIvCOutputs))]
                          
  IOutcomes <-
  as.data.table(rbindlist(

   lapply(meanFields, function(col) {

     mean = signif(PSAIvCOutputs[interventionCohort == 1, mean(get(col))],4)
     sd = signif(PSAIvCOutputs[interventionCohort == 1, sd(get(col))],3)
     CI_lower = mean-(1.96*sd)
     CI_higher = mean+(1.96*sd)

     data.table(var = col,
                mean = paste0(mean, ' (', sd, ')'),
                CI_lower = CI_lower,
                CI_higher = CI_higher)
      
   })
   
   ))
  
  
  IOutcomes[outputTables$popMean_outputsTable, 
           `:=` (Outcome = i.MeanOutcome, 
                 include = i.Include),
           on = .(var = MeanVariable)]
  
  IOutcomes <- IOutcomes[include == 1]
  IOutcomes[, c('var', 'include') := NULL]

    
  setcolorder(IOutcomes, 'Outcome')
 
  
  
   COutcomes <-
  
     as.data.table(rbindlist(

       lapply(meanFields, function(col) {
 
       mean = signif(PSAIvCOutputs[interventionCohort == 0, mean(get(col))],4)
       sd = signif(PSAIvCOutputs[interventionCohort == 0, sd(get(col))],3)
       CI_lower = mean-(1.96*sd)
       CI_higher = mean+(1.96*sd)
  
       data.table(var = col,
                  mean = paste0(mean, ' (', sd, ')'),
                  CI_lower = CI_lower,
                  CI_higher = CI_higher)
        
   })
   
   ))
  
  
  COutcomes[outputTables$popMean_outputsTable, 
           `:=` (Outcome = i.MeanOutcome, 
                 include = i.Include),
           on = .(var = MeanVariable)]
  
  COutcomes <- COutcomes[include == 1]
  COutcomes[, c('var', 'include') := NULL]

    
  setcolorder(COutcomes, 'Outcome')
 
  

  allOutcomes <- copy(outcomes)
  allOutcomes[, Outcome := str_replace(Outcome, 'Difference in ', '')]

  allOutcomes[, Outcome := paste0(toupper(substr(Outcome, 1, 1)), substr(Outcome, 2, nchar(Outcome)))]

  
  allOutcomes <- allOutcomes[IOutcomes, Intervention := i.mean, on = .(Outcome)]
  allOutcomes <- allOutcomes[COutcomes, Comparator := i.mean, on = .(Outcome)]
  
  
   writeData(outputsWkbk, 'WholePopMeanOutcomes', allOutcomes,
              startCol = 2, startRow = 2, borders = 'surrounding',
              borderStyle = 'thick', colNames = TRUE)

  
  
  
#===============================================================================
#                            Individual Profile Outcomes                       #
#===============================================================================
 
    PSAAllDeltas[, meandiff_cost_qaly_discounted := 
                   delta_lifetime_cycleCosts_discounted/delta_lifetime_utility_discounted]
  
    numericFields <- colnames(PSAAllDeltas %>% select_if(is.numeric))
    numericFields <- numericFields[!grepl('seedGroup', numericFields)]
    
    PSAAllDeltasSDs <- copy(PSAAllDeltas)
    
    PSAAllDeltasSDs[, c(numericFields) := lapply(.SD, sd), .SDcols = numericFields,
                 by = .(seedGroup)]
    
    PSAAllDeltasSDs <- PSAAllDeltasSDs[, .SD[1], by = .(seedGroup)]
    

    PSAAllDeltas[, c(numericFields) := lapply(.SD, mean), .SDcols = numericFields,
                 by = .(seedGroup)]
    
    PSAAllDeltas <- PSAAllDeltas[,.SD[1], by = .(seedGroup)]
  
    PSAAllDeltas[, individualDescription := paste0(ifelse(femaleGender == 1,
                                                      'Female \n',
                                                      'Male \n'),
                                             'EDSS: ', as.integer(onsetEDSS), '\n',
                                             'MS onset: ', as.integer(onsetAge), '\n',
                                             'Age: ', as.integer(startingAge),  '\n',
                                             'PSA ID: ', parameterSetID)]
    
    setorder(PSAAllDeltas, onsetEDSS, startingAge, onsetAge)
  
  
     
  # Add mins and maxes to generate a range
    
    mins <- do.call(rbind,
                    lapply(numericFields, function(name)
                      {data.table(varDescription = name, Min = min(PSAAllDeltas[, get(name)]))})
    )

    maxes <- do.call(rbind,
                    lapply(numericFields, function(name)
                      {data.table(varDescription = name, Max = max(PSAAllDeltas[, get(name)]))})
    )


  # Format numbers

    costsCols <- colnames(PSAAllDeltas)[grepl('cost', colnames(PSAAllDeltas),
                                                    ignore.case = TRUE)]

    PSAAllDeltas[, c(costsCols) :=
                           lapply(.SD, function(col)
                               {paste0('\U00a3',
                                       as.character(formatC(as.numeric(col), digits = 2,
                                                     big.mark=",", format = 'f')))}),
                       .SDcols = costsCols]


    numericCols <- names(Filter(is.numeric, PSAAllDeltas))

    PSAAllDeltas[, c(numericCols) :=
                             lapply(.SD, function(col)
                               {as.character(formatC(as.numeric(col), digits = 4,
                                                     big.mark=",", format = 'f'))}),
                       .SDcols = numericCols]
    
    
  # SD
    
     costsCols <- colnames(PSAAllDeltasSDs)[grepl('cost', colnames(PSAAllDeltasSDs),
                                                    ignore.case = TRUE)]

    PSAAllDeltasSDs[, c(costsCols) :=
                           lapply(.SD, function(col)
                               {paste0('\U00a3',
                                       as.character(formatC(as.numeric(col), digits = 2,
                                                     big.mark=",", format = 'f')))}),
                       .SDcols = costsCols]


    numericCols <- names(Filter(is.numeric, PSAAllDeltasSDs))

    PSAAllDeltasSDs[, c(numericCols) :=
                             lapply(.SD, function(col)
                               {as.character(formatC(as.numeric(col), digits = 4,
                                                     big.mark=",", format = 'f'))}),
                       .SDcols = numericCols]
    
    
  # Combine mean and ses
    
    PSAAllDeltas <- PSAAllDeltas[PSAAllDeltasSDs[, c(..numericCols, ..costsCols)], on = .(seedGroup)]
    
    PSAAllDeltas[, c(numericFields) := lapply(numericFields, function(col){
      paste0(get(col), 
             ' (', 
             get(paste0('i.', col)), 
             ')')
    })]
    
    PSAAllDeltas[, colnames(PSAAllDeltas)[grepl('i\\.', colnames(PSAAllDeltas))] := NULL]


  # Transpose wide to long

    PSAAllDeltas <- data.table::transpose(PSAAllDeltas, keep.names = 'Outcome')


  # Merge in min and max and create range

    PSAAllDeltas[mins, Min := i.Min, on = .(Outcome = varDescription)]

    PSAAllDeltas[maxes, Max := i.Max, on = .(Outcome = varDescription)]

    PSAAllDeltas[grepl('cost', Outcome, ignore.case = TRUE),
                       minCosts := paste0('\U00a3', as.character(formatC(as.numeric(Min), digits = 2,
                                                     big.mark=",", format = 'f')))]
    PSAAllDeltas[grepl('cost', Outcome, ignore.case = TRUE),
                       maxCosts := paste0('\U00a3', as.character(formatC(as.numeric(Max), digits = 2,
                                                     big.mark=",", format = 'f')))]

    PSAAllDeltas[!grepl('cost', Outcome, ignore.case = TRUE),
                       minOther := as.character(formatC(as.numeric(Min), digits = 4,
                                                     big.mark=",", format = 'f'))]
    PSAAllDeltas[!grepl('cost', Outcome, ignore.case = TRUE),
                       maxOther := as.character(formatC(as.numeric(Max), digits = 4,
                                                     big.mark=",", format = 'f'))]

    PSAAllDeltas[grepl('cost', Outcome, ignore.case = TRUE),
                       Range := paste0(minCosts, ' - ', maxCosts)]
    PSAAllDeltas[!grepl('cost', Outcome, ignore.case = TRUE),
                       Range := paste0(minOther, ' - ', maxOther)]

    PSAAllDeltas[grepl('NA', Range), Range := ' - ']
    PSAAllDeltas[, c('Min', 'minCosts', 'minOther', 'Max',
                           'maxCosts', 'maxOther') := NULL]



  # Merge in variable descriptions and ordering and inclusion

    PSAAllDeltas[, Outcome := sub('delta', 'meandiff', Outcome)]
      PSAAllDeltas[popMean_outputsTable,
                         `:=` (Outcome = i.DifferenceOutcome,
                                Include = i.Include,
                                Order = i.TableRowOrder),
                  on = .(Outcome = DifferenceVariable)]

      
  # Order and remove fields not marked for inclusion in the input
  # spreadsheet (Keep range)

      PSAAllDeltas <-
         PSAAllDeltas[Include == 1 | Outcome == 'individualDescription']
       PSAAllDeltas[, Include := NULL]
       setorder(PSAAllDeltas, Order)

       PSAAllDeltas[, Order := NULL]


  # Add description for Range column in row 1

       PSAAllDeltas[1, Range := 'Population Range']
       
       setcolorder(PSAAllDeltas, c('Outcome', 'Range'))


  # Add to workbook 
  
        writeData(outputsWkbk, 'ProfileMeanOutcomes', PSAAllDeltas,
              startCol = 2, startRow = 2, colNames = FALSE)
    
        
        addStyle(outputsWkbk, 'ProfileMeanOutcomes', doubleBorder,
                 rows = 1,
                 cols = c(2:(ncol(PSAAllDeltas)+1)))
        
        addStyle(outputsWkbk, 'ProfileMeanOutcomes', doubleBorder,
                 rows = nrow(PSAAllDeltas)+1,
                 cols = c(2:(ncol(PSAAllDeltas)+1)))
        
        setRowHeights(outputsWkbk, "ProfileMeanOutcomes", rows = 2, height = 80)
        
        setColWidths(outputsWkbk, "ProfileMeanOutcomes", cols = c(1:10000), width = 12)
     
        addStyle(outputsWkbk, sheet = 'ProfileMeanOutcomes', wrapTextStyle,
               rows = 2, cols = c(1:10000))
        
         addStyle(outputsWkbk, 'ProfileMeanOutcomes', doubleBorder,
                 rows = 2,
                 cols = c(2:(ncol(PSAAllDeltas)+1)))
    
    
#===============================================================================
         
         

#===============================================================================
#                               Save Excel                                     #
#===============================================================================

     setColWidths(outputsWkbk, sheet = 'WholePopMeanOutcomes',
                   widths = 'auto', cols = c(1:20))

     saveWorkbook(outputsWkbk, outputWkbkFilePath, overwrite = TRUE)

#===============================================================================
  