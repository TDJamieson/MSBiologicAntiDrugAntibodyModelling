#==============================================================================#
#==============================================================================#
#                                                                              #
#---                        Aggregation functions                              #
#                                                                              #
#==============================================================================#  
#==============================================================================#


# Aggregated single iteration outcomes -----------------------------------------

  aggregateSingleIterationOutcomes <- function(DT, byGrouping, aggregationDescription, 
                                               outputDirectory){
 
      # Create Filename root
    
        filename <- paste0(runDescription, '_iteration_', iteration, aggregationDescription)
 
 
      # Generate weight
   
          DT[, weight := .N, 
             by = c(eval(byGrouping), 'interventionCohort')]
        
          DT[, c(meanFieldNames) := lapply(.SD, mean, na.rm = TRUE),
                     .SDcols = outcomeFieldNames,
             by = c(eval(byGrouping), 'interventionCohort')]
  
          DT[, c(sdFieldNames) := lapply(.SD, sd, na.rm = TRUE),
                     .SDcols = outcomeFieldNames,
             by = c(eval(byGrouping), 'interventionCohort')]
  
          DT[, c(varFieldNames) := lapply(.SD, var, na.rm = TRUE),
                     .SDcols = outcomeFieldNames,
             by = c(eval(byGrouping), 'interventionCohort')]

          
      # Keep first instance by grouping and intervention
      
         IandC <- DT[, .SD[1], by = c(eval(byGrouping), 'interventionCohort')]
         
      
      # Save I and C outcomes before creating IvsC outcomes
    
          
        saveFile(filename = paste0(filename, 'IandCOutcomes'),
                 objectToSave = IandC[, c('runDescription', 'interventionCohort', 'interventionID', 'onsetAge', 
                                                   'onsetEDSS', 'femaleGender', 'startingAge', 'seedGroup',
                                                   'personID', 'parameterSetID', ..meanFieldNames, ..sdFieldNames,
                                                   ..varFieldNames, 'meanADATestingCycle', 'meanADAsTested', 
                                                    'numberADAsTested', 'weight')],
                 savePath = outputDirectory)
        
         
  
          
      # Save IvsC outcomes 
        iVcFieldNames <- colnames(DT)[grepl('iVc', colnames(DT))]
        DT <- DT[interventionCohort == 1, .SD[1], by = c(eval(byGrouping))]
        
        DT <- DT[, c('runDescription', 'interventionCohort', 'interventionID', 'onsetAge', 
                     'onsetEDSS', 'femaleGender', 'startingAge', 'seedGroup',
                     'personID', 'parameterSetID', ..iVcFieldNames, 
                     'meanADATestingCycle', 'meanADAsTested', 
                      'numberADAsTested', 'weight')]
        
        DT[, iteration := iteration]
                                    
                       
      # Save IvC outcomes
       
        saveFile(filename = paste0(filename, 'IvsCOutcomes'),
                 objectToSave = DT[interventionCohort == 1],
                 savePath = outputDirectory)
        
        print(paste0('Outcomes saved in ', outputDirectory, 'called ', filename))
    
    
  }
  
# ------------------------------------------------------------------------------
  
  
  
# Combine outputs folder -------------------------------------------------------
  
  
  combineFolder <- function(folder, byGrouping, outputDirectory, aggregationDescription){
     
 
  # Read files
      
      folderFiles <- list.files(folder)
      IVCFiles <- folderFiles[grepl('IvsC', folderFiles)]
      IandCFiles <- folderFiles[grepl('IandC', folderFiles)]
  
      
   # Load input populations that should have run
    
      iterations <- readRDS(paste0(runDirectory, 'inputPopulations'))

      
  # Check all expected populations are included
    print(paste0('variationsSwitch is ', variations_Switch))
    
      if(variations_Switch == 1) {
      
        if(length(IVCFiles) != nrow(iterations)) {
          
          print(paste0('Runs: ', length(IVCFiles)))
          print(paste0('Expected: ', nrow(iterations)))
          
          expectedIterations <- nrow(iterations)
          expectedIterations <- seq(1, expectedIterations, 1)
          
          completedIVCFiles <- as.data.table(IVCFiles)
          completedIandCFiles <- as.data.table(IandCFiles)
          
          completedIVCFiles[, iteration := parse_number(IVCFiles)]
          completedIVCFiles <- unique(completedIVCFiles[, iteration])
            
          completedIandCFiles[, iteration := parse_number(IandCFiles)]
          completedIandCFiles <- unique(completedIandCFiles[, iteration])

          missingIterations1 <- as.data.frame(setdiff(expectedIterations, completedIVCFiles))
          missingIterations2 <- as.data.frame(setdiff(expectedIterations, completedIandCFiles))
          missingIterations <- append(missingIterations1, missingIterations2)
          missingIterations <- unique(missingIterations)
          
          saveRDS(missingIterations, paste0(runDirectory, 'missingIters'))
                
          stop('Runs missing')
          
        }
        
         combinedIVC <- lapply(IVCFiles, function(file) {
           print(file)
          iterDT <- qread(paste0(folder, file))
        })
        
        combinedIVC <- rbindlist(combinedIVC)
        
      
        combinedIandC <- lapply(IandCFiles, function(file) {
          iterDT <- qread(paste0(folder, file))
        })
        
        combinedIandC <- rbindlist(combinedIandC)
      
      } else {
         
  
        
    # Combine IVC
       
        print(folder)
  
        combinedIVC <- lapply(IVCFiles, function(file) {
           print(file)
          iterDT <- qread(paste0(folder, file))
        })
        
        combinedIVC <- rbindlist(combinedIVC)
        
      
        combinedIandC <- lapply(IandCFiles, function(file) {
          iterDT <- qread(paste0(folder, file))
        })
        
        combinedIandC <- rbindlist(combinedIandC)
            
        #Presence of all seed groups and parameter set IDs
        IVCSeedGroupPSAs <- unique(combinedIVC[, .(seedGroup, parameterSetID)])
        IandCSeedGroupPSAs <- unique(combinedIandC[, .(seedGroup, parameterSetID)])
        inputSeedGroupPSAs <- unique(iterations[, .(seedGroup, parameterSetID)])
        
        test1 <- setdiff(IVCSeedGroupPSAs,IandCSeedGroupPSAs)
        test2 <-setdiff(IVCSeedGroupPSAs,inputSeedGroupPSAs)
        test3 <- setdiff(IandCSeedGroupPSAs,inputSeedGroupPSAs)
        
        if(nrow(test1) > 0 | nrow(test2) > 0 | nrow(test3) > 0){
          
          stop('Runs missing')
          print(paste0(test1, test2, test3))
          
        }
        
      }
      
      
    
    
       
    # Identify fields to produce statistics for
      
        meanFields <- colnames(combinedIVC)[grepl('mean', colnames(combinedIVC))]
        varFields <- colnames(combinedIVC)[grepl('var', colnames(combinedIVC))]
        sdFields <- colnames(combinedIVC)[grepl('sd', colnames(combinedIVC))]
        seFields <- sub('sd', 'se', sdFields)


        
    # Weighted fields
        
        weightedMeanFields <- paste0('weighted_', meanFields)
        weightedVarFields <- paste0('weighted_', varFields)

                
    # Aggregate - I vs C
      
         aggregationIVC <- copy(combinedIVC)
         
         
      # Population sizing
         
         aggregationIVC[, totalPop := sum(weight), by = c(eval(byGrouping))]
         aggregationIVC[, rootPop := sqrt(totalPop)]
         aggregationIVC[, totalPopWithDOF := totalPop - .N, by = c(eval(byGrouping))]
  
         
      # Mean
      
         aggregationIVC[, c(weightedMeanFields) := lapply(colnames(.SD), function(field) {
                                 
                                aggregationIVC[, (get(field)*weight)/totalPop]
                               
                              }), 
                      .SDcols = meanFields]
    
         
         aggregationIVC[, c(meanFields) := lapply(.SD, sum), 
                            .SDcols = weightedMeanFields,
                        by = c(eval(byGrouping))]
         
         
      # Var
         
        aggregationIVC[, c(weightedVarFields) := lapply(colnames(.SD), function(field) {
                                
                                aggregationIVC[, get(field)*(weight-1)]
                               
                              }), 
                      .SDcols = varFields]
        
        
        aggregationIVC[, c(varFields) := lapply(.SD, sum), 
                            .SDcols = weightedVarFields,
                        by = c(eval(byGrouping))]
        
        
        aggregationIVC[, c(varFields) := lapply(colnames(.SD), function(field) {
          
                        aggregationIVC[, get(field)/totalPopWithDOF]
                         
                      }), 
                        .SDcols = varFields]
         
      # SD 
        
        aggregationIVC[, c(sdFields) := lapply(.SD, sqrt),
                        .SDcols = varFields]
        
        
     # SE
          
        aggregationIVC[, c(seFields) := lapply(colnames(.SD), function(field) {
          
                        aggregationIVC[, get(field)/rootPop]
                         
                      }), 
                        .SDcols = sdFields]
        
    
      # Save
        
        aggregationIVC <- 
           aggregationIVC[, .SD[1], by = c(eval(byGrouping))]
         
         
        weightedFields <- colnames(aggregationIVC)[grepl('weighted', colnames(aggregationIVC))]
        aggregationIVC[,  c(weightedFields, varFields, sdFields) := NULL]
                
          
        saveFile(filename = paste0('aggregated_', runDescription, '_IVCOutcomesBy_',
                                   aggregationDescription),
                aggregationIVC, 
                outputDirectory)
        
        remove(aggregationIVC)
      
      
        
    #  Aggregate - I and C ----
      
        
     # Identify fields to produce statistics for
      
        meanFields <- colnames(combinedIandC)[grepl('mean', colnames(combinedIandC))]
        varFields <- colnames(combinedIandC)[grepl('var', colnames(combinedIandC))]
        sdFields <- colnames(combinedIandC)[grepl('sd', colnames(combinedIandC))]
        seFields <- sub('sd', 'se', sdFields)


        
    # Weighted fields
        
        weightedMeanFields <- paste0('weighted_', meanFields)
        weightedVarFields <- paste0('weighted_', varFields)
        
        aggregationI <- copy(combinedIandC[interventionCohort == 1])
        aggregationC <- copy(combinedIandC[interventionCohort == 0])
       
        aggregationIandC <- lapply(list(aggregationI, aggregationC),
               
            function(DT) {
             
 
          # Population sizing
               
               DT[, totalPop := sum(weight), by = c(eval(byGrouping))]
               DT[, rootPop := sqrt(totalPop)]
               DT[, totalPopWithDOF := totalPop - .N, by = c(eval(byGrouping))]
        
         
          # Mean
      
               DT[, c(weightedMeanFields) := lapply(colnames(.SD), function(field) {
                                      
                                      DT[, (get(field)*weight)/totalPop]
                                     
                                    }), 
                  .SDcols = meanFields]
          
               
               DT[, c(meanFields) := lapply(.SD, sum), 
                                  .SDcols = weightedMeanFields,
                              by = c(eval(byGrouping))]
               
               
          # Var
         
                DT[, c(weightedVarFields) := lapply(colnames(.SD), function(field) {
                                        
                                        DT[, get(field)*(weight-1)]
                                       
                                      }), 
                    .SDcols = varFields]
                
                
                DT[, c(varFields) := lapply(.SD, sum), 
                    .SDcols = weightedVarFields,
                   by = c(eval(byGrouping))]
                
                
                DT[, c(varFields) := lapply(colnames(.SD), function(field) {
                  
                                DT[, get(field)/totalPopWithDOF]
                                 
                              }), 
                  .SDcols = varFields]
                 
          # SD 
          
              DT[, c(sdFields) := lapply(.SD, sqrt),
                .SDcols = varFields]
              
              
          # SE
  
            DT[, c(seFields) := lapply(colnames(.SD), function(field) {
              
                            DT[, get(field)/rootPop]
                             
                          }), 
              .SDcols = sdFields]
            
      }) 
 
        aggregationIandC <- rbindlist(aggregationIandC)
        aggregationIandC[, c(weightedMeanFields, weightedVarFields, varFields, sdFields) := NULL]
        aggregationIandC <- aggregationIandC[, .SD[1], by = c(eval(byGrouping), 'interventionCohort')]
          
        saveFile(filename = paste0('aggregated_', runDescription, '_IandCOutcomesBy_', 
                                   aggregationDescription),
                aggregationIandC, 
                outputDirectory)
        
      
  }
  
# ------------------------------------------------------------------------------