#==============================================================================#
#==============================================================================#
#                                                                              #
#---                 Save at desired level of aggregation                   ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#
   
  # If running in cluster 
  
   if(interactive() == FALSE){
      
          iteration <- passedProfile
          
   }
   
   
  # Define aggregation function
   
   source(paste0(RProjectDirectory, "aggregationMasters/aggregationFunctions.R"))

 
   
  # Variations -----------------------------------------------------------------
     
    # If this is a variations run, assume that only a single population
    # outcome is wanted for each variation
     
     if(variations_Switch == 1){
       
       
       # Turn off all other aggregations
       
       lapply(ls(pattern='ggregat.*_Switch'), function(switch){
         assign(switch, 0, envir = globalenv()) 
       })
       
       
       # Create folder 
       
       variationsAggregatedOutputDirectory <- paste0(outputsDirectory, 'variationsOutputs/')
       
       
       # Identify grouping level
       
        byGrouping <- c('runDescription')
    
        
       # Run aggregating function
        
        aggregatedOutcomes <- aggregateSingleIterationOutcomes(individualOutcomes, 
                                                                byGrouping, 
                                                                'variationAggregation_w_PSA_SG_runDesc',
                                                                variationsAggregatedOutputDirectory)
     
      }
 
   
  # ----------------------------------------------------------------------------

   
   
   
  # Unaggregated outputs -------------------------------------------------------
  
    # Save all without any procesing 
     
      if(saveUnaggregated_Switch == 1){
        
         # Save intervention and cohort outcomes
    
          saveFile(filename = paste0(runDescription, '_iteration_', 
                                     iteration, '_costedIandCOutcomes'),
                       objectToSave = individualOutcomes,
                       savePath = unaggregatedOutputDirectory)
          
          print(paste0('Saved ', paste0(runDescription, '_iteration_', 
                                     iteration, '_costedIandCOutcomes'),
                       'in ', unaggregatedOutputDirectory))
          
        
        # Save IvsCOutcomes
    
          saveFile(filename = paste0(runDescription, '_iteration_', 
                                     iteration, 'costedIvsCOutcomes'),
                   objectToSave = individualOutcomes[interventionCohort == 1],
                   savePath = unaggregatedOutputDirectory)
          
           print(paste0('Saved ', paste0(runDescription, '_iteration_', 
                                     iteration, 'costedIvsCOutcomes'),
                       'in ', unaggregatedOutputDirectory))
          
      
       }
  
  # ----------------------------------------------------------------------------

  
  
  # Outputs aggregated by chunk size for tracing -------------------------------
        
    if (chunkSizeAggregation_Switch == 1){
      
      # Add chunk ID, variable depending on chunksize requested
       
        chunkNo = nrow(individualOutcomes)/(chunkSize_Switch*2)
        
        chunkIDs <- 
          rbindlist(lapply(1:chunkNo, function(no){ 
            data.table(chunkID = rep(no, nrow(individualOutcomes)/chunkNo))
            }))
      
        individualOutcomes <- cbind(individualOutcomes, chunkIDs)
        
        
        byGrouping = paste0("chunkID")
      
        aggregatedOutcomes <- aggregateSingleIterationOutcomes(individualOutcomes, 
                                                              byGrouping, 
                                                                'chunkAggregation',
                                                              chunkAggregatedOutputDirectory)
    }
  
  # ----------------------------------------------------------------------------
   
   
  
  # Save outputs aggregated at a personID level --------------------------------
  
    if(aggregatePersonID_Switch == 1){
          
         
        byGrouping <- paste0("personID")
  
        aggregatedOutcomes <- aggregateSingleIterationOutcomes(individualOutcomes, 
                                                              byGrouping,
                                                              'personIDaggregation',
                                                              personIDaggregatedOutputDirectory)
        
      }
    
   
  # ----------------------------------------------------------------------------
   
   
   
  # Save outputs aggregated at a seedGroup level -------------------------------
  
   # Gives outcome for person 'type'
   
    if(aggregateSeedGroup_Switch == 1){
          
         
        byGrouping <- paste0("seedGroup")
  
        aggregatedOutcomes <- aggregateSingleIterationOutcomes(individualOutcomes, 
                                                              byGrouping,
                                                              'seedGroupaggregation',
                                                              seedGroupaggregatedOutputDirectory)
        
      }
    
   
  # ----------------------------------------------------------------------------
   
   
   
  # Save outputs aggregated at a seedGroup and parameter set level -------------
  
   # Gives outcome for person 'type' for each parameter set
   
    if(aggregateSeedGroupPSAID_Switch == 1){
          
         
        byGrouping <- c("seedGroup", "parameterSetID")
  
        aggregatedOutcomes <- aggregateSingleIterationOutcomes(individualOutcomes, 
                                                              byGrouping,
                                                              'seedGroupaggregation',
                                                              seedGroupPSAIDaggregatedOutputDirectory)
        
      }
    
   
  # ----------------------------------------------------------------------------
   
   
   
   
  # Save outputs collapsed across population    --------------------------------
  
    if(aggregateAllExcludingVariation_Switch == 1){
          
         
        byGrouping <- paste0("runDescription")
  
        aggregatedOutcomes <- aggregateSingleIterationOutcomes(individualOutcomes, 
                                                              byGrouping, 
                                                              'fullyCollapsedPop',
                                                              singleAggregatedOutputDirectory)
        
      }
    
   
  # ----------------------------------------------------------------------------
   
   
  
  
   