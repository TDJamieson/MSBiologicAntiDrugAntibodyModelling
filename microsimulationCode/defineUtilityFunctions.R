#==============================================================================#
#==============================================================================#
#                                                                              #
#---         Relapsing-remitting Multiple Sclerosis Markov Model            ---#
#                                                                              #                         
#                          Utility functions                                   #
#                                                                              #
#==============================================================================#  
#==============================================================================#



#==============================================================================#
#  Define a function to check if a file has been saved and re-save if needed   #
#  as large numbers of simultaneous runs may clash accessing disk              #
#===============================================================================


saveFile <- function(filename, objectToSave, savePath){
  
  
  counter <<- 1    
  
  
  # Save - attempt 1
  
  qsave(x = objectToSave, file = paste0(savePath, filename))
  
  
  # If file name is not found in files list then following this then save, and 
  # increment counter - this is allowed to be attempted up to 5 times
  
  while(counter < 5) {
    
    
    # Read in files in directory
  
       checkFiles <- list.files(savePath)
       
    # If not in driectory then save
    
    if(length(intersect(checkFiles, filename))==0){
      
      qsave(x = objectToSave, file = paste0(savePath, filename))
      
    } 
    
    counter <<- counter + 1
    
  }
  
 remove(counter)
  
}


#===============================================================================



#==============================================================================#
#  Define a function to run all files in a folder for defining functions etc.  #
#===============================================================================

runAllInFolder <- function(folder) {
  
  files <- list.files(folder, full.names=TRUE)
  
  files <- files[grepl('\\.[rR]$', files, ignore.case = FALSE)]
  
  if(length(files) == 0){
    
    warning(paste0('No files found in ', folder))
    
  } else {
    
    lapply(files, source)
    
  }
  
}

#===============================================================================



#==============================================================================#
#               Check for missing files prior to aggregation                   #
#===============================================================================

missingCheck <- function(fileSuffix, inputPopulations, populationToAggregate){
  
  aggregationPop <- copy(populationToAggregate)
  
 
 # Construct filenames
  
    inputPopulations[, filename := paste0(runDescription, '__',
                                                'SG_', seedGroup, '_',
                                                'PSAID_', parameterSetID, '_',
                                                'personID_', startID, 
                                                '_to_', endID)]
    inputPopulations[, filename := gsub(" ", "", filename)]
    
    aggregationPop[, filename := paste0(runDescription, '__',
                                                'SG_', seedGroup, '_',
                                                'PSAID_', parameterSetID)]
  
  
  # Identify which filenames pertain to this aggregation population and are
  # expected to be present
    
    expectedFiles <- inputPopulations[, filename]
    expectedFiles <- expectedFiles[grepl(aggregationPop[, filename], expectedFiles)]
    expectedFiles <- paste0(expectedFiles, '_', fileSuffix)
    
    
  # Identify relevant files in directory
    
    folderFiles <- list.files(outputsDirectory)
    filesPresent <- folderFiles[grepl(fileSuffix, folderFiles)]
    
    filesPresent <- filesPresent[grepl(aggregationPop[, filename], filesPresent)]
    
  
  # Compare to identify any missing, save if so
    
    missingFiles <- expectedFiles[expectedFiles %in% filesPresent == FALSE]
    
    if(is_empty(missingFiles) == FALSE){
      
      
      print(missingFiles)
   
      missingFiles <- str_replace(missingFiles, paste0('_', fileSuffix), '')
      missing <- inputPopulations[filename %in% missingFiles]
 
      saveRDS(missing, paste0(aggregateOutputsDirectory, runDescription, '__',
                              'SG_', seedGroup, '_',
                              'PSAID_', parameterSetID,
                              '_missing'))
      
      stop(paste0('Attempt to aggregate ', aggregationPop[, filename],
                  ' failed due to missing runs - these have been saved.'))
      

    }
    
  
  # Otherwise proceed
    
    writeLines(paste0('Proceeding with aggregation of ', aggregationPop[, filename], ' ', fileSuffix))
  

}
