#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Relapsing-remitting Multiple Sclerosis Microsimulation Model       ---#
#          Specification of missing population runs and processing             #
#                                                                              #
#==============================================================================#  
#==============================================================================#


#===============================================================================
#                              Load libraries                                  #
#===============================================================================

    # Load libraries

      library(Rcpp)         # --
      library(tidyverse)    # -- General helper functions      
      library(plyr)         # --

      library(openxlsx)     # Reading in from excel
      library(data.table)   # Providing big data type containers for population
      library(doParallel)   # Providing capability for parallelising processes
      
                            # --
      library(stringr)      # -- String processing functions
      library(stringi)      # --
      
      library(dqrng)        # Random numbers - 64-bit general purpose
      library(dirmult)      # Random numbers - dirichlet distributions

      options(scipen=999)


#==============================================================================#



#===============================================================================
#       Set overall project, R project directory and define arguments          #
#===============================================================================


  # Project directories --
      
      rootDirectory <- "D:/QMULOneDrive/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting_optimised/")
      
      
  # Specific run directory location and description --

      runDirectoryDescription <- readline('Please enter run directory description')
      runDescription <- readline('Please enter run description (probably same as above)')
      runsDirectory <- paste0(rootDirectory, 'modelRuns/')
    
  # ---- # 
    
 
#==============================================================================#
    


#===============================================================================
#               Define directories to source, gather, and output               #
#===============================================================================


  # Define all run non-specific locations

    source(paste0(RProjectDirectory, 'universalCode/', 'defineDirectoryLocations.R'))

  # ---- #


  # Specific run directory (needs a 'run directory description' to be defined) --

    runDirectory <- paste0(runsDirectory, runDirectoryDescription, '/')

  # ---- #


#==============================================================================#



#===============================================================================
#                     Define some generic functions                            #
#===============================================================================

    source(paste0(codeDirectory, "defineUtilityFunctions.R"))

    source(paste0(codeDirectory,
                  "defineStatisticalFunctions.R"))

    source(paste0(codeDirectory, 'postMicrosimulationProcesses/',
                  "meltMicrosimulationOutcome.R"))

#==============================================================================#

 
    
#===============================================================================
#                              Load inputs                                     #
#===============================================================================
#                                                                              #
# This uses the same process as has been coded into the microsimulation to     #
# initialise a population, so although in principle we are only interested in  #
# generating the cohorts to be run, the whole population initialisation        #
# process needs to be run to do that, which requires access to the run         #
# non-specific inputs as well as the run-specific components; these are all    #
# loaded here.                                                                 #
#                                                                              #
#------------------------------------------------------------------------------#

  # Run-specific components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_runControls.R"))

  # Universal components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_universal.R"))

  # Project-specific components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_ADA.R"))

  # Sense check inputs
    source(paste0(microsimulationInputProcessesDirectory, "inputs_check.R"))

  # Undertake some generic processing of inputs - e.g. PSA parameterisation
   # source(paste0(microsimulationInputProcessesDirectory, "inputs_prepare.R"))
    
  # Load cost-utility inputs
    source(paste0(costUtilityCodeDirectory, 'loadCostUtilityInputs.R'))    


#===============================================================================

    
    missingIterations <- readRDS(paste0(runDirectory, 'missingIters'))
    missingIterations <- as.vector(unlist(missingIterations))

    numRuns <- length(missingIterations)
    filename <- paste0(runDirectory, 'missingIterationJob.sh')
    
    file.create(filename)

    # Open connection to file - 'wb' opens binary connection to create unix
    # carriage returns
    
      fileConn <- file(filename, "wb")

      
    # Add instructions
    
      writeLines(paste0("#!/bin/bash" , "\n",
                        "#$ -pe smp 4", "\n",
                        "#$ -l h_vmem=8G", "\n",
                        "#$ -l h_rt=1:0:0", "\n",
                        "#$ -wd /data/scratch/wpw004/", runDirectoryDescription, "/", "\n",
                        "#$ -o /data/scratch/wpw004/", runDirectoryDescription, "_missingIterOutputFiles/", "\n",
                        "#$ -j y", "\n",
                        "#$ -t ", 1, "-",numRuns, " \n", "\n",
                        "INPUT_ARGS=$(sed -n ",  '"', "${SGE_TASK_ID}p", '"', " missingIterationArgs.txt)", "\n",
                        "module load R/4.2.2 ", "\n", 
                        "Rscript /data/home/wpw004/NIHR245601MSADA/MSMicrosimulationModel_withADATesting_optimised/",
                        "lifeCourseMasters/generateLifeCoursesMaster.R ", "$INPUT_ARGS"),
                 fileConn)

    # Close 
    
      close(fileConn)
    
      
      
      filename <-  filename <- paste0(runDirectory, 'missingIterationArgs.txt')
    
      
    # Open connection to file - 'wb' opens binary connection to create unix
    # carriage returns
    
      fileConn <- file(filename, "wb")

      
    # Add instructions

      txtFile <- paste0() 
     
     for (i in missingIterations){
        txtFile <- paste0(txtFile, i, " ", runDirectoryDescription, " ",
                        runDirectoryDescription, " ", "0", "\n")
        
     }
     
      writeLines(txtFile,
                 fileConn)
      
      close(fileConn)
    
     
    