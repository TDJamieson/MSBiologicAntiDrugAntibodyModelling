#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Relapsing-remitting Multiple Sclerosis Microsimulation Model       ---#
#                   Apocrita instruction files generation                      #
#                                                                              #
#==============================================================================#  
#==============================================================================#


    numApocritaInstructions <- seq(0, lifeCourseIterations, 74999)
    remaining <- lifeCourseIterations - max(numApocritaInstructions)
    apocritaInstructionSequence <- append(numApocritaInstructions, 
                                          max(numApocritaInstructions) + remaining)
    
    apocritaInstructionSequence[apocritaInstructionSequence == 0] <- 1

    filenameCSV <- data.table(file = '')
    
  # Microsimulation of life courses ----
  
    for (i in 1:(length(apocritaInstructionSequence)-1)){
    
    filename <- paste0(runDirectory, 
                       'task1_generateLifeCourseMasters_', runDirectoryDescription, '_',
                       apocritaInstructionSequence[i], '_', apocritaInstructionSequence[i+1], '.sh')

    file.create(filename)

    filenameForCsv <- data.table(file = paste0('qsub ', 'task1_generateLifeCourseMasters_', runDirectoryDescription, '_',
                       apocritaInstructionSequence[i], '_', apocritaInstructionSequence[i+1], '.sh'))
    filenameCSV <- rbind(filenameCSV, filenameForCsv )
    
    # Open connection to file - 'wb' opens binary connection to create unix
    # carriage returns
    
      fileConn <- file(filename, "wb")

      
    # Add instructions
    
      writeLines(paste0("#!/bin/bash" , "\n",
                        "#$ -pe smp 1", "\n",
                        "#$ -l h_vmem=24G", "\n",
                        "#$ -l h_rt=1:0:0", "\n",
                        "#$ -wd /data/scratch/wpw004/", runDirectoryDescription, "/", "\n",
                        "#$ -o /data/scratch/wpw004/", runDirectoryDescription, "_OutputFiles/", "\n",
                        "#$ -j y", "\n",
                        "#$ -t ", apocritaInstructionSequence[i], "-", apocritaInstructionSequence[i+1], " \n", "\n",
                        "echo ${SGE_TASK_ID}", "\n", "\n",
                        "module load R/4.2.2", "\n", "\n",
                        "Rscript /data/home/wpw004/NIHR245601MSADA/MSMicrosimulationModel_withADATesting_optimised/",
                        "lifeCourseMasters/generateLifeCoursesMaster.R  ${SGE_TASK_ID} ", runDirectoryDescription, " ",
                        runDirectoryDescription, " ", "0"), 
                 fileConn)

    # Close 
    
      close(fileConn)
    }
    
    write_csv(filenameCSV, paste0(runDirectory, 'qsub_filenames_forpasting.csv'))
    
  # ----------------------------------------------------------------------------
  
      
      
  # Aggregation across populations ----
  
    filename <- paste0(runDirectory, 
                       'task2_aggregateLifeCourses_', runDirectoryDescription, '.sh')

    file.create(filename)

    
    # Open connection to file - 'wb' opens binary connection to create unix
    # carriage returns
    
      fileConn <- file(filename, "wb")

      
    # Add instructions
    
      writeLines(paste0("#!/bin/bash" , "\n",
                        "#$ -pe smp 16", "\n",
                        "#$ -l h_vmem=2G", "\n",
                        "#$ -l h_rt=1:0:0", "\n",
                        "#$ -wd /data/scratch/wpw004/", runDirectoryDescription, "/", "\n",
                        "#$ -o /data/scratch/wpw004/", runDirectoryDescription, "_aggregationOutputFiles/", "\n",
                        "#$ -j y", "\n",
                        "module load R/4.2.2", "\n", "\n",
                        "Rscript /data/home/wpw004/NIHR245601MSADA/MSMicrosimulationModel_withADATesting_optimised/",
                        "aggregationMasters/combineIterationOutputs.R ", runDirectoryDescription, " ",
                        runDirectoryDescription, " ", "0"), 
                 fileConn)

    # Close 
    
      close(fileConn)

  # ----------------------------------------------------------------------------

  
      
