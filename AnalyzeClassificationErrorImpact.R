## AnalyzeClassificationErrorImpact.R
## ---------------------------------------------------------------------------------
## Script to analyze the impact of error in failure classification on decisions in 
## detecting faults.  
## ---------------------------------------------------------------------------------
## Copyright (C) 2013 Billy Kidwell, Davide Falessi, Jane Hayes, Forrest Shull
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 2
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
## 
## ---------------------------------------------------------------------------------
##
## Author Contact Information
## Kidwell, Billy <bill.kidwell@uky.edu>
## Davide Falessi <dfalessi@fc-md.umd.edu>
## Jane Hayes <hayes@cs.uky.edu>
## Forrest Shull <fshull@fc-md.umd.edu>

##                                    analyzePhase
## ==================================================================================
## Analyze one phase of data to select appropriate methods for each failure type
## 
## ----------------------------------------------------------------------------------
## failures - The dataset of failures for a phase. "ID","Method","Failure","Phase"
## actuals  - a cross tab of actual recorded results.  Used to get the number
##            of defects that are found when a certain method is used
## sums     - A vector to aggregate the number of defects found for each failure type
## ----------------------------------------------------------------------------------
analyzePhase<-function(failures, actuals, sums) {

   FailureTypes <- unique(failures$Failure)		 # All failure types in this phase
   library(abind, pos=4)			
   # Create a table.  Rows are methods, Columns are failures
   table <- xtabs(~Method+Failure, data=failures)      

   f <- 1
   while ( f <= length(FailureTypes) )  {
      
      failureType <- FailureTypes[f]	

	# What method do we predict will find the most of this failureType
      maxDefects <- max(table[,failureType])

	# Store the name of the method we expect to find the most
      method     <- names(table[,failureType])[which.max(table[,failureType])]

      # Get the actual number of failures we will find using this method
	actual     <- actuals[method, failureType]
      
	# add to the running sum of defects for this type
      sums[failureType] <- sums[failureType] + actual

      f <- f + 1
   }

   # Return running sum of defects found for each failure type
   sums
}

##                                        replaceValue
## ======================================================================================
## Function to replace a value with another value (cannot be the same as current value)
## possibleValues is a list of valid choices.  currentValue should be one of the choices.
## sample(possibleValues, 1) will return a random member of possibleValues.
## if the newValue is the same as the currentValue, we will randomly choose again
## ---------------------------------------------------------------------------------------
## currentValue - the value to be replaced.  Pass in to guarantee a new value is returned.
## possibleValues - an array of valid values that can be selected.
## ---------------------------------------------------------------------------------------
replaceValue <- function( currentValue, possibleValues ) {
   # I should verify that there is at least one other choice
   if ( isTRUE( length(possibleValues) == 1 ) ) {
      print("Assertion Failure: replaceValue cannot replace a value from a single choice")
      stop("Precondition failure, possibleValues must be larger than one item")
   }

   repeat {
      newValue = sample(possibleValues, 1)
	  if( is.na(currentValue) || newValue != currentValue) 
	     break
   }

   # This is our guarantee that we did not select the same value
   if (isTRUE(currentValue == newValue)) {
      print(sprintf("No difference in value from %s to %s", currentValue, newValue))
      stop("Error replacing value")
   }

   newValue
}

##                                        introduceError
## ===============================================================================================
## Function to introduce Error into a dataset.  Randomly selects indexes, then changes the value
## of the failure to another valid value
## -----------------------------------------------------------------------------------------------
## dataSet is expected to be ID, Method, Failure, Phase
## Error is a value between 0 and 1 
## -----------------------------------------------------------------------------------------------
introduceError <- function ( dataSet, error ) {
   failures <- dataSet   # Make a copy of the data
   FailureTypes <- unique(failures$Failure)		 # All failure types in this phase

   #select a subset of the samples by index
   sampleIndexes <- failures$ID[sample(1:nrow(failures), ceiling(error*nrow(failures)), replace=FALSE)]

   #Debug Statement
   #print(sprintf("Changing : %i == %i", ceiling(error*nrow(failures)), length(sampleIndexes) ))

   # Iterate through the indexes and change the failure type to some random other type
   i<-1
   while (i <= length(sampleIndexes)) {
      index <- toString(sampleIndexes[i])      

      #print(paste("Index is ", index))
      #print(failures[index ,])

      oldFailureType <- failures[index,"Failure"]
      failures[index,"Failure"] <- replaceValue(failures[index,"Failure"], FailureTypes)
      newFailureType <- failures[index,"Failure"]
      
	#print(failures[index ,])

	#print(paste("Old failure type is ", oldFailureType, ", new FailureType is ", newFailureType, ", Index is ", i))

      # Assert that we changed this value
      if (isTRUE(oldFailureType == newFailureType)) {
         print(sampleIndexes[i])
         print(failures[index ,]) 
         stop("Error changing Failure Type")
      }
   
      i <- i + 1
    }
   failures
}   

##                                buildResults
## =============================================================================
## Given the necessary parameters, build the results in the appropriate data 
## frame format to aggregate and report the results.
## Columns are Phase, E, Failure Type, TD, TDE
## -----------------------------------------------------------------------------
## phase         - a string that indicates the phase 
## error         - a number between 0 and 1 that indicates the error rate
## failureTypes  - a vector of the failure types for this phase
## tdVector      - a vector with the best case # of defects for this failure type 
## meanTDEVector - avg number of defects found in simulation
## -----------------------------------------------------------------------------
buildResults <- function( phase, error, failureTypes, 
                          tdVector, meanTDEVector ) {
   # Data Structure that I want...
   # Phase, E, Failure Type, TD, TDE
   size <- length(failureTypes)
   
   phaseVector <- rep(phase, size)
   errorVector <- rep(error, size)   
   blankTDVector <- rep(0, size)
   blankTDEVector<- rep(0, size)

   results <- data.frame(phaseVector, errorVector, failureTypes, 
                         blankTDVector, blankTDEVector)
   colnames(results) <- c("phase", "error", "failure type", "td", "tde")
   
   # NOTE: This assumes that indexes of the names in sums, meanTDE, and the failure Type 
   #       rows are in the same order
   #       I got incorrect results when indexing by the name of the failureType
   #       Since I construct them all with the same FailureType vector, it works
   i<-1
   while( i <= size) {
   
      results[i, "td"]  <- tdVector[i]
      results[i, "tde"] <- meanTDEVector[i]

	 # Assertion.  results[i, failureType] should match
       # the name of tdVector[i] and meanTDEVector[i]
	 if ( (names(tdVector[i]) != names(meanTDEVector[i])) ||
            (names(tdVector[i]) != results[i, "failure type"])
       ) {
          print( sprintf("Index name mistmatch. i is %i. tdVector = %s.  meanTDE = %s. results[i,'failure'] = %s", 
                         i, names(tdVector[i]), names(meanTDE[i]), results[i, "failure type"]))
         stop("Name mismatch")
      }

      i <- i + 1
   }
   
   results
}


##                                       doRUN
## ================================================================================
## Given the parameters below, introduce random error, analyze the phase, and 
## return the results data for this phase, at this error rate
## 
## --------------------------------------------------------------------------------- 
## phase - a String that identifies the phase that is being analyzed
## error - a number between 0 and 1 that indicates the amount of error to 
##         introduce (e.g. 0.5 means introduce 50% error)
## PhaseFailures - failure/detection information for this phase
## actuals - a cross tabulation of PhaseFailures that can be used to determine 
##           how many faults of a specific failure type will be found by a method
## tdVector - A vector with row names for failure types that will contain the 
##            best results based on the actual data
## ----------------------------------------------------------------------------------
doRun<-function (phase, error, PhaseFailures, actuals, tdVector) {

   #print(sprintf("Phase %s, E=%3.2f", phase, error))	# Print a Header


   FailureTypes <- unique(PhaseFailures$Failure)	# All failure types in this phase
   size <- length(FailureTypes)

   sums <- rep(0, size) 					# Initialize a Vector to hold the sums
   names(sums) <- FailureTypes

   meanTDE <- rep(0, size)
   names(meanTDE) <- FailureTypes

   run <-1								# from 1 to runCount
   while(run <= runCount) {    
	failures = introduceError( PhaseFailures, error )			# Make a copy of PhaseFailures and introduce errors
	sums = analyzePhase(failures, actuals, sums)				# Analyze decisions
	run <- run + 1									# Increment run
   }

   # Iterate through all of the failure types, calculate and store the mean
   i<-1
   while ( i <= size ) {
      failureType = names(sums)[i]      
      meanTDE[failureType] <- sums[failureType] / runCount
      i <- i+1
   }

   # Build the results data set
   runResults <- buildResults( phase, error, FailureTypes, tdVector, meanTDE )

   # Return the runResults for this error rate
   runResults
}

##                                      doPhase
## ===============================================================================================
## Create a subset of the failures dataset for the given phase.  
## Build the actuals contingency table that is used to get simulation results
## Build the subset of runResults for E=0
## Call doRun for each level of error that we want to analyze.
## Aggregate the results and write them to a CSV file named after the phase.
## -----------------------------------------------------------------------------------------------
## Failures - a Dataset in the form of "ID","Method","Failure","Phase"
## phase    - the name of the phase to analyze
## -----------------------------------------------------------------------------------------------
doPhase<-function(Failures, phase) {
   #Start the clock, time each phase and output to console
   ptm <- proc.time()

   #subset of failures for this phase
   PhaseFailures <- subset(Failures, Phase==phase)

   # Create a contingency table of method/failure for this phase
   library(abind, pos=4)
   # Create a table.  Rows are methods, Columns are failures   
   actuals <- xtabs(~Method+Failure, data=PhaseFailures)      

   FailureTypes <- unique(PhaseFailures$Failure)	# All failure types in this phase

   size <- length(FailureTypes)
   tdVector <- rep(0, size) 					# Initialize a Vector to hold the sums
   names(tdVector) <- FailureTypes

   # Analyze the base phase (for E=0).  These are used for comparison.  This is the number
   # of defects that would be found if the best method was selected.
   tdVector <- analyzePhase(PhaseFailures, actuals, tdVector)

   # Build the base results data set
   # Note that td=tde for error = 0 
   baseResults <- buildResults( phase, 0, FailureTypes, tdVector, tdVector )

   # 5, 10, 25, 50, 75, 90, 95
   runResults <- doRun(phase, 0.05, PhaseFailures, actuals, tdVector)
   baseResults <- rbind(baseResults, runResults)    

   runResults <- doRun(phase, 0.10, PhaseFailures, actuals, tdVector)
   baseResults <- rbind(baseResults, runResults)

   runResults <- doRun(phase, 0.25, PhaseFailures, actuals, tdVector)
   baseResults <- rbind(baseResults, runResults)

   runResults <- doRun(phase, 0.50, PhaseFailures, actuals, tdVector)
   baseResults <- rbind(baseResults, runResults)

   runResults <- doRun(phase, 0.75, PhaseFailures, actuals, tdVector)
   baseResults <- rbind(baseResults, runResults)

   runResults <- doRun(phase, 0.90, PhaseFailures, actuals, tdVector)
   baseResults <- rbind(baseResults, runResults)

   runResults <- doRun(phase, 0.95, PhaseFailures, actuals, tdVector)
   baseResults <- rbind(baseResults, runResults)

   runResults <- doRun(phase, 1.0, PhaseFailures, actuals, tdVector)
   baseResults <- rbind(baseResults, runResults)


   # Output phase runResults to CSV
   fileName <- paste("Failure-Phase", phase, ".csv", sep="")
   print(paste("Writing...", fileName), sep="")
   write.table(baseResults,file=fileName,sep=",",row.names=F)
   print("done.")

   # Print the time statistics
   print(proc.time() - ptm)
   flush.console()			# Update the console	 
}

runCount <- 10000  # The number of runs to use when randomly introducing error

# Read in the Failures data from disk
Failures <- read.table("./NewFailureDataQuery.txt", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# Phases = {I, II, III, IV, V}

doPhase(Failures, "I")
doPhase(Failures, "II")
doPhase(Failures, "III")
doPhase(Failures, "IV")
doPhase(Failures, "V")



