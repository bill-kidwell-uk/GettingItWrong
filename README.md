GettingItWrong
==============

This R script is used in software engineering research to determine the impact that misclassified bugs have on software engineering decisions.  In this study we examine the classification of the software failure and their impact on verification and validation technique selection.  

Publication details on this work will be updated when they are available.  

The script expects a file named "NewFailureDataQuery.txt" in the same directory as the script.
The data for the experiment is not publically available.

The file should be a CSV formatted file with the following headings: "ID","Method","Failure","Phase"   
**ID** - an internal identifier.  In our experiment that was a system generated id.   
**Method** - The verification and validation activity that found the bug   
**Failure** - The type of failure that was observed   
**Phase** - An indicator of the software lifecycle.  This script uses roman numerals I-V.   

The output of the file is a CSV file for each phase.  The output consists of five columns.   
**Phase** - The phase, as described above   
**Error** - A value from 0 to 1 to indicate the amount of random error that was introduced   
**Failure type** - The type of failure    
**TD** - The number of faults of this type that are detected by the optimal choice   
**TDE** - The number of faults that were detected using the technique that was selected with error introduced   







