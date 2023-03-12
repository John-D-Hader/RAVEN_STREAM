rm(list=ls())
cat("\014")
# This script scrapes sewage treatment plant predicted no effect concentration
# (i.e., STP PNEC) data from the European Chemical Agency's brief profiles for a user-defined
# list of chemical CAS numbers w/corresponding Infocard numbers.
# The primary output of the script is a .csv file that contains chemical CAS numbers,
# STP PNEC values, and associated flags.
# The secondary output is summary information regarding the scraped STP PNEC values.
#
# Note: Prior to 2023/03/02, the number of CAS numbers that had multiple infocards (i.e,.
#       brief profile web pages) was tracked and nothing was done with these. As of 
#       2023/03/02, these instances are being removed, so only CAS numbers that occur once
#       are kept.
#
# Script QA'ed up to the point of STP PNEC Data file being written out (Hader; 2023/03/06).
# See for details: C:\Users\JohnHader\Desktop\Käppala\Paper\Analysis\QA\2023_03_03_Webscraping_QA.xlsx
#
# Author: John Hader


# Loads in required packages
require(data.table)
require(openxlsx)
require(rvest)
require(zoo) # Used for rollapply function (fixing badly reported STP PNEC values)
require(ggplot2)
require(stringr)


# Custom function definitions ---------------------------------------------


# # Temp code for accessing row_extractor function
# html_code_vector <- extracted_02
# test_row         <- which(CAS_rows_binary)
# parameter_string <- "CAS number"
# row_offset       <- 0

# Defines function for validating and extracting parameter info from HTML code.
row_extractor <- function(html_code_vector,test_row,parameter_string,row_offset){
  # Function for checking and extracting a variable's raw data from the HTML code.
  # Definition of arguments:
  # - html_code_vector: One of the bulk extracted HTML code sets from the webpage being scaped
  # - test_row: Vector indicating row where the parameter of interest's label was identified (obtained from which(logical) from the HTML data.
  # - parameter_string: Label for the variable of interest (only for reporting error if one occurs).
  # - row_offset: Number of rows to offset extraction (e.g., if the parameter of interest is actually located
  #               one row below the label itself).
  
  # Ensures the number of parameter rows does not exceed 1.
  if(length(test_row)>1){
    stop(paste("More than one ",parameter_string," label row identified for chemical ",chemical_data$CAS_number[i],".",sep=""))
    
    # If the number of parameter rows is zero, warns the user of the parameter's absence and returns NA for the row.
  } else if(length(test_row)==0){
    return(NA)
    
    # If only one parameter row found, returns the corresponding row's info, with row offset applied.
  } else{
    # Extracts parameter info.
    return(html_code_vector[test_row+row_offset])
  }
}

# Function for checking existence of and fixing poorly reported numeric values
# (i.e., number followed by space followed by another number, usually using spaces to separate 
# groups of three digits in large values).
bad_value_fixer <- function(STP_string,raw_PNEC,PNEC_numeric,PNEC_num_ind){
  # This function will only attempt to repair bad reporting of numbers to the right
  # of a hyphen (i.e., it assumes that there is a range of values if the numbers are
  # bad, and that they are bad because the larger of the range exceeds the hundreds place
  # in the units of the smaller number). Any badly reported values outside of this definition
  # are not handled, and NAs are returned.
  
  # Converts numeric vector to binary for number (1) or NA (0).
  numeric_vector <- PNEC_numeric
  numeric_vector[which(is.finite(numeric_vector)==TRUE)] <- TRUE
  numeric_vector[which(is.na(numeric_vector)==TRUE)]     <- FALSE
  
  # Calculates rolling sum (per 2 elements) to catch adjacent numeric values.
  vector_roll_sum <- rollapply(numeric_vector,2,sum)
  
  # Ensures all rolling values are 0, 1, or 2, which should be the case
  # since numeric vector was converted to binary before 2-element rolling sum taken.
  if(all((vector_roll_sum==0)|(vector_roll_sum==1)|(vector_roll_sum==2))!=TRUE){
    stop("Unexpected error in converting parsed STP PNEC string to binary numeric values.")
  }
  
  # Checks for, and attempts to repair if found, instances of badly reported numbers.
  #
  # Bad numbers found
  if(any(vector_roll_sum==2)){
    
    # Attempts to find the hyphen (i.e., between actual numbers).
    hyphen_ind <- which(raw_PNEC=="-")
    
    # If no hyphen found for this instance where two numbers were found adjacent to eachother, return NAs.
    if(length(hyphen_ind)==0){
      warning(paste0("Bad value found in number without a range for the following raw ECHA STP PNEC string: ",STP_string))
      return(list(NA,NA,NA,NA))
    }
    
    # Return NAs if none or more than one numeric reported to the left of the hyphen.
    if(length(which(PNEC_num_ind<hyphen_ind))!=1){
      warning(paste0("Value reporting issue present to the left of hyphen in the following raw ECHA STP PNEC string: ",STP_string))
      return(list(NA,NA,NA,NA))
    }
    
    # Finds hyphen in the character vector, and returns NAs if not only 1 found.
    hyphen_ind_char <- which(str_detect(raw_PNEC,"-")==TRUE)
    if(length(hyphen_ind_char)!=1){
      warning(paste0("More than one hyphen found in the following raw ECHA STP PNEC string: ",STP_string))
      return(list(NA,NA,NA,NA))
    }
    
    # Attempts to find where the units label is, and returns NAs if none found.
    unit_ind <- which(str_detect(raw_PNEC,paste(STP_units,collapse = "|"))==TRUE)
    if(length(unit_ind)!=1){
      warning(paste0("No or multiple units matching found for the following raw ECHA STP PNEC string: ",STP_string))
      return(list(NA,NA,NA,NA))
    }
    
    # Checks that all characters between hyphen and units can be coerced to numeric.
    if(any(is.na(as.numeric(raw_PNEC[(hyphen_ind_char+1):(unit_ind-1)])))==TRUE){
      warning(paste0("Non-numeric characters found between the hyphen and units for the following raw ECHA STP PNEC string: ",STP_string))
      return(list(NA,NA,NA,NA))
    }
    
    # Concatenates all characters between the hyphen and the units and builds back full information into STP_string,
    # with the badly-reported value corrected.
    STP_string <- paste(paste(raw_PNEC[1:hyphen_ind_char],collapse = " "),paste0(raw_PNEC[(hyphen_ind_char+1):(unit_ind-1)],collapse = ""),
                        paste(raw_PNEC[unit_ind:length(raw_PNEC)],collapse = " "),collapse = " ")
    
    # Re-does the STP_string splitting done in the calling function and
    # checks again for bad value reporting.
    #
    # Finds indices of numbers in PNEC string.
    raw_PNEC <- strsplit(STP_string,"\\s+")[[1]]
    PNEC_numeric <- suppressWarnings(as.numeric(raw_PNEC))
    PNEC_num_ind <- which(is.na(PNEC_numeric)==FALSE)
    #
    # Converts numeric vector to binary for number (1) or NA (0).
    numeric_vector <- PNEC_numeric
    numeric_vector[which(is.finite(numeric_vector)==TRUE)] <- TRUE
    numeric_vector[which(is.na(numeric_vector)==TRUE)]     <- FALSE
    #
    # Calculates rolling sum (2 elements) to catch adjacent numeric values.
    final_vector_roll_sum <- rollapply(numeric_vector,2,sum)
    #
    # Ensures all rolling values are 0, 1, or 2.
    if(all((final_vector_roll_sum==0)|(final_vector_roll_sum==1)|(final_vector_roll_sum==2))!=TRUE){
      stop("Unexpected error in converting parsed string to binary numeric values and determining where adjacent values were (for final check of bad values).")
    }
    #
    # Checks for bad values, and returns NAs if there still are any.
    if(any(final_vector_roll_sum==2)){
      warning(paste0("Attempted to repair bad value reporting but failed for the following raw ECHA STP PNEC string: ",STP_string))
      return(list(NA,NA,NA,NA))
      
      # Returns PNEC numeric info if bad values were repaired successfully.
    } else if(any(final_vector_roll_sum==2)==FALSE){
      return(list(raw_PNEC,PNEC_numeric,PNEC_num_ind,"Bad_value_repaired"))
      
      # Generic error handling.
    } else{
      stop(paste0("Unexpected error encountered when attempting to detect bad value reporting from ECHA."))
    }
    
    # No bad numbers found.
  } else if(any(vector_roll_sum==2)==FALSE){
    return(list(raw_PNEC,PNEC_numeric,PNEC_num_ind,"N/A"))
    
    # Generic error catching.
  } else{
    stop(paste0("Unexpected error encountered when attempting to detect bad value reporting from ECHA."))
  }
}

# Function for processing raw STP PNEC values from ECHA webscraping
# into usable toxicity values within RAVEN STREAM.
STP_PNEC_extractor <- function(STP_string){
  # Function takes as input the string from the STP PNEC region of the ECHA webpage,
  # and processes this text string into three elements of a vector:
  # - Minimum STP PNEC value
  # - Maximum STP PNEC value
  # - Flag for the value
  #
  # Note that the input string cannot be converted to numeric for if/else testing.
  
  
  # If PNEC is NA, returns NA.
  if(is.na(STP_string)==TRUE){
    return(c(NA,NA,"Missing_PNEC"))
    
    # If PNEC is Inf, returns Inf (checks text, not numeric, version).
  } else if(STP_string=="Inf"){
    return(c(Inf,Inf,"Hazard_unlikely/not_identified"))
    
    # If PNEC is Inf, returns Inf (checks numeric version).
  } else if(is.infinite(STP_string)==TRUE){
    return(c(Inf,Inf,"Hazard_unlikely/not_identified"))
    
    # If PNEC is valid string not handled above, extracts value(s).
  } else{
    
    # Finds indices of numbers in PNEC string.
    raw_PNEC <- strsplit(STP_string,"\\s+")[[1]]
    PNEC_numeric <- suppressWarnings(as.numeric(raw_PNEC))
    PNEC_num_ind <- which(is.na(PNEC_numeric)==FALSE)
    
    # If no numbers are found, returns NA (this would be e.g., for "-" entered as PNEC).
    if(length(PNEC_num_ind)==0){
      return(c(NA,NA,"Missing_PNEC"))
    }
    
    # Sends the input string information to function for checking and fixing (if possible)
    # of value reporting issues (i.e., spaces as intra-number separators).
    value_fixer_out <- bad_value_fixer(STP_string,raw_PNEC,PNEC_numeric,PNEC_num_ind)
    #
    # Unpacks output of the bad_value_fixer function.
    raw_PNEC             <- value_fixer_out[[1]]
    PNEC_numeric         <- value_fixer_out[[2]]
    PNEC_num_ind         <- value_fixer_out[[3]]
    bad_value_fixer_flag <- value_fixer_out[[4]]
    #
    # If value was not fixable:
    if(all(is.na(raw_PNEC))==TRUE){
      return(c(NA,NA,"Value_Reporting_Issue"))
      
      # If value was fine or fixed:
    } else if(all(is.na(raw_PNEC))==FALSE){
      
      # Saves off first PNEC value.
      first_PNEC_value <-  PNEC_numeric[PNEC_num_ind[1]]
      
      # Handles existence of range of values.
      if(identical("-",raw_PNEC[(PNEC_num_ind[1]+1)])==TRUE){
        
        # Extracts the second value in the range of PNECs.
        second_PNEC_value   <- PNEC_numeric[PNEC_num_ind[2]]
        last_PNEC_value_ind <- PNEC_num_ind[2]
        
      } else if(identical("-",raw_PNEC[(PNEC_num_ind[1]+1)])==FALSE){
        
        # Repeats existing single PNEC value for 'second' value.
        second_PNEC_value   <- first_PNEC_value
        last_PNEC_value_ind <- PNEC_num_ind[1]
        
        # Error catch.
      } else{
        stop("Error when trying to determine whether a range of PNECs is provided.")
      }
      
      # Combines first and second PNEC values into one vector.
      out_PNECs <- c(first_PNEC_value,second_PNEC_value)
      
      # Extracts multiplier for PNEC normalization.
      units_check <- raw_PNEC[(last_PNEC_value_ind+1)]
      unit_norm   <- STP_unit_conv[which(STP_units==units_check)]
      
      # Applies the PNEC normalization if possible and stops script if not.
      if(length(unit_norm)==0){
        stop("For one or more chemical, units were provided for STP PNEC that did not match user's expectations.")
        
        # Applies the value normalization if one unique unit match identified.
      } else if((length(unit_norm)==1)&(is.numeric(unit_norm)==TRUE)){
        out_PNECs <- out_PNECs*unit_norm
        
        # Handles off chance that more than one unit value was found to be a match (this should not ever happen since only one string pulled for unit check)
      } else if(length(unit_norm)>1){
        stop("More than one unit match found in labeling for an STP value.")
        
        # Generic error catch.
      } else{
        stop("Unexpected error in matching units of STP to one of expected STP units options.")
      }
      
      # Sets PNEC to NA if any are defined as zero (this is an error within ECHA reporting
      # that was identified with one or otherwise very few STP PNECs).
      if((out_PNECs[1]==0)|(out_PNECs[2]==0)){
        return(c(NA,NA,"Zero_value(s)_detected"))
      }
      
      # Sets PNEC to NA if the first value of a range is larger than the second.
      if((out_PNECs[1]>out_PNECs[2])==TRUE){
        return(c(NA,NA,"Range_value_mismatch"))
      }
      
      # Returns normalized (and fixed, if this was done) PNEC values as a range of values.
      return(c(out_PNECs,bad_value_fixer_flag))
      
      # Generic error catch.
    } else{
      stop("Error when trying to check STP string for poorly reported values.")
    }
  }
}

# Function for cleaning data returned from the STP_PNEC_extractor function
# having been applied to the ECHA STPN PNEC strings.
data_cleaner <- function(output_data){
  
  # Removes column names and transposes data.
  dimnames(output_data) <- NULL
  output_data <- t(output_data)
  
  # Organizes as data table and returns to user.
  output_data <- as.data.table(output_data,stringsAsFactors=FALSE)
  return(output_data)
  
}


# User-defined variables --------------------------------------------


# Defines the name of the ECHA CAS-infocard mapping data, base ECHA URL, and output dir.
CAS_infocard_file <- "C:/Users/JohnHader/Desktop/Käppala/Paper/Analysis/ECHA_Raw_CAS_Infocard_mappings.xlsx"
base_ECHA_url     <- "https://echa.europa.eu/brief-profile/-/briefprofile/"
chem_stats_dir    <- "C:/Users/JohnHader/Desktop/Käppala/Paper/Analysis/STP_PNEC_stats/"
final_out_file    <- "C:/Users/JohnHader/Desktop/Käppala/Paper/Analysis/ECHA_STP_PNEC_Data.csv"


# Semi-permanent variables --------------------------------------------


# Defines parameters for the raw ECHA-infocard datafile.
sheet_of_interest <- "results"
start_row         <- 3

# Strings for finding info of interest from HTML code (extracts 1 and 2).
CAS_num_label     <- "CAS no.:" # Note this is also actually hard-coded under the "CAS number extraction/comparison" section for extracting the CAS number from the HTML code...
STP_PNEC_label <- "Sewage treatment plant (STP)" # Extract 1

# Web scraping miscelaneous inputs.
extract_01 <- "td" # Identifier for HTML building blocks ("Sewage treatment plant (STP)" (value is element after the element with this text string)); Used
extract_02 <- "dl" # Identifier for HTML building blocks (CAS number and lots of the phys-chem props and other things in straight text form w/labels); Used
report_every <- 10 # Defines how frequently to report to user the progress of webscraping.

# STP PNEC processing details.
STP_NA_flags  <- c("testing technically not feasible","No emission to STP expected") # This text just has to be present, other characters may be around these
STP_Inf_flags <- c("aquatic toxicity unlikely","No hazard identified")               # This text just has to be present, other characters may be around these
STP_units     <- c("ng/L","µg/L","mg/L","g/L")
STP_unit_conv <- c(1.0E-6,1.0E-3,1,1.0E3) # Indicates multiplier of above units for normalization to mg/L.


# Pre-processing of CAS infocard file -----------------------------------------------------


# Note that a chemical can be registered multiple times, and have the same CAS number and same infocard URL,
# across multiple registrations in ECHA, but may have different registration info in each of these.
# The algorithms used here account for a CAS number being registered with the same Infocard more than
# once (and will only count this once), while indications of a CAS frequency >1 indicates
# more than 1 infocard associated with that CAS number! These are removed.

# Reads in raw infocard/Cas number data:
library(openxlsx)
raw_ECHA_CAS_infocard_data <- as.data.table(openxlsx::read.xlsx(CAS_infocard_file,sheet=sheet_of_interest,startRow=start_row),stringsAsFactors=FALSE)

# Extracts only CAS and infocard columns.
ECHA_base_data           <- raw_ECHA_CAS_infocard_data[,c("Cas.Number","ID")]
colnames(ECHA_base_data) <- c("CAS_number","infocard")

# Reorders data based on CAS number and removes missing CAS (these are un-matcheable to Käppala data).
setorder(ECHA_base_data,CAS_number)
ECHA_base_data <- ECHA_base_data[CAS_number != "-"]

# Copies ECHA base data for determination of CAS number/Infocard matches.
CAS_infocard <- copy(ECHA_base_data)

# Extract unique infocards (CAS number repeats likely to exist, as chemicals
# can be registered more than once, still under same infocard number, but also
# as repeated CAS numbers).
CAS_infocard <- CAS_infocard[,unique(.SD),by="CAS_number"]
if(nrow(CAS_infocard)!=length(unique(CAS_infocard$infocard))){
  stop("One infocard number is shared by two or more CAS numbers!")
}

# Adds in column of CAS num frequency to CAS infocard data.
CAS_counts   <- CAS_infocard[, .N, by=CAS_number]
library(data.table)
CAS_infocard <- merge(CAS_infocard,CAS_counts,by="CAS_number")
colnames(CAS_infocard)[colnames(CAS_infocard)=="N"] <- "CAS_Freq"

# Removes CAS numbers with a CAS_Freq greater than one, reporting to user
# number of these instances.
CAS_Freq_over_1 <- length(which(CAS_infocard$CAS_Freq>1))
CAS_infocard    <- CAS_infocard[CAS_Freq==1]
CAS_infocard    <- CAS_infocard[,CAS_Freq:=NULL]
warning(paste0(CAS_Freq_over_1," CAS numbers identified with frequencies over 1. These were removed!"))



# URL Reading and Initial Data Extraction -----------------------------------------------------


# Copies the CAS-infocard data mapping & initializes column for STP values and URL reading error log.
chemical_data <- copy(CAS_infocard)
chemical_data[,STP_PNEC := rep(NA,nrow(chemical_data))]
chemical_data[,URL_Log  := rep(NA,nrow(chemical_data))]

### Note: When script was run on 2023/03/03, processing stalled out for some reason at i = 17632
###       Some testing revealed no issues, so the loop below was re-started from i = 17630, to run to completion,
###       and the generated data from this is what is used in the Käppala paper's final analysis as of March, 2023.
### Note: When running this, had problem with how errors were built into the data table (i.e., did not index with i).
###       Warnings should have been thrown for this, did not see any, but could be a good idea to re-run later on to
###       check that no actual errors were encountered (as the run on 2023/03/03 showed none).

# Loops through available infocards and corresponding URLs.
for(i in c(1:nrow(chemical_data))){
  
  # Ensures current InfoCard is valid.
  if(is.na(chemical_data$infocard[i])==TRUE){
    chemical_data$URL_Log[i] <- "Infocard was NA"
    next
  }
  
  # Reads in raw HTML code from current infocard's webpage.
  raw_html <- try(read_html(paste(base_ECHA_url,chemical_data$infocard[i],sep="")),silent = TRUE)

  # URLs causing errors are skipped.
  if(any(class(raw_html)=="try-error")){
    chemical_data$URL_Log[i] <- raw_html[1]
    next
  }
  
  # Extracts various, pre-defined building blocks of the HTML code.
  extracted_01 <- raw_html %>% html_nodes(extract_01) %>% html_text() # Type of study provided data and PNEC info
  extracted_02 <- raw_html %>% html_nodes(extract_02) %>% html_text() # Study results + summaries (not labeled with those headings though...)
  
  # Temporary code for manually exploring output of HTML extracts
  ext_01_mat <- as.data.frame(extracted_01)
  ext_02_mat <- as.data.frame(extracted_02)
  
  # Checks that a valid Brief Profile was found, and if not skips to next infocard.
  if((length(extracted_01)==0)|(length(extracted_02)==0)){
    chemical_data$URL_Log[i] <- "Necessary HTLM data not found in brief profile"
    next
  }
  
  
  # CAS number extraction/comparison ----------------------------------------
  
  
  # Extracts CAS number row info.
  CAS_rows_binary <- str_detect(extracted_02,coll(CAS_num_label))
  CAS_row_info    <- row_extractor(extracted_02,which(CAS_rows_binary),"CAS number",0) # CAS number is on same line as label

  # Strips out just the CAS number and compares to current CAS number from the input data CAS-infocard data.
  brief_profile_CAS_num <- str_trim(sub(".*CAS no.: *(.*?) *Index number:.*", "\\1", CAS_row_info))
  if(identical(brief_profile_CAS_num,chemical_data$CAS_number[i])==FALSE){
    stop(paste("CAS numbers in ECHA brief profile and that stored in row ",i," of the input infocard-CAS number mapping dataset don't match.",sep=""))
  }

  
  # STP PNEC value extraction ----------------------------------------
  
  
  # Extracts row with the STP PNEC label.
  PNEC_rows_binary          <- str_detect(extracted_01,coll(STP_PNEC_label))
  chemical_data$STP_PNEC[i] <- str_trim(row_extractor(extracted_01,which(PNEC_rows_binary),"PNEC",1)) # STP PNEC value is one row below label.
  
  # Informs user of progress of script.
  if((i%%report_every)==0){
    print(paste("Finished processing CAS number ",i," out of ",nrow(chemical_data),".",sep = ""))
  }
}


# Data cleaning ----------------------------------------------------------


# Cleans up STP PNEC data.
STP_PNEC_column <- chemical_data$STP_PNEC
STP_NA_rows     <- str_detect(tolower(STP_PNEC_column),tolower(paste(STP_NA_flags,collapse = "|")))
STP_Inf_rows    <- str_detect(tolower(STP_PNEC_column),tolower(paste(STP_Inf_flags,collapse = "|")))
STP_PNEC_column[which(STP_NA_rows==TRUE)]      <- NA
STP_PNEC_column[which(STP_Inf_rows==TRUE)]     <- Inf
chemical_data$STP_PNEC <- STP_PNEC_column

# Applies STP PNEC extraction and data cleaner functions to data.
STP_data <- sapply(chemical_data$STP_PNEC,STP_PNEC_extractor)
STP_data <- data_cleaner(STP_data)
colnames(STP_data) <- c("min_STP_PNEC_mg/L","max_STP_PNEC_mg/L","STP_PNEC_flag")

# Builds extracted values into existing chem data.
chemical_data <- cbind(chemical_data,STP_data)

# Converts min and max STP PNEC columns to numeric.
chemical_data$`min_STP_PNEC_mg/L` <- as.numeric(chemical_data$`min_STP_PNEC_mg/L`)
chemical_data$`max_STP_PNEC_mg/L` <- as.numeric(chemical_data$`max_STP_PNEC_mg/L`)

# Writes out the compiled database of chemical properties.
fwrite(chemical_data,file = final_out_file,row.names = FALSE,sep = ";",na="NA")

#### As of 2023/03/04: Code up to this point has been fully manually reviewed and run, with
####                   the output file being: ECHA_STP_PNEC_Data.csv in the following
####                   directory: C:/Users/JohnHader/Desktop/Käppala/Paper/Analysis


# Data reporting and writing (This is not QA'ed; only left in for illustrative purposes) ----------------------------------------------


# Compiles tabulated report of value statistics and numbers of flags.
num_chems        <- nrow(chemical_data)
num_fixed_values <- length(which(chemical_data$STP_PNEC_flag=="Bad_value_repaired"))
num_tox          <- length(which(chemical_data$STP_PNEC_flag=="N/A"))
num_tox          <- num_tox+num_fixed_values
num_unlikely_tox <- length(which(chemical_data$STP_PNEC_flag=="Hazard_unlikely/not_identified"))
num_tox_missing  <- length(which(chemical_data$STP_PNEC_flag=="Missing_PNEC"))
num_report_issue <- length(which(chemical_data$STP_PNEC_flag=="Value_Reporting_Issue"))
num_zero_values  <- length(which(chemical_data$STP_PNEC_flag=="Zero_value(s)_detected"))
num_bad_range    <- length(which(chemical_data$STP_PNEC_flag=="Range_value_mismatch"))
#
# Compiles and writes out PNEC classification counts to summary table.
row_labels <- c("Total Chems","Total Valid PNECs (includes repaired)","Bad value Repaired","Hazard unlikely/not identified",
                "Missing PNEC","Value reporting Issue","Zero value","Range mismatch")
row_values <- c(num_chems,num_tox,num_fixed_values,num_unlikely_tox,num_tox_missing,num_report_issue,num_zero_values,num_bad_range)
table_data <- as.data.frame(cbind(row_labels,row_values),stringsAsFactors = FALSE)
colnames(table_data) <- c("Descriptor","Counts")
fwrite(table_data,paste0(chem_stats_dir,"ECHA_STP_PNEC_Summary.csv"),row.names = FALSE,sep = ";",na="NA")

# Generates distribution curve of upper and lower toxicity values.
#
# Pulls out and combines min and max PNEC data for plotting
tox_plot_data <- chemical_data[((chemical_data$STP_PNEC_flag=="N/A")|(chemical_data$STP_PNEC_flag=="Bad_value_repaired")),]
min_plot_data <- as.data.table(cbind(tox_plot_data$`min_STP_PNEC_mg/L`,rep("min",nrow(tox_plot_data))))
max_plot_data <- as.data.table(cbind(tox_plot_data$`max_STP_PNEC_mg/L`,rep("max",nrow(tox_plot_data))))
tox_plot_data <- rbind(min_plot_data,max_plot_data)
tox_plot_data$V1 <- log10(as.numeric(tox_plot_data$V1))
colnames(tox_plot_data) <- c("STP PNEC log10(mg/L)","type")
#
# Plots bar graph distributions of the minimum and maximum PNEC values.
# Plotting code from Miquel Ramal: http://rstudio-pubs-static.s3.amazonaws.com/374857_5a23bad9783a43c1b102aa80aa5c1a7c.html
p <- ggplot(tox_plot_data, aes(x = `STP PNEC log10(mg/L)`, fill = type)) +
      geom_histogram(binwidth = .3, alpha =.5, position = "identity") + scale_y_continuous(expand = c(0, 0))
p <- p  + theme(text = element_text(size=40),panel.grid = element_blank(),panel.border = element_blank(),
            axis.line.y.left=element_line(size=1),axis.line.x.bottom=element_line(size=1),
            panel.background = element_rect(fill = 'white', colour = 'white'))
#
# Saves off distribution of PNEC values.
ggsave(filename = paste0(chem_stats_dir,"ECHA_STP_PNEC_Dist.png"),plot=p,width=50,height=37.5,units="cm",dpi = 300)


# Code for testing URL error handling.
# raw_html_good  <- try(read_html(paste(base_ECHA_url,chemical_data$infocard[i],sep="")),silent = TRUE)
# raw_html_blank <- try(read_html(paste(base_ECHA_url,"100.132.846",sep="")),silent = TRUE)
# raw_html_404   <- try(read_html("https://echa.europa.eu/bxxxxfile/-/briefprofile/"),silent = TRUE)
# raw_html_bad   <- try(read_html("https://öjaewlkfjoewiafsfaödklsn.eu"),silent = TRUE)
