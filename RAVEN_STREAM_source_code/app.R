rm(list=ls())
cat("\014")
# This script hosts the source code for the RAVEN STREAM R Shiny app.

# Loads necessary packages
require(data.table)
require(DT)
require(shiny)
require(ggplot2)
require(shinyWidgets)
require(shinyFiles)
require(shinythemes)
require(plotly)
require(xlsx)
require(rvest)
require(stringr)
require(scales) # to access break formatting functions
require(zoo)
require(purrr)
require(glue)
require(viridis)

# Acknowledgment notes:
#
# Code below for accepting a user-defined input file is from: https://shiny.rstudio.com/reference/shiny/0.14/fileInput.html
# Code below for user input data table heavily from here: https://community.rstudio.com/t/how-do-i-create-an-editable-table-to-allow-user-input-draw-scatterplot-and-fit-a-curve-through-those-points/83802 
#
##### Important note on input data for tool! #####
#
# The ECHA_STP_PNEC_Data.csv file was copied from the following directory: 
# "C:/Users/JohnHader/Desktop/Käppala/Paper/Analysis/"
# into the working directory (i.e., "C:/Users/JohnHader/Desktop/Code/RAVEN_STREAM/")
# on 2023/03/06
#
#######


# RAVEN STREAM processing functions --------------------------------------------------

# 
# # Debug scratch code for initializing and running RAVE_STREAM outside of the GUI.
# 
# # Establishes the input data table for the STP structure:
# user_input_DT           <- data.table(v1=c("0","0","0"),v2=c("0","0","0"),v3=c("0","0","0"),stringsAsFactors = FALSE)
# colnames(user_input_DT) <- c("Region name", "Volume (m^3)","Flow fraction")
# rownames(user_input_DT) <- c("Region 01","Region 02","Region 03")
# #
# # Copies the user-fillable data table and inputs default values for sample run.
# sample_input_DT <- copy(user_input_DT)
# sample_input_DT$`Region name`   <- c("Larger_AS","Two_ADs","Smaller_AS")
# sample_input_DT$`Volume (m^3)`  <- c(92430,18000,48606)
# sample_input_DT$`Flow fraction` <- c(0.64,1,0.36)
# #
# upstream_data      <- fread(input = "C:/Users/JohnHader/Desktop/Käppala/Paper/Analysis/Upstream_Käppala_Chem_Data.csv", encoding = "UTF-8",stringsAsFactors = FALSE)
# scenario_label     <- "Debug_test"
# yearly_to_MIS_perc <- "25th, 50th, & 75th"
# STP_region_table   <- sample_input_DT
# user_defined_plot_region <- "Old_AS"
# setwd("C:/Users/JohnHader/Desktop/Code/RAVEN_STREAM")


# Primary control function for the RAVEN STREAM analysis
RAVEN_STREAM_controller <- function(upstream_data,scenario_label,STP_region_table,user_defined_plot_region,updateProgress){
  #
  # Inputs to function are:
  #
  # upstream_data:        Data table of the upstream industrial product/chemical usage (properly formatted; see template). This must have been read in via 'fread' with UTF-8 encoding with stringsAsFactors = FALSE
  # scenario_label:       Label (as string) that will be appended to all output files.
  # STP_region_table:     Data table with 3 columns and a number of rows (hard-coded at 3 as of 2021/09/02). Columns are "Region name", "Volume (m^3)",
  #                       "Flow fraction", and are extracted into separate vectors within this function.
  # updateProgress:       Function for updating a progress bar displayed to the user. Takes as input "value" (numeric) and "detail" (text)
  # 
  # This script is the controller for the RAVEN STREAM tool.
  # This tool analyzes chemical product use values upstream
  # from a wastewater treatment plant and calculates
  # risk values for high-end chemical spill and WWTP
  # contamination scenarios. The user is provided with a 
  # prioritization ranking of upstream chemicals and the
  # facilities they are used at which pose the highest risk
  # to the WWTP if spilled.
  #
  # Notes on Version History:
  #
  # Next steps:
  # - Fix facility names where there were errors in the original datasheet
  # - Revise/iterate warnings that indicate manual review is required to clean up formatted data
  # 
  #
  # S/N: Need to check with Marcus: For Max in Stock and Consumption/year, all zero or NA values are just placeholders for 
  #      actual use values from first row of product, right??
  # S/N: I have a general concern that some of the CAS numbers could possibly have been converted to dates somewhere along
  #      the line in the data compilation process.
  #  
  # Notes/some other things to do!: 
  #
  # Related to the Käppala upstream processor script:
  # After the script has been run, the user will likely wish to review the bulk output data to determine whether there are instances
  # where the reporting of a chemical concentration could be manually corrected so that the code can read it and thus
  # employ the corresponding chemical amounts in the final values (any chemicals without automatically-readable concentration
  # data are removed from analysis).This would likely need to be an iterative process of correcting concentration values and re-running code.
  #
  # Also, while an attempt was made to ensure that the facility names, where provided, in row two of the original bulk dataset matched the 
  # facility names throughout the rest of the data, there were several instances where the first facility had the name missing in the actual
  # data. Also, more importantly, there were several instances where anomalies occured, namely in "Bra Bil 3 anl", where no facility names
  # were provided, though they were sectioned off, so fake facility names were provided (...01, 02, and 03) and the "Vehicles other" sheet,
  # where additional facilities are present in the data relative to the row 2 list, and these are also out of order from the row 2 list (check with Marcus)
  # Further, may want to check that the first rows of the facility data aren't incorrectly labeled with the actual facility name in the row (e.g., chem. industries)
  #
  ###### Figure out where all Paragon 0 chemical usage values are coming from!!!
  #
  # S/N: This is what the WWTP parameter table columns should look like when input to the GUI (AS = activated sludge region, AD = anaerobic digestor)
  # STP_region_name           <- c("New_AS","Two_ADs","Old_AS")
  # STP_region_vol_m3         <- c(92430,18000,48606)
  # STP_region_flow_fractions <- c(0.64,1,0.36) # Fraction of influent that reaches each section of the plant.
  # 
  # Author: John Hader

  
  # User interface input checking and formatting  -----------------------------
  
  ############# These user input checks should be modified to direct to the GUI and 
  ############# inform the user about issues!!!

  # Increment the progress bar, and update the detail text.
  updateProgress(value=(2/5),detail="Checking input settings")
  
  # Ensures the scenario_label is a string.
  if(((is.character(scenario_label))&(length(scenario_label)==1))==FALSE){
    stop("Scenario label is not a single string!")
  }
  
  # Ensures user-defined region of interest is valid (i.e., either blank or matches named region in table).
  if(user_defined_plot_region==""){
    # Converts user-defined blank region to NA.
    user_defined_plot_region <- NA
    
  } else if(any(STP_region_table[[1]]%in%user_defined_plot_region)==TRUE){
    # Nothing is done if the user-defined region matches one from the user input table.
    
  } else{
    stop(paste0("User-defined region to analyze does not match any regions Region names from the STP parameter input table: ",user_defined_plot_region))
  }
  
  # Extracts rows of user input WWTP parameters, checks for validity, and assigns to separate vectors.
  if(ncol(STP_region_table)!=3){
    stop("Wastewater treatment plant parameter table was not set properly; three columns are required!.")
  }
  #
  # Region labels
  STP_region_name <- STP_region_table[[1]]
  if(is.character(STP_region_name)==FALSE){
    stop("Wastewater treatment plant region names not entered as strings.")
  }
  #
  # Region volumes
  STP_region_vol_m3 <- as.numeric(STP_region_table[[2]])
  if(any(is.na(STP_region_vol_m3))==TRUE){
    stop("Wastewater treatment plant region volume(s) not entered as numbers.")
  }
  #
  # Region flow fractions
  STP_region_flow_fractions <- as.numeric(STP_region_table[[3]])
  if(any(is.na(STP_region_flow_fractions))==TRUE){
    stop("Wastewater treatment plant region flow fraction(s) not entered as numbers.")
  }
  
  
  # Hard-coded analysis parameters: -----------------------------------------
  
  # Sewage Treatment Plant PNEC value data file definition.
  ECHA_STP_PNEC_data_file <- c("ECHA_STP_PNEC_Data.csv")  # Existing chemical property file names (as of 2023/03/06, this is just the ECHA STP PNEC data, stored on server-side after production by Hader). (prior to this date, other chemical datasets had been included).
  #
  # Miscellaneous analysis parameters.
  fac_to_remove        <- c("")    # Character vector of upstream facilities to exclude from analysis.
  MIS_thresh           <- 30       # Number above which all months-in-stock values will be set to 1+ this value (pretty sure this is just for plotting purposes, not any real calculations).
  MIS_adjust           <- TRUE     # Logical indicating whether or not the yearly usage chemical values should be adjusted to max-in-stock based on custom extrapolation method (as of 2023/01/08, hard-coding this as true).
  yearly_to_MIS_perc   <- c(0.25,0.5,0.75) # Sets percentile of # months product in stock used to adjust yearly-use data to max-in-stock data. As of 2023/01/08, these are being hard-coded as being 3 (lower, middle, and upper), but some code structure exists for only handling a central-tendency value).
  plot_title_size      <- 20
  plot_axes_size       <- 50
  
  
  # Pre-processing and user input error handling -------------------------

  
  # Ensure tank delineation/numbers align.
  if(all(c(length(STP_region_name),length(STP_region_vol_m3),length(STP_region_flow_fractions))==length(STP_region_flow_fractions))==FALSE){
    stop("Number of elements in the STP_region_name, STP_region_vol_m3, and STP_region_flow_fractions variables must be equal!")
  }
  
  # Ensure values don't exceed practical thresholds. (Still need to do this!)
  
  ############# End input checker function ####################
  
  # Establishes data table for tracking counts and warnings from data processing.
  process_tracking            <- as.data.table(matrix(data = NA,nrow = 5,ncol = 2))
  colnames(process_tracking)  <- c("Descriptor","Value") 
  process_tracking$Descriptor <- c("Num_prod_yrly_MIS_units_mismatch","X","X","X","X")
  
  ########### (Need to figure out how else this process_tracking thing is being used and how to implement it more!) ######
  
  # RAVEN STREAM processing-------------------------
  
  
  # Increment the progress bar, and update the detail text.
  updateProgress(value=(3/5),detail="Pre-processing input data")
  
  # Reads in the input STP PNEC data file and ensures all rows contain unique CAS numbers.
  CAS_chem_prop       <- fread(ECHA_STP_PNEC_data_file,stringsAsFactors=FALSE)
  if(length(unique(CAS_chem_prop$CAS_number))!=nrow(CAS_chem_prop)){
    stop("The ECHA STP PNEC data contains one or more repeated CAS numbers!")
  }

  # Calls the Upstream data pre-processing function.
  upstream_preprocess_out <- Upstream_data_preprocess(upstream_data,fac_to_remove,MIS_adjust,yearly_to_MIS_perc,MIS_thresh,scenario_label,process_tracking,plot_title_size,plot_axes_size)
  upstream_data           <- upstream_preprocess_out[[1]]
  year_to_MIS_adj         <- upstream_preprocess_out[[2]]
  process_tracking        <- upstream_preprocess_out[[3]]
  MIS_adjust_CDF          <- upstream_preprocess_out[[4]]
  MIS_adjust_hist         <- upstream_preprocess_out[[5]]
  MIS_output_table        <- upstream_preprocess_out[[6]]
  
  # Increment the progress bar, and update the detail text.
  updateProgress(value=(4/5),detail="Calculating exposures and risks")
  
  ##### Need to build in a user-defined plot region box in the GUI!!! (not here, but it will be used in function call below...)
  
  # Calls exposure/risk calculator and plotting function.
  plotting_output <- Exp_Risk_Plotting(upstream_data,CAS_chem_prop,yearly_to_MIS_perc,STP_region_name,STP_region_vol_m3,STP_region_flow_fractions,user_defined_plot_region,scenario_label,plot_title_size,plot_axes_size)
  
  # Builds the MIS adjust CDF and histogram, MIS summary output table, as well as summary data statistics, into the output bundle.
  final_output <- list(list(MIS_adjust_CDF,MIS_adjust_hist,MIS_output_table),plotting_output)
  
  # Informs user of successful completion.
  print("RAVEN STREAM has successfully completed running!")
  
  # Returns plotting output to Shiny.
  return(final_output)
  
}

# Processing of upstream data function.
Upstream_data_preprocess <- function(upstream_data,fac_to_remove,MIS_adjust,yearly_to_MIS_perc,MIS_thresh,scenario_label,process_tracking,plot_title_size,plot_axes_size){
  # This function pre-processes upstream chemical usage data and the database of phys-chem properties
  # The following actions are taken on the datasets:
  #
  # - If the user has chosen to, max-in-stock values are estimated* when missing for a product based on the 
  #   relationship between existing yearly-usage and max-in-stock values (finding the distribution and 
  #   applying a user-defined percentile).
  # - Masses of constituent chemicals (based on CAS numbers) are determined based on Max-in-stock values and
  #   % ingredient makeup.
  # - If the user has chosen to, volumes of chemicals are converted to masses (using CompTox TEST QSAR density).
  # 
  # *Notes regarding calculation of max-in-stock values from yearly usage:
  # - The yearly usage can be reported in Kg and/or L. Preference is given to the Kg reporting, and L will only
  #   be used if the Kg value is NA or 0 (though both may be 0 or NA).
  # - The max-in-stock amounts are divided by the yearly usage and multipled by 12. This is done regardless of 
  #   units for either of these values, effectively assuming the density of all products is 1kg/L for this calculation.
  #   While this assumption is not taken later if the user chooses to adjust volumes to masses by densities, this simplifies
  #   having to calculate the contribution to density of each constituent chemical or assuming a density of 
  #   the inactive ingredients.
  # - Before any adjustments to chemical amounts are done based on densities or %concentration
  #   of constituent CAS numbers, # of months that the maximum amount in stock corresponds to
  #   is calculated and stored (set to NA if not able to calculate). This is done for all rows
  #   of data, but will only be calculated for unique products prior to final analysis.
  #   Reason for doing this before density adjustment is that we are interested in the bulk product 
  #   amount held in stock and used annually. Taking weighted averages of the constituent chemical
  #   densities would require additional assumptions about the inactive ingredients of the mixture, and
  #   so simply using a density of 1g/ml of the bulk product is done to simplify things...
  #
  # Author: John Hader
  
  
  ##### Need to implement this code still!!! #####
  # Provides warning to user for instances where a MIS value is provided of zero (this is misleading if
  # the facility actually uses the chemical, and instead of using this value an estimated value of the MIS
  # will be generated if possible/if the user has chosen to do so)
  # These are taken as 'missing' values, and will have MIS values estimated based on yearly usage data if user choose to do that.
  
  ##### Need to implement this code still!!! #####
  # Checks for and warns for instances where both a yearly usage per year in kg as well as liters is
  # provided in the upstream usage data.
  
  ##### Need to implement this code still!!! #####
  # Removes rows containing data from facilities slated for removal by the user
  
  ##### Need to implement this code still!!! (or at least maybe) #####
  # Count number of instances where a product max (or min) conc sums are above 100%!
  
  
  # Conducts product-based max-in-stock estimation from yearly usage -------------------------------------------------- 
  
  
  # Assigns a unique product ID.
  upstream_data[,product_ID := paste0(upstream_data$Facility_category,"_",upstream_data$facility_name,"_",str_pad(upstream_data$product_number,3,pad="0"))]
  
  # Preliminary months-in-stock analysis.
  if(MIS_adjust==TRUE){
    # Selects which yearly usage value to use for each row.
    yearly_usage <- upstream_data[,apply(.SD,1,function(x) if((is.na(x[1]))|(x[1]==0)) x[2] else x[1]),
                                  .SDcols=c("Consumption_Kg_per_yr_full","Consumption_L_per_yr_full")]
    
    # Calculates months_in_stock based on yearly usage and max-in-stock.
    upstream_data[,months_in_stock := (upstream_data$Max_in_stock_amount_full/yearly_usage)*12]
    
    # Warns user of # products where units mismatch between available max-in-stock and yearly usage values.
    #
    # Extracts dataset with one row per unique product where months in stock was calcuable, non-zero, 
    # and finite (since use data is same for a given product, only want one row per product).
    #
    # Pulls out 1 row per product.
    single_prod_data    <- upstream_data[match(unique(upstream_data$product_ID),upstream_data$product_ID),]
    #
    # IDs and removes NA valued rows.
    months_in_stock_bad <- is.na(single_prod_data$months_in_stock)
    single_prod_data    <- single_prod_data[months_in_stock_bad != TRUE]
    #
    # IDs and removes infinite valued rows.
    months_in_stock_bad <- is.infinite(single_prod_data$months_in_stock)
    single_prod_data    <- single_prod_data[months_in_stock_bad != TRUE]
    #
    # IDs and removes zero-valued rows.
    months_in_stock_bad <- single_prod_data$months_in_stock==0
    single_prod_data    <- single_prod_data[months_in_stock_bad != TRUE]
    
    # Determines units of yearly value used.
    yearly_units <- single_prod_data[,apply(.SD,1,function(x) if((is.na(x[1]))|(x[1]==0)) "l" else "kg"),
                                     .SDcols=c("Consumption_Kg_per_yr_full","Consumption_L_per_yr_full")]
    #
    # Extracts indices where valid units for max-in-stock values found.
    valid_MIS_ind <- which((is.na(single_prod_data$Max_in_stock_unit_full)==FALSE)&(single_prod_data$Max_in_stock_unit_full!=""))
    #
    # Quick check that all rows of single_prod_data contained valid max-in-stock units.
    if(length(valid_MIS_ind)!=length(yearly_units)){
      stop("Unexpected error encountered when attempting to determine rows w/valid max-in-stock units and yearly units.")
    }
    #
    # Counts instances where units of yearly usage value employed do not match max-in-stock values.
    process_tracking$Value[which(process_tracking$Descriptor=="Num_prod_yrly_MIS_units_mismatch")] <- 
      length(which(tolower(yearly_units[valid_MIS_ind])!=tolower(single_prod_data$Max_in_stock_unit_full[valid_MIS_ind])))
    
  }
  
  
  # Upstream data density adjustment (multiplication by 1) and standardization of values to kg -------------------------------------------------- 
  
  
  # Sets value of density in upstream data as 1 g/cm3.
  upstream_data[,`Chem_dens_g/cm3` := rep(1,nrow(upstream_data))]

  # Standardizes yearly usage data to Kg/year
  #
  # Converts L/year column
  upstream_data[,Consumption_Kg_per_yr_full_converted := upstream_data$Consumption_L_per_yr_full*upstream_data$`Chem_dens_g/cm3`]
  #
  # Combines native kg/yr data and converted kg/yr data into final column.
  left_or_right <- upstream_data[,apply(.SD,1,function(x) if((is.na(x[1]))|(x[1]==0)) x[2] else x[1]),
                                 .SDcols=c("Consumption_Kg_per_yr_full","Consumption_Kg_per_yr_full_converted")]
  upstream_data[,Final_consumption_Kg_per_yr_full := left_or_right]
  
  # Standardizes Max-in-stock column w/units of liters.
  #
  # Adds new columns for adjusted max-in-stock amounts/units
  upstream_data[,Max_in_stock_Kg_full_converted   := upstream_data$Max_in_stock_amount_full]
  upstream_data[,Max_in_stock_unit_full_converted := upstream_data$Max_in_stock_unit_full]
  #
  # Determines rows where Max-in-stock units are liters.
  MIS_liters_ind <- which(tolower(upstream_data$Max_in_stock_unit_full)=="l")
  #
  # If there are any rows with MIS values in liters, converts to Kg.
  if(length(MIS_liters_ind)>0){
    # Converts from L to Kg.
    upstream_data$Max_in_stock_Kg_full_converted[MIS_liters_ind] <-
      upstream_data$`Chem_dens_g/cm3`[MIS_liters_ind]*
      upstream_data$Max_in_stock_amount_full[MIS_liters_ind]
    
    # Replaces MIS units of 'l' with 'kg' where density conversion has occured.
    upstream_data$Max_in_stock_unit_full_converted[MIS_liters_ind] <- "kg"
  }
  
  
  # Months-in-stock product amount histogram -----------------------------
  
  
  # Only conducts months-in-stock analysis if user has selected to do so.
  if(MIS_adjust==TRUE){
    
    # Cleans up distribution of # months max-held-in-stock amounts across products and facilities.
    #
    # Ensures only one value of months_in_stock parameter is present for each product_ID (just a sanity check).
    MIS_unique_check <- upstream_data[,length(unique(months_in_stock)), by=c("product_ID")]
    if((all(MIS_unique_check$V1==1)==FALSE)){
      stop("Error encountered calculating months-in-stock: More than one unique value per month reported for at least one unique product.")
    }
    
    # Using the "single_prod_data" table from earlier, stores num products for later use.
    num_prods_MIS_Yrly_dist <- nrow(single_prod_data)
    
    # Generates table storing the explicit number of months in stock for all percentiles
    # 0-100 by 5% (for output to the user).
    MIS_output_table_vals   <- (seq(from=0,to=100,by=5))/100
    MIS_output_table_labels <- as.character(MIS_output_table_vals*100)
    MIS_output_table        <- data.table(Percentile=MIS_output_table_labels, Months_in_Stock = quantile(single_prod_data$months_in_stock,probs = MIS_output_table_vals,type = 4,names = FALSE))
    
    # Determines adjustment value of # months in stock to assume for yearly-only data
    # ("type" indicates linear interpolation of the empirical cdf).
    year_to_MIS_adj <- quantile(single_prod_data$months_in_stock,probs = yearly_to_MIS_perc,type = 4,names = FALSE)
    
    # Establishes the font style to use.
    #windowsFonts(Times=windowsFont("Times New Roman"))
    
    # Generates copy of single_prod_data just for plotting the CDF (otherwise, this plot will 
    # be modified when the data.table has values truncated to the MIS_thresh) because of an 
    # oddity in data.table.
    CDF_plotting_single_prod_data <- copy(single_prod_data)
    
    # CDF generation/plotting prior to artificially setting upper bound on histogram.
    p_CDF <- ggplot(CDF_plotting_single_prod_data, aes(months_in_stock)) + stat_ecdf(geom = "step")
    p_CDF <- p_CDF + labs(title=paste0("CDF # Months Product in Stock; N=",num_prods_MIS_Yrly_dist), x="# Months", y="Cumulative frac.")
    p_CDF <- p_CDF + theme_bw() + theme(axis.text    = element_text(size=plot_axes_size),
                                        axis.title   = element_text(size=plot_axes_size),
                                        plot.title   = element_text(size=plot_title_size,hjust = 0.5),
                                        panel.grid   = element_blank(),
                                        panel.border = element_blank(),axis.line.y.left=element_line(size=1),axis.line.x.bottom=element_line(size=1),
                                        text = element_text(family = "Times"))

    # Adjusts all values of months-in-stock to be just above user-defined threshold (for plotting histogram).
    single_prod_data <- single_prod_data[months_in_stock>MIS_thresh,months_in_stock:=(MIS_thresh+1)]
    
    # Establishes the font style to use.
    #windowsFonts(Times=windowsFont("Times New Roman"))
    
    # Plotting of months-in-stock histogram.
    p_hist <- ggplot(data=single_prod_data, aes(single_prod_data$months_in_stock))
    p_hist <- p_hist + geom_histogram(breaks=seq(0,(MIS_thresh+1), by=1),closed="right",col="black",fill="cadetblue3") + scale_y_continuous(expand = c(0, 0))
    p_hist <- p_hist + labs(title=paste0("# Months Product in Stock; N=",num_prods_MIS_Yrly_dist," (values >",MIS_thresh," artificially set)"), x="# Months", y="Count")
    p_hist <- p_hist + xlim(c(0,(MIS_thresh+1))) + theme_bw()
    p_hist <- p_hist + theme(axis.text    = element_text(size=plot_axes_size),
                             axis.title   = element_text(size=plot_axes_size),
                             plot.title   = element_text(size=plot_title_size,hjust = 0.5),
                             panel.grid   = element_blank(),
                             panel.border = element_blank(),axis.line.y.left=element_line(size=1),axis.line.x.bottom=element_line(size=1),
                             text = element_text(family = "Times"))
    
    # Adds column(s) for final max-in-stock values, using reported if available and if not estimated.
    #
    # Column initialization
    yrl_MIS_pctl_str <- as.character((yearly_to_MIS_perc*100))
    upstream_data[,paste0("Final_MIS_Kg_",yrl_MIS_pctl_str,"_pctl") := as.data.table(apply(as.data.frame(year_to_MIS_adj),1,function(x) x*(upstream_data$Final_consumption_Kg_per_yr_full/12)))]
    # 
    # Overwriting of estimated MIS values with reported values where possible.
    #
    # Selection of estimated MIS index to use as the 'central' value comparable to any reported MIS values.
    if(length(yearly_to_MIS_perc)==1){
      # If there is only one percentile value, the corresponding single column name is used.
      MIS_est_cent_ind <- which(colnames(upstream_data)==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str,"_pctl"))
      remove_MIS_bounds <- FALSE # Flag for removing the upper/lower MIS estimates from instances where reported values exist.
      
    } else if(length(yearly_to_MIS_perc)==3){
      # If there are three percentile values, the 2nd corresponding column name is used.
      MIS_est_cent_ind <- which(colnames(upstream_data)==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[2],"_pctl"))
      
      # Establishes info needed to remove the upper/lower MIS estimates from instances where reported values exist.
      remove_MIS_bounds <- TRUE 
      MIS_est_low_ind   <- which(colnames(upstream_data)==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[1],"_pctl"))
      MIS_est_high_ind  <- which(colnames(upstream_data)==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[3],"_pctl"))
      
      # Error catch if yearly_to_MIS_perc parameter was set incorrectly (should never happen since checked earlier).
    } else{
      stop("The 'yearly_to_MIS_perc' parameter must have either 1 or 3 values!")
    }
    #
    # Overwrites MIS estimates with reported values. 
    good_MIS_ind                                 <- which((is.na(upstream_data$Max_in_stock_Kg_full_converted)==FALSE)&(upstream_data$Max_in_stock_Kg_full_converted!=0))
    upstream_data[good_MIS_ind,MIS_est_cent_ind] <- upstream_data$Max_in_stock_Kg_full_converted[good_MIS_ind]
    #
    # Inserts flags for MIS values (legacy notation: 1 = reported, 2 = estimated).
    upstream_data[,MIS_flag := rep("Estimated",nrow(upstream_data))]
    upstream_data$MIS_flag[good_MIS_ind] <- "Reported"
    #
    # For reported MIS values, removes upper and lower bound estimates if present.
    if(remove_MIS_bounds==TRUE){
      upstream_data[good_MIS_ind,MIS_est_low_ind]  <- NA
      upstream_data[good_MIS_ind,MIS_est_high_ind] <- NA
    }
    
  }
  
  # CAS mass determination and data return -----------------------------
  
  # Calculates mass of each CAS in products
  #
  # Minimum conc values.
  upstream_data[,paste0("Final_MIS_Kg_",yrl_MIS_pctl_str,"_pctl_","min_CAS") := 
                  upstream_data$min_conc_frac*(upstream_data[,paste0("Final_MIS_Kg_",yrl_MIS_pctl_str,"_pctl"),with=FALSE])]
  #
  # Maximum conc values.
  upstream_data[,paste0("Final_MIS_Kg_",yrl_MIS_pctl_str,"_pctl_","max_CAS") := 
                  upstream_data$max_conc_frac*(upstream_data[,paste0("Final_MIS_Kg_",yrl_MIS_pctl_str,"_pctl"),with=FALSE])]
  
  
  # Organizes analysis parameters, data, and MIS histogram/CDF figures for return.
  final_output <- list(upstream_data,year_to_MIS_adj,process_tracking,p_CDF,p_hist,MIS_output_table)
  return(final_output)
  
}

# Exposure and risk calculation and plotting function.
Exp_Risk_Plotting <- function(upstream_data,CAS_chem_prop,yearly_to_MIS_perc,STP_region_name,STP_region_vol_m3,STP_region_flow_fractions,user_defined_plot_region,scenario_label,plot_title_size,plot_axes_size){
  # Function for calculating exposure and risk to the STP region of interest, and plotting results.
  # The code conducts the following steps:
  # - Pulls out a data table of chemical properties where just the valid PNEC values are present.
  # - Matches the Min and Max PNEC values to the upstream data based on CAS_number
  # - Calculates exposure and risk for the high-end risk assumptions (i.e., lowest PNEC,
  #   max CAS mass, lowest dilution compartment or user-defined region)
  # - Plots scatter plot of exposure vs. risk (points differentiated by real versus estimated MIS).
  # - Generates prioritization list of compounds (both constituent chemicals as well as full products).
  # - Keeps track of counts of important classifications (e.g., num unique products, num chems risk >=1, etc.).
  #
  # Author: John Hader
  
  
  # Custom-made functions ------------------------------------------------
  
  # Function for calculating the % of non-NA values present in a vector. Intended for use to
  # calculate % of constituent chemicals for which valid risk data exists in a given product.
  CAS_perc_calc <- function(input_data){
    # Calculates percentage of input vector that comprises non-NA values.
    non_NA_perc <- (length(input_data[is.na(input_data)==FALSE])/length(input_data))*100
    return(non_NA_perc)
    
  }
  
  # Function for calculating statistics of upstream chem data.
  data_stats_prioritization <- function(PNEC_Label,CAS_label,STP_region,constituent_data,na_risk_rows,inf_risk_rows,zero_risk_rows,product_data,final_chem_stats,row_counter){
    
    # Stores off current iteration subset info.
    final_chem_stats$PNEC[row_counter]       <- PNEC_Label
    final_chem_stats$CAS_conc[row_counter]   <- CAS_label
    final_chem_stats$STP_region[row_counter] <- STP_region
    
    # Stores off total, NA, Inf, and 0 stats from constituent chems.
    final_chem_stats$Central_Chems_valid_count[row_counter] <- nrow(constituent_data)
    final_chem_stats$Central_Chems_NA[row_counter]          <- length(which(na_risk_rows==TRUE))
    final_chem_stats$Central_Chems_Inf[row_counter]         <- length(which(inf_risk_rows==TRUE))
    final_chem_stats$Central_Chems_Zero[row_counter]        <- length(which(zero_risk_rows==TRUE))
    
    # Stores off numbers of products and chemicals from constituent chems database.
    final_chem_stats$Central_Chems_Unique_CAS[row_counter]  <- length(unique(constituent_data$CAS_number))
    final_chem_stats$Central_Chems_Prods[row_counter]       <- length(unique(constituent_data$product_ID))
    
    # Stores off numbers of MIS reporting types and risks over 1 for constituent chemicals.
    final_chem_stats$Central_Chems_CAS_MIS_reported[row_counter] <- length(which(constituent_data$MIS_flag==1))
    final_chem_stats$Central_Chems_CAS_MIS_estimate[row_counter] <- length(which(constituent_data$MIS_flag==2))
    final_chem_stats$Central_Chems_CAS_over_one[row_counter]     <- length(which(constituent_data$Cent_Risk>=1))
    #
    # Stores off number of constituent chemicals with risk over 1 for upper and lower estimates.
    final_chem_stats$Lower_Chems_CAS_over_one[row_counter] <- length(which(constituent_data$Low_Risk>=1))
    final_chem_stats$Upper_Chems_CAS_over_one[row_counter] <- length(which(constituent_data$High_Risk>=1))
    
    # Stores off numbers of MIS reporting types and risks over 1 for products.
    final_chem_stats$Central_Chems_Prods_MIS_reported[row_counter] <- length(which(product_data$MIS_flag==1))
    final_chem_stats$Central_Chems_Prods_MIS_estimate[row_counter] <- length(which(product_data$MIS_flag==2))
    final_chem_stats$Central_Chems_Prods_over_one[row_counter]     <- length(which(product_data$Cent_Risk>=1))
    #
    # Stores off number of products with risk over 1 for upper and lower estimates.
    final_chem_stats$Lower_Chems_Prods_over_one[row_counter] <- length(which(product_data$Low_Risk>=1))
    final_chem_stats$Upper_Chems_Prods_over_one[row_counter] <- length(which(product_data$High_Risk>=1))
    
    # Returns the final_chem_stats object so that the newly added data overwrites previous data.
    return(list(final_chem_stats,constituent_data[order(constituent_data$Cent_Risk,decreasing = TRUE),],product_data[order(product_data$Cent_Risk,decreasing = TRUE),]))
    
  }
  
  # Function for plotting scatterplot of two variables over log-space.
  log_plot <- function(plotted_data,error_switch,risk_over_one,legend_title,flag_meaning,x_label,y_label,fig_title){
    # Notes on function use:
    #
    # - Generates log scatterplot of input data exposure and risk values, optionally with 'error' bars along the y axis.
    # - Required column names in "plotted_data" variable are:
    #    - "MIS_flag": Values of 1 or 2 indicating differing data type (i.e., whether Max-in-stock was reported or estimated).     
    #    - "Cent_Exp": Central-tendency exposure values; this will be either the single reported MIS value, or the single or middle-value estimated MIS exposure value.
    #    - "Cent_Risk": Central-tendency exposure values; this will be either the single reported MIS value, or the single or middle-value estimated MIS exposure value.
    #    - "Low_Exp", "High_Exp","Low_Risk", and "High_Risk": Optional parameters required if "error_switch" is True, provides the lower and upper MIS exposure and risk values.
    # - error_switch: parameter is a boolean indicating whether to implement error bars on scatter points.
    # - risk_over_one: parameter is a boolean indicating whether or not the generated plot should have the Y axis lower limit set to
    #   be just below a risk value of 1 (i.e., to avoid visualizing messy values of low interest).
    # - legend_title: Title to be applied to the legend of the figure.
    # - flag_meaning: parameter is a vector of strings for labels explaining the "MIS_flag" parameters in the legend.
    # - x_label & y_label: X and Y axis labels, respectively
    # - fig_title: Title to be applied to the figure.
    # - Much of code from: https://www.datanovia.com/en/blog/ggplot-log-scale-transformation/
    
    # Sets ggplot2 to be the operating plotting package.
    library(ggplot2)
    
    # Sets the color and shape scheme to be used within the scatterplot.
    plot_colors <- c("orchid1","springgreen4")
    plot_shapes <- c(22,21)
    
    # Derives good bounds to use for the final plot, which must be done differently for 
    # normal versus uncertainty plotting.
    if(error_switch==TRUE){
      bounds <- rbind(log10(c(min(plotted_data$Low_Exp),max(plotted_data$High_Exp))),log10(c(min(plotted_data$Low_Risk),max(plotted_data$High_Risk))))
      if(any(is.infinite(bounds))==TRUE){stop("Zero value of exposure and/or risk found in final plotting data!")}
      bounds[,1] <- bounds[,1]-0.5
      bounds[,2] <- bounds[,2]+0.5
      bounds      <- rbind(10^c(min(bounds),max(bounds)),10^c(min(bounds),max(bounds)))
      
      # For if error bars will not be plotted.
    } else if(error_switch==FALSE){
      bounds <- rbind(log10(range(plotted_data$Cent_Exp)),log10(range(plotted_data$Cent_Risk)))
      if(any(is.infinite(bounds))==TRUE){stop("Zero value of exposure and/or risk found in final plotting data!")}
      bounds[,1] <- bounds[,1]-0.5
      bounds[,2] <- bounds[,2]+0.5
      bounds      <- rbind(10^c(min(bounds),max(bounds)),10^c(min(bounds),max(bounds)))
      
      # Generic error catch.
    } else{
      stop("The error_switch parameter must be either TRUE or FALSE.")
    }
    
    # Adjusts the axis bounds if user selected to focus on risk values over 1.
    if(risk_over_one==TRUE){
      
      # If error bars are not being plotted, sets the y-value min bound to 0.1 ( i.e., -1 in log10 space),
      # and does not do anything to the bulk data to be plotted.
      if(error_switch==FALSE){
        bounds[2,1] <- 1.0E-01
        
        # If the error bars are being plotted, determines the minimum possible Y-bound value that will still
        # allow all chemicals that have a risk uncertainty range exceeding 1 to be plotted fully.
        # And also, removes all data points from the data to be plotted that do not have at least the 
        # high-end risk exceed 1 (this makes it substantially easier to read the plot).
      } else{
        # Identifies the row(s) with the lowest high risk value greater than 1,
        # and the corresponding low risk values
        over_one_high_risk_ind     <- which(plotted_data$High_Risk>1)
        low_risk_for_over_one_high <- plotted_data$Low_Risk[over_one_high_risk_ind]
        
        # Sets the Y-min bound to that of a buffered (0.2)
        # log10 value of the lowest low-risk value with the corresponding high risk value over 1.
        log10_lowest_low_risk <- log10(min(low_risk_for_over_one_high))-0.2
        bounds[2,1]           <- (10^log10_lowest_low_risk)
        
        # Only retains the rows in the plotted_data that have at least a high-end risk value over 1.
        plotted_data <- plotted_data[over_one_high_risk_ind,]
        
      }
    }
    
    # Overwrites the MIS number flag with the user-defined labels.
    plotted_data$MIS_flag[plotted_data$MIS_flag==1] <- flag_meaning[1]
    plotted_data$MIS_flag[plotted_data$MIS_flag==2] <- flag_meaning[2]
    
    # Establishes the font style to use.
    #windowsFonts(Times=windowsFont("Times New Roman"))
    
    # Establishes the main part of the scatterplot, differentiating markers based on the input "MIS_flag" variable.
    p <- ggplot(plotted_data, aes(x=Cent_Exp, y=Cent_Risk, fill=as.character(MIS_flag), shape=as.character(MIS_flag), colour = as.character(MIS_flag)))
    p <- p + geom_point(size=3) + theme(legend.position="right") + 
         scale_shape_manual(values=plot_shapes) + scale_fill_manual(values=plot_colors) + scale_colour_manual(values=plot_colors)
    
    # For plotting uncertainty/error bars.
    if(error_switch==TRUE){
      
      # Adds vertical and horizontal error bars.
      p <- p + geom_errorbar(aes(ymin = Low_Risk,ymax = High_Risk))
      p <- p + geom_errorbarh(aes(xmin = Low_Exp, xmax = High_Exp))
      
    }
    
    # Converts axis to log10 space.
    p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)),limits=bounds[1,])
    p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)),limits=bounds[2,])
    
    # Adds axis and legend labels and completes format cleaning.  
    p <- p + theme_bw() + annotation_logticks(size=1,short = unit(0.2,"cm"),mid = unit(0.4, "cm"),long = unit(0.6,"cm")) + labs(x=x_label,y=y_label) + ggtitle(fig_title) 
    p <- p + theme(axis.text    = element_text(size=plot_axes_size),
                   axis.title   = element_text(size=plot_axes_size),
                   plot.title   = element_text(size=plot_title_size,hjust = 0.5),
                   panel.grid   = element_blank(),
                   panel.border = element_blank(),
                   axis.line.y.left=element_line(size=1.5),axis.line.x.bottom=element_line(size=1.5),
                   legend.title=element_blank(),
                   text = element_text(family = "Times"))
    
    # Returns plot.
    return(p)
    
  }
  
  # Function for calculating/plotting the % of constituent chemicals for which final risk values can be caluclated that
  # are ultimately used to calculate additive risk of the products.
  product_fraction_plot <- function(upstream_chemicals_data,summed_product_data,risk_over_one,legend_title,flag_meaning,x_label,y_label,fig_title){
    ############ Note: Should probably not analyze these figures for min CAS numbers, since these would have some chemicals missing due to 
    ############ values of 0 % inclusion being present on the minimum side!
    
    # Sets the size and shape scheme to be used within the scatterplot.
    point_size  <- 5
    plot_shapes <- c(22,21)
    
    ########## Data Organization and Calculation ##############
    
    
    # Determines column of risk values to use for constituent chemical counting.
    if(length(yearly_to_MIS_perc)==1){
      const_chem_ind <- which(colnames(upstream_chemicals_data)==paste0(STP_region_RS_plot,"_Risk_",yrl_MIS_pctl_str[1],"_pctl_",CAS_conc_label,"_",PNEC_Label))
      
    } else if(length(yearly_to_MIS_perc)==3){
      const_chem_ind <- which(colnames(upstream_chemicals_data)==paste0(STP_region_RS_plot,"_Risk_",yrl_MIS_pctl_str[2],"_pctl_",CAS_conc_label,"_",PNEC_Label))
      
      # Redundant error catch (should never happen).
    } else{
      stop("Unexpected error: yearly_to_MIS_perc is not of length either 1 or 3!")
    }
    
    # Checks for Inf values in the risk values column (this would imply toxicity values of zero).
    if(any(is.infinite(upstream_chemicals_data[[const_chem_ind]]))){
      stop(paste0("RIsk values of Infinity found when attempting to calculate % constituent chemicals per product for: ",fig_title))
    }
    
    # Calculates the % of constituent chemicals within each product for which non-NA risk values could be calculated.
    # Note that the 'get' call here is required to extract the actual column of data from the data table.
    perc_const_chem <- upstream_chemicals_data[,CAS_perc_calc(get(colnames(upstream_chemicals_data)[const_chem_ind])),
                                               by=product_ID]
    
    
    ########## Data Checks ##############
    
    
    # Ensures there are only unique product IDs in the % of constituent chemicals data and the summed product data
    # (this is required for the merging steps to work properly).
    if((length(unique(perc_const_chem$product_ID))!=nrow(perc_const_chem)) | (length(unique(summed_product_data$product_ID))!=nrow(summed_product_data))){
      stop(paste0("Error encountered before mergning % constituent chemicals data w/summed product data; non-unique product IDs identified for: ",fig_title))
    }
    
    # Ensures all product IDs from the summed product data are present in the % constituent chemicals data.
    perc_chem_test   <- perc_const_chem$product_ID
    summed_prod_test <- summed_product_data$product_ID
    if(length(which((summed_prod_test%in%perc_chem_test)==FALSE))!=0){
      stop(paste0("Error encountered in calculation of % constituent chemicals in product risk values (some IDs missing) in: ",fig_title))
    }
    
    # Merges the % of constituent chemical data with the summed product data. Note the "all.x" ensures that all
    # summed_product_data rows are included, but not all perc_const_chem if there are any 'extras' that don't have valid summed risk values.
    library(data.table)
    plotting_summed_product_data <- merge(summed_product_data,perc_const_chem, by="product_ID",all.x=TRUE)
    
    # Renames column of data containing % constituent chemicals.
    colnames(plotting_summed_product_data)[which(colnames(plotting_summed_product_data)=="V1")] <- "Perc_chems"
    
    
    ########## Data Plotting ##############
    
    
    # Sets ggplot2 to be the operating plotting package.
    library(ggplot2)
    
    # Derives good bounds to use for the final plot.
    bounds <- rbind(log10(range(plotting_summed_product_data$Cent_Exp)),log10(range(plotting_summed_product_data$Cent_Risk)))
    if(any(is.infinite(bounds))==TRUE){stop("Zero value of exposure and/or risk found in final plotting data!")}
    bounds[,1] <- bounds[,1]-0.5
    bounds[,2] <- bounds[,2]+0.5
    bounds      <- rbind(10^c(min(bounds),max(bounds)),10^c(min(bounds),max(bounds)))
    
    # Adjusts the axis bounds if user selected to focus on risk values over 1.
    if(risk_over_one==TRUE){
      # Minimum Y-value in the bounds set instead to 0.01 (i.e., -1 in log10 space)
      bounds[2,1] <- 1.0E-01
      
      # Sets point size to something more visible with the closer-in zoom.
      point_size <- 8
      
    }
    
    # Overwrites the MIS number flag with the user-defined labels.
    plotting_summed_product_data$MIS_flag[plotting_summed_product_data$MIS_flag==1] <- flag_meaning[1]
    plotting_summed_product_data$MIS_flag[plotting_summed_product_data$MIS_flag==2] <- flag_meaning[2]
    
    # Establishes the font style to use.
    #windowsFonts(Times=windowsFont("Times New Roman"))
    
    # Establishes the main part of the scatterplot, differentiating markers based on the input "MIS_flag" variable.
    p <-  ggplot(data=plotting_summed_product_data,aes(x=Cent_Exp, y=Cent_Risk, fill=Perc_chems,shape=as.character(MIS_flag),colour = Perc_chems)) + 
      geom_point(size=point_size) + scale_shape_manual(values=plot_shapes)  + 
      scale_fill_viridis(discrete=FALSE,limits = c(0,100)) + scale_colour_viridis(discrete=FALSE,limits = c(0,100)) + theme(legend.position="right")
    
    # Converts axis to log10 space.
    p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)),limits=bounds[1,])
    p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)),limits=bounds[2,])
    
    # Adds axis and legend labels and completes format cleaning.
    p <- p + theme_bw() + annotation_logticks(size=1,short = unit(0.2,"cm"),mid = unit(0.4, "cm"),long = unit(0.6,"cm")) + labs(x=x_label,y=y_label) + ggtitle(fig_title)
    p <- p + theme(axis.text    = element_text(size=plot_axes_size),
                   axis.title   = element_text(size=plot_axes_size),
                   plot.title   = element_text(size=plot_title_size,hjust = 0.5),
                   panel.grid   = element_blank(),
                   panel.border = element_blank(),
                   axis.line.y.left=element_line(size=1.5),axis.line.x.bottom=element_line(size=1.5),
                   legend.title=element_blank(),
                   text = element_text(family = "Times"))
    
    
    # Calculates total number of products, and number of these products above/below the 75% Perc chemicals level.
    total_prods               <- nrow(plotting_summed_product_data)
    prods_under_75            <- length(which(plotting_summed_product_data$Perc_chems<75))
    prods_75_and_over         <- length(which(plotting_summed_product_data$Perc_chems>=75))
    near_risk_prod_data       <- plotting_summed_product_data[(Cent_Risk>=0.1)&(Cent_Risk<1)]
    num_near_risk_prods       <- nrow(near_risk_prod_data)
    near_risk_prods_under_100 <- length(which(near_risk_prod_data$Perc_chems<100))
    
    # Combines count information into data table for export.
    counts_labels          <- c("Num_uncert_prods","Num_uncert_prods_under_75","Num_uncert_prods_75_and_over","Num_uncert_prods_0.1_to_1.0","Num_uncert_prods_0.1_to_1.0_under_100")
    counts_values          <- c(total_prods,prods_under_75,prods_75_and_over,num_near_risk_prods,near_risk_prods_under_100)
    prod_uncertainty_stats <- data.table(Metric = counts_labels,Value = counts_values,stringsAsFactors = FALSE)
    
    # Returns figure and counts of product uncertainty information.
    return(list(p,prod_uncertainty_stats))
    
  }
  
  
  # Data pre-processing and exp/risk calculations ---------------------------
  
  
  # Generates strings for MIS percentile labels.
  yrl_MIS_pctl_str <- as.character((yearly_to_MIS_perc*100))
  
  ######## Note: For pulling out STP PNEC values, the "N/A" was manually set by me for instances where there were no issues with the PNEC. Instances of NA are natural and won't be captured here.
  
  # Ensures CAS numbers in the chem. prop dataset are unique.
  if(length(CAS_chem_prop$CAS_number)!=length(unique(CAS_chem_prop$CAS_number))){
    stop("Non-unique CAS numbers detected in the Chemical Property data while conducting Exposure and Risk plotting!")
  }
  
  # Generates separate chem prop dataset with only valid STP PNEC values.
  valid_STP_PNEC_data <- CAS_chem_prop[,c("CAS_number","min_STP_PNEC_mg/L","max_STP_PNEC_mg/L","STP_PNEC_flag"),with=FALSE]
  valid_STP_PNEC_data <- valid_STP_PNEC_data[(STP_PNEC_flag=="Bad_value_repaired")|(STP_PNEC_flag=="Hazard_unlikely/not_identified")|(STP_PNEC_flag=="N/A")]
  
  # Initializes the new columns of chem. prop data into the chem usage data.
  upstream_data[,`min_STP_PNEC_mg/L` := rep(NA,nrow(upstream_data))]
  upstream_data[,`max_STP_PNEC_mg/L` := rep(NA,nrow(upstream_data))]
  
  # Extracts indices for matching rows between usage and property CAS numbers.
  STP_PNEC_ind                                              <- match(upstream_data$CAS_number,valid_STP_PNEC_data$CAS_number)
  chem_usage_CAS_matches                                    <- which(is.na(STP_PNEC_ind)==FALSE)
  upstream_data$`min_STP_PNEC_mg/L`[chem_usage_CAS_matches] <- valid_STP_PNEC_data$`min_STP_PNEC_mg/L`[STP_PNEC_ind[chem_usage_CAS_matches]]
  upstream_data$`max_STP_PNEC_mg/L`[chem_usage_CAS_matches] <- valid_STP_PNEC_data$`max_STP_PNEC_mg/L`[STP_PNEC_ind[chem_usage_CAS_matches]]
  
  #  Ensures column names of upstream data are unique (required for proper functioning of code below).
  if(length(colnames(upstream_data))!=length(unique(colnames(upstream_data)))){
    stop("Non-unique column names deteted in upstream chemical use data while conducting Exposure and Risk plotting!")
  }
  
  ############### Important note!! #################
  #
  # Several (over 100) instances of a chemical usage have been found
  # that are the same CAS number for a given product. For at least some
  # of these, the reported min/max conc value is different. SHould probably
  # treat these as valid entries, but should screen for instances of these chemicals
  # and ensure that all instances have this differential reporting characteristic.
  # If there are any instances where they are complete copies of eachother, this may 
  # still be a valid entry, but would probably want to check...?
  #
  # Also importantly, will probably want to reflect on how this may impact calculation
  # of the MIS adjustment histogram, and anything else that numbers products specially,
  # since these potentially unique instances of product usage would actually be being
  # reported as one product (which is determined now as a unique combination of facility and product).
  #
  # Actually, upon more thought, I think just leaving these data as they are is fine.
  # Ultimately, if the user sees an anomaly like this in the higher-end risk screening (which is the case
  # for us!!!) this is just a good example of a place where more information gathering is needed
  # to understand how this chemical is actually stored/get better information from the upstream facility!
  #
  # However, may want to change way of counting products to those with same facility type, facility name, product name,
  # and usage mass/vol reporting information (if this last one is different, then this is a different use of the chemical)
  # (possibly just do here, but if wildly different will probably want to reconsider this approach for the MIS adjustment calcs)
  # 
  # From a quick analysis (see code below), it appears that there are 5 extra instances when concatenating the full product information (i.e., names + usage patterns) 
  # compared to just concatenating the names. For now, will go with simplified way of counting, but should adjust in next round so that
  # product numbering is assigned based on facility cat + name + product name + mass/vol used (also with histogram calculations).
  #
  # upstream_data_counting$Product_counting       <- paste0(upstream_data_counting$Facility_category,"_",upstream_data_counting$facility_name,"_",upstream_data_counting$product_name)
  # upstream_data_counting$Product_counting_full  <- paste0(upstream_data_counting$Facility_category,"_",upstream_data_counting$facility_name,"_",upstream_data_counting$product_name,"_",upstream_data_counting$Max_in_stock_amount_full,"_",upstream_data_counting$Max_in_stock_unit_full,"_",upstream_data_counting$Consumption_L_per_yr_full,"_",upstream_data_counting$Consumption_Kg_per_yr_full)
  # length(unique(upstream_data_counting$Product_counting_full))
  # length(unique(upstream_data_counting$Product_counting))
  #
  # Update on 2023/03/08: From discussion with Matt, just use the data as they are, don't adjust any of this!
  #
  ##################################################
  
  
  # Conducts count of a number of miscellaneous items needed for reporting in paper.
  num_industries   <- length(unique(paste0(upstream_data$Facility_category,"_",upstream_data$facility_name)))
  num_unique_prods <- length(unique(upstream_data$product_name))
  num_unique_chems <- length(unique(upstream_data$CAS_number))
  num_valid_PNEC   <- nrow(valid_STP_PNEC_data)
  num_tox_PNEC     <- length(which(is.infinite(valid_STP_PNEC_data$`min_STP_PNEC_mg/L`)==FALSE))
  num_non_tox_PNEC <- length(which(is.infinite(valid_STP_PNEC_data$`min_STP_PNEC_mg/L`)==TRUE))
  num_range_PNEC   <- length(which(apply(valid_STP_PNEC_data[,c("min_STP_PNEC_mg/L","max_STP_PNEC_mg/L"),with=FALSE],1,function(x) x[1]==x[2])==FALSE))
  
  ######## Consider making this a function #################
  
  
  # Develops the overall and industry-specific statistics of chemical/product data availability.
  #
  # Copies upstream data for below caluclations.
  upstream_data_counting <- copy(upstream_data)
  #
  # Adds on column joining facility category, name, product name (and another col like this with CAS number too).
  upstream_data_counting$Product_counting  <- paste0(upstream_data_counting$Facility_category,"_",upstream_data_counting$facility_name,"_",upstream_data_counting$product_name)
  upstream_data_counting$Chemical_counting <- paste0(upstream_data_counting$Facility_category,"_",upstream_data_counting$facility_name,"_",upstream_data_counting$product_name,"_",upstream_data_counting$CAS_number)
  #
  # Counts overall total number of products, constituent chemicals (i.e., individual uses),
  # and numbers of chemicals with various STP PNEC value classifications.
  total_num_products       <- length(unique(upstream_data_counting$Product_counting))
  total_num_chems          <- nrow(upstream_data_counting)
  total_chems_toxic_PNEC   <- length(which(((!is.infinite(upstream_data_counting$`min_STP_PNEC_mg/L`))&(!is.na(upstream_data_counting$`min_STP_PNEC_mg/L`)))==TRUE))
  total_chems_non_tox_PNEC <- length(which(is.infinite(upstream_data_counting$`min_STP_PNEC_mg/L`)==TRUE))
  total_chems_missing_PNEC <- length(which(is.na(upstream_data_counting$`min_STP_PNEC_mg/L`)==TRUE))
  #
  # Performs same counts as above, but on per-category basis.
  nums_by_cat_01 <- upstream_data_counting[,.(Prod_num = length(unique(Product_counting)),Chem_num = .N,
                                                Toxic_PNEC_num     = length(which(((!is.infinite(`min_STP_PNEC_mg/L`))&(!is.na(`min_STP_PNEC_mg/L`)))==TRUE)),
                                                Non_toxic_PNEC_num = length(which(is.infinite(`min_STP_PNEC_mg/L`)==TRUE)),
                                                Missing_PNEC_num   = length(which(is.na(`min_STP_PNEC_mg/L`)==TRUE))),by=Facility_category]
  #
  # Counts numbers of products with max-in-stock vs. yearly usage reporting.
  #
  # Determines and extracts only rows where first instance of unique product is found.
  first_prod_data_counting <- upstream_data_counting[match(unique(upstream_data_counting$Product_counting),upstream_data_counting$Product_counting),]
  first_prod_data_counting <- first_prod_data_counting[,c("Facility_category","Max_in_stock_amount_full" ,
                                                          "Consumption_L_per_yr_full","Consumption_Kg_per_yr_full"),with=FALSE]
  #
  # Calculates number of products with only max-in-stock, only yearly usage, and both across all industries
  # (Note this is done with a custom function, as working through the logic of this in vectorized form gets too messy).
  max_in_stock_counter <- function(x){
    # This should be applied down rows of a data table
    # that contains unique product information per row.
    # (I.e, input is the row of a data table, with elements:
    # max-in-stock amount, max-in-stock unit, yearly consumption (L), yearly consumption (kg))
    # Output is then either M, Y, or B, indicating only Max-in-stock, only yearly usage, or both being available
    # Note that for M to be uniquely available, all yearly usage values must be either NA and/or zero. Yearly values
    # can be unique if M is present as zero.
    MIS  <- as.numeric(x[2])
    Y_l  <- as.numeric(x[3])
    Y_kg <- as.numeric(x[4])
    
    # Works through the possible options
    if(((is.na(MIS)==TRUE)|(MIS==0)) & ((is.na(Y_l)==FALSE)|(is.na(Y_kg)==FALSE))){
      classification <- "Y"
      
    } else if(((is.na(MIS)==FALSE)&(MIS!=0)) & (((is.na(Y_l)==TRUE)&(is.na(Y_kg)==TRUE))|(sum(Y_l,Y_kg,na.rm = TRUE)==0))){
      classification <- "M"
      
      
    } else if((is.na(MIS)==FALSE) & (((is.na(Y_l)==FALSE)|(is.na(Y_kg)==FALSE))&(sum(Y_l,Y_kg,na.rm = TRUE)!=0))){
      classification <- "B"

    } else{
      stop("Unexpected error when attempting to determine numbers of products with max-in-stock/yearly usage data available")
    }
    
    return(classification)
    
  }
  MIS_classification   <- apply(first_prod_data_counting,1,max_in_stock_counter)
  num_only_max         <- length(which(MIS_classification=="M"))
  num_only_yearly      <- length(which(MIS_classification=="Y"))
  num_max_and_yearly   <- length(which(MIS_classification=="B"))
  #
  # Builds the MIS classification into the first product data counting data table.
  first_prod_data_counting$MIS_class <- MIS_classification
  #
  # Applies MIS classification function per Facility category.
  nums_by_cat_02  <- first_prod_data_counting[,.(Num_prods_max  = length(which(MIS_class=="M")),
                                                 Num_prods_yrl  = length(which(MIS_class=="Y")),
                                                 Num_prods_both = length(which(MIS_class=="B"))),by=Facility_category]
  #
  # Merges the two counting-by-category data tables (by category).
  library(data.table)
  nums_by_cat <- merge(nums_by_cat_01,nums_by_cat_02,by="Facility_category",all.x=TRUE,all.y=TRUE)
  nums_by_cat <- setcolorder(nums_by_cat,c("Facility_category","Prod_num","Num_prods_max","Num_prods_yrl",
                                           "Num_prods_both","Chem_num","Toxic_PNEC_num","Non_toxic_PNEC_num","Missing_PNEC_num"))
  #
  # Adds a row for the calculated totals across all facility categories
  all_industries_values <- c("All industries",total_num_products,num_only_max,num_only_yearly,num_max_and_yearly,total_num_chems,
                             total_chems_toxic_PNEC,total_chems_non_tox_PNEC,total_chems_missing_PNEC)
  nums_by_cat <- rbind(as.data.frame(nums_by_cat,stringsAsFactors=FALSE),all_industries_values)
  
  
  ########### End of proposed function #################

  
  
  # Plots the exposure/risk plots for the select STP region of interest --------
  
  
  # Establishes the PNEC and CAS indices and labels.
  PNEC_ind       <- which(colnames(upstream_data)=="min_STP_PNEC_mg/L")
  PNEC_Label     <- "min_PNEC"
  CAS_ind        <- which((colnames(upstream_data)%in%paste0("Final_MIS_Kg_",yrl_MIS_pctl_str,"_pctl_max_CAS"))==TRUE)
  CAS_conc_label <- "max_CAS"
  
  # Defines which STP regoin will have plots generated, targeting highest risk potential but
  # allowing for user-defined override.
  if(is.na(user_defined_plot_region)==FALSE){
    
    # Sets the STP region to plot as that defined by the user.
    STP_region_RS_plot <- user_defined_plot_region
    STP_vol_m3         <- STP_region_vol_m3[which(STP_region_name==user_defined_plot_region)]
    STP_flow_frac      <- STP_region_flow_fractions[which(STP_region_name==user_defined_plot_region)]
    
    # Determines the lowest-dilution region and sets this as the STP region to plot.
  } else{
    unit_region_conc   <- STP_region_flow_fractions/STP_region_vol_m3
    STP_region_RS_plot <- STP_region_name[which.max(unit_region_conc)]
    STP_vol_m3         <- STP_region_vol_m3[which.max(unit_region_conc)]
    STP_flow_frac      <- STP_region_flow_fractions[which.max(unit_region_conc)]
  }
  
  # Copies upstream data.
  current_upstream_data <- copy(upstream_data)
  
  # Calculates exposure concentrations for STP region of interest.
  exp_cols <- ((current_upstream_data[,(CAS_ind),with=FALSE])*1000*STP_flow_frac)/STP_vol_m3 # 1000 converts units to mg/L (converts plant volume to L and mass of chem from kg to mg).
  current_upstream_data[,paste0(STP_region_RS_plot,"_Exp_mg/L_",yrl_MIS_pctl_str,"_pctl_",CAS_conc_label) := exp_cols ]
  
  # Calculates risk for current STP region.
  risk_cols <- sweep(exp_cols,1,as.matrix(upstream_data[,PNEC_ind,with=FALSE]),"/")
  current_upstream_data[,paste0(STP_region_RS_plot,"_Risk_",yrl_MIS_pctl_str,"_pctl_",CAS_conc_label,"_",PNEC_Label) := risk_cols]
  
  # Determines column of exposure risk values to use as 'central' value for removal of NAs and zeros (will include both measured and estimated MIS, and not lower/upper values).
  cent_risk_col <- which(colnames(current_upstream_data)==paste0(STP_region_RS_plot,"_Risk_",yrl_MIS_pctl_str[2],"_pctl_",CAS_conc_label,"_",PNEC_Label))
  
  # Copies off current upstream data prior to 'bad value' removal for calculation later of 
  # % of constituent chemicals that have risk values in each product.
  current_upstream_data_for_prod_frac <- copy(current_upstream_data)
  
  # Removes instances where Risk is zero or NA.
  na_risk_rows          <- as.vector(is.na(current_upstream_data[,cent_risk_col,with=FALSE]))
  current_upstream_data <- current_upstream_data[na_risk_rows != TRUE]
  inf_risk_rows         <- as.vector(is.infinite(as.matrix(current_upstream_data[,cent_risk_col,with=FALSE])))
  current_upstream_data <- current_upstream_data[inf_risk_rows != TRUE]
  zero_risk_rows        <- as.vector(as.matrix(current_upstream_data[,cent_risk_col,with=FALSE])==0)
  current_upstream_data <- current_upstream_data[zero_risk_rows != TRUE]
  
  # Organizes data.frame for plotting data.
  #
  # Pulls out only columns needed for plotting ############### (Probably want to pull out facility, product ID, and CAS number too! (For plotly))
  plotting_upstream_data <- as.data.frame(current_upstream_data[,which((colnames(current_upstream_data)%in%
                                                                          c("product_ID","CAS_number","MIS_flag",paste0(STP_region_RS_plot,"_Exp_mg/L_",yrl_MIS_pctl_str,"_pctl_",CAS_conc_label),
                                                                            paste0(STP_region_RS_plot,"_Risk_",yrl_MIS_pctl_str,"_pctl_",CAS_conc_label,"_",PNEC_Label)))==TRUE),with=FALSE])
  #
  # Pulls out indices of data label columns.
  product_col     <- which(colnames(plotting_upstream_data)=="product_ID")
  CAS_col         <- which(colnames(plotting_upstream_data)=="CAS_number")
  MIS_col         <- which(colnames(plotting_upstream_data)=="MIS_flag")
  #
  # Reassigns column names of data to be plotted.
  #
  # Extracts min, cent, and upper exp and risk col indices.
  low_exp_col  <- which(colnames(plotting_upstream_data)==paste0(STP_region_RS_plot,"_Exp_mg/L_",yrl_MIS_pctl_str[1],"_pctl_",CAS_conc_label))
  low_risk_col <- which(colnames(plotting_upstream_data)==paste0(STP_region_RS_plot,"_Risk_",    yrl_MIS_pctl_str[1],"_pctl_",CAS_conc_label,"_",PNEC_Label))
  #
  cent_exp_col  <- which(colnames(plotting_upstream_data)==paste0(STP_region_RS_plot,"_Exp_mg/L_",yrl_MIS_pctl_str[2],"_pctl_",CAS_conc_label))
  cent_risk_col <- which(colnames(plotting_upstream_data)==paste0(STP_region_RS_plot,"_Risk_",    yrl_MIS_pctl_str[2],"_pctl_",CAS_conc_label,"_",PNEC_Label))
  #
  high_exp_col  <- which(colnames(plotting_upstream_data)==paste0(STP_region_RS_plot,"_Exp_mg/L_",yrl_MIS_pctl_str[3],"_pctl_",CAS_conc_label))
  high_risk_col <- which(colnames(plotting_upstream_data)==paste0(STP_region_RS_plot,"_Risk_",    yrl_MIS_pctl_str[3],"_pctl_",CAS_conc_label,"_",PNEC_Label))
  #
  # Assigns new column names to the plotting data.
  colnames(plotting_upstream_data)[c(product_col,CAS_col,MIS_col,low_exp_col,cent_exp_col,high_exp_col,low_risk_col,cent_risk_col,high_risk_col)] <- 
    c("product_ID","CAS_number","MIS_flag","Low_Exp","Cent_Exp","High_Exp","Low_Risk","Cent_Risk","High_Risk")
  
  
  # Figure plotting and data reporting ---------------------------------------------------------

  
  # Plots the constituent chemical exposure and risk.
  chem_risk_plot <- log_plot(plotting_upstream_data,FALSE,FALSE,"Chem Method",c("Reported","Estimated"),"Exposure mg/L","Risk",
                             paste0("Exp vs. Risk Chems: ",STP_region_RS_plot," ",PNEC_Label," ",CAS_conc_label))
  
  # Sums risks from constituent chemicals based on product ID.
  sum_prod_upstream_data <- as.data.table(plotting_upstream_data)
  cols                   <- colnames(sum_prod_upstream_data)[which(colnames(sum_prod_upstream_data)%in%c("product_ID","CAS_number","MIS_flag")==FALSE)]
  full_product_exp_risk  <- sum_prod_upstream_data[,(cols) := lapply(.SD, sum, na.rm=TRUE), .SDcols = cols,by=product_ID]
  #
  # Pull out rows of first product ID (values repeated down rows for a given product ID.
  unique_prod_rows               <- match(unique(full_product_exp_risk$product_ID),full_product_exp_risk$product_ID)
  plotting_full_product_exp_risk <- full_product_exp_risk[unique_prod_rows,]
  
  # Plots the full product exposure and risk.
  prod_risk_plot <- log_plot(plotting_full_product_exp_risk,FALSE,FALSE,"Chem Method",c("Reported","Estimated"),"Summed Exposure mg/L","Summed Risk",
                             paste0("Exp vs. Risk Products: ",STP_region_RS_plot," ",PNEC_Label," ",CAS_conc_label))
  
  
  ########## For data stats reporting: ############
  #
  # Take below code and use as starting point for function that takes the overall upstream data, as well as the 
  # prod frac. uncertainty and the constituent chems uncertainty data, as input and calculates all necessary
  # pieces of information from these datasets, then call this function towards end of the Exp_plotting function here.
  # Possibly do like the data_stats_prioritization function and just add into function that prioritizes chems/products...
  
  # Initializes data.frame for storing statistics on plotted chemicals/products
  final_chem_stats <- as.data.frame(matrix(data = NA,nrow=(2*2*2*length(STP_region_name)),ncol=19))
  colnames(final_chem_stats) <- c("PNEC","CAS_conc","STP_region","Central_Chems_valid_count","Central_Chems_NA",
                                  "Central_Chems_Inf","Central_Chems_Zero","Central_Chems_Unique_CAS","Central_Chems_Prods","Central_Chems_CAS_MIS_reported",
                                  "Central_Chems_CAS_MIS_estimate","Central_Chems_CAS_over_one","Lower_Chems_CAS_over_one","Upper_Chems_CAS_over_one",
                                  "Central_Chems_Prods_MIS_reported","Central_Chems_Prods_MIS_estimate","Central_Chems_Prods_over_one",
                                  "Lower_Chems_Prods_over_one","Upper_Chems_Prods_over_one")
  
  # Sends plotted data to function for calculating chem stats and writing out prioritization list.
  data_stats_output <- data_stats_prioritization(PNEC_Label,CAS_conc_label,STP_region_RS_plot,plotting_upstream_data,na_risk_rows,
                                                 inf_risk_rows,zero_risk_rows,plotting_full_product_exp_risk,final_chem_stats,1)
  
  # Unpacks chemical prioritization outputs (this area needs work possibly for the final_chem_stats).
  final_chem_stats  <- data_stats_output[[1]]
  prioritized_chems <- data_stats_output[[2]]
  prioritized_prods <- data_stats_output[[3]]

  
  ##################################################
  
  
  # Sends upstream chemical and product data to function for caluclating % of constituent chemicals
  # that have risk value for calculating additive risk of product.
  prod_frac_output <- product_fraction_plot(current_upstream_data_for_prod_frac,plotting_full_product_exp_risk,TRUE,
                                            "Chem Method",c("Reported","Estimated"),"Summed Exposure mg/L","Summed Risk",paste0("Exp vs. Risk % Chems in Products: ",STP_region_RS_plot," ",PNEC_Label," ",CAS_conc_label))
  
  # Unpacks outputs of product_fraction_plot function (first is figure, second is stats counts).
  prod_frac_figure <- prod_frac_output[[1]]
  prod_frac_stats  <- prod_frac_output[[2]]
  
  # Conducts counts of various risk threshold statistics for reporting to user later.
  num_chems_risk_1_or_over <- length(which(plotting_upstream_data$Cent_Risk>=1))
  num_prods_risk_1_or_over <- length(which(plotting_full_product_exp_risk$Cent_Risk>=1))
  
  
  # Results uncertainty plotting --------------------------------------------
  
  
  # Extracts max/min PNEC indices. Note these are being ordered with the max STP first (i.e., least toxic),
  # and the min STP last (i.e., most toxic) to aligh with the lower-end and higher-end exposure columns going to be
  # first and second, respectively, so as to capture the actual spread in risk uncertainty.
  PNEC_ind   <- c(which(colnames(upstream_data)=="max_STP_PNEC_mg/L"),which(colnames(upstream_data)=="min_STP_PNEC_mg/L"))
  
  # Extracts column indices with the lower-end pct/min CAS concentration and the upper-end pct/max CAS concentrations.
  CAS_perc_ind <- c(which((colnames(upstream_data)==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[1],"_pctl_min_CAS"))==TRUE),
                    which((colnames(upstream_data)==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[3],"_pctl_max_CAS"))==TRUE))
  
  # Extracts column indices where existing MIS values are for the corresponding columns in the CAS_perc_ind parameter above
  # (i.e., since the values in the lower-end and upper-end values may not be there if MIS values were directly reported, and this
  # will be used to fill in these missing ones later).
  CAS_perc_ind_fill <-  c(which((colnames(upstream_data)==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[2],"_pctl_min_CAS"))==TRUE),
                          which((colnames(upstream_data)==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[2],"_pctl_max_CAS"))==TRUE))
  
  # Extracts column indices for building in max CAS values into min CAS values that are zero.
  data_col_names <- colnames(upstream_data)
  CAS_min_max_fill <- rbind(c(which(data_col_names==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[1],"_pctl_min_CAS")),
                              which(data_col_names==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[2],"_pctl_min_CAS")),
                              which(data_col_names==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[3],"_pctl_min_CAS"))),
                            c(which(data_col_names==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[1],"_pctl_max_CAS")),
                              which(data_col_names==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[2],"_pctl_max_CAS")),
                              which(data_col_names==paste0("Final_MIS_Kg_",yrl_MIS_pctl_str[3],"_pctl_max_CAS"))))
  
    
    ################# Data prep and organization #######################
    
  
    # Copies upstream data.
    current_upstream_data <- copy(upstream_data)
    
    # For instances where the min CAS value is zero (using just the 50th percentile to test) copies over the 25th, 50th, and 75th percentile
    # values from the Max CAS into these min CAS ones!
    #
    # Identifies rows with zero minimum 
    zero_min_rows <- which(current_upstream_data[,CAS_perc_ind_fill[1],with=FALSE]==0)
    #
    # Pulls out max CAS data columns
    max_CAS_lower <- current_upstream_data[[CAS_min_max_fill[2,1]]]
    max_CAS_cent  <- current_upstream_data[[CAS_min_max_fill[2,2]]]
    max_CAS_upper <- current_upstream_data[[CAS_min_max_fill[2,3]]]
    #
    # Fills in instances of zero in the min CAS values.
    current_upstream_data[zero_min_rows,CAS_min_max_fill[1,1] := max_CAS_lower[zero_min_rows]]
    current_upstream_data[zero_min_rows,CAS_min_max_fill[1,2] := max_CAS_cent[zero_min_rows]]
    current_upstream_data[zero_min_rows,CAS_min_max_fill[1,3] := max_CAS_upper[zero_min_rows]]
    
    # For instances where MIS were provided (i.e., not estimated by percentile extrapolation method), copies the existing values to the 
    # upper/lower bounds to enable streamlined analysis and plotting (uncertainty still present in these, but not for percentiles aspect).
    #
    # Pulls out data columns.
    MIS_min_CAS <- current_upstream_data[[CAS_perc_ind_fill[1]]]
    MIS_max_CAS <- current_upstream_data[[CAS_perc_ind_fill[2]]]
    #
    # Assigns missing selected percentile values those from existing MIS data.
    current_upstream_data[which(is.na(current_upstream_data[,CAS_perc_ind[1],with=FALSE])==TRUE),CAS_perc_ind[1] := MIS_min_CAS[which(is.na(current_upstream_data[,CAS_perc_ind[1],with=FALSE])==TRUE)]]
    current_upstream_data[which(is.na(current_upstream_data[,CAS_perc_ind[2],with=FALSE])==TRUE),CAS_perc_ind[2] := MIS_max_CAS[which(is.na(current_upstream_data[,CAS_perc_ind[2],with=FALSE])==TRUE)]]
    
    # Calculates low- and high-end exposure concentrations for STP region of interest.
    exp_cols <- ((current_upstream_data[,(CAS_perc_ind),with=FALSE])*1000*STP_flow_frac)/STP_vol_m3 # 1000 converts units to mg/L (converts plant volume to L and mass of chem from kg to mg).
    current_upstream_data[,paste0(STP_region_RS_plot,"_Exp_mg/L_",c(yrl_MIS_pctl_str[1],yrl_MIS_pctl_str[3]),"_pctl_",c("min_CAS","max_CAS")) := exp_cols ]
    
    # Calculates low- and high-end risk concentrations for current STP region.
    risk_cols <- exp_cols/upstream_data[,PNEC_ind,with=FALSE]
    current_upstream_data[,paste0(STP_region_RS_plot,"_Risk_",c(yrl_MIS_pctl_str[1],yrl_MIS_pctl_str[3]),"_pctl_",c("min_CAS","max_CAS"),"_",c("max_PNEC","min_PNEC")) := risk_cols]
    
    
    ################# Data cleaning before plotting #######################
    
    
    # Identifies column indices for lower and upper risk values.
    lower_ind <- which(colnames(current_upstream_data)==paste0(STP_region_RS_plot,"_Risk_",yrl_MIS_pctl_str[1],"_pctl_","min_CAS","_","max_PNEC"))
    upper_ind <- which(colnames(current_upstream_data)==paste0(STP_region_RS_plot,"_Risk_",yrl_MIS_pctl_str[3],"_pctl_","max_CAS","_","min_PNEC"))
    
    # Identifies and removes rows where risk (either upper or lower estimates) are zero, NA, or Inf.
    # Removes instances where Risk is zero or NA.
    #
    # NA values.
    na_risk_rows          <- as.vector(is.na(current_upstream_data[,lower_ind,with=FALSE])) | as.vector(is.na(current_upstream_data[,upper_ind,with=FALSE]))
    current_upstream_data <- current_upstream_data[na_risk_rows != TRUE]
    #
    # Infinity values.
    inf_risk_rows         <- as.vector(is.infinite(as.matrix(current_upstream_data[,lower_ind,with=FALSE]))) | as.vector(is.infinite(as.matrix(current_upstream_data[,upper_ind,with=FALSE])))
    current_upstream_data <- current_upstream_data[inf_risk_rows != TRUE]
    #
    # Zero values.
    zero_risk_rows        <- as.vector(as.matrix(current_upstream_data[,lower_ind,with=FALSE])==0) | as.vector(as.matrix(current_upstream_data[,upper_ind,with=FALSE])==0)
    #
    # Stops if any NA values returned from finding zero values, and if so throws an error.
    if(any(is.na(zero_risk_rows))==TRUE){
      stop("NA values found in the current upstream data risk values prior to plotting that were not removed!")
    }
    #
    # Removes zero values.
    current_upstream_data <- current_upstream_data[zero_risk_rows != TRUE]
    
    
    ################# Data plotting #######################
    
    
    # Organizes data.frame for plotting data.
    #
    # Pulls out only columns needed for plotting ###############
    plotting_upstream_data <- as.data.frame(current_upstream_data[,which((colnames(current_upstream_data)%in%
                                                                            c("product_ID","CAS_number","MIS_flag",
                                                                              paste0(STP_region_RS_plot,"_Exp_mg/L_",c(yrl_MIS_pctl_str[1],yrl_MIS_pctl_str[3]),"_pctl_",c("min_CAS","max_CAS")),
                                                                              paste0(STP_region_RS_plot,"_Risk_",c(yrl_MIS_pctl_str[1],yrl_MIS_pctl_str[3]),"_pctl_",c("min_CAS","max_CAS"),"_",c("max_PNEC","min_PNEC"))))==TRUE),with=FALSE])
    #
    # Pulls out indices of data label columns.
    product_col     <- which(colnames(plotting_upstream_data)=="product_ID")
    CAS_col         <- which(colnames(plotting_upstream_data)=="CAS_number")
    MIS_col         <- which(colnames(plotting_upstream_data)=="MIS_flag")

    # Constructs the central value of exposure and risk, which are the exposure and risk from the 50th percentile, MAX CAS, lowest PNEC
    # (this is done so the center points of the uncertainty plot align with the baseline exp/risk plot.
    exp_mid_points  <- (current_upstream_data$Final_MIS_Kg_50_pctl_max_CAS*1000*STP_flow_frac)/STP_vol_m3 # 1000 converts units to mg/L (converts plant volume to L and mass of chem from kg to mg).
    risk_mid_points <- exp_mid_points/(current_upstream_data$`min_STP_PNEC_mg/L`)
    
    # Builds in the Exp and Risk midpoints to the plotting data.
    plotting_upstream_data$Cent_Exp  <- exp_mid_points
    plotting_upstream_data$Cent_Risk <- risk_mid_points
    
    # Reassigns column names of data to be plotted (handling central value and min/max values if applicable).
    #
    # Extracts exposure and risk min/max column indices.
    exp_col_inds  <- which(colnames(plotting_upstream_data)%in%paste0(STP_region_RS_plot,"_Exp_mg/L_",c(yrl_MIS_pctl_str[1],yrl_MIS_pctl_str[3]),"_pctl_",c("min_CAS","max_CAS"))==TRUE)
    risk_col_inds <- which(colnames(plotting_upstream_data)%in%paste0(STP_region_RS_plot,"_Risk_",c(yrl_MIS_pctl_str[1],yrl_MIS_pctl_str[3]),"_pctl_",c("min_CAS","max_CAS"),"_",c("max_PNEC","min_PNEC"))==TRUE) 
    
    # Assigns new column names to the plotting data.
    colnames(plotting_upstream_data)[c(product_col,CAS_col,MIS_col,exp_col_inds[1],exp_col_inds[2],risk_col_inds[1],risk_col_inds[2])] <- 
      c("product_ID","CAS_number","MIS_flag","Low_Exp","High_Exp","Low_Risk","High_Risk")
    
    # Plots the constituent chemical exposure and risk
    chem_uncertainty_plot <- log_plot(plotting_upstream_data,TRUE,TRUE,"Chem Method",c("Reported","Estimated"),"Exposure mg/L",
                                      "Risk",paste0("Exp vs. Risk Chems Uncertainty: ",STP_region_RS_plot," ","PNEC_Range"," ","CAS_conc_Range"))
    
    # Counts number of chemicals that meet or exceed risk of 1 with high-end risk.
    num_uncert_chems_risk_1_or_over <- length(which(plotting_upstream_data$High_Risk>=1))
    

# Final statistics count, organization, and return of values to user. ------------------

    
    # Organizes various statistics/counts of items as table output to user.
    counts_labels <- c("Num_unique_facilities","Num_unique_prods","Num_unique_chems","Num_valid_PNEC","Num_toxic_PNEC",
                       "Num_non_toxic_PNEC","Num_range_PNEC","Num_chems_risk_1_or_over","Num_prods_risk_1_or_over","Num_uncert_chems_high_risk_1_or_over")
    counts_values <- c(num_industries,num_unique_prods,num_unique_chems,num_valid_PNEC,num_tox_PNEC,num_non_tox_PNEC,num_range_PNEC,
                       num_chems_risk_1_or_over,num_prods_risk_1_or_over,num_uncert_chems_risk_1_or_over)
    summary_stats <- data.table(Metric = counts_labels,Value = counts_values,stringsAsFactors = FALSE)
    
    # Merges above summary statistics with those output from the product % chemicals with PNEC uncertainty figure.
    summary_stats <- rbindlist(list(summary_stats,prod_frac_stats))
    
    # Returns plots, prioritization tables, and summary tables that were generated within function.
    return(list(chem_risk_plot,prod_risk_plot,chem_uncertainty_plot,prod_frac_figure,prioritized_chems,prioritized_prods,nums_by_cat,summary_stats,STP_region_RS_plot))
  
}


# User interface code --------------------------------------------------


# Establishes the data table for the user to enter wastewater treatment plant parameters into.
user_input_DT           <- data.table(v1=c("0","0","0"),v2=c("0","0","0"),v3=c("0","0","0"),stringsAsFactors = FALSE)
colnames(user_input_DT) <- c("Region name", "Volume (m^3)","Flow fraction")
rownames(user_input_DT) <- c("Region 01","Region 02","Region 03")

# Copies the user-fillable data table and inputs default values for Example run.
sample_input_DT                 <- copy(user_input_DT)
sample_input_DT$`Region name`   <- c("Sample_Region_01","Sample_Region_02","Sample_Region_03")
sample_input_DT$`Volume (m^3)`  <- c(3000,6500,4500)
sample_input_DT$`Flow fraction` <- c(1,0.35,0.65)

# Begins user interface.
ui <- fluidPage(
  
  # Sets the theme and title line for the tool.
  theme = shinythemes::shinytheme("spacelab"),
  titlePanel(title=div("RAVEN STREAM: Rapid Assessment of Vulnerability from Emissions Upstream",img(src="logo.PNG",height="10%", width="10%"))),
  
  # Organizes the GUI into panels.
  sidebarLayout(
    sidebarPanel(
      
      # Button for user to download zipped User's Guide zip file.
      # Note: As of 2023/03/12, this is temporarily only allowing download of the template data file.
      downloadButton("users_guide_download", label = "Download Template Data"),
      
      # Button for user to load in example simulation settings.
      actionButton(
        inputId = "example_data_button",
        label= "Load Sample Run"),
      
            # User input .csv file.
      fileInput(inputId = "input_file",
                label =  "Input data file",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
      # User input analysis label.
      textInput(inputId = "analysis_label",
                label = "Analysis name",
                value = "",
                placeholder = "Analysis_01"),
      verbatimTextOutput("value"),
      
      # Temp user input data table.
      DTOutput("my_datatable"),
      
      # User-defined STP region of interest.
      textInput(inputId = "STP_region_of_interest",
                label = "Region to analyze",
                value = "",
                placeholder = "Optional: leave blank to analyze highest risk region"),
      
      # User input for running the tool.
      actionButton(
        inputId = "run_button",
        label= "Run RAVEN STREAM"),
      
      # Button
      downloadButton("downloadData", "Download Output")

    ),
    
    # Displays the user-input data on the RHS of the page.
    mainPanel(
      tabsetPanel(
        
        # Tab for displaying preview of user input data.
        tabPanel("Input Data Preview",
                 tableOutput("contents")
        ),
        
        # Tab for displaying consituent chemicals and products risk assessment.
        tabPanel("Risk Visualizations", 
                 plotOutput("chem_risk",width = "1200px",height = "900px"),
                 h1(" "),
                 plotOutput("prod_risk",width = "1200px",height = "900px")),
        
        # Tab for displaying uncertainties associated with risk values.
        tabPanel("Uncertainty Visualizations",
                 plotOutput("chem_uncertainty",width = "1200px",height = "900px"),
                 h1(" "),
                 plotOutput("prod_uncertainty",width = "1200px",height = "900px")),
        
        # Tab for displaying constituent chemicals risk prioritization.
        tabPanel("Upstream Constituent Chemical Prioritizations",
                 DT::DTOutput('chems_prioritzed')),
        
        # Tab for displaying products risk prioritization.
        tabPanel("Upstream Product Prioritizations",
                 DT::DTOutput('prods_prioritzed'))
        
      )
    )
  )
)

# Server-side of the GUI.
server <- function(input, output, session) {
  
  # User input table display.
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    # Attempts to access user input file (this is always happening so will update right when
    # user makes new selection).
    inFile <- input$input_file
    if (is.null(inFile))
      return(NULL)
    
    # Once input file exists, reads it in for display.
    user_input_data <- reactiveValues(data = {
      fread(input = inFile$datapath, encoding = "UTF-8",stringsAsFactors = FALSE)
    })
    #
    # Displays only first 50 rows (or fewer).
    if(nrow(user_input_data$data)<=50){
      user_input_data$data
      
      # For indexing into the first 50 rows.
    } else{
      user_input_data$data[c(1:50),]
      
    }
  })
  
  # Reads in the sample data, regardless of whether the user will use it, so that it is 
  # globally visible.
  # Note, this data is the same file as the "Upstream_Käppala_Chem_Data.csv" file located here: C:\Users\JohnHader\Desktop\Käppala\Paper\Analysis
  # (i.e., as of 2022/03/17, this is the real full-data analysis file, which needs to be switched out with a proper sample data file before going live).
  #
  # Note this is established using the reactiveValues function so as to make it globally accessible (so that it can be used as the input data to RAVENSTREAM later!)
  sample_input_data <- reactiveValues(data = {
    fread(input = "Load_Sample_Data.csv", encoding = "UTF-8",stringsAsFactors = FALSE)
  })
  
  # Handles downloading of User's Guide (as of 2023/03/12, this is temporarily only the template data).
  output$users_guide_download <- downloadHandler(
    filename <- function() {
      paste("RAVEN_STREAM_Data_Template", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("RAVEN_STREAM_Data_Template.csv", file)
    },
    contentType = "application/csv"
  )
  
  # Establishes the base display for the WWTP parameters table.
  v <- reactiveValues(data = {
    user_input_DT
  })
  
  #output the datatable based on the dataframe (and make it editable).
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE,
                  options = list(paging=FALSE,searching=FALSE,columns.orderable=FALSE))
  })
  
  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  observeEvent(input$my_datatable_cell_edit, {
    #get values
    info = input$my_datatable_cell_edit
    i = info$row
    j = info$col
    k = info$value
    
    #write values to reactive
    v$data[i,j] <- k
  })
  
  # User loading of sample run data.
  observeEvent(input$example_data_button,{
    
    #insert the sample run dataframe into the display object.
    v$data <- sample_input_DT
    
    # Outputs the sample data table.
    output$my_datatable <- renderDT({
      DT::datatable(v$data, editable = TRUE,
                    options = list(paging=FALSE,searching=FALSE,columns.orderable=FALSE))
    })
    
    # Loads and displays the sample data.
    output$contents <- renderTable({

      # Displays only first 50 rows (or fewer).
      if(nrow(sample_input_data$data)<=50){
        sample_input_data$data
        
        # For indexing into the first 50 rows.
      } else{
        sample_input_data$data[c(1:50),]
        
      }
    })
  })
  
  # Running RAVEN STREAM processing.
  observeEvent(input$run_button, {
    
    # Creates a Progress object and ensures it will close upon exit (even in the event of an error)
    progress <- shiny::Progress$new()
    progress$set(message = "Progress:", value = 0)
    on.exit(progress$close())
    
    # Functionalizes updating of the progress bar, code for implementation mainly from: https://shiny.rstudio.com/articles/progress.html
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    # Increment the progress bar, and update the detail text.
    updateProgress(value=(1/5),detail="Initializing")
    
    # Extracts the file path from either the user-defined input file or the sample data,
    # depending on the user's specifications (the test data, if selected, is taken with precedent).
    if(is.null(input$input_file)==TRUE){
      
      ##### Need to add a catch in here for if the user did not press the 'load sample' button to throw error and not run
      
      # The data to be used for RAVENSTREAM is set to the sample data.
      RS_data <- sample_input_data$data
      
    } else if(is.null(input$input_file)==FALSE){
      
      # Extracts input file path and reads in for sending to RAVEN STREAM.
      file_path <- input$input_file
      RS_data   <- fread(input = file_path$datapath, encoding = "UTF-8",stringsAsFactors = FALSE)
      
    } else{
      stop("Unexpected error when preparing input data for RAVENSTREAM")
    }
    
    # Passes all user inputs to processing function. Note 4th argument had been "input$MIS_extrapolation"; this set permanantely to TRUE.
    RS_output <- RAVEN_STREAM_controller(RS_data,input$analysis_label,
                                         v$data,input$STP_region_of_interest,updateProgress)
    
    # Increment the progress bar, and update the detail text.
    updateProgress(value=(4.5/5),detail="Unpacking Results")
    
    # Extracts outputs returned from processing function (first element is list of items
    # from upstream data pre-processing function, while second element is list
    # of outputs from exposure/risk plotting function)
    RS_output_1           <- RS_output[[1]]
    MIS_adjust_CDF_plot   <- RS_output_1[[1]]
    MIS_adjust_hist_plot  <- RS_output_1[[2]]
    MIS_adjust_out_table  <- RS_output_1[[3]]
    #
    RS_output_2           <- RS_output[[2]]
    chem_risk_plot        <- RS_output_2[[1]]
    prod_risk_plot        <- RS_output_2[[2]]
    chem_uncertainty_plot <- RS_output_2[[3]]
    prod_frac_figure      <- RS_output_2[[4]]
    prioritized_chems     <- RS_output_2[[5]]
    prioritized_prods     <- RS_output_2[[6]]
    counts_by_categories  <- RS_output_2[[7]]
    summary_counts        <- RS_output_2[[8]]
    analyzed_region       <- RS_output_2[[9]]
    
    ### Need to build the monthly CDF, histogram, and plotted
    ### region into the RS_output object, so can put into
    ### outputs/display on figures as necessary"
    
    # Test of making variable reactive.
    to_download <- reactiveValues(dataset1 = chem_risk_plot, dataset2 = prod_risk_plot, dataset3 = chem_uncertainty_plot,
                                  dataset4 = prod_frac_figure, dataset5 = prioritized_chems, dataset6 = prioritized_prods,
                                  dataset7 = MIS_adjust_CDF_plot,dataset8 = MIS_adjust_hist_plot,dataset9 = MIS_adjust_out_table,
                                  dataset10 = counts_by_categories,dataset11 = summary_counts)
    
    # Increment the progress bar, and update the detail text.
    updateProgress(value=(5/5),detail="Processing complete")
    
    # Informs user of successful completion.
    # See here for other ideas about user interfacing with messages: https://shiny.rstudio.com/articles/notifications.html
    showNotification("Processing complete; explore other tabs for results.",type = "message")
    
    # Updates figures in risk visualization tab.
    #
    # Constituent chemicals risk plot.
    output$chem_risk <- renderPlot({
      
      # Displays constituent chemical risks.
      chem_risk_plot <- chem_risk_plot + geom_point(size=2) + ggtitle(paste0("Chemical Risks to ",analyzed_region))
      chem_risk_plot <- chem_risk_plot + geom_hline(yintercept = 1, linetype = 'dashed', col = 'black') +
        annotate("text", x = 10E-8, y = 1, label = "Risk of Concern",vjust = -0.5,size=10)
      
      # Returns adjusted plot (not actually sure this is totally necessary...)
      chem_risk_plot
      
    })
    #
    # Product risks plot.
    output$prod_risk <- renderPlot({
      
      # Displays chemical product risks.
      prod_risk_plot <- prod_risk_plot + geom_point(size=2) + ggtitle(paste0("Product Risks to ",analyzed_region))
      prod_risk_plot <- prod_risk_plot + geom_hline(yintercept = 1, linetype = 'dashed', col = 'black') +
        annotate("text", x = 10E-8, y = 1, label = "Risk of Concern",vjust = -0.5,size=10)
      
      # Returns adjusted plot (not actually sure this is totally necessary...)
      prod_risk_plot
      
    })
    #
    # Constituent chemicals uncertainty.
    output$chem_uncertainty <- renderPlot({
      
      # Displays constituent chemical uncertainty plot.
      chem_uncertainty_plot <- chem_uncertainty_plot + geom_point(size=2) + ggtitle(paste0("Uncertainties of Chemical Risks to ",analyzed_region))
      chem_uncertainty_plot <- chem_uncertainty_plot + geom_hline(yintercept = 1, linetype = 'dashed', col = 'black') +
        annotate("text", x = 10E-8, y = 1, label = "Risk of Concern",vjust = -0.5,size=10)
      
      # Returns adjusted plot (not actually sure this is totally necessary...)
      chem_uncertainty_plot
      
    })
    #
    # Product uncertainty.
    output$prod_uncertainty <- renderPlot({
      
      # Displays chemical product uncertainty plot.
      prod_frac_figure <- prod_frac_figure + geom_point(size=2) + ggtitle(paste0("Uncertainties w/Product Risks to ",analyzed_region))
      prod_frac_figure <- prod_frac_figure + geom_hline(yintercept = 1, linetype = 'dashed', col = 'black') +
        annotate("text", x = 10E-8, y = 1, label = "Risk of Concern",vjust = -0.5,size=10)
      
      # Returns adjusted plot (not actually sure this is totally necessary...)
      prod_frac_figure
      
    })
    
    # Updates tables in risk prioritization tabs.
    #
    # Constituent chemicals table.
    output$chems_prioritzed <- DT::renderDT({
      
      # Calls the output prioritized constituent chemicals table. For some reason, this is not a DT and so doesn't need the "with=FALSE".
      prioritized_chems <- prioritized_chems[,c("product_ID","CAS_number","Cent_Risk","MIS_flag")]
      
      # Returns filtered table (not sure this is entirely necessary...)
      prioritized_chems
      
    },rownames = FALSE)
    #
    # Products table.
    output$prods_prioritzed <- DT::renderDT({
      
      # Calls the output prioritized products table.
      prioritized_prods <- prioritized_prods[,c("product_ID","Cent_Risk","MIS_flag"),with=FALSE]
      
      # Returns filtered table (not sure this is entirely necessary...)
      prioritized_prods
      
    },rownames = FALSE)
    
    # Prepares prioritization tables, figures, and summary tables for download by user in a .zip file.
    output$downloadData <- downloadHandler(
      filename = function(){
        paste0("RAVEN_STREAM_output.zip")
      },
      content = function(file){
        
        # Displays message to user indicating download is in progress.
        showModal(modalDialog("Preparing RAVEN STREAM output for download.", footer=NULL))
        on.exit(removeModal())
        
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        
        to_download_list <- reactiveValuesToList(to_download)
        file_names       <- c(paste0("Constituent_Chemicals_Risk_Plot_",analyzed_region,".png"),paste0("Products_Risk_Plot_",analyzed_region,".png"),paste0("Constituent_Chemicals_Uncertainty_Plot_",analyzed_region,".png"),
                              paste0("Product_Fractions_Plot_",analyzed_region,".png"),paste0("Prioritized_Constituent_Chemicals_Risks_",analyzed_region,".csv"),paste0("Prioritized_Product_Risks_",analyzed_region,".csv"),
                              paste0("Max-in-stock_adjustment_CDF_",analyzed_region,".png"),paste0("Max-in-stock_adjustment_histogram_",analyzed_region,".png"),paste0("Max-in-stock_adjustment_percentiles_",analyzed_region,".csv"),
                              paste0("Counts_by_Categories_",analyzed_region,".csv"),paste0("Analysis_summary_statistics_",analyzed_region,".csv"))
        
        # Makes sure the to_download_list and corresponding file names are the same length.
        if(length(to_download_list)!=length(file_names)){
          stop("Unexpected error when attemptint to write out zip file (mismatching number of outputs and file labels.")
        }
        
        # Loops through the elements of the download list and 
        # writes out files (depending on whether figure or table).
        for(i in c(1:length(to_download_list))){
          
          # Determines extension of file to be saved (impacts how it is saved).
          file_ext <- str_sub(file_names[i],start=-4,end=-1)
          
          # Handles the figures.
          if(file_ext==".png"){
            ggsave(filename = paste0(temp_directory,"/",file_names[i]),plot=to_download_list[[paste0("dataset",i)]],width=50,height=37.5,units="cm",dpi = 300)
            
            # Handles the tables.
          } else if(file_ext==".csv"){
            fwrite(file = paste0(temp_directory,"/",file_names[i]),x=to_download_list[[paste0("dataset",i)]],sep = ";")
            
          } else{
            stop("Unexpected file extension for files to be output with zip file (only .png or .csv are permitted)")
          }
          
        }
        
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
      },
      contentType = "application/zip"
    )
  })
}

shinyApp(ui, server)
