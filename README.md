# RAVEN_STREAM
John Hader

Update 2024/06/07:

The following updates were made to the code to reflect changes made to the R Shiny version of the tool on this same day:
- A new version of app.R was uploaded with small changes to the GUI
- New version of Load_Sample_Data.csv uploaded with non-latin characters removed
- New version of RAVEN_STREAM_Data_Template.csv uploaded with non-latin characters removed
- New User's Guide file uploaded

Notes from original upload on 2023/03/12:

This is a repository for the files associated with the "RAVEN_STREAM" online tool, which is an open source tool intended for use by sewage treatment plants (STPs) to screen for the risk of spills of chemicals used at industries connected upstream to contaminate and negatively influence the biological treatment process at the STP. The study behind the development of this tool is currently under review, and the link to the full paper will be provided here upon publishing of this paper.

Files available in this repository are:

-----------------------

99_ECHA_Webscraper.R:

Script for accessing, extracting, and formatting sewage treatment plant predicted no effect concentration (STP PNEC) values from the European Chemicals Agency (ECHA) chemical brief profiles, for use in the RAVEN STREAM tool. The primary input to this script is a .xlsx file containing the full list of registered chemicals under the REACH framework, which can be downloaded from the ECHA website here: https://echa.europa.eu/information-on-chemicals/registered-substances

This script loops through the full list of registered chemicals (identifiable by their CAS numbers) and extracts their corresponding 'infocard' numbers which can be used to direct to a URL containing data on the chemical's properties obtained from registration dossiers submitted to ECHA.

This script then outputs a .csv file with the list of registered chemicals' CAS numbers, raw STP PNEC information stored from across the webscraped webpages, and ultimately machine-readable STP PNEC values in a constant unit from post-processing of the scraped data.

-----------------------

RAVEN_STREAM_source_code/ECHA_STP_PNEC_Data.csv:

File output from the 99_ECHA_Webscraper.R script, containing STP PNEC data for use in the RAVEN STREAM online tool.

-----------------------

RAVEN_STREAM_source_code/Load_Sample_Data.csv:

Small dataset intended for use in the RAVEN STREAM online tool's example data run.

-----------------------

RAVEN_STREAM_source_code/RAVEN_STREAM_Data_Template.csv:

File containing a template of the data formatting for input to the RAVEN STREAM online tool, intended for use by prospective users of RAVEN STREAM to format their upstream chemical use data.

-----------------------

RAVEN_STREAM_source_code/app.R:

Full code associated with the RAVEN STREAM online tool, including the code for the graphical user interface of the tool, the pre-processing of the user's input data on chemicals used at industrial facilities upstream, merging of the extracted STP PNEC data from ECHA with user's input chemical data, risk calculations, visualization of results, downloading of results to the user's computer, and data/parameterizations for an example run of the tool. See the code itself for additional documentation.

-----------------------

RAVEN_STREAM_source_code/rsconnect/shinyapps.io/raven-stream & RAVEN_STREAM_source_code/www:

Additional files associated with the tool for running on the R Shiny platform (i.e., a .dcf file and the image for the tool's logo).

