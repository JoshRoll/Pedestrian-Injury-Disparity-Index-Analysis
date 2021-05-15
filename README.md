# Project Summary  
This project created information for the Oregon Department of Transportation (ODOT) Transportation System Action Plan (TSAP) which was sumamrized in a technical memo titled Pedestrain Injury and Social Equity in Oregon.  The technical memo can be accessed at the link below.  
https://www.oregon.gov/odot/Safety/Documents/Pedestrian_Safety_and_Social_Equity.pdf  

The data and scripts in this project gather Census data and then calculates a Race, Ethnicity, and Income Index (REII) using a standardized score method, also referred to as a z-score.  Using Oregon DOT crash data this project then calculates
pedestrain injury rates for each REII category to demonstrate that communities with a higher concentration of poverty and/or BIPOC populations have a higher rates of pedestrian injury.  

# Script Details  
Two scripts are available in this repository.  The script Z_Score_Analysis_prod.r will take previously compiled data and develop the Race, Ethnicity, and Income Index (REII)
and use it to calculate pedestrain injury rates.  The data compiled in Tracts_Data_2014-2014.RData and Tracts_Data_2008_2012.RData contain data elements from Census 
which can be acquired using the script titled download_Census_Prepare_data_prod.r.  Other data elements inlcuding pedestrain injury data and transportation netowrk data
are derived from different data sets which are not easy to link to in this project

## Z_Score_Analysis_prod.r 
This script uses previosuly compiled data to calculate z-scores using percent of the Census tract population that is Black, Indigenous, and Persons of Color (BIPOC) and the percent of the population living in poverty.
Pedestrain injury rates (per 100,000 people) are calculated along with some travel and built environmental summaries.  Charts are generated to summarise pedestrain injury rates by index category for two injury severities 
including fatal and severe injuries and total pedestrain injuries.  Two periods of data are available including years from 2008 to 2012 and from 2014 to 2018.  REII is calculated for both periods for comparison.  Another item in  
the script summarizes measures for injuries, population, and travel and built environmental measures are created and charted. Lasly, dynamic maps are created that utilize the leaflet package to see how the REII appears 
spatially and also to explore the other data elements.  

## download_Census_Prepare_data_prod.r 
This script uses some R packages to automatically access tract level Census data elements including sociodemographics, vehicle ownership, commute mode, among others.  The script is written to pull tract level data but
could easily be modified to pull other Census geographies.  You will need to sign up for a free Census API key.  

# Data Details



# Contact
Principle Investigator: Josh Roll  josh.f.roll@odot.state.or.us  
Co-Investigator: Nathan McNeil   nmcneil@pdx.edu
