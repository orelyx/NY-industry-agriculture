# NY-industry-agriculture
Report of industry and agriculture greenhouse gas emissions from energy use (fuel combustion or electricity use) 
in a user-selected region of New York State (economic development regions), or for the state as a whole. Data are 
for years 2010-2016; primary data source is the NREL Industrial Energy Data Book (IEDB) at 
`https://data.nrel.gov/system/files/122/Updated%20county%20energy%20estimates.gzip`. 

The code in this repository can be used to do either of two things: 

1. Generate a report of energy use in the industry and agriculture sectors, for the state of New York or any of its 
economic development regions. To do this, open the file `Generic-report-industry-and-agriculture.Rmd` in Rstudio and 
select "Knit with parameters" from the "knit" pull-down menu. In the window that opens, select either "New York State" 
for a report of the entire state's industrial and agricultural energy use and emissions, or the name of an economic 
development region (such as "Long Island" or "North Country") for a report of its energy use and emissions. To add or modify 
the text of the report for greater relevance to a specific region, just rename the `.Rmd` file and modify the text as desired. 

2. Generate dataset files facilitating further analysis. The code that knits the report (in #1) at the same time generates a collection 
of dataset files containing the energy use and emissions data, in RDS and csv formats (sometimes compressed). 
These can also be generated by simply running either the script `NYS-industry-and-agriculture.R`, 
which generates the dataset files and the plots from the report in #1, or the script `NYS-industry-and-agriculture-noplots.R`, 
which generates only the dataset files (and runs the fastest). 

`industry-agriculture-energy-metadata.xlsx` contains metadata for the dataset files. The script `installPackages.R` 
installs the R packages required to knit the document or create the dataset files. Each of the reports in #1 contains 
and extensive list of references providing background information about emissions inventories and data sources. 

We gratefully acknowledge the diligent and insightful work at the National Renewable Energy Laboratories that 
produced the Industrial Energy Data Book on which the capabilities provided by code in this repository are based. 
See McMillan, Colin and Vinayak Narwade. 2018. *The Industry Energy Tool (IET):
Documentation*. Golden, CO: National Renewable Energy Laboratory.
NREL/TP-6A20-71990. https://www.nrel.gov/docs/fy19osti/71990.pdf for further background information. 

