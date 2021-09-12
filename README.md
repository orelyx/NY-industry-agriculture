# NY-industry-agriculture

Report of industry and agriculture greenhouse gas emissions from energy use (fuel combustion) 
in the Genesee-Finger Lakes Region for years 2010-2016; could be easily adapted to produce a similar 
report for any portion (county or counties) of New York State. Principal data source is the NREL 
Industrial Energy Data Book (IEDB).  Prepared using R and R markdown; main 
Rmd file is Interim-report-GFL-industry-and-agriculture.Rmd, producing the pdf file 
Interim-report-GFL-industry-and-agriculture.pdf. To re-generate the report, open the project in Rstudio
and knit Interim-report-GFL-industry-and-agriculture.Rmd to pdf. installPackages.R lists the required 
packages and supplies a function installAsOfDate() which can be used to re-create the R environment in 
which a prior version of the document was created. Uses the Tex-Live LaTeX environment provided by the 
R package Tinytex. 
