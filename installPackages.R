# installPackages.R -- stand-alone script to install all R packages required for 
#                      this report. 

requiredPackages <- c(
    "tidyverse",
    "tidyselect",
    "stringi",
    "readxl",
    "scales",
    "fs",
    "utils",
    "markdown",
    "rmarkdown",
    "kableExtra",
    "bookdown",
    "git2r",
    "R6",
    "glue",
    "latex2exp",
    "ltxsparklines")

# Install current versions of required packages. May reveal some incompatibilities 
# between repo code and package versions extant sometime after repo version was 
# released. 
installCurrent <- function() 
{
  for (p in requiredPackages) {
    install.packages(p, character.only = TRUE)
  }
}

# A halfhearted gesture at reproducibility. installAsOfDate() can be used from 
# the console to install packages as of a particular date -- say, the date on 
# which a version of the github repository was released. 
installAsOfDate <- function(date) {
  install.packages("versions")
  library(versions)
  library(stringr)
  if (date == str_extract(date, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) {
    year <- as.integer(str_extract(date, "[0-9]{4}"))
    if (year >= 2021) {
      install.dates(requiredPackages, date)
    } else {
      errorCondition("Date must be 2021-01-01 or later.")
    }
  } else {
    errorCondition("Date must be formatted yyyy-mm-dd.")
  }
}
