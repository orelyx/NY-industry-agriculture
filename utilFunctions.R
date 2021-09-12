# title: "utilFunctions"
# author: "Eric Koski"
# date: "12/9/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

# Trim integer (or vector of integers) to specified # of most dignificant digits. 
trimInt <- function(int, toDigits = 1) {
  if (toDigits <= 0) {
    toDigits <- 0
    warning("toDigits <= 0; result coerced to NA(s)\n")
  }
  chrInt <- str_extract(as.character(int), 
                        str_c("([-]){0,1}[[:digit:]]{0,", as.character(toDigits), "}"))
  as.integer(as.numeric(chrInt))
}

# scale function for plotting y-axis labels
scientific_10 <- function(x) {
    s <- scales::scientific_format()(x)
    # substitute for exact zeros
    s[s=="0e+00"] <- "0"
    # regex: [+]?  = "zero or one occurrences of '+'"
    parse(text=gsub("e[+]?", " %*% 10^", s ))
}

#################################################################
# Evaluate expr without messages or warnings
# It's usually better to use the Rmarkdown chunk parameters echo, 
# warnings, messages, etc. 
suppressSnark <- function(expr) {
  suppressWarnings(suppressMessages(expr))
}


#################################################################
# Render table, assigning it a latex label based on the code 
# chunk filename (returned by current_input()) and an optional 
# suffix for when a code chunk creates more than one table. 
tableCaption <- function(str, suffix = "") {
  str_c("\\label{tab:", 
        stri_replace_all_fixed(knitr::current_input(),
                               ".Rmd",
                               ""),
        suffix,
        "}", 
        str)
}

###################################################################
# Supplied either a single string or a vector of strings, build 
# a regex to use in searching for whether the string or strings 
# (all of them) occur anywhere in the string being searched (e.g., 
# by using str_match()). Use only strings of alphanumeric 
# characters to avoid strange regex interactions. 
searchRegex <- function(vec) {
  regex <- "[A-Za-z0-9_, -()]*"
  if (is.character(vec)) {
    for(c in vec) {
      regex <- str_c(regex, c, "[A-Za-z0-9_, -()]*")
    }
  } else regex <- NA
  regex
}

#####################################################################
# In column indexCol of df, look up the unique row in which the value
# of indexCol # contains all of the char values in rowSearch. Then 
# select the unique column whose name contains all of the char values 
# in colSearch. Return the unique value found as double. Returns NA
# if no value or multiple values found. 
dfLookup <- function (df, indexCol, rowSearch, colSearch){
  if(suppressWarnings(is.null(dim(df)) | 
                      anyNA(indexCol) |
                      anyNA(rowSearch) | 
                      anyNA(colSearch))) {
    NA
  } else {
    if(indexCol %in% names(df)){
      found <- select(
        filter(df, !is.na(str_match(!!ensym(indexCol), 
                                    searchRegex(rowSearch)))), 
        matches(searchRegex(colSearch)))
      if ((nrow(found) == 1) & (ncol(found) == 1))
      {
        as.double(found)
      } else {
        warning("wrong number of values found in lookup")
        NA
      }
    } else NA
  }
}


# highlight color for stripes in pdf tables
honeydew <- "#F0FFF0"
