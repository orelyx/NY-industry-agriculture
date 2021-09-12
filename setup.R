# title: "setup.R"
# author: "Eric Koski"
# date: "12/9/2019"
#     Copyright (c) 2021 Orebed Analytics LLC under MIT License; see LICENSE.md. 
# 
#     Data files produced by this software are licensed under a Creative Commons 
#     Attribution 4.0 International License; see
#     https://creativecommons.org/licenses/by/4.0/. 

library(tidyverse)
library(tidyselect)
library(stringi)
library(readxl)
library(scales)
library(fs)
library(utils)
library(markdown)
library(rmarkdown)
library(kableExtra)
library(bookdown)
library(git2r)
library(citr)
library(R6)
library(glue)
library(latex2exp)
library(ltxsparklines)
usepackage_latex("siunitx")

options(ltxsparklines.output='knitr')
options(
 ltxsparklines.width = 10,
 ltxsparklines.clip = FALSE,
 ltxsparklines.na.rm = TRUE,
 ltxsparklines.bottomline = TRUE,
 ltxsparklines.bottomlinex = c(NA, NA),
 ltxsparklines.startdotcolor = NA,
 ltxsparklines.enddotcolor = NA,
 ltxsparklines.dotcolor='blue'
 )


# Lets us set the font size for code chunks. From 
# https://stackoverflow.com/questions/25646333/code-chunk-font-size-in-rmarkdown-with-knitr-and-latex
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})

# Are we in a git repository? If so, get status information from the repository. 
if (in_repository(".")) {
  gitSHA <- substring(as.character(sha(repository_head(repository(".")))), 1, 8)
  headCommitDate <- stri_sub(stri_extract_first_regex(
    capture.output(lookup_commit(repository_head(repository(".")))), 
    "([a-z0-9]){6}..[0-9]{4}[-][0-9]{2}[-][0-9]{2}"), from = -10)
  gitRepoStatus <- capture.output(status(repository(".")))
  gitRepoStamp <- str_c(gitSHA, "\n", 
                        headCommitDate, "\n",
                        gitRepoStatus)
  gitStatusBrief <- str_c(gitSHA, " ", headCommitDate)
  if (max(str_detect(gitRepoStatus, coll("untracked", ignore_case = TRUE, locale = "en")))) {
    gitStatusBrief <- str_c(gitStatusBrief, " UF!")
  }
  if (max(str_detect(gitRepoStatus, coll("unstaged", ignore_case = TRUE, locale = "en")))) {
    gitStatusBrief <- str_c(gitStatusBrief, " UC!")
  }
  
  # Write a new "gitStamp.txt" file
  if (file_exists("gitStamp.txt")) {
    file_delete("gitStamp.txt")
  }
  gitStamp <- file("gitStamp.txt", open = 'a')
  writeLines(gitRepoStamp, con = gitStamp)
  close(gitStamp)
} else {
  
  # Check for a gitStamp.txt file
  if (!file_exists("gitStamp.txt")) {
    gitStatusBrief <- "Couldn't find gitStamp.txt!"
  } else {  # Get git stamp information from file
    gitStamp <- file("gitStamp.txt", open = 'r')
    line1and2 <- readLines(con = gitStamp, n = 2)
    fileSHA <- line1and2[1]
    headCommitDate <- line1and2[2]
    gitStampText <- readLines(con = gitStamp)
    close(gitStamp)
    gitStatusBrief <- str_c(fileSHA, " ", headCommitDate) 
    if (max(str_detect(gitStampText, coll("untracked", ignore_case = TRUE, locale = "en")))) {
      gitRepoUntracked <- TRUE
      gitStatusBrief <- str_c(gitStatusBrief, " UF!")
    }
    if (max(str_detect(gitStampText, coll("unstaged", ignore_case = TRUE, locale = "en")))) {
      gitRepoUnstaged <- TRUE
      gitStatusBrief <- str_c(gitStatusBrief, " UC!")
    }
  }
}
