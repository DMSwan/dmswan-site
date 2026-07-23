library(pandoc)
library(bib2df)
library(tidyverse)
library(yaml)

#removes a bunch of extra characters that might be lurking in bibtex titles
#for the purpose of creating clean subfolder names
cleanTitle <- function(title){
  str_replace_all(str_remove_all(str_trim(str_sub(title, 1, 48)), "\\,|\\?|\\:|\\{|\\}"), " ", "-")
}

#Creates the subfolders where the citations are going to live
createFolders <- function(title){
  title <- file.path(publication_folder, cleanTitle(title))
  
  if(dir.exists(title)) return(NA) #doesn't create a folder if one already exists
  
  dir.create(title)
  print(paste("Created", title, "directory"))
}

#Helper function to write an individual citation to each folder
createBibs <- function(df){
  write_dr <- file.path(publication_folder,cleanTitle(df$TITLE))
  
  if(file.exists(file.path(write_dr,"citation.bib"))) return (NA) #doesn't create a bibfile if one already exists
  
  df2bib(df, file = file.path(write_dr, "citation.bib"))
}

#helper function that produces a yaml file from a bibtex
convertBibs <- function(folder){
  if(file.exists(file.path(folder, "index.qmd"))|!file.exists(file.path(folder, "citation.bib"))) return(NA) #makes sure not to create a file if one exists
  
  pandoc_convert(file = file.path(folder, "citation.bib"), from = "biblatex", to = "markdown", standalone = TRUE, output = file.path(folder, "index.qmd"))
}

reorderYaml <- function(folder) {
  qmd_path <- file.path(folder, "index.qmd")
  if (!file.exists(qmd_path)) return(NA)
  
  yaml_block <- yaml.load_file(qmd_path)

  yaml_block <- yaml_block$references[[1]]

  yaml_block$citation$type <- yaml_block$type
  yaml_block$citation$`container-title` <- yaml_block$`container-title`
  yaml_block$citation$url <- yaml_block$url
  yaml_block$citation$doi <- yaml_block$doi
  yaml_block$citation$volume <- yaml_block$volume
  yaml_block$citation$page <- yaml_block$page

  yaml_block$type <- NULL
  yaml_block$`container-title` <- NULL
  yaml_block$url <- NULL
  yaml_block$doi <- NULL
  yaml_block$volume <- NULL
  yaml_block$page <- NULL

write_lines(c("---", as.yaml(yaml_block), "---"), qmd_path)
}

#set the publications folder location
publication_folder <- "publications"

#Get the data, Mendeley groups makes this process blow up later
ref_dat <- bib2df(file.path(publication_folder, "pubs.bib"))

#create the individual folders
walk(ref_dat$TITLE, createFolders)

#Puts a single-publication citation in each folder
ref_dat |>
  rowwise() |>
  group_walk(~createBibs(.x))

#Get all the top-level directory names
dirs <- list.dirs(publication_folder, recursive = FALSE)

#Get only those folders that don't presently have a qmd file
dirs <- dirs[!file.exists(file.path(dirs, "index.qmd"))]

walk(dirs, convertBibs)

walk(dirs, reorderYaml)

pandoc_convert(file = "pubs.bib", from = "biblatex", to = "html", standalone = TRUE, output =  "publications.qmd")
