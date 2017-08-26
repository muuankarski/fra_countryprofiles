# Main script for producing the documents
library(tidyverse)
library(extrafont)
loadfonts()
library(hrbrthemes)
library(xtable)

upload <- T
debug <- F
all_in <- T


# Which doctypes to process
smartphone <- F
a4 <- F
html <- T

# set debug countrycodes
cntrycodes <- c("FIN",
                "ITA",
                "IND",
                "CAN",
                "AFG",
                "AUS", 
                "COG", # republic of Congo
                "BRA",
                "KAZ")

include_intro <- T
include_plots <- T
include_tables <- T


setwd("~/faosync/fra/fra_countryprofiles/")

# remove and re-create process folder where 
unlink("./output/process", recursive = FALSE, force = TRUE)
dir.create("./output/process", showWarnings = FALSE, recursive = TRUE)

FAOcountryProfile <- read_csv("./input/data/FAOcountryProfile.csv")
library(maptools)
data("wrld_simpl")
library(rgdal)
library(rgeos)
ccentroids <- SpatialPointsDataFrame(gCentroid(wrld_simpl, byid=TRUE), 
                                      wrld_simpl@data, match.ID=FALSE) %>% 
  as_data_frame() %>% 
  select(ISO3, LON, LAT)

mapdata <- broom::tidy(wrld_simpl)

dat <- read.csv("./input/data/fra_data/Country_profiles_20160211.csv") %>% 
  mutate(PA_perc = as.numeric(PA_perc),
         MP_perc = as.numeric(MP_perc))
# melt the data as needed for ggplot2
mdat <- gather(dat, key = variable, value = value, 2:72) %>%
  mutate(year = as.integer(gsub("^.+_", "", variable)),
         variable = gsub("_[0-9]+$", "", variable)) %>% 
  select(Country, variable, year, value)

header  <- read_csv("./input/data/fra_data/Header_20160308.csv")
forstat <- read_csv("./input/data/fra_data/Forest_statistics.csv")
faostat <- read_csv("./input/data/fra_data/FAOSTAT_2013_4.csv")

## Preprocess the introductory texts
if (!file.exists("./input/data/intro_texts/FIN.txt")){
  cntries_in_introtexts <- gsub("\\.tmp$", "", list.files("./input/data/intro_texts/", ".tmp"))
  for (i in cntries_in_introtexts){
    
    # find matchin ISO3
    if (!i %in% FAOcountryProfile$SHORT_NAME) next()
    iso3tmp <- FAOcountryProfile %>% 
      filter(SHORT_NAME %in% i,
             !is.na(ISO3_CODE)) %>% 
      slice(1) %>% 
      pull(ISO3_CODE) 
    
    file.copy(from = paste0("./input/data/intro_texts/",i,".tmp"), 
              to = paste0("./input/data/intro_texts/",iso3tmp,".tex"), overwrite = TRUE)
    system(paste0("pandoc -s ./input/data/intro_texts/",iso3tmp,
                  ".tex -o ./input/data/intro_texts/",iso3tmp,".txt"))
    file.remove(paste0("./input/data/intro_texts/",iso3tmp,".tex"))
    # remove tex markup
    readLines(paste0("./input/data/intro_texts/",iso3tmp,".txt")) %>% 
      # remove temperature
      gsub("\\\\,\\^\\{\\\\circ\\}\\\\mathrm\\{C\\}\\$", 
           " Celsius", 
           .) %>% 
      gsub(" \\$|^\\$", " ", .) %>% 
      gsub("-\\$", "-", .) %>% 
      gsub("\\(\\$", "(", .) %>% 
      # remove square kilometers
      gsub("km\\^2\\^", "km2", .) %>% 
      # remove latitudes
      gsub("\\\\,\\^\\{\\\\circ\\}\\\\mathrm\\{S\\}\\$", 
           " Sdegrees", 
           .) %>%
      # remove latitudes
      gsub("\\\\,\\^\\{\\\\circ\\}\\\\mathrm\\{F\\}\\$", 
           " Fdegrees", 
           .) %>% 
      # some more
      gsub("\\$\\^\\{\\\\circ\\}\\$", 
           " degrees", 
           .) %>%
      writeLines(paste0("./input/data/intro_texts/",iso3tmp,".txt"))
  }
}

# Create folder for final smarphone pdfs, a4 pdfs and html
dir.create("./output/final_smartphone/", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/final_a4/", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/final_html/", recursive = TRUE, showWarnings = FALSE)


# add a test that only run for countries you can find a matching introductory text
cntries_in_introtexts <- gsub("\\.tmp$", "", list.files("./input/data/intro_texts/", ".tmp"))

if (all_in){
cntrycodes <- FAOcountryProfile %>% 
  # lets work only with countries that match in introtext filenames with 
  # SHORT_NAME variable in FAOcountryProfile 
  filter(SHORT_NAME %in% cntries_in_introtexts,
         !is.na(ISO3_CODE)) %>% 
  # slice(65:70) %>%
  pull(ISO3_CODE)
}


################################################################
#                           _         _                                      _  __ 
#  ___ _ __ ___   __ _ _ __| |_ _ __ | |__   ___  _ __   ___       _ __   __| |/ _|
# / __| '_ ` _ \ / _` | '__| __| '_ \| '_ \ / _ \| '_ \ / _ \_____| '_ \ / _` | |_ 
# \__ \ | | | | | (_| | |  | |_| |_) | | | | (_) | | | |  __/_____| |_) | (_| |  _|
# |___/_| |_| |_|\__,_|_|   \__| .__/|_| |_|\___/|_| |_|\___|     | .__/ \__,_|_|  
#                              |_|                                |_|              

###### SMARTPHONE OPTIMIZED BEGINS #########

unlink("./output/process/figure", recursive = TRUE, force = TRUE)
file.remove(list.files("./output/process/", full.names = TRUE))
file.copy("./input/figures/FAO_logo_Black_2lines_en.pdf", 
          to = "./output/process/")



# loop for smartphone begins
if (smartphone){
doctype <- "latex"
# if (debug) 
  # cntrycode="FIN"
for (cntrycode in cntrycodes){
  
  # Lets check if it is produced already
  if (file.exists(paste0("./output/final_smartphone/smartphone_",cntrycode,".pdf"))) next()

cntryname <- FAOcountryProfile %>% 
  filter(ISO3_CODE == cntrycode) %>% 
  mutate(SHORT_NAME = gsub(" |ö|ä|å|", "", SHORT_NAME)) %>% 
  pull(SHORT_NAME)

# Flag
iso2c <- FAOcountryProfile %>% filter(ISO3_CODE == cntrycode) %>% pull(ISO2_CODE) %>% tolower()
if (is.na(iso2c)) next() # if there is no valid iso2c code
file.copy(paste0("~/faosync/fra/fra_countryprofiles/input/figures/flags/",iso2c,".pdf"),
          "~/faosync/fra/fra_countryprofiles/output/process/")

rnwfile <- "./output/process/smartphone.Rnw"
if (file.exists(rnwfile)) file.remove(rnwfile)
file.create(rnwfile)

# \\documentclass[10pt]{scrartcl}
cat('
\\documentclass[a4]{article}
\\usepackage[paperwidth=4.28in, paperheight=7.75in,top=15mm,bottom=20mm,left=5mm,right=5mm]{geometry}
', file = rnwfile, append=TRUE)

# Add common header
readLines("~/faosync/fra/fra_countryprofiles/input/templates/header_pdf.tex") %>% 
  cat(., sep = "\n", file = rnwfile, append = TRUE)

# add content
readLines("./input/templates/content_smartphone.Rnw") %>% 
  cat(., sep = "\n", file = rnwfile, append = TRUE)


setwd("~/faosync/fra/fra_countryprofiles/output/process/")
knitr::knit("smartphone.Rnw", encoding = "utf-8")
# Embed fonts
flist <- list.files("./figure",
                    recursive = TRUE,
                    include.dirs = TRUE,
                    full.names = TRUE)

for (plot in flist) {
  embed_fonts(plot)
}

system("xelatex smartphone.tex")
system("xelatex smartphone.tex")
setwd("~/faosync/fra/fra_countryprofiles/")
# Copy the final pdf into the output/smartphone folder
file.copy(from = "./output/process/smartphone.pdf", 
          to = paste0("./output/final_smartphone/smartphone_", cntrycode,".pdf"), 
          overwrite = TRUE)

} # loop for smartphone ends
}


#####################################################3
#      _   _  _                   _  __ 
#     / \ | || |        _ __   __| |/ _|
#    / _ \| || |_ _____| '_ \ / _` | |_ 
#   / ___ \__   _|_____| |_) | (_| |  _|
#  /_/   \_\ |_|       | .__/ \__,_|_|  
#                      |_|              
###### A4 BEGINS #########

unlink("./output/process/figure", recursive = TRUE, force = TRUE)
file.remove(list.files("./output/process/", full.names = TRUE))
file.copy("./input/figures/FAO_logo_Black_2lines_en.pdf", 
          to = "./output/process/")


if (a4){
doctype <- "latex"
for (cntrycode in cntrycodes){
  
  # cntrycode <- "FIN" # debug with Finland
  
  # Lets check if it is produced already
  if (file.exists(paste0("./output/final_a4/a4_",cntrycode,".pdf"))) next()
  
  cntryname <- FAOcountryProfile %>% 
    filter(ISO3_CODE == cntrycode) %>% 
    mutate(SHORT_NAME = gsub(" |ö|ä|å|", "", SHORT_NAME)) %>% 
    pull(SHORT_NAME)
  # Flag
  iso2c <- FAOcountryProfile %>% filter(ISO3_CODE == cntrycode) %>% pull(ISO2_CODE) %>% tolower()
  if (is.na(iso2c)) next() # if there is no valid iso2c code
  file.copy(paste0("~/faosync/fra/fra_countryprofiles/input/figures/flags/",iso2c,".pdf"),
            "~/faosync/fra/fra_countryprofiles/output/process/")
  
  rnwfile <- "./output/process/a4.Rnw"
  if (file.exists(rnwfile)) file.remove(rnwfile)
  file.create(rnwfile)
  
  cat('
\\documentclass[10pt]{scrartcl}
%% Use full page in book style
\\usepackage[top=1.5cm, bottom=3.5cm, left=1.5cm, right=1.5cm]{geometry}
', file = rnwfile, append=TRUE)
  
  
  # add header
  readLines("~/faosync/fra/fra_countryprofiles/input/templates/header_pdf.tex") %>% 
    cat(., sep = "\n", file = rnwfile, append = TRUE)
  
  # add content
  readLines("./input/templates/content_a4.Rnw") %>% 
    cat(., sep = "\n", file = rnwfile, append = TRUE)
  
  
  setwd("~/faosync/fra/fra_countryprofiles/output/process/")
  knitr::knit("a4.Rnw", encoding = "utf-8")
  # Embed fonts
  flist <- list.files("./figure",
                      recursive = TRUE,
                      include.dirs = TRUE,
                      full.names = TRUE)
  
  for (plot in flist) {
    embed_fonts(plot)
  }
  
  system("xelatex a4.tex")
  system("xelatex a4.tex")
  setwd("~/faosync/fra/fra_countryprofiles/")
  # Copy the final pdf into the output/smartphone folder
  file.copy(from = "./output/process/a4.pdf", 
            to = paste0("./output/final_a4/a4_", cntrycode,".pdf"), 
            overwrite = TRUE)
  
} # loop for a4 ends
}



######################################################
#  _     _             _ 
# | |__ | |_ _ __ ___ | |
# | '_ \| __| '_ ` _ \| |
# | | | | |_| | | | | | |
# |_| |_|\__|_| |_| |_|_|
#  
###### html BEGINS #########

unlink("./output/process/", recursive = TRUE, force = TRUE)
dir.create("./output/process/", recursive = TRUE, showWarnings = FALSE)

jpgflags <- list.files("~/faosync/fra/fra_countryprofiles/input/figures/flags/", pattern = ".jpg", full.names = TRUE)
dir.create("~/faosync/fra/fra_countryprofiles/output/final_html/jpgflags", recursive = TRUE, showWarnings = FALSE)
file.copy(from = jpgflags, to = "~/faosync/fra/fra_countryprofiles/output/final_html/jpgflags/", overwrite = FALSE)

if (html){
doctype <- "html"
for (cntrycode in cntrycodes){
  
  # Lets check if it is produced already
  if (file.exists(paste0("./output/final_html/html_",cntrycode,".html"))) next()
  
  # cntrycode <- "FIN" # debug with Finland
  setwd("~/faosync/fra/fra_countryprofiles/")
  cntryname <- FAOcountryProfile %>% 
    filter(ISO3_CODE == cntrycode) %>% 
    mutate(SHORT_NAME = gsub(" |ö|ä|å|", "", SHORT_NAME)) %>% 
    pull(SHORT_NAME)
  # Flag
  iso2c <- FAOcountryProfile %>% filter(ISO3_CODE == cntrycode) %>% pull(ISO2_CODE) %>% tolower()
  
  if (is.na(iso2c)) next() # if there is no valid iso2c code

  rnwfile <- "./output/process/html.Rmd"
  if (file.exists(rnwfile)) file.remove(rnwfile)
  file.create(rnwfile)
  
cat(paste0(
'---
title: ',cntryname,'
date: "`r Sys.time()`"
output: 
  html_document: 
    toc: true
    toc_float: true
    self_contained: false
    lib_dir: libs
    number_sections: yes
    theme: yeti
---

<img src="./jpgflags/',iso2c,'.jpg" alt="Smiley face" width="150" align="right" style="box-shadow: 2px 2px 2px #888888;">

'), file = rnwfile)

  
  # add content
  readLines("./input/templates/content_html.Rmd") %>% 
    cat(., sep = "\n", file = rnwfile, append = TRUE)
  head <- readLines("./input/templates/head.html")
  
  setwd("~/faosync/fra/fra_countryprofiles/output/process/")
  rmarkdown::render("html.Rmd", encoding = "utf-8" )
  
  # input the new heading & default navbar
  html_content <- readLines("./html.html")
  stop <- match('<div class="fluid-row" id="header">', html_content)
  html_content <- html_content[-1:-stop]
  c(head,html_content) %>% 
    writeLines("./html.html")

  
  setwd("~/faosync/fra/fra_countryprofiles/")
  # Copy the final pdf into the output/smartphone folder
  file.copy(from = "./output/process/html.html", 
            to = paste0("./output/final_html/html_", cntrycode,".html"), 
            overwrite = TRUE)
  
  
  file.copy(from = paste0("./output/process/figure_",cntrycode,"/"), 
            to = "./output/final_html/", 
            overwrite = TRUE, recursive = TRUE)
  
  file.copy(from = "./output/process/libs/", 
            to = "./output/final_html/", 
            overwrite = FALSE, recursive = TRUE)
  
  
} # loop for html ends
}

###

cat("Options +Indexes", file = "./output/final_a4/.htaccess")
cat("Options +Indexes", file = "./output/final_html/.htaccess")
cat("Options +Indexes", file = "./output/final_smartphone/.htaccess")

if (upload) source("~/faosync/fra/fra_countryprofiles/input/code/run_comparisons.R")

