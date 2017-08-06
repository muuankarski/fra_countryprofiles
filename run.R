# Main script for producing the documents
library(tidyverse)
library(extrafont)
library(hrbrthemes)
library(xtable)

upload = F

# include_intro <- T
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
      # remove square kilometers
      gsub("km\\^2\\^", "km2", .) %>% 
      writeLines(paste0("./input/data/intro_texts/",iso3tmp,".txt"))
  }
}

###### SMARTPHONE OPTIMIZED BEGINS #########

# Create folder for final smarphone pdfs
dir.create("./output/final_smartphone/", recursive = TRUE, showWarnings = FALSE)
# copy static files required in processing
file.copy("./input/figures/FAO_logo_Black_2lines_en.pdf", 
          to = "./output/process/")

# add a test that only run for countries you can find a matching introductory text
cntries_in_introtexts <- gsub("\\.tmp$", "", list.files("./input/data/intro_texts/", ".tmp"))
cntrycodes <- FAOcountryProfile %>% 
  # lets work only with countries that match in introtext filenames with 
  # SHORT_NAME variable in FAOcountryProfile 
  filter(SHORT_NAME %in% cntries_in_introtexts,
         !is.na(ISO3_CODE)) %>% 
  slice(10:30) %>%
  pull(ISO3_CODE)

# loop for smartphone begins
# for (cntrycode in cntrycodes){

cntrycode <- "FIN" # debug with Finland
# cntrycode <- 106 # debug with Italy
# cntrycode <- 33 # debug with Canada

cntryname <- FAOcountryProfile %>% 
  filter(ISO3_CODE == cntrycode) %>% 
  mutate(SHORT_NAME = gsub(" |ö|ä|å|", "", SHORT_NAME)) %>% 
  pull(SHORT_NAME)

rnwfile <- "./output/process/smartphone.Rnw"
if (file.exists(rnwfile)) file.remove(rnwfile)
file.create(rnwfile)

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

system("pdflatex smartphone.tex")
system("pdflatex smartphone.tex")
setwd("~/faosync/fra/fra_countryprofiles/")
# Copy the final pdf into the output/smartphone folder
file.copy(from = "./output/process/smartphone.pdf", 
          to = paste0("./output/final_smartphone/smartphone_", cntrycode,".pdf"), 
          overwrite = TRUE)

# } # loop for smartphone ends


###### A4 BEGINS #########



if (upload) source("./input/code/run_comparisons.R")

