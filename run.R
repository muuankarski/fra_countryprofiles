# Main script for producing the documents
library(tidyverse)
library(extrafont)
library(hrbrthemes)

upload = F


# remove and re-create process folder where 
unlink("./output/process", recursive = FALSE, force = TRUE)
dir.create("./output/process", showWarnings = FALSE, recursive = TRUE)

FAOcountryprofile <- read_csv("./input/data/FAOcountryProfile.csv")

###### SMARTPHONE OPTIMIZED BEGINS #########

# Common preface across countries
preface <- readLines("./input/templates/preface_smartphone.tex")
ending <- readLines("./input/templates/ending_smartphone.tex")
# Create folder for final smarphone pdfs
dir.create("./output/final_smartphone/", recursive = TRUE, showWarnings = FALSE)
# copy static files required in processing
file.copy("./input/figures/FAO_logo_Black_2lines_en.pdf", 
          to = "./output/process/")

# add a test that only run for countries you can find a matching introductory text
cntries_in_introtexts <- gsub("\\.tmp$", "", list.files("./input/data/intro_texts/", ".tmp"))
cntrycodes <- FAOcountryprofile %>% 
  # lets work only with countries that match in introtext filenames with 
  # SHORT_NAME variable in FAOcountryprofile 
  filter(SHORT_NAME %in% cntries_in_introtexts) %>% 
  pull(FAOST_CODE)

# loop for smartphone begins
# for (cntrycode in cntrycodes){

cntrycode <- 67 # debug with Finland
cntryname <- FAOcountryprofile %>% 
  filter(FAOST_CODE == cntrycode) %>% 
  mutate(SHORT_NAME = gsub(" |ö|ä|å|", "", SHORT_NAME)) %>% 
  pull(SHORT_NAME)

rnwfile <- "./output/process/smartphone.Rnw"
if (file.exists(rnwfile)) file.remove(rnwfile)
file.create(rnwfile)
# add preface
writeLines(preface, con = rnwfile)

# Add title

cat(paste0(
'
\\title{A draft  for a \\textit{smartphone optimized pdf} for FAOSTAT}
\\author{\\Large ',cntryname,'}
\\maketitle
'  
), file = rnwfile, append = TRUE)

# add introtext
readLines(paste0("./input/data/intro_texts/",cntryname,".tmp")) %>% 
  cat(., sep = "\n", file = rnwfile, append = TRUE)


# add content

readLines("./input/templates/content_smartphone.Rnw") %>% 
  cat(., sep = "\n", file = rnwfile, append = TRUE)

# add ending
cat(ending, file = rnwfile, append = TRUE)


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
          to = paste0("./output/final_smartphone/smartphone_", cntryname,".pdf"), 
          overwrite = TRUE)

# } # loop for smartphone ends


###### A4 BEGINS #########

# Common preface across countries
preface <- readLines("./input/templates/preface_a4.tex")
ending <- readLines("./input/templates/ending_a4.tex")



if (upload) source("./input/code/run_comparisons.R")

