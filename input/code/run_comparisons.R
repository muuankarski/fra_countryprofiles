library(dplyr)
root.dir <- "~/faosync/fra/fra_countryprofiles/"

dir.create(paste0(root.dir,"/output"), recursive = TRUE, showWarnings = FALSE)
file.copy(paste0(root.dir,"/input/templates/toc.Rmd"), to = paste0(root.dir,"/output"), overwrite = TRUE)

setwd(paste0(root.dir,"/output"))
rmarkdown::render("toc.Rmd",output_file = "index.html")
file.remove("./toc.Rmd")
setwd(root.dir)

unlink(paste0(root.dir,"/output/process"), recursive = TRUE, force = TRUE) 

# Sync to kapsi!!
if (Sys.info()["nodename"] =="markus-desktop-mint18")     system('rsync -avzhe "ssh -i /home/aurelius/avaimet/nuc-rsync-key" --progress --delete /home/aurelius/faosync/fra/fra_countryprofiles/output/ muuankarski@kapsi.fi:sites/software.markuskainu.fi/www/fao/fra_countryprofiles/')
if (Sys.info()["nodename"] =="markus-x220") system('rsync -avzhe "ssh -i /home/aurelius/avaimet/x220-rsync-key" --progress --delete /home/aurelius/faosync/fra/fra_countryprofiles/output/ muuankarski@kapsi.fi:sites/software.markuskainu.fi/www/fao/fra_countryprofiles/')
if (Sys.info()["nodename"] == "markus-T430")              system('rsync -avzhe "ssh -i /home/aurelius/avaimet/t430-rsync-key" --progress --delete /home/aurelius/faosync/fra/fra_countryprofiles/output/ muuankarski@kapsi.fi:sites/software.markuskainu.fi/www/fao/fra_countryprofiles/')
