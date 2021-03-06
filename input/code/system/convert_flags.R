# svg maps are in ~/faosync/

setwd("~/faosync/fra/flag-icon-css/flags/4x3/")
svgs <- gsub("\\.svg","", list.files("./", pattern = ".svg"))
for (i in svgs){
  system(paste0("rsvg-convert -f pdf -o ~/faosync/fra/fra_countryprofiles/input/figures/flags/",i,".pdf ",i,".svg"))
  system(paste0("convert -density 150 -alpha remove ~/faosync/fra/fra_countryprofiles/input/figures/flags/",i,".pdf ~/faosync/fra/fra_countryprofiles/input/figures/flags/",i,".jpg"))
}

# for i in *.JPG; do exiv2 -v -r '%Y%m%d.%H%M%S.tammihelmi2014' rename "$i"; done