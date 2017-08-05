# All the plots, maps and tables are defined in this script

setwd("~/faosync/fra/fra_countryprofiles/output/process/")

## ---- setup ----
source("../../input/code/ggplot2_themes.R")
source("../../input/code/knitr_hooks.R")
# source('../../input/code/plot/map_categories.R')


## ---- bar1 ----
ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
       title= paste("Forest coverage in",cntryname),
       subtitle="A plot that is only useful for demonstration purposes",
       caption = Sys.time()) +
  theme_ipsum_rc()
