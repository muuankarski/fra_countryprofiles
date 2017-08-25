# All the plots, maps and tables are defined in this script

setwd("~/faosync/fra/fra_countryprofiles/output/process/")

if (debug){
  
  cntrycode = "FIN"
  cntryname = "Finland"

}

  


## ---- setup ----
library(knitr)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
loadfonts()


## ---- introtexts ----
readLines(paste0("../../input/data/intro_texts/",cntrycode,".txt")) %>% 
  Hmisc::latexTranslate(.) %>% 
  cat(., sep = "\n")

## ---- map1 ----
map.plot <- mapdata %>% 
  mutate(fill = ifelse(id == cntrycode, "fill", NA))

mapcentr <- ccentroids %>% filter(ISO3 == cntrycode)

worldmap <- ggplot(map.plot, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = fill), color = alpha(alpha = .5, colour = "white"), show.legend = FALSE)
worldmap + coord_map("ortho", orientation = c(mapcentr$LAT, mapcentr$LON, 0)) + 
  # labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
  #      title= paste(cntryname, "is here!"),
  #      subtitle="Some dynamic content could be added here",
  #      caption = "Source: Cite the right data here!") +
  ggrepel::geom_label_repel(data =   map.plot %>% filter(!is.na(fill)) %>% 
                              summarise(long = mean(long),
                                        lat = mean(lat)),
                            aes(label = cntryname, group = 1), nudge_x = 10, 
                            nudge_y = -10, fill = "#a25027", color = "white", family = "Roboto", fontface = "bold", size = 7) +
  theme_ipsum(grid = FALSE) +
  # scale_fill_manual(values = c("#a25027","#a9ccc4","#a9ccc4","#a9ccc4","#a9ccc4")) +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

## ---- cntrytable
# colnames(forstat) <- c("Country", 
#                        "Forest area (1000 ha)", 
#                        "Average annual forest area change 1990 - 2015 (1000 ha)",
#                        "Growing stock (m\\textsuperscript{3}\\textbackslash ha)", 
#                        "Protected areas (\\%)", 
#                        "Forest area under management plan (\\%)",
#                        "Employment\\footnotemark (1000 FTE)", 
#                        "Gross value added from forestry (1000 USD)")  
cforstat <- forstat %>% filter(Country == cntrycode) %>% 
  gather(., key = indicator, value = value, 2:8) %>% 
  mutate(label = recode(indicator, 
                        `F_area_2015` = "Forest area (1000 ha)" ,
                        `Annforchange_9015` = "Average annual forest area change 1990 - 2015 (1000 ha)",
                        `Gr_stock_2015` = "Growing stock (m\\textsuperscript{3}\\textbackslash ha)",
                        `Percprotfor_2015` = "Protected areas (\\%)",
                        `Managplan_Perc` = "Forest area under management plan (\\%)",
                        `Employm_2010` = "Employment\\footnotemark (1000 FTE)",
                        `USDForGrosVal` = "Gross value added from forestry (1000 USD)"
                        )) %>% 
  select(label,value)
if (nrow(cforstat) > 0){
  
  if (doctype == "latex"){
    print.xtable(xtable(cforstat), floating = FALSE,
                 include.rownames = FALSE, 
                 include.colnames = FALSE,
                 booktabs = FALSE,
                 timestamp=NULL, 
                 sanitize.text.function = identity
                 # only.contents = TRUE,
                 # zero.print=zero.print
    ) 
    } else {
      # html-table
      print.xtable(xtable(cforstat),
                   type = "html", 
                   include.rownames = FALSE, 
                   include.colnames = FALSE,
                   timestamp=NULL
      )
    }


}


## ---- line_forest_area_pie_land_use
pdat <- mdat %>% 
  filter(Country == cntrycode, 
         variable %in% "F_area")

if (nrow(pdat) > 0){
ggplot(data = pdat, aes(x=year, y=value)) + 
  geom_point() + geom_line() +
  labs(x = "Year",
       y = "Forest Area (1000 ha)",
       title= "Forest area 1990-2015",
       subtitle="Perhaps we can add some vague definition of the indicator here...?",
       caption = "Source: Cite the right data here!") +
  theme_ipsum(base_size = 14)
}

# ## -- pie_land_use
pdat <- mdat %>% 
  filter(Country == cntrycode, 
         variable %in% c("F_area","OWL","OL"),
         year == 2015) %>% 
  mutate(label = c("Forest", "Other wooded land", "Other land"),
         share = round(value / sum(value) * 100, 1),
         sharesum = sum(share)) %>% 
  arrange(share)

pdat$label <- factor(pdat$label, levels = pdat$label)

if (nrow(pdat) > 0){
p <- ggplot(pdat, aes(x=sharesum/2, y = share, fill = label, width = sharesum, ymax=1))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + geom_label(aes(x=sharesum * 2/2,y=share+10,label=paste0(share,"%")),
                    label.padding = unit(0.20, "lines"),
                    position="fill",
                    color="white",lineheight=.7,family = "Roboto",
                    stat="identity",alpha=.9,show.legend=FALSE)
p <- p + coord_polar("y")
p <- p + labs(x = "Year",
     y = "Forest Area (1000 ha)",
     title= "Land use",
     subtitle="Share of total land area in 2015",
     caption = "Source: Cite the right data here!") +
  theme_ipsum(grid = FALSE, base_size = 14) + scale_fill_ipsum()
p <- p + theme(axis.text = element_blank(),
               axis.title = element_blank(),
               axis.ticks = element_blank(),
               legend.title = element_blank(),
               legend.position = "right")
p
}


## ---- land_use_comparison
pdat <- mdat %>% 
  filter(variable %in% c("F_area","OWL","OL"),
         year == 2015) %>% 
  group_by(Country) %>% 
  mutate(share = round(value / sum(value) * 100, 3),
         sharesum = sum(share)) %>% 
  ungroup() %>% 
  filter(variable == "F_area") %>% 
  select(Country,share) %>% 
  arrange(desc(share)) %>% 
  mutate(fill = ifelse(Country %in% cntrycode, 
         FAOcountryProfile %>% filter(ISO3_CODE %in% cntrycode) %>% pull(SHORT_NAME),
         NA))

if (nrow(pdat) > 0 & any(!is.na(pdat$fill))){
ggplot(pdat, aes(x=reorder(Country, share),y=share,fill=fill)) + 
  geom_col(show.legend = FALSE) +
  theme_ipsum(base_size = 14) + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  geom_label(data = pdat[!is.na(pdat$fill),],
             aes(x=Country,y=share*1.3,
                 label = paste0(fill,"\n",round(share,1), " %")), 
             show.legend = FALSE, 
             color = "white", lineheight = .8, alpha = .8) +
  labs(y = "share of total land use",
       title= "Share of forest area of total land use",
       subtitle="Ranking of all FAO member countries",
       caption = "Source: Cite the right data here!")
}


## ---- line_forest_type_pie_forest_type
pdat <- mdat %>% 
  filter(Country == cntrycode, 
         variable %in% c("F_prim", "F_onr", "F_pla")) %>% 
  mutate(label = recode(variable, 
                        `F_prim` = "Primary forest",
                        `F_onr` = "Other naturally regenerated forest",
                        `F_pla` = "Planted forest"))

if (nrow(pdat) > 0){
ggplot(data = pdat, aes(x=year, color=label, y=value, group = label)) + 
  geom_point() + geom_line() +
  labs(x = "Year",
       y = "Forest Area (1000 ha)",
       title= "Trends in forest types 1990-2015",
       subtitle="Perhaps we can add some vague definition of the indicator here...?",
       caption = "Source: Cite the right data here!") +
  theme_ipsum(base_size = 14) + scale_color_ipsum() +
  theme(legend.position = "top",
        legend.title = element_blank())
}


# # pie_forest_type ----
pdat <- mdat %>% 
  filter(Country == cntrycode, 
         variable %in% c("F_prim", "F_onr", "F_pla"),
         year == 2015,
         !is.na(value)) %>% 
  mutate(label = recode(variable, 
                        `F_prim` = "Primary forest",
                        `F_onr` = "Other naturally \nregenerated forest",
                        `F_pla` = "Planted forest"),
         share = round(value / sum(value) * 100, 1),
         sharesum = sum(share)) %>% 
  arrange(share)

pdat$label <- factor(pdat$label, levels = pdat$label)

if (nrow(pdat) > 0){
p <- ggplot(pdat, aes(x=sharesum/2, y = share, fill = label, width = sharesum, ymax=1))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + geom_label(aes(x=sharesum * 2/2,y=share+10,label=paste0(share,"%")),
                    label.padding = unit(0.20, "lines"),
                    position="fill",
                    color="white",lineheight=.7,family = "Roboto",
                    stat="identity",alpha=.9,show.legend=FALSE)
p <- p + coord_polar("y")
p <- p + labs(x = "Year",
              y = "Forest Area (1000 ha)",
              title= "Forest types",
              subtitle="Share of forest area in 2015",
              caption = "Source: Cite the right data here!") +
  theme_ipsum(grid = FALSE, base_size = 14) + scale_fill_ipsum()
p <- p + theme(axis.text = element_blank(),
               axis.title = element_blank(),
               axis.ticks = element_blank(),
               legend.title = element_blank(),
               legend.position = "right")
p
}


## ---- forest_type_comparison ----
pdat <- mdat %>% 
  filter(variable %in% c("F_prim", "F_onr", "F_pla"),
         year == 2015) %>% 
  group_by(Country) %>% 
  mutate(share = round(value / sum(value) * 100, 3),
         sharesum = sum(share)) %>% 
  ungroup() %>% 
  filter(variable == "F_prim",
         !is.na(share)) %>% 
  select(Country,share) %>% 
  arrange(desc(share)) %>% 
  mutate(fill = ifelse(Country %in% cntrycode, 
                       FAOcountryProfile %>% filter(ISO3_CODE %in% cntrycode) %>% pull(SHORT_NAME),
                       NA))

if (nrow(pdat) > 0 & any(!is.na(pdat$fill))){
ggplot(pdat, aes(x=reorder(Country, share),y=share,fill=fill)) + 
  geom_col(show.legend = FALSE) +
  theme_ipsum(base_size = 14) + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  geom_label(data = pdat[!is.na(pdat$fill),],
             aes(x=Country,y=share*1.3,
                 label = paste0(fill,"\n",round(share,1), " %")), 
             show.legend = FALSE, 
             color = "white", lineheight = .8, alpha = .8) +
  labs(y = "share of primary forests",
       title= "Share of primary forest area of total forests",
       subtitle="Ranking of all FAO member countries",
       caption = "Source: Cite the right data here!")
}

## ---- line_forest_fires_line_forest_designation_trends
pdat <- mdat %>% 
  filter(Country == cntrycode, 
         variable %in% "Fires")

if (nrow(pdat) > 0){
ggplot(data = pdat, aes(x=year, y=value)) + 
  geom_point() + geom_line() +
  labs(x = "Year",
       y = "Forest Area (1000 ha)",
       title= "Forest area affected by fire (1000 ha)",
       subtitle="Perhaps we can add some vague definition of the indicator here...?",
       caption = "Source: Cite the right data here!") +
  theme_ipsum(base_size = 14)
}

# # line_forest_designation_trends ----
pdat <- mdat %>% 
  filter(Country == cntrycode, 
         variable %in% c("D_prod", "D_MU", "D_BD")) %>% 
  mutate(label = recode(variable, 
                        `D_prod` = "Production",
                        `D_MU` = "Multiple use",
                        `D_BD` = "Conservation \n of biodiversity"))

if (nrow(pdat) > 0){
ggplot(data = pdat, aes(x=year, color=label, y=value, group = label)) + 
  geom_point() + geom_line() +
  labs(x = "Year",
       y = "Forest Area (1000 ha)",
       title= "Forest designation trends 1990-2015",
       subtitle="Perhaps we can add some vague definition of the indicator here...?",
       caption = "Source: Cite the right data here!") +
  theme_ipsum(base_size = 14) + scale_color_ipsum() +
  theme(legend.position = "top",
        legend.title = element_blank())
}

## ---- pie_ownership_line_carbon
pdat <- mdat %>% 
  filter(Country == cntrycode, 
         variable %in% c("O_pub", "O_priv", "O_unk"),
         year == 2010,
         !is.na(value)) %>% 
  mutate(label = recode(variable, 
                        `O_pub` = "Public",
                        `O_priv` = "Private",
                        `O_unk` = "Unknown"),
         share = round(value / sum(value) * 100, 1),
         sharesum = sum(share)) %>% 
  arrange(share)

pdat$label <- factor(pdat$label, levels = pdat$label)

if (nrow(pdat) > 0){
p <- ggplot(pdat, aes(x=sharesum/2, y = share, fill = label, width = sharesum, ymax=1))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + geom_label(aes(x=sharesum * 2/2,y=share+10,label=paste0(share,"%")),
                    label.padding = unit(0.20, "lines"),
                    position="fill",
                    color="white",lineheight=.7,family = "Roboto",
                    stat="identity",alpha=.9,show.legend=FALSE)
p <- p + coord_polar("y")
p <- p + labs(x = "Year",
              y = "Forest Area (1000 ha)",
              title= "Forest ownership",
              subtitle="Share of forest area in 2010",
              caption = "Source: Cite the right data here!") +
  theme_ipsum(grid = FALSE, base_size = 14) + scale_fill_ipsum()
p <- p + theme(axis.text = element_blank(),
               axis.title = element_blank(),
               axis.ticks = element_blank(),
               legend.title = element_blank(),
               legend.position = "right")
p
}

# line_carbon ----
pdat <- mdat %>% 
  filter(Country == cntrycode, 
         variable %in% "C_FGB")
if (nrow(pdat) > 0){
ggplot(data = pdat, aes(x=year, y=value)) + 
  geom_point() + geom_line() +
  labs(x = "Year",
       y = "million metric tonnes",
       title= "Carbon in forest biomass",
       subtitle="million metric tonnes",
       caption = "Source: Cite the right data here!") +
  theme_ipsum()
}
## ---- table_production ----
# colnames(faostat) <- c("Country", 
#                        "Forest product categories", 
#                        "Import quantity (tonnes)", 
#                        "Import quantity (m\\textsuperscript{3})",
#                        "Export quantity (tonnes)", 
#                        "Export quantity (m\\textsuperscript{3})",
#                        "Production (tonnes)", 
#                        "Production (m\\textsuperscript{3})") 
cfaostat <- faostat %>% filter(ISO == cntrycode) %>% 
  select(-ISO)
if (nrow(cfaostat) > 0){
  
  if (doctype == "latex"){
    print.xtable(xtable(cfaostat), 
                 include.rownames = FALSE, 
                 include.colnames = FALSE,
                 booktabs = TRUE,
                 timestamp=NULL, 
                 sanitize.text.function = identity
                 # only.contents = TRUE,
                 # zero.print=zero.print
    ) 
    } else {
      # html-table
      print.xtable(xtable(cfaostat),
                   type = "html", 
                   include.rownames = FALSE, 
                   include.colnames = FALSE,
                   timestamp=NULL
      )
    }

}
