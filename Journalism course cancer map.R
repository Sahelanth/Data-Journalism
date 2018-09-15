#Chloropleth map
#Want to do NY cancer incidence by census tract, look for hot spots
install.packages("censusapi")
library(censusapi)

#Request a census API key at https://api.census.gov/data/key_signup.html ; this one's mine.
Sys.setenv(CENSUS_KEY= "745a31864da14e4b53a4c020f47ba45895e750f3")
library(rvest)
library(tidyverse)
install.packages("tigris")

###Get the shape files for New York State census tracts
library(tigris)
options(tigris_class = "sf")
ny <- tracts("NY")


###Get cancer data from NY census tracts

  url <- "https://www.health.ny.gov/statistics/cancer/registry/tract/index.htm"
  session <- html_session(url)
  censustractlinks <- session %>% read_html() %>% 
    html_nodes(xpath='//ul[@id="countylist"]') %>% html_nodes('li a') %>%
    html_attr('href')
  censustractlinks <- paste0("https://www.health.ny.gov",
                            censustractlinks)
  #Fix their broken link
  censustractlinks <- gsub("st.lawrence", "stlawrence", censustractlinks)

#Define get_table function
get_table <- function(url){
  cancerdata <- jump_to(session, url) %>% html_nodes('table') %>%
    html_table(fill=TRUE) %>% .[[1]]

names(cancerdata)[seq(3, 13, by=2)] <- paste(names(cancerdata)[seq(3, 13, by=2)], "observed")
names(cancerdata)[seq(4, 14, by=2)] <- paste(names(cancerdata)[seq(4, 14, by=2)], "expected")
cancerdata$`Primary Census Tract` <- gsub(".", "", cancerdata$`Primary Census Tract`, fixed=TRUE)
cancerdata$`Primary Census Tract`<- str_pad(cancerdata$`Primary Census Tract`, width=6, side="left", pad="0")
cancerdata <- cancerdata[-1,]
cancerdata[,3:14] <- apply(cancerdata[,3:14], 2, as.numeric)

#Add county name to table
cancerdata$County <- str_extract(url, 
                                 "(?<=/tract/)[a-z]+(?=\\.htm)")
cancerdata
}

#Apply that function to all the links
cancertable <- map_dfr(censustractlinks, get_table)

#Result has 62 counties, perfect! I may need to append
#the county code for each to its tract codes.
#length(unique(ny$TRACTCE))
#[1] 2701
#length(unique(cancertable$`Primary Census Tract`))
#[1] 2651
#length(unique(cancertable$`Included Census Tracts`))
#90

#Okay, proper is combine county code and tract code
#Need to do this in both ny and in cancertable
#Do this after have padded tract codes to 6 digits,
#so should be okay to do it outside the function.

#Separate st. lawrence in cancertable$County, then convert 

cancertable$County <- gsub("stlawrence", "St. Lawrence", cancertable$County)
cancertable$County <- str_to_title(cancertable$County)

cancertable$County <- paste(cancertable$County, "County")

test <- inner_join(fips_codes[fips_codes$state=="NY",], cancertable,
                   by=c("county" = "County"))
test$county_and_tract_code <- paste(test$county_code, test$`Primary Census Tract`, sep="")

ny$county_and_tract_code <- paste(ny$COUNTYFP, ny$TRACTCE, sep="")

test2 <- inner_join(ny, test,
                    by="county_and_tract_code")

test2 <- test2 %>% mutate(Colorectal.Ratio = `Colorectal observed`/ `Colorectal expected`,
                                        Lung.and.Bronchus.Ratio = `Lung and Bronchus observed`/ `Lung and Bronchus expected`,
                                        Female.Breast.Ratio = `Female Breast observed`/ `Female Breast expected`,
                                        Prostate.Ratio = `Prostate observed`/ `Prostate expected`,
                                        Urinary.Bladder.Ratio = `Urinary Bladder (incl. in situ) observed`/ `Urinary Bladder (incl. in situ) expected`,
                                        Non.Hodgkin.Lymphoma.Ratio = `Non-Hodgkin Lymphoma observed`/ `Non-Hodgkin Lymphoma expected`)


colo <- ggplot(test2) + 
  geom_sf(aes(fill=Colorectal.Ratio)) +
  scale_fill_distiller(palette="RdYlBu",
                       limits=c(0, 2.7)) +
  labs(title="Ratio of Observed to Expected Cancer Cases Colorectal Cancer Cases",
       caption="Sources: https://www.health.ny.gov/statistics/cancer/registry/tract/index.htm \n Census API") +
  theme_void() +
  theme(plot.title=element_text(size=15, hjust=0.5, vjust=-2),
        panel.grid.major = element_line(colour = 'transparent'))

#It works! Now, if I can add annotations...



test2[test2$Colorectal.Ratio > 2, ] %>% nrow
#107 rows

#Found a colorectal with ratio 20,
#prostate with ratio 30, and lung with ratio 10
#I could just highlight those, or I could do some *statistics*
#These are cases where expected is a fraction and observed is a
#whole number, though, so probably bullshit to use ratio there.

colobronx <- ggplot(test2[test2$county=="Bronx County",]) + 
  geom_sf(aes(fill=Colorectal.Ratio)) +
  scale_fill_distiller(palette="RdYlBu",
                       limits=c(0, 2.7)) +
  labs(title="Colorectal") +
  theme_void() +
  theme(plot.title=element_text(size=15, hjust=0.5, vjust=-2),
        panel.grid.major = element_line(colour = 'transparent'))


prostatebronx <- ggplot(test2[test2$county=="Bronx County",]) + 
  geom_sf(aes(fill=Prostate.Ratio)) +
  scale_fill_distiller(palette="RdYlBu",
                       limits=c(0, 2.7)) +
  labs(title="Prostate") +
  theme_void() +
  theme(plot.title=element_text(size=15, hjust=0.5, vjust=-2),
        panel.grid.major = element_line(colour = 'transparent'))

lungbronx <- ggplot(test2[test2$county=="Bronx County",]) + 
  geom_sf(aes(fill=Lung.and.Bronchus.Ratio)) +
  scale_fill_distiller(palette="RdYlBu",
                       limits=c(0, 2.7)) +
  labs(title="Lung and Bronchus") +
  theme_void() +
  theme(plot.title=element_text(size=15, hjust=0.5, vjust=-2),
        panel.grid.major = element_line(colour = 'transparent'))

library(gridExtra)
library(grid)
grid.arrange(colobronx, lungbronx, prostatebronx, 
             top=textGrob("Ratio of Observed to Expected Cancer Cases\nBronx Census Tracts", gp=gpar(fontsize=20)))




###########ALBANY EXAMPLE###################################################

#Something I need to fix:
#line 146.08 also has the data of 4.04, they're combined.
#If they're adjacent, can I merge them somehow?
#Alternately, add another row to the table, asterisk.
#Ignore for the moment


#You can use tigris's built in fips_codes table to look up what
#county code corresponds to what county name.

#Get the data for just Albany County.
albany <- ny[ny$COUNTYFP=="001",]

#Join the shape data to the cancer data
joinedalbany <- inner_join(albany, cancertable, by=c("TRACTCE" = 'Primary Census Tract'))


###Create ratios of observed to expected values so we can find outliers

#The proper way to do this is to use the dplyr gather() function to
#reshape the cancer data so there's a cancer type column, an observed column,
#and an expected column. Then you can calculate the ratios more efficiently,
#and make all the graphs with a single ggplot() call using facet_wrap() and
#faceting by type of cancer.

#I have had no breakfast or coffee, so instead here is the clunky way to do it.
#I'm going to make one column for each ratio, and then one map for each
#ratio.
#Then I'm going to combine all the maps into one image using the grid package. 

joinedalbany <- joinedalbany %>% mutate(Colorectal.Ratio = `Colorectal observed`/ `Colorectal expected`,
                  Lung.and.Bronchus.Ratio = `Lung and Bronchus observed`/ `Lung and Bronchus expected`,
                  Female.Breast.Ratio = `Female Breast observed`/ `Female Breast expected`,
                  Prostate.Ratio = `Prostate observed`/ `Prostate expected`,
                  Urinary.Bladder.Ratio = `Urinary Bladder (incl. in situ) observed`/ `Urinary Bladder (incl. in situ) expected`,
                  Non.Hodgkin.Lymphoma.Ratio = `Non-Hodgkin Lymphoma observed`/ `Non-Hodgkin Lymphoma expected`)

  
colo <- ggplot(joinedalbany) + 
  geom_sf(aes(fill=Colorectal.Ratio)) +
  scale_fill_distiller(palette="RdYlBu",
                       limits=c(0, 2.7)) +
  labs(title="Colorectal") +
  theme_void() +
  theme(plot.title=element_text(size=15, hjust=0.5, vjust=-2),
        panel.grid.major = element_line(colour = 'transparent'))

lung <- ggplot(joinedalbany) + 
  geom_sf(aes(fill=Lung.and.Bronchus.Ratio)) +
  scale_fill_distiller(palette="RdYlBu",
                       limits=c(0, 2.7)) +
  labs(title="Lung and bronchus") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))
  
  breast <- ggplot(joinedalbany) + 
    geom_sf(aes(fill=Female.Breast.Ratio)) +
    scale_fill_distiller(palette="RdYlBu",
                         limits=c(0, 2.7)) +
    labs(title="Breast") +
    theme_void() +
    theme(plot.title=element_text(size=15, hjust=0.5, vjust=-0.2),
          panel.grid.major = element_line(colour = 'transparent'))
  
  prostate <- ggplot(joinedalbany) + 
    geom_sf(aes(fill=Prostate.Ratio)) +
    scale_fill_distiller(palette="RdYlBu",
                         limits=c(0, 2.7)) +
    labs(title="Prostate") +
    theme_void() +
    theme(plot.title=element_text(size=15, hjust=0.5, vjust=-0.2),
          panel.grid.major = element_line(colour = 'transparent'))
  
  bladder <- ggplot(joinedalbany) + 
    geom_sf(aes(fill=Urinary.Bladder.Ratio)) +
    scale_fill_distiller(palette="RdYlBu",
                         limits=c(0, 2.7)) +
    labs(title="Bladder") +
    theme_void() +
    theme(plot.title=element_text(size=15, hjust=0.5, vjust=-0.2),
          panel.grid.major = element_line(colour = 'transparent'))
  
  lymphoma <- ggplot(joinedalbany) + 
    geom_sf(aes(fill=Non.Hodgkin.Lymphoma.Ratio)) +
    scale_fill_distiller(palette="RdYlBu",
                         limits=c(0, 2.7)) +
    labs(title="Lymphoma") +
    theme_void() +
    theme(plot.title=element_text(size=15, hjust=0.5, vjust=-0.2),
          panel.grid.major = element_line(colour = 'transparent'))

library(gridExtra)
library(grid)
grid.arrange(colo, lung, breast, prostate, bladder, lymphoma, 
             nrow=2, 
             top=textGrob("Ratio of Observed to Expected Cancer Cases, Albany Census Tracts", gp=gpar(fontsize=20)))