# data preparations

library(sf)
library(tidyverse)
library(haven)

## read raw data and join to lsoa shape-files
districts <- read_dta("rawdata/ethnic_lsoa_town.2011_reduced.dta") %>% 
  mutate(town = as.character.factor(as_factor(town)), 
         lsoa11cd = as.character(lsoa11cd)) %>% 
  select(lsoa11cd, town, all, all1674valid, starts_with("whiteb_"), starts_with("asian_"), 
         starts_with("black_"), starts_with("othereth_")) %>% 
  filter(town != "999")
englandwales <- read_sf(dsn ="rawdata/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BSC/",
                        stringsAsFactors=FALSE) %>% 
  st_transform("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs") %>% 
  inner_join(districts, by = c("LSOA11CD" = "lsoa11cd"))
save(districts, englandwales, file = "R/rawdata_shp")

## export LSOA of some towns including raw data to ESRI shapefile readable with NetLogo GIS extension
for (TOWN in c("Southampton","Leeds", "Bradford", "Leicester", "London", "Manchester", "Birmingham", "Brighton and Hove", 
               "Kingston upon Hull", "Stoke-on-Trent", "Plymouth", "Derby", "Nottingham", "Newcastle upon Tyne","Leeds", "Sheffield",
               "Coventry", "St Albans" , "Guildford", "Cambridge", "Middlesbrough", "Sunderland", "Oxford","Luton",
               "Blackburn", "Oldham","Wolverhampton","Slough","Walsall","Bristol", "Liverpool", "Leeds", "Reading")) {
  if (!dir.exists(paste0("shp_NetLogo/",TOWN))) englandwales %>% filter(town == TOWN) %>%
    st_write(dsn = paste0("shp_NetLogo/",TOWN), layer = TOWN, driver = "ESRI Shapefile", delete_dsn = TRUE)
}

## prepare datasets with segregation indices for descriptive statistics
long_districts <- districts %>% pivot_longer(cols = contains("_"), values_to = "count") %>% 
  mutate(Ethnicity = word(name, sep="_"), SES = word(name, -1, sep="_"))
lsoa_ethn_ses <- long_districts %>% group_by(lsoa11cd) %>% mutate(fraction = count/sum(count))
lsoa_ethn <- lsoa_ethn_ses %>% 
  group_by(lsoa11cd, town, all, all1674valid, Ethnicity) %>% 
  summarize(fraction = sum(count) / max(all1674valid), count = sum(count))
lsoa_ses <- lsoa_ethn_ses %>% 
  group_by(lsoa11cd, town, all, all1674valid, SES) %>%  
  summarize(fraction = sum(count) / max(all1674valid), count = sum(count))
lsoa <- lsoa_ethn_ses %>% mutate(SESnum = if_else(SES=="low", 0, if_else(SES=="mid",1,2))) %>% 
  group_by(lsoa11cd, town, all, all1674valid) %>% 
  # summarize(average_ses = sum(count * SESnum)/sum(count)) %>% 
  group_by(town) %>% 
  mutate(fraction_lsoa = all1674valid/sum(all1674valid)) %>% group_by()
towns_ethn_ses <- lsoa_ethn_ses %>% group_by(town, Ethnicity, SES) %>% 
  summarize(count = sum(count)) %>% group_by(town) %>% mutate(fraction = count / sum(count)) %>% group_by()
towns_ethn <- towns_ethn_ses %>%  group_by(town, Ethnicity) %>% 
  summarize(count = sum(count)) %>% group_by(town) %>% mutate(fraction = count / sum(count)) %>% group_by()
towns_ses <- towns_ethn_ses %>%  group_by(town, SES) %>% 
  summarize(count = sum(count)) %>% group_by(town) %>% mutate(fraction = count / sum(count)) %>% group_by()
towns <- towns_ethn_ses %>% mutate(SESnum = if_else(SES=="low", 0, if_else(SES=="mid",1,2))) %>% 
  group_by(town) %>% 
  summarize(count = sum(count)) # average_ses = sum(count * SESnum)/sum(count)
# Dissimilarity, Location Quotient
lsoa_ethn <- lsoa_ethn %>% left_join(select(towns_ethn, town, Ethnicity, fraction_ethnicity_town = fraction),
                                     by = c("town", "Ethnicity")) %>% 
  group_by(lsoa11cd) %>% mutate(Dissimilarity = abs(fraction - fraction_ethnicity_town) / 
                                  (2*fraction_ethnicity_town*(1-fraction_ethnicity_town)),
                                Location_Quotient = fraction/fraction_ethnicity_town) 
towns_ethn <- lsoa_ethn %>% left_join(select(lsoa,town,lsoa11cd,fraction_lsoa), c("lsoa11cd", "town")) %>%
  group_by(town,Ethnicity) %>% 
  summarize(Dissimilarity_Index = sum(fraction_lsoa * Dissimilarity)) %>% 
  right_join(towns_ethn, c("town", "Ethnicity")) %>% group_by()
towns <- towns_ethn %>% group_by(town) %>% 
  summarize(Simpson_town = sum(fraction^2), 
            Entropy_town = -1/log(4)*sum(fraction*log(fraction))) %>% 
  right_join(towns, by = "town")
towns <- lsoa_ethn %>% left_join(rename(towns, count_town=count), by = "town") %>% 
  left_join(select(lsoa,town,lsoa11cd,fraction_lsoa), c("lsoa11cd", "town")) %>% 
  group_by(lsoa11cd, town, fraction_lsoa, Simpson_town, Entropy_town) %>%
  summarize(Simpson = sum(fraction^2), 
            Entropy = -1/log(4)*sum(fraction*
                                      if_else(fraction == 0, 0, log(fraction)))) %>% 
  mutate(Excess_Simpson = Simpson - Simpson_town, Loss_Entropy = Entropy_town - Entropy) %>% 
  group_by(town) %>% summarize(Avg_Simpson = sum(fraction_lsoa*Simpson), 
                               Avg_Excess_Simpson = sum(fraction_lsoa*Excess_Simpson), 
                               Avg_Entropy = sum(fraction_lsoa*Entropy),
                               Avg_Loss_Entropy = sum(fraction_lsoa*Loss_Entropy)) %>% 
  right_join(towns, by = "town")

save(lsoa, lsoa_ethn, lsoa_ethn_ses, lsoa_ses, towns, towns_ethn, towns_ethn_ses, towns_ses, file = "R/lsoa_towns")
write_dta(lsoa, "stata/lsoa.dta")
write_dta(lsoa_ethn, "stata/lsoa_ethn.dta")
write_dta(lsoa_ethn_ses, "stata/lsoa_ethn_ses.dta")
#write_dta(lsoa_ses, "stata/lsoa_ses.dta")
write_dta(towns, "stata/towns.dta")
write_dta(towns_ethn, "stata/towns_ethn.dta")
write_dta(towns_ethn_ses, "stata/towns_ethn_ses.dta")
#write_dta(towns_ses, "stata/towns_ses.dta")

