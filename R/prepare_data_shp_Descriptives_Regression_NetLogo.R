## Documentation
# This produces dataframes including GIS information for LSOA's of Englands Major Towns and Cities
#
# The final frames are saved as 
# lsoa_2001 and lsoa_2011
#
# The frames bradford_2001 and bradford_2011 are simply filtered version of the above complete dataframes
#
# Several segregation indices on the town and lsoa level are produced from the raw count numbers
#
# In 2001 and 2011 two ethnic groupings are used with the following name convention:
# _eth_: All ethnic categories which exist in this year (almost identical, see code for details)
# _ethgroupded_: Groups by whiteb, asian, black, and other (see code below for details)
#
# In 2011 another ethnic grouping is used:
# _ethgroupedses_: as _ethgrouped_ but uses a smaller subset of the population. 
#       Counted are only those individuals which have a valid SES, i.e., one of the categories 1-7. 
#       Thus, people younger than 16 and those with SES class 8 or no class category are not counted.
# 
# The segregation index labels start with 
# segrlsoa_ : This is a segregation index for which only data from the LSOA is used
# segrtown_ : This is a segregation index which uses the total counts of the town where the LSOA belongs to
#
# The segregation indices are 
# _fraction_: The fraction of the total population (either lsoa or town) or a particular ethnicity which 
#             is part of the label after the last "_"
# _simpson_: The standard Simpson index based on the fractions of all ethnicities (either town or lsoa, and either 
#            either based on _eth_, _ethgrouped_, or _ethgroupedses_)
# _simspon2_ : The same as simpson but setting all ethnicities from the "other" category to zero 
#              (this represents that "other" agents do not consider others as similar)
# 
# Examples:
# "segrtown_simpson2_eth" is the simpson index for all ethnicities (not grouped) but ingnoring all ethnicities 
#     which have "other" in their name computed based on the numbers of the full town
# "segrlsoa_fraction_ethgroupedses_asian" is the fraction of the asians (as a grouped ethnicity) on the lsoa level



# data preparations

library(sf)
library(tidyverse)
library(readxl)
library(haven)

## read GIS 
shp_2001 <- read_sf(dsn ="rawdata/Lower_Layer_Super_Output_Areas__December_2001__Boundaries_EW_BGC-shp/",
                    stringsAsFactors=FALSE) %>% 
  st_transform("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs")
shp_2011 <- read_sf(dsn ="rawdata/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC-shp/",
                    stringsAsFactors=FALSE) %>% 
  st_transform("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs")

## read lookup tables 
lsoa_2001_2011 <- read_csv("rawdata/Lower_Layer_Super_Output_Area__2001__to_Lower_Layer_Super_Output_Area__2011__to_Local_Authority_District__2011__Lookup_in_England_and_Wales.csv",
                           col_types = cols(LAD11NMW = col_character() ) )
lsoa_2001_towns_2015 <- read_csv("rawdata/Lower_Layer_Super_Output_Area__2001__to_Major_Towns_and_Cities__December_2015__Lookup_in_England_and_Wales.csv")
lsoa_2011_towns_2015 <- read_csv("rawdata/Lower_Layer_Super_Output_Area__2011__to_Major_Towns_and_Cities__December_2015__Lookup_in_England_and_Wales.csv")
names(lsoa_2001_2011) <- tolower(names(lsoa_2001_2011))
names(lsoa_2001_towns_2015) <- tolower(names(lsoa_2001_towns_2015))
names(lsoa_2011_towns_2015) <- tolower(names(lsoa_2011_towns_2015))

## read data
ethnames_01 <- c("eth_white_brit_01", "eth_white_irish_01", "eth_white_other_01", 
                 "eth_mix_carib_01", "eth_mix_africa_01", "eth_mix_asia_01", "eth_mix_other_01", 
                 "eth_asia_indian_01", "eth_asia_pakistani_01", "eth_asia_bangla_01", "eth_asia_chi_01", "eth_asia_other_01",  "eth_black_caribbean_01", 
                 "eth_black_caribbean_01", "eth_black_african_01", "eth_black_other_01", "eth_other_01")
ethnames_11 <- c("eth_white_brit_11", "eth_white_irish_11", "eth_white_gypsy_11", "eth_white_other_11", 
                 "eth_mix_carib_11", "eth_mix_africa_11", "eth_mix_asia_11", "eth_mix_other_11",
                 "eth_asia_indian_11", "eth_asia_pakistani_11", "eth_asia_bangla_11", "eth_asia_chi_11", "eth_asia_other_11", 
                 "eth_black_caribbean_11", "eth_black_african_11", "eth_black_other_11", 
                 "eth_other_arab_11", "eth_other_other_11")
ethgroupednames <- c("ethgrouped_whiteb","ethgrouped_asian","ethgrouped_black","ethgrouped_other")

lsoa_2001 <- read_xlsx("rawdata/england_vars_2001_full2.xlsx",1) %>% 
  left_join(select(lsoa_2001_towns_2015, lsoa01cd, tcity15nm), by = "lsoa01cd") %>% 
  filter(!is.na(tcity15nm), !is.na(total_people_01)) %>% group_by(tcity15nm) %>% 
  mutate(ethgrouped_whiteb = eth_white_brit_01,
         ethgrouped_asian = eth_asia_indian_01+eth_asia_pakistani_01+eth_asia_bangla_01+eth_asia_chi_01+eth_asia_other_01,
         ethgrouped_black = eth_black_caribbean_01+eth_black_african_01+eth_black_other_01,
         ethgrouped_other = eth_mix_africa_01+eth_mix_asia_01+eth_mix_carib_01+eth_mix_other_01+eth_other_01,
         all_01=total_people_01, all_01_town = sum(all_01), fraction_01 = all_01/all_01_town) %>% 
  select(lsoa01cd, lsoa01nm, tcity15nm, all_01, all_01_town, fraction_01,
         ethgroupednames, ethnames_01, all1674_01=total_people1674_01, everything() ) %>% 
  group_by()

lsoa_2011 <- read_xlsx("rawdata/ethnic_ses_2011.xlsx",1) %>% 
  left_join(select(lsoa_2011_towns_2015, lsoa11cd, tcity15nm), by = "lsoa11cd") %>% 
  filter(!is.na(tcity15nm), !is.na(allpop)) %>% group_by(tcity15nm) %>% 
  mutate(ethgrouped_whiteb = eth_white_brit_11,
         ethgrouped_asian = eth_asia_indian_11+eth_asia_pakistani_11+eth_asia_bangla_11+eth_asia_chi_11+eth_asia_other_11,
         ethgrouped_black = eth_black_caribbean_11+eth_black_african_11+eth_black_other_11,
         ethgrouped_other = eth_white_gypsy_11+eth_mix_africa_11+eth_mix_asia_11+eth_mix_carib_11+eth_mix_other_11+eth_other_arab_11+eth_other_other_11,
         all_11=allpop, all_11_town = sum(all_11), fraction_11 = all_11/all_11_town,
         ethgroupedses_whiteb_high = ses1_whiteb+ses2_whiteb, 
         ethgroupedses_whiteb_mid  = ses3_whiteb+ses4_whiteb, 
         ethgroupedses_whiteb_low  = ses5_whiteb+ses6_whiteb+ses7_whiteb,
         ethgroupedses_asian_high = ses1_asian+ses2_asian, 
         ethgroupedses_asian_mid  = ses3_asian+ses4_asian, 
         ethgroupedses_asian_low  = ses5_asian+ses6_asian+ses7_asian,
         ethgroupedses_black_high = ses1_black+ses2_black, 
         ethgroupedses_black_mid  = ses3_black+ses4_black, 
         ethgroupedses_black_low  = ses5_black+ses6_black+ses7_black,
         ethgroupedses_other_high = ses1_irish+ses1_otherwhite+ses1_mixed+ses1_other+ses2_irish+ses2_otherwhite+ses2_mixed+ses2_other, 
         ethgroupedses_other_mid  = ses3_irish+ses3_otherwhite+ses3_mixed+ses3_other+ses4_irish+ses4_otherwhite+ses4_mixed+ses4_other, 
         ethgroupedses_other_low  = ses5_irish+ses5_otherwhite+ses5_mixed+ses5_other+ses6_irish+ses6_otherwhite+ses6_mixed+ses6_other+ses7_irish+ses7_otherwhite+ses7_mixed+ses7_other,
         ethgroupedses_whiteb = ethgroupedses_whiteb_high+ethgroupedses_whiteb_mid+ethgroupedses_whiteb_low,
         ethgroupedses_asian  = ethgroupedses_asian_high +ethgroupedses_asian_mid +ethgroupedses_asian_low ,
         ethgroupedses_black  = ethgroupedses_black_high +ethgroupedses_black_mid +ethgroupedses_black_low ,
         ethgroupedses_other  = ethgroupedses_other_high +ethgroupedses_other_mid +ethgroupedses_other_low ,
         allvalidses_11 = ethgroupedses_whiteb+ethgroupedses_asian+ethgroupedses_black+ethgroupedses_other,
         allvalidses_11_town = sum(allvalidses_11),
         fractionvalidses_11 = allvalidses_11/allvalidses_11_town) %>%
  select(lsoa11cd, lsoa11nm, tcity15nm, all_11, all_11_town, fraction_11,
         allvalidses_11, allvalidses_11_town, fractionvalidses_11,
         ethgroupednames, ethnames_11, everything() ) %>% 
  group_by()

## segregation indices towns
towns_2001 <- lsoa_2001 %>% select(-lsoa01cd,-lsoa01nm,-all_01_town,-fraction_01) %>% 
  group_by(tcity15nm) %>% summarize_all(sum) %>% group_by()
segr_towns_2001 <- towns_2001 %>% select(tcity15nm, all_01, c(contains("ethgrouped_"),contains("eth_"))) %>% 
  pivot_longer(c(contains("ethgrouped_"),contains("eth_")), names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(tcity15nm, ethnicgrouping) %>% 
  mutate(segrtown_fraction = count/all_01, segrtown_simpson = sum(segrtown_fraction^2), 
         segrtown_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrtown_fraction)^2)) %>% 
  group_by() %>% 
  select(-count, -all_01)
towns_2001 <- segr_towns_2001 %>% select(tcity15nm, ethnicgrouping, ethnicity, segrtown_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrtown_fraction, names_prefix="segrtown_fraction_") %>% 
  right_join(towns_2001, by = "tcity15nm")
towns_2001 <- segr_towns_2001 %>% select(tcity15nm, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrtown_simpson, segrtown_simpson2)) %>% 
  right_join(towns_2001, by = "tcity15nm")

towns_2011 <- lsoa_2011 %>% select(-lsoa11cd,-lsoa11nm,-all_11_town,-fraction_11,-allvalidses_11_town,-fractionvalidses_11,
                                   -ethgroupedses_whiteb_high, -ethgroupedses_whiteb_mid, -ethgroupedses_whiteb_low, 
                                   -ethgroupedses_asian_high, -ethgroupedses_asian_mid, -ethgroupedses_asian_low,
                                   -ethgroupedses_black_high, -ethgroupedses_black_mid, -ethgroupedses_black_low,
                                   -ethgroupedses_other_high, -ethgroupedses_other_mid, -ethgroupedses_other_low) %>% 
  group_by(tcity15nm) %>% summarize_all(sum) %>% group_by()
segr_towns_2011 <- towns_2011 %>% select(tcity15nm, all_11, allvalidses_11,
                                         c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_"))) %>% 
  pivot_longer(c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_")), 
               names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(tcity15nm, ethnicgrouping) %>% 
  mutate(segrtown_fraction = count/if_else(ethnicgrouping=="ethgroupedses",allvalidses_11,all_11), 
         segrtown_simpson = sum(segrtown_fraction^2), 
         segrtown_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrtown_fraction)^2)) %>% 
  group_by() %>% 
  select(-count, -all_11, -allvalidses_11)
towns_2011 <- segr_towns_2011 %>% select(tcity15nm, ethnicgrouping, ethnicity, segrtown_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrtown_fraction, names_prefix="segrtown_fraction_") %>% 
  right_join(towns_2011, by = "tcity15nm")
towns_2011 <- segr_towns_2011 %>% select(tcity15nm, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrtown_simpson, segrtown_simpson2)) %>% 
  right_join(towns_2011, by = "tcity15nm")

# segregation indices lsoa
segr_lsoa_2001 <- lsoa_2001 %>% select(lsoa01cd, lsoa01nm, tcity15nm, all_01, c(contains("ethgrouped_"),contains("eth_"))) %>% 
  pivot_longer(c(contains("ethgrouped_"),contains("eth_")), names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(lsoa01cd, ethnicgrouping) %>% 
  mutate(segrlsoa_fraction = count/all_01, segrlsoa_simpson = sum(segrlsoa_fraction^2), 
         segrlsoa_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrlsoa_fraction)^2)) %>% 
  group_by() %>% 
  select(-count, -all_01)
lsoa_2001 <- segr_lsoa_2001 %>% select(lsoa01cd, lsoa01nm, tcity15nm, ethnicgrouping, ethnicity, segrlsoa_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrlsoa_fraction, names_prefix="segrlsoa_fraction_") %>% 
  right_join(lsoa_2001, by = c("lsoa01cd","lsoa01nm","tcity15nm"))
lsoa_2001 <- segr_lsoa_2001 %>% select(lsoa01cd, lsoa01nm, tcity15nm, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrlsoa_simpson, segrlsoa_simpson2)) %>% 
  right_join(lsoa_2001, by = c("lsoa01cd","lsoa01nm","tcity15nm"))

segr_lsoa_2011 <- lsoa_2011 %>% select(lsoa11cd, lsoa11nm, tcity15nm, all_11, allvalidses_11, 
                                       c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_")),
                                       -ethgroupedses_whiteb_high, -ethgroupedses_whiteb_mid, -ethgroupedses_whiteb_low, 
                                       -ethgroupedses_asian_high, -ethgroupedses_asian_mid, -ethgroupedses_asian_low,
                                       -ethgroupedses_black_high, -ethgroupedses_black_mid, -ethgroupedses_black_low,
                                       -ethgroupedses_other_high, -ethgroupedses_other_mid, -ethgroupedses_other_low) %>% 
  pivot_longer(c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_")), 
               names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(lsoa11cd, ethnicgrouping) %>% 
  mutate(segrlsoa_fraction = count/if_else(ethnicgrouping=="ethgroupedses",allvalidses_11,all_11),
         segrlsoa_simpson = sum(segrlsoa_fraction^2), 
         segrlsoa_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrlsoa_fraction)^2)) %>% 
  group_by() %>% 
  select(-count, -all_11, -allvalidses_11)
lsoa_2011 <- segr_lsoa_2011 %>% select(lsoa11cd, lsoa11nm, tcity15nm, ethnicgrouping, ethnicity, segrlsoa_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrlsoa_fraction, names_prefix="segrlsoa_fraction_") %>% 
  right_join(lsoa_2011, by = c("lsoa11cd","lsoa11nm","tcity15nm"))
lsoa_2011 <- segr_lsoa_2011 %>% select(lsoa11cd, lsoa11nm, tcity15nm, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrlsoa_simpson, segrlsoa_simpson2)) %>% 
  right_join(lsoa_2011, by = c("lsoa11cd","lsoa11nm","tcity15nm"))

# final lsoa dataset
lsoa_2001 <- lsoa_2001 %>% select(lsoa01cd, lsoa01nm, tcity15nm) %>% 
  right_join(select(towns_2001, tcity15nm, contains("segr")), by = "tcity15nm") %>% 
  right_join(lsoa_2001, by = c("lsoa01cd", "lsoa01nm", "tcity15nm")) %>% 
  left_join(select(shp_2001,LSOA01CD,Shape__Are,Shape__Len,geometry), by = c("lsoa01cd" = "LSOA01CD"))
lsoa_2011 <- lsoa_2011 %>% select(lsoa11cd, lsoa11nm, tcity15nm) %>% 
  right_join(select(towns_2011, tcity15nm, contains("segr")), by = "tcity15nm") %>% 
  right_join(lsoa_2011, by = c("lsoa11cd", "lsoa11nm", "tcity15nm")) %>% 
  left_join(select(shp_2011,LSOA11CD,Shape__Are,Shape__Len,geometry), by = c("lsoa11cd" = "LSOA11CD"))
save(lsoa_2001,lsoa_2011,file = "R/lsoa_2001_lsoa_2011")

# Bradford
bradford_2001 <- lsoa_2001 %>% filter(tcity15nm == "Bradford")
bradford_2011 <- lsoa_2011 %>% filter(tcity15nm == "Bradford")
save(bradford_2001,bradford_2011,file = "R/bradford_2001_2011")


# 
# common <- intersect(bradford_2001$lsoa01cd, bradford_2011$lsoa11cd)
# bradford_2001 %>% filter(lsoa01cd %in% common)
# bradford_2001 %>% filter(lsoa01cd %in% common) %>% 
#   ggplot(aes(geometry = geometry)) + geom_sf(aes(fill = segrlsoa_simpson_ethgrouped))
# bradford_2011 %>% filter(lsoa11cd %in% common) %>% 
#   ggplot(aes(geometry = geometry)) + geom_sf(aes(fill = segrlsoa_simpson_ethgrouped))
# cf <- bradford_2011 %>% left_join(lsoa_2001_2011) %>% select(lsoa11cd, lsoa01cd, chgind) %>% mutate(check=lsoa11cd==lsoa01cd)
# 
# 
# 
# ## export LSOA of some towns including raw data to ESRI shapefile readable with NetLogo GIS extension
# for (TOWN in c("Southampton","Leeds", "Bradford", "Leicester", "London", "Manchester", "Birmingham", "Brighton and Hove", 
#                "Kingston upon Hull", "Stoke-on-Trent", "Plymouth", "Derby", "Nottingham", "Newcastle upon Tyne","Leeds", "Sheffield",
#                "Coventry", "St Albans" , "Guildford", "Cambridge", "Middlesbrough", "Sunderland", "Oxford","Luton",
#                "Blackburn", "Oldham","Wolverhampton","Slough","Walsall","Bristol", "Liverpool", "Leeds", "Reading")) {
#   if (!dir.exists(paste0("shp_NetLogo/",TOWN))) shp_2011 %>% filter(tcity15nm == TOWN) %>%
#     st_write(dsn = paste0("shp_NetLogo/",TOWN), layer = TOWN, driver = "ESRI Shapefile", delete_dsn = TRUE)
# }
# 
# # Dissimilarity, Location Quotient
# lsoa_ethn <- lsoa_ethn %>% left_join(select(towns_ethn, town, Ethnicity, fraction_ethnicity_town = fraction),
#                                      by = c("town", "Ethnicity")) %>% 
#   group_by(lsoa11cd) %>% mutate(Dissimilarity = abs(fraction - fraction_ethnicity_town) / 
#                                   (2*fraction_ethnicity_town*(1-fraction_ethnicity_town)),
#                                 Location_Quotient = fraction/fraction_ethnicity_town) 
# towns_ethn <- lsoa_ethn %>% left_join(select(lsoa,town,lsoa11cd,fraction_lsoa), c("lsoa11cd", "town")) %>%
#   group_by(town,Ethnicity) %>% 
#   summarize(Dissimilarity_Index = sum(fraction_lsoa * Dissimilarity)) %>% 
#   right_join(towns_ethn, c("town", "Ethnicity")) %>% group_by()
# towns <- towns_ethn %>% group_by(town) %>% 
#   summarize(Simpson_town = sum(fraction^2), 
#             Entropy_town = -1/log(4)*sum(fraction*log(fraction))) %>% 
#   right_join(towns, by = "town")
# towns <- lsoa_ethn %>% left_join(rename(towns, count_town=count), by = "town") %>% 
#   left_join(select(lsoa,town,lsoa11cd,fraction_lsoa), c("lsoa11cd", "town")) %>% 
#   group_by(lsoa11cd, town, fraction_lsoa, Simpson_town, Entropy_town) %>%
#   summarize(Simpson = sum(fraction^2), 
#             Entropy = -1/log(4)*sum(fraction*
#                                       if_else(fraction == 0, 0, log(fraction)))) %>% 
#   mutate(Excess_Simpson = Simpson - Simpson_town, Loss_Entropy = Entropy_town - Entropy) %>% 
#   group_by(town) %>% summarize(Avg_Simpson = sum(fraction_lsoa*Simpson), 
#                                Avg_Excess_Simpson = sum(fraction_lsoa*Excess_Simpson), 
#                                Avg_Entropy = sum(fraction_lsoa*Entropy),
#                                Avg_Loss_Entropy = sum(fraction_lsoa*Loss_Entropy)) %>% 
#   right_join(towns, by = "town")
# 
# save(lsoa, lsoa_ethn, lsoa_ethn_ses, lsoa_ses, towns, towns_ethn, towns_ethn_ses, towns_ses, file = "R/lsoa_towns")
# write_dta(lsoa, "stata/lsoa.dta")
# write_dta(lsoa_ethn, "stata/lsoa_ethn.dta")
# write_dta(lsoa_ethn_ses, "stata/lsoa_ethn_ses.dta")
# #write_dta(lsoa_ses, "stata/lsoa_ses.dta")
# write_dta(towns, "stata/towns.dta")
# write_dta(towns_ethn, "stata/towns_ethn.dta")
# write_dta(towns_ethn_ses, "stata/towns_ethn_ses.dta")
# #write_dta(towns_ses, "stata/towns_ses.dta")
# 
