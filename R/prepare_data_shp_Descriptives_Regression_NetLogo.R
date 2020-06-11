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
# 
# change from 2001 to 2011:
# The prefix "change_" marks differences of variables from 2011 with common variables from 2001, 
# appended to the dataset 2001



# data preparations
library(sf)
library(tidyverse)
library(readxl)
library(haven)

## read GIS 
shp_2001 <- read_sf(dsn ="rawdata/Lower_Layer_Super_Output_Areas__December_2001__Boundaries_EW_BGC-shp/",
                    stringsAsFactors=FALSE) %>% 
  st_transform("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs") %>%
  select(-FID, -Shape__Are, -Shape__Len, -LSOA01NMW)
shp_2011 <- read_sf(dsn ="rawdata/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC-shp/",
                    stringsAsFactors=FALSE) %>% 
  st_transform("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs") %>%
  select(-FID, -Shape__Are, -Shape__Len, -Age_Indica, -LSOA11NMW)

## read lookup tables 
lsoa_2001_2011 <- read_csv("rawdata/Lower_Layer_Super_Output_Area__2001__to_Lower_Layer_Super_Output_Area__2011__to_Local_Authority_District__2011__Lookup_in_England_and_Wales.csv",
                           col_types = cols(LAD11NMW = col_character() ) ) %>% 
  select(-LAD11CD, -LAD11NM, -LAD11NMW, -FID)
lsoa_2001_towns_2015 <- read_csv("rawdata/Lower_Layer_Super_Output_Area__2001__to_Major_Towns_and_Cities__December_2015__Lookup_in_England_and_Wales.csv") %>% 
  select(-FID)
lsoa_2011_towns_2015 <- read_csv("rawdata/Lower_Layer_Super_Output_Area__2011__to_Major_Towns_and_Cities__December_2015__Lookup_in_England_and_Wales.csv") %>% 
  select(-FID)
lookup01 <- lsoa_2001_2011 %>% left_join(lsoa_2001_towns_2015) %>% 
  filter(!(LSOA01CD %in% c("E01016111", "E01007676", "E01010174")) & CHGIND!="X")
lookup11 <- lsoa_2001_2011 %>% left_join(lsoa_2011_towns_2015) %>% 
  filter(!(LSOA01CD %in% c("E01016111", "E01007676", "E01010174")) & CHGIND!="X")
lookup <- lookup01 %>% filter(TCITY15CD == lookup11$TCITY15CD, substr(LSOA01CD,1,1)=="E")
# It holds: lookup == lookup11 %>% filter(TCITY15CD == lookup01$TCITY15CD)
rm(lookup01, lookup11, lsoa_2001_2011, lsoa_2001_towns_2015, lsoa_2011_towns_2015)

# NOTES:
# There are 3 LSOA which remain unchanged but which have switched the town assignment
# In 2001
# LSOA01CD  LSOA01NM       LSOA11CD  LSOA11NM       CHGIND TCITY15CD TCITY15NM 
# <chr>     <chr>          <chr>     <chr>          <chr>  <chr>     <chr>     
# 1 E01016111 Medway 015E    E01016111 Medway 015E    U      J01000037 Gillingham
# 2 E01007676 Rotherham 025A E01007676 Rotherham 025A U      J01000079 Rotherham 
# 3 E01010174 Solihull 013A  E01010174 Solihull 013A  U      J01000007 Birmingham 
# In 2011
# LSOA01CD  LSOA01NM       LSOA11CD  LSOA11NM       CHGIND TCITY15CD TCITY15NM
# <chr>     <chr>          <chr>     <chr>          <chr>  <chr>     <chr>    
# 1 E01016111 Medway 015E    E01016111 Medway 015E    U      J01000022 Chatham  
# 2 E01007676 Rotherham 025A E01007676 Rotherham 025A U      J01000082 Sheffield
# 3 E01010174 Solihull 013A  E01010174 Solihull 013A  U      J01000085 Solihull 
# Solution: We remove these three LSOA
# 
# We also remove all 163 LSOA complex matchingsmarked with CHGIND=="X". 
# Note that these are often not too complex but and could be resolved by eyeballing on maps
# for particular towns. 
#
# Finally, some lookup lines show a city assignment only in one for the years 2001 and 2011
# We include only those LSOA which are part of the town in both years. 

shp <- bind_rows(
  lookup %>% left_join(shp_2001) %>% filter(CHGIND != "M") %>% 
    group_by(LSOA01CD, LSOA01NM, TCITY15CD, TCITY15NM) %>% 
    mutate(LSOA11CD = paste(LSOA11CD, collapse = "_"), LSOA11NM = paste(LSOA11NM, collapse = "_"),
           CHGIND = paste(CHGIND, collapse = "_")) %>% 
    distinct(LSOA01CD, .keep_all = TRUE),
  lookup %>% left_join(shp_2011) %>% filter(CHGIND == "M") %>% 
    group_by(LSOA11CD, LSOA11NM, TCITY15CD, TCITY15NM) %>% 
    mutate(LSOA01CD = paste(LSOA01CD, collapse = "_"), LSOA01NM = paste(LSOA01NM, collapse = "_"),
           CHGIND = paste(CHGIND, collapse = "_")) %>% 
    distinct(LSOA11CD, .keep_all = TRUE)
) %>% group_by() %>% arrange(LSOA01CD) %>% st_as_sf() 
rm(shp_2001, shp_2011)

lsoa_2001 <- lookup %>% left_join(read_xlsx("rawdata/england_vars_2001_full2.xlsx",1), 
                                  by = c("LSOA01CD" = "lsoa01cd", "LSOA01NM" = "lsoa01nm")) %>% 
  select(-objectid, -st_areasha, -st_lengths) %>% arrange(LSOA01CD)
lsoa_2011 <- lookup %>% left_join(read_xlsx("rawdata/ethnic_ses_2011.xlsx",1), 
                                  by = c("LSOA11CD" = "lsoa11cd", "LSOA11NM" = "lsoa11nm")) %>% 
  arrange(LSOA01CD)
validLSOA01CD <- intersect(lsoa_2001 %>% filter(!is.na(total_people_01)) %>% pull(LSOA01CD),
                           lsoa_2011 %>% filter(!is.na(all)) %>% pull(LSOA01CD))
lsoa_2001 <- lsoa_2001 %>% filter(LSOA01CD %in% validLSOA01CD)
lsoa_2011 <- lsoa_2011 %>% filter(LSOA01CD %in% validLSOA01CD)

lsoa01 <- lsoa_2001 %>%  
  group_by(LSOA01CD, LSOA01NM, TCITY15CD, TCITY15NM) %>% 
  mutate(LSOA11CD = paste(LSOA11CD, collapse = "_"), LSOA11NM = paste(LSOA11NM, collapse = "_"),
         CHGIND = paste(CHGIND, collapse = "_")) %>% 
  group_by(LSOA01CD, LSOA01NM, TCITY15CD, TCITY15NM, LSOA11CD, LSOA11NM, CHGIND) %>% 
  summarise_all(first) %>% 
  group_by(LSOA11CD, LSOA11NM, TCITY15CD, TCITY15NM) %>% 
  mutate(LSOA01CD = paste(LSOA01CD, collapse = "_"), LSOA01NM = paste(LSOA01NM, collapse = "_"),
         CHGIND = paste(CHGIND, collapse = "_")) %>% 
  group_by(LSOA01CD, LSOA01NM, TCITY15CD, TCITY15NM, LSOA11CD, LSOA11NM, CHGIND) %>% 
  summarise_all(sum) %>%  group_by() %>% arrange(LSOA01CD)
lsoa11 <- lsoa_2011 %>%  
  group_by(LSOA11CD, LSOA11NM, TCITY15CD, TCITY15NM) %>% 
  mutate(LSOA01CD = paste(LSOA01CD, collapse = "_"), LSOA01NM = paste(LSOA01NM, collapse = "_"),
         CHGIND = paste(CHGIND, collapse = "_")) %>% 
  group_by(LSOA11CD, LSOA11NM, TCITY15CD, TCITY15NM, LSOA01CD, LSOA01NM, CHGIND) %>% 
  summarise_all(first) %>% 
  group_by(LSOA01CD, LSOA01NM, TCITY15CD, TCITY15NM) %>% 
  mutate(LSOA11CD = paste(LSOA11CD, collapse = "_"), LSOA11NM = paste(LSOA11NM, collapse = "_"),
         CHGIND = paste(CHGIND, collapse = "_")) %>% 
  group_by(LSOA11CD, LSOA11NM, TCITY15CD, TCITY15NM, LSOA01CD, LSOA01NM, CHGIND) %>% 
  summarise_all(sum) %>%  group_by() %>% arrange(LSOA01CD)

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

lsoa01 <- lsoa01 %>% rename(all_01=total_people_01) %>% group_by(TCITY15NM) %>% 
  mutate(ethgrouped_whiteb = eth_white_brit_01,
         ethgrouped_asian = eth_asia_indian_01+eth_asia_pakistani_01+eth_asia_bangla_01+eth_asia_chi_01+eth_asia_other_01,
         ethgrouped_black = eth_black_caribbean_01+eth_black_african_01+eth_black_other_01,
         ethgrouped_other = eth_mix_africa_01+eth_mix_asia_01+eth_mix_carib_01+eth_mix_other_01+eth_other_01,
         all_01_town = sum(all_01), fraction_01 = all_01/all_01_town) %>% 
  select(LSOA01CD, LSOA01NM, TCITY15NM, all_01, all_01_town, fraction_01,
         all_of(ethgroupednames), all_of(ethnames_01), all1674_01=total_people1674_01, everything()) %>% 
  group_by() 
lsoa11 <- lsoa11 %>% rename(all_11=allpop) %>% group_by(TCITY15CD) %>% 
  mutate(ethgrouped_whiteb = eth_white_brit_11,
         ethgrouped_asian = eth_asia_indian_11+eth_asia_pakistani_11+eth_asia_bangla_11+eth_asia_chi_11+eth_asia_other_11,
         ethgrouped_black = eth_black_caribbean_11+eth_black_african_11+eth_black_other_11,
         ethgrouped_other = eth_white_gypsy_11+eth_mix_africa_11+eth_mix_asia_11+eth_mix_carib_11+eth_mix_other_11+eth_other_arab_11+eth_other_other_11,
         all_11_town = sum(all_11), fraction_11 = all_11/all_11_town,
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
  select(LSOA11CD, LSOA11NM, TCITY15NM, all_11_town, fraction_11,
         allvalidses_11, allvalidses_11_town, fractionvalidses_11,
         all_of(ethgroupednames), all_of(ethnames_11), everything() ) %>% 
  group_by()

## segregation indices towns
towns01 <- lsoa01 %>% select(-LSOA01CD,-LSOA01NM,-all_01_town,-fraction_01,-LSOA11CD,-LSOA11NM,-CHGIND, -TCITY15CD) %>% 
  group_by(TCITY15NM) %>% summarize_all(sum) %>% group_by()
segr_towns01 <- towns01 %>% select(TCITY15NM, all_01, c(contains("ethgrouped_"),contains("eth_"))) %>% 
  pivot_longer(c(contains("ethgrouped_"),contains("eth_")), names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(TCITY15NM, ethnicgrouping) %>% 
  mutate(segrtown_fraction = count/all_01, segrtown_simpson = sum(segrtown_fraction^2), 
         segrtown_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrtown_fraction)^2)) %>% 
  group_by() %>% 
  select(-count, -all_01)
towns01 <- segr_towns01 %>% select(TCITY15NM, ethnicgrouping, ethnicity, segrtown_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrtown_fraction, names_prefix="segrtown_fraction_") %>% 
  right_join(towns01, by = "TCITY15NM")
towns01 <- segr_towns01 %>% select(TCITY15NM, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrtown_simpson, segrtown_simpson2)) %>% 
  right_join(towns01, by = "TCITY15NM")

towns11 <- lsoa11 %>% select(-LSOA11CD,-LSOA11NM,-LSOA01CD,-LSOA01NM, -CHGIND, -TCITY15CD,
                             -all_11_town,-fraction_11,-allvalidses_11_town,-fractionvalidses_11,
                             -ethgroupedses_whiteb_high, -ethgroupedses_whiteb_mid, -ethgroupedses_whiteb_low, 
                             -ethgroupedses_asian_high, -ethgroupedses_asian_mid, -ethgroupedses_asian_low,
                             -ethgroupedses_black_high, -ethgroupedses_black_mid, -ethgroupedses_black_low,
                             -ethgroupedses_other_high, -ethgroupedses_other_mid, -ethgroupedses_other_low) %>% 
  group_by(TCITY15NM) %>% summarize_all(sum) %>% group_by()
segr_towns11 <- towns11 %>% select(TCITY15NM, all_11, allvalidses_11,
                                   c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_"))) %>% 
  pivot_longer(c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_")), 
               names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(TCITY15NM, ethnicgrouping) %>% 
  mutate(segrtown_fraction = count/if_else(ethnicgrouping=="ethgroupedses",allvalidses_11,all_11), 
         segrtown_simpson = sum(segrtown_fraction^2), 
         segrtown_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrtown_fraction)^2)) %>% 
  group_by() %>% 
  select(-count, -all_11, -allvalidses_11)
towns11 <- segr_towns11 %>% select(TCITY15NM, ethnicgrouping, ethnicity, segrtown_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrtown_fraction, names_prefix="segrtown_fraction_") %>% 
  right_join(towns11, by = "TCITY15NM")
towns11 <- segr_towns11 %>% select(TCITY15NM, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrtown_simpson, segrtown_simpson2)) %>% 
  right_join(towns11, by = "TCITY15NM")

# segregation indices lsoa
segr_lsoa01 <- lsoa01 %>% 
  select(LSOA01CD, LSOA01NM, TCITY15NM, all_01, c(contains("ethgrouped_"),contains("eth_"))) %>% 
  pivot_longer(c(contains("ethgrouped_"),contains("eth_")), names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(LSOA01CD, ethnicgrouping) %>% 
  mutate(segrlsoa_fraction = count/all_01, segrlsoa_simpson = sum(segrlsoa_fraction^2), 
         segrlsoa_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrlsoa_fraction)^2)) %>% 
  group_by() %>% 
  select(-count, -all_01)
lsoa01 <- segr_lsoa01 %>% select(LSOA01CD, LSOA01NM, TCITY15NM, ethnicgrouping, ethnicity, segrlsoa_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrlsoa_fraction, names_prefix="segrlsoa_fraction_") %>% 
  right_join(lsoa01, by = c("LSOA01CD","LSOA01NM","TCITY15NM"))
lsoa01 <- segr_lsoa01 %>% select(LSOA01CD, LSOA01NM, TCITY15NM, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrlsoa_simpson, segrlsoa_simpson2)) %>% 
  right_join(lsoa01, by = c("LSOA01CD","LSOA01NM","TCITY15NM"))

segr_lsoa11 <- lsoa11 %>% select(LSOA11CD, LSOA11NM, TCITY15NM, all_11, allvalidses_11, 
                                    c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_")),
                                    -ethgroupedses_whiteb_high, -ethgroupedses_whiteb_mid, -ethgroupedses_whiteb_low, 
                                    -ethgroupedses_asian_high, -ethgroupedses_asian_mid, -ethgroupedses_asian_low,
                                    -ethgroupedses_black_high, -ethgroupedses_black_mid, -ethgroupedses_black_low,
                                    -ethgroupedses_other_high, -ethgroupedses_other_mid, -ethgroupedses_other_low) %>% 
  pivot_longer(c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_")), 
               names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(LSOA11CD, ethnicgrouping) %>% 
  mutate(segrlsoa_fraction = count/if_else(ethnicgrouping=="ethgroupedses",allvalidses_11,all_11),
         segrlsoa_simpson = sum(segrlsoa_fraction^2), 
         segrlsoa_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrlsoa_fraction)^2)) %>% 
  group_by() %>% 
  select(-count, -all_11, -allvalidses_11)
lsoa11 <- segr_lsoa11 %>% select(LSOA11CD, LSOA11NM, TCITY15NM, ethnicgrouping, ethnicity, segrlsoa_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrlsoa_fraction, names_prefix="segrlsoa_fraction_") %>% 
  right_join(lsoa11, by = c("LSOA11CD","LSOA11NM","TCITY15NM"))
lsoa11 <- segr_lsoa11 %>% select(LSOA11CD, LSOA11NM, TCITY15NM, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrlsoa_simpson, segrlsoa_simpson2)) %>% 
  right_join(lsoa11, by = c("LSOA11CD","LSOA11NM","TCITY15NM"))
rm(segr_lsoa01, segr_lsoa11, segr_towns01, segr_towns11, lsoa_2001, lsoa_2011)

# final lsoa dataset
lsoa_2001 <- shp %>% right_join(lsoa01) 
lsoa_2011 <- shp %>% right_join(lsoa11) 
lsoa <- bind_rows(
  lsoa_2001 %>% select(LSOA01CD, LSOA01NM, LSOA11CD, LSOA11NM, CHGIND, TCITY15CD, TCITY15NM, 
                           starts_with("segrlsoa_simpson"), starts_with("segrlsoa_fraction_ethgrouped_")) %>% 
    mutate(year = 2001),
  lsoa_2011 %>% select(LSOA01CD, LSOA01NM, LSOA11CD, LSOA11NM, CHGIND, TCITY15CD, TCITY15NM, 
                           starts_with("segrlsoa_simpson"), starts_with("segrlsoa_fraction_ethgrouped_"), 
                           -segrlsoa_simpson_ethgroupedses, -segrlsoa_simpson2_ethgroupedses) %>% 
    mutate(year = 2011)
)
lsoa_2001 <- lsoa %>% as_tibble() %>% 
  pivot_wider(id_cols = c(LSOA01CD, LSOA01NM, LSOA11CD, LSOA11NM, CHGIND, TCITY15CD, TCITY15NM, geometry), 
              names_from = year, values_from = starts_with("segr")) %>% 
  transmute(LSOA01CD, LSOA01NM, LSOA11CD, LSOA11NM, CHGIND, TCITY15CD, TCITY15NM,
            change_segrlsoa_simpson_ethgrouped = segrlsoa_simpson_ethgrouped_2011 - segrlsoa_simpson_ethgrouped_2001,
            change_segrlsoa_simpson_eth = segrlsoa_simpson_eth_2011 - segrlsoa_simpson_eth_2001,
            change_segrlsoa_simpson2_ethgrouped = segrlsoa_simpson2_ethgrouped_2011 - segrlsoa_simpson2_ethgrouped_2001,
            change_segrlsoa_simpson2_eth = segrlsoa_simpson2_eth_2011 - segrlsoa_simpson2_eth_2001,
            change_segrlsoa_fraction_ethgrouped_whiteb = segrlsoa_fraction_ethgrouped_whiteb_2011 - segrlsoa_fraction_ethgrouped_whiteb_2001,
            change_segrlsoa_fraction_ethgrouped_asian = segrlsoa_fraction_ethgrouped_asian_2011 - segrlsoa_fraction_ethgrouped_asian_2001,
            change_segrlsoa_fraction_ethgrouped_black = segrlsoa_fraction_ethgrouped_black_2011 - segrlsoa_fraction_ethgrouped_black_2001,
            change_segrlsoa_fraction_ethgrouped_other = segrlsoa_fraction_ethgrouped_other_2011 - segrlsoa_fraction_ethgrouped_other_2001
  ) %>% 
  right_join(lsoa_2001) %>% st_as_sf()
save(lsoa_2001,lsoa_2011,file = "R/lsoa_2001_lsoa_2011")
lsoa_2001 %>% as_tibble() %>% select(-geometry) %>% write_csv("R/lsoa_2001.csv")
lsoa_2011 %>% as_tibble() %>% select(-geometry) %>% write_csv("R/lsoa_2011.csv")

# Bradford
bradford_2001 <- lsoa_2001 %>% filter(TCITY15NM == "Bradford")
bradford_2011 <- lsoa_2011 %>% filter(TCITY15NM == "Bradford")
save(bradford_2001,bradford_2011,file = "R/bradford_2001_2011")
write_csv(bradford_2001, "R/bradford_2001.csv")
write_csv(bradford_2011, "R/bradford_2011.csv")


## export LSOA of some towns including raw data to ESRI shapefile readable with NetLogo GIS extension
for (TOWN in c("Southampton","Leeds", "Bradford", "Leicester", "London", "Manchester", "Birmingham", "Brighton and Hove",
               "Kingston upon Hull", "Stoke-on-Trent", "Plymouth", "Derby", "Nottingham", "Newcastle upon Tyne","Leeds", "Sheffield",
               "Coventry", "St Albans" , "Guildford", "Cambridge", "Middlesbrough", "Sunderland", "Oxford","Luton",
               "Blackburn", "Oldham","Wolverhampton","Slough","Walsall","Bristol", "Liverpool", "Leeds", "Reading")) {
  if (!dir.exists(paste0("shp_NetLogo/",TOWN))) {
    town <- lsoa_2011 %>% filter(TCITY15NM == TOWN) %>%
      select(LSOA11CD, allvalidses_11, starts_with("ethgroupedses_"), 
             -ethgroupedses_whiteb, -ethgroupedses_asian, -ethgroupedses_black, -ethgroupedses_other)
    names(town) <- sub("ethgroupedses_","",names(town))
    town %>% st_write(dsn = paste0("shp_NetLogo/",TOWN), layer = TOWN, driver = "ESRI Shapefile", delete_dsn = TRUE)
}}



# # Dissimilarity, Location Quotient
# lsoa_ethn <- lsoa_ethn %>% left_join(select(towns_ethn, town, Ethnicity, fraction_ethnicity_town = fraction),
#                                      by = c("town", "Ethnicity")) %>% 
#   group_by(LSOA11CD) %>% mutate(Dissimilarity = abs(fraction - fraction_ethnicity_town) / 
#                                   (2*fraction_ethnicity_town*(1-fraction_ethnicity_town)),
#                                 Location_Quotient = fraction/fraction_ethnicity_town) 
# towns_ethn <- lsoa_ethn %>% left_join(select(lsoa,town,LSOA11CD,fraction_lsoa), c("LSOA11CD", "town")) %>%
#   group_by(town,Ethnicity) %>% 
#   summarize(Dissimilarity_Index = sum(fraction_lsoa * Dissimilarity)) %>% 
#   right_join(towns_ethn, c("town", "Ethnicity")) %>% group_by()
# towns <- towns_ethn %>% group_by(town) %>% 
#   summarize(Simpson_town = sum(fraction^2), 
#             Entropy_town = -1/log(4)*sum(fraction*log(fraction))) %>% 
#   right_join(towns, by = "town")
# towns <- lsoa_ethn %>% left_join(rename(towns, count_town=count), by = "town") %>% 
#   left_join(select(lsoa,town,LSOA11CD,fraction_lsoa), c("LSOA11CD", "town")) %>% 
#   group_by(LSOA11CD, town, fraction_lsoa, Simpson_town, Entropy_town) %>%
#   summarize(Simpson = sum(fraction^2), 
#             Entropy = -1/log(4)*sum(fraction*
#                                       if_else(fraction == 0, 0, log(fraction)))) %>% 
#   mutate(Excess_Simpson = Simpson - Simpson_town, Loss_Entropy = Entropy_town - Entropy) %>% 
#   group_by(town) %>% summarize(Avg_Simpson = sum(fraction_lsoa*Simpson), 
#                                Avg_Excess_Simpson = sum(fraction_lsoa*Excess_Simpson), 
#                                Avg_Entropy = sum(fraction_lsoa*Entropy),
#                                Avg_Loss_Entropy = sum(fraction_lsoa*Loss_Entropy)) %>% 
#   right_join(towns, by = "town")


load("R/bradford_2001_2011")

bradford_2001 %>% ggplot(aes(fill = change_segrlsoa_simpson2_ethgrouped)) + geom_sf()
bradford_2001 %>% ggplot(aes(fill = change_segrlsoa_fraction_ethgrouped_asian)) + geom_sf()
bradford_2001 %>% ggplot(aes(fill = segrlsoa_simpson2_ethgrouped)) + geom_sf()
bradford_2001 %>% ggplot(aes(fill = segrlsoa_fraction_ethgrouped_asian)) + geom_sf()

glm(change_segrlsoa_fraction_ethgrouped_asian ~ tenure_owned_01, data = bradford_2001)


bradford_2001 %>% mutate(id=1:n()) %>% 
  ggplot(aes(fill = change_segrlsoa_simpson2_ethgrouped, label = id)) + geom_sf() + 
  geom_sf_text()

a <- st_intersects(bradford_2001)
