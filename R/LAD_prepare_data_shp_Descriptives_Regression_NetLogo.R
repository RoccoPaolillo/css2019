### Same as prepare_data but with aggregation to Local Authorities LA!

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
  dplyr::select(-FID, -Shape__Are, -Shape__Len, -LSOA01NMW)
shp_2011 <- read_sf(dsn ="rawdata/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC-shp/",
                    stringsAsFactors=FALSE) %>% 
  st_transform("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs") %>%
  dplyr::select(-FID, -Shape__Are, -Shape__Len, -Age_Indica, -LSOA11NMW)

## read lookup tables 
lsoa_2001_2011 <- read_csv("rawdata/Lower_Layer_Super_Output_Area__2001__to_Lower_Layer_Super_Output_Area__2011__to_Local_Authority_District__2011__Lookup_in_England_and_Wales.csv",
                           col_types = cols(LAD11NMW = col_character() ) ) %>% 
  dplyr::select(-LAD11NMW, -FID)

# We also remove all 163 LSOA complex matchingsmarked with CHGIND=="X". 
# Note that these are often not too complex but and could be resolved by eyeballing on maps
# for particular towns. 
#
# Finally, some lookup lines show a city assignment only in one for the years 2001 and 2011
# We include only those LSOA which are part of the town in both years. 

shp <- bind_rows(
  lsoa_2001_2011 %>% left_join(shp_2001) %>% filter(CHGIND != "M") %>% 
    group_by(LSOA01CD, LSOA01NM, LAD11CD, LAD11NM) %>% 
    mutate(LSOA11CD = paste(LSOA11CD, collapse = "_"), LSOA11NM = paste(LSOA11NM, collapse = "_"),
           CHGIND = paste(CHGIND, collapse = "_")) %>% 
    distinct(LSOA01CD, .keep_all = TRUE),
  lsoa_2001_2011 %>% left_join(shp_2011) %>% filter(CHGIND == "M") %>% 
    group_by(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM) %>% 
    mutate(LSOA01CD = paste(LSOA01CD, collapse = "_"), LSOA01NM = paste(LSOA01NM, collapse = "_"),
           CHGIND = paste(CHGIND, collapse = "_")) %>% 
    distinct(LSOA11CD, .keep_all = TRUE)
) %>% group_by() %>% arrange(LSOA01CD) %>% st_as_sf() 
rm(shp_2001, shp_2011)

lsoa_2001 <- lsoa_2001_2011 %>% left_join(read_xlsx("rawdata/england_vars_2001_full2.xlsx",1), 
                                  by = c("LSOA01CD" = "lsoa01cd", "LSOA01NM" = "lsoa01nm")) %>% 
  dplyr::select(-objectid, -st_areasha, -st_lengths) %>% arrange(LSOA01CD)
lsoa_2011 <- lsoa_2001_2011 %>% left_join(read_xlsx("rawdata/ethnic_ses_2011.xlsx",1), 
                                  by = c("LSOA11CD" = "lsoa11cd", "LSOA11NM" = "lsoa11nm")) %>% 
  arrange(LSOA01CD)
validLSOA01CD <- intersect(lsoa_2001 %>% filter(!is.na(total_people_01)) %>% pull(LSOA01CD),
                           lsoa_2011 %>% filter(!is.na(all)) %>% pull(LSOA01CD))
lsoa_2001 <- lsoa_2001 %>% filter(LSOA01CD %in% validLSOA01CD)
lsoa_2011 <- lsoa_2011 %>% filter(LSOA01CD %in% validLSOA01CD)

lsoa01 <- lsoa_2001 %>%  
  group_by(LSOA01CD, LSOA01NM, LAD11CD, LAD11NM) %>% 
  mutate(LSOA11CD = paste(LSOA11CD, collapse = "_"), LSOA11NM = paste(LSOA11NM, collapse = "_"),
         CHGIND = paste(CHGIND, collapse = "_")) %>% 
  group_by(LSOA01CD, LSOA01NM, LAD11CD, LAD11NM, LSOA11CD, LSOA11NM, CHGIND) %>% 
  summarise_all(first) %>% 
  group_by(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM) %>% 
  mutate(LSOA01CD = paste(LSOA01CD, collapse = "_"), LSOA01NM = paste(LSOA01NM, collapse = "_"),
         CHGIND = paste(CHGIND, collapse = "_")) %>% 
  group_by(LSOA01CD, LSOA01NM, LAD11CD, LAD11NM, LSOA11CD, LSOA11NM, CHGIND) %>% 
  summarise_all(sum) %>%  group_by() %>% arrange(LSOA01CD)
lsoa11 <- lsoa_2011 %>%  
  group_by(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM) %>% 
  mutate(LSOA01CD = paste(LSOA01CD, collapse = "_"), LSOA01NM = paste(LSOA01NM, collapse = "_"),
         CHGIND = paste(CHGIND, collapse = "_")) %>% 
  group_by(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM, LSOA01CD, LSOA01NM, CHGIND) %>% 
  summarise_all(first) %>% 
  group_by(LSOA01CD, LSOA01NM, LAD11CD, LAD11NM) %>% 
  mutate(LSOA11CD = paste(LSOA11CD, collapse = "_"), LSOA11NM = paste(LSOA11NM, collapse = "_"),
         CHGIND = paste(CHGIND, collapse = "_")) %>% 
  group_by(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM, LSOA01CD, LSOA01NM, CHGIND) %>% 
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
save(lsoa01, lsoa11, file="LAD_interrim")
load("LAD_interrim")

lsoa01 <- lsoa01 %>% rename(all_01=total_people_01) %>% group_by(LAD11NM) %>% 
  mutate(ethgrouped_whiteb = eth_white_brit_01,
         ethgrouped_asian = eth_asia_indian_01+eth_asia_pakistani_01+eth_asia_bangla_01+eth_asia_chi_01+eth_asia_other_01,
         ethgrouped_black = eth_black_caribbean_01+eth_black_african_01+eth_black_other_01,
         ethgrouped_other = eth_mix_africa_01+eth_mix_asia_01+eth_mix_carib_01+eth_mix_other_01+eth_other_01,
         all_01_town = sum(all_01), fraction_01 = all_01/all_01_town) %>% 
  dplyr::select(LSOA01CD, LSOA01NM, LAD11NM, all_01, all_01_town, fraction_01,
         all_of(ethgroupednames), all_of(ethnames_01), all1674_01=total_people1674_01, everything()) %>% 
  group_by() 
lsoa11 <- lsoa11 %>% rename(all_11=allpop) %>% group_by(LAD11CD) %>% 
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
  dplyr::select(LSOA11CD, LSOA11NM, LAD11NM, all_11_town, fraction_11,
         allvalidses_11, allvalidses_11_town, fractionvalidses_11,
         all_of(ethgroupednames), all_of(ethnames_11), everything() ) %>% 
  group_by()

## segregation indices towns
towns01 <- lsoa01 %>% dplyr::select(-LSOA01CD,-LSOA01NM,-all_01_town,-fraction_01,-LSOA11CD,-LSOA11NM,-CHGIND, -LAD11CD) %>% 
  group_by(LAD11NM) %>% summarize_all(sum) %>% group_by()
segr_towns01 <- towns01 %>% dplyr::select(LAD11NM, all_01, c(contains("ethgrouped_"),contains("eth_"))) %>% 
  pivot_longer(c(contains("ethgrouped_"),contains("eth_")), names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(LAD11NM, ethnicgrouping) %>% 
  mutate(segrtown_fraction = count/all_01, segrtown_simpson = sum(segrtown_fraction^2), 
         segrtown_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrtown_fraction)^2)) %>% 
  group_by() %>% 
  dplyr::select(-count, -all_01)
towns01 <- segr_towns01 %>% dplyr::select(LAD11NM, ethnicgrouping, ethnicity, segrtown_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrtown_fraction, names_prefix="segrtown_fraction_") %>% 
  right_join(towns01, by = "LAD11NM")
towns01 <- segr_towns01 %>% dplyr::select(LAD11NM, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrtown_simpson, segrtown_simpson2)) %>% 
  right_join(towns01, by = "LAD11NM")

towns11 <- lsoa11 %>% dplyr::select(-LSOA11CD,-LSOA11NM,-LSOA01CD,-LSOA01NM, -CHGIND, -LAD11CD,
                             -all_11_town,-fraction_11,-allvalidses_11_town,-fractionvalidses_11,
                             -ethgroupedses_whiteb_high, -ethgroupedses_whiteb_mid, -ethgroupedses_whiteb_low, 
                             -ethgroupedses_asian_high, -ethgroupedses_asian_mid, -ethgroupedses_asian_low,
                             -ethgroupedses_black_high, -ethgroupedses_black_mid, -ethgroupedses_black_low,
                             -ethgroupedses_other_high, -ethgroupedses_other_mid, -ethgroupedses_other_low) %>% 
  group_by(LAD11NM) %>% summarize_all(sum) %>% group_by()
segr_towns11 <- towns11 %>% dplyr::select(LAD11NM, all_11, allvalidses_11,
                                   c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_"))) %>% 
  pivot_longer(c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_")), 
               names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  group_by(LAD11NM, ethnicgrouping) %>% 
  mutate(segrtown_fraction = count/if_else(ethnicgrouping=="ethgroupedses",allvalidses_11,all_11), 
         segrtown_simpson = sum(segrtown_fraction^2), 
         segrtown_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrtown_fraction)^2)) %>% 
  group_by() %>% 
  dplyr::select(-count, -all_11, -allvalidses_11)
towns11 <- segr_towns11 %>% dplyr::select(LAD11NM, ethnicgrouping, ethnicity, segrtown_fraction) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=segrtown_fraction, 
              names_prefix="segrtown_fraction_") %>% 
  right_join(towns11, by = "LAD11NM")
towns11 <- segr_towns11 %>% dplyr::select(LAD11NM, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, values_from=c(segrtown_simpson, segrtown_simpson2)) %>% 
  right_join(towns11, by = "LAD11NM")

# segregation indices lsoa
segr_lsoa01 <- lsoa01 %>% 
  dplyr::select(LSOA01CD, LSOA01NM, LAD11NM, all_01, fraction_01, c(contains("ethgrouped_"),contains("eth_"))) %>% 
  pivot_longer(c(contains("ethgrouped_"),contains("eth_")), names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  right_join(segr_towns01, by = c("LAD11NM", "ethnicity", "ethnicgrouping")) %>% 
  group_by(LSOA01CD, ethnicgrouping) %>% 
  mutate(segrlsoa_fraction = count/all_01, 
         segrlsoa_lq = segrlsoa_fraction/segrtown_fraction,
         segrlsoa_dissim = abs(segrlsoa_fraction - segrtown_fraction)/(2*segrtown_fraction*(1-segrtown_fraction)),
         segrlsoa_simpson = sum(segrlsoa_fraction^2), 
         segrlsoa_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrlsoa_fraction)^2)) %>% 
  group_by() %>% 
  dplyr::select(-count, -all_01)
lsoa01 <- segr_lsoa01 %>% 
  dplyr::select(LSOA01CD, LSOA01NM, LAD11NM, ethnicgrouping, ethnicity, segrlsoa_fraction, segrlsoa_lq) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), 
              values_from=c(segrlsoa_fraction, segrlsoa_lq)) %>% 
  right_join(lsoa01, by = c("LSOA01CD","LSOA01NM","LAD11NM"))
lsoa01 <- segr_lsoa01 %>% dplyr::select(LSOA01CD, LSOA01NM, LAD11NM, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
    pivot_wider(names_from = ethnicgrouping, 
                values_from=c(segrlsoa_simpson, segrlsoa_simpson2, segrtown_simpson, segrtown_simpson2)) %>% 
    right_join(lsoa01, by = c("LSOA01CD","LSOA01NM","LAD11NM"))
# compute town dissimilarity indices and add to towns
towns01 <- segr_lsoa01 %>% 
  dplyr::select(LAD11NM, ethnicity, ethnicgrouping, fraction_01, segrlsoa_dissim) %>% 
  group_by(LAD11NM, ethnicity, ethnicgrouping) %>% 
  summarize(segrtown_dissim = sum(fraction_01 * segrlsoa_dissim)) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), 
              values_from=c(segrtown_dissim), names_prefix = "segrtown_dissim_") %>% 
  left_join(towns01, by = "LAD11NM")
# add average simpson to towns
towns01 <- lsoa01 %>% dplyr::select(LAD11NM, fraction_01, contains("simpson")) %>% group_by(LAD11NM) %>% 
  summarize(across(contains("lsoa_simpson"),function(x) sum(x*fraction_01))) %>% 
  left_join(towns01, by = "LAD11NM")
names(towns01) <- sub("lsoa_simpson","town_avgsimpson",names(towns01))

segr_lsoa11 <- lsoa11 %>% dplyr::select(LSOA11CD, LSOA11NM, LAD11NM, all_11, allvalidses_11, 
                                        fraction_11, fractionvalidses_11,
                                        c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_")),
                                        -ethgroupedses_whiteb_high, -ethgroupedses_whiteb_mid, -ethgroupedses_whiteb_low, 
                                        -ethgroupedses_asian_high, -ethgroupedses_asian_mid, -ethgroupedses_asian_low,
                                        -ethgroupedses_black_high, -ethgroupedses_black_mid, -ethgroupedses_black_low,
                                        -ethgroupedses_other_high, -ethgroupedses_other_mid, -ethgroupedses_other_low) %>% 
  pivot_longer(c(contains("ethgroupedses_"),contains("ethgrouped_"),contains("eth_")), 
               names_to = "ethnicity", values_to = "count") %>% 
  mutate(ethnicgrouping = word(ethnicity,1,sep="_"), ethnicity = word(ethnicity,2,-1,sep="_")) %>% 
  right_join(segr_towns11, by = c("LAD11NM", "ethnicity", "ethnicgrouping")) %>% 
  group_by(LSOA11CD, ethnicgrouping) %>% 
  mutate(segrlsoa_fraction = count/if_else(ethnicgrouping=="ethgroupedses",allvalidses_11,all_11), 
         segrlsoa_lq = segrlsoa_fraction/segrtown_fraction,
         segrlsoa_dissim = abs(segrlsoa_fraction - segrtown_fraction)/(2*segrtown_fraction*(1-segrtown_fraction)),
         segrlsoa_simpson = sum(segrlsoa_fraction^2), 
         segrlsoa_simpson2 = sum((if_else(str_detect(ethnicity,"other"),0,1)*segrlsoa_fraction)^2)) %>% 
  group_by() %>% 
  dplyr::select(-count, -all_11, -allvalidses_11)
lsoa11 <- segr_lsoa11 %>% dplyr::select(LSOA11CD, LSOA11NM, LAD11NM, ethnicgrouping, ethnicity, segrlsoa_fraction, segrlsoa_lq) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), values_from=c(segrlsoa_fraction,segrlsoa_lq)) %>% 
  right_join(lsoa11, by = c("LSOA11CD","LSOA11NM","LAD11NM"))
lsoa11 <- segr_lsoa11 %>% dplyr::select(LSOA11CD, LSOA11NM, LAD11NM, ethnicgrouping, contains("simpson")) %>% distinct() %>% 
  pivot_wider(names_from = ethnicgrouping, 
              values_from=c(segrlsoa_simpson, segrlsoa_simpson2, segrtown_simpson, segrtown_simpson2)) %>% 
  right_join(lsoa11, by = c("LSOA11CD","LSOA11NM","LAD11NM"))
# compute town dissimilarity indices and add to towns
towns11 <- segr_lsoa11 %>% 
  dplyr::select(LAD11NM, ethnicity, ethnicgrouping, fraction_11, fractionvalidses_11, segrlsoa_dissim) %>% 
  group_by(LAD11NM, ethnicity, ethnicgrouping) %>% 
  summarize(segrtown_dissim = sum(if_else(ethnicgrouping=="ethgroupedses",fractionvalidses_11, fraction_11) * segrlsoa_dissim)) %>% 
  pivot_wider(names_from = c(ethnicgrouping, ethnicity), 
              values_from=c(segrtown_dissim), names_prefix = "segrtown_dissim_") %>% 
  left_join(towns11, by = "LAD11NM")
# add average simpson to towns
towns11 <- lsoa11 %>% dplyr::select(LAD11NM, fraction_11, fractionvalidses_11, contains("simpson")) %>% 
  group_by(LAD11NM) %>% 
  summarize(segrlsoa_simpson_ethgroupedses = sum(segrlsoa_simpson_ethgroupedses*fractionvalidses_11),
            segrlsoa_simpson_ethgrouped = sum(segrlsoa_simpson_ethgrouped*fraction_11),
            segrlsoa_simpson_eth = sum(segrlsoa_simpson_eth*fraction_11),
            segrlsoa_simpson2_ethgroupedses = sum(segrlsoa_simpson2_ethgroupedses*fractionvalidses_11),
            segrlsoa_simpson2_ethgrouped = sum(segrlsoa_simpson2_ethgrouped*fraction_11),
            segrlsoa_simpson2_eth = sum(segrlsoa_simpson2_eth*fraction_11)) %>% 
  left_join(towns11, by = "LAD11NM")
names(towns11) <- sub("lsoa_simpson","town_avgsimpson",names(towns11))

rm(segr_lsoa01, segr_lsoa11, segr_towns01, segr_towns11)

# final lsoa dataset
lsoa_2001 <- shp %>% right_join(lsoa01) 
lsoa_2011 <- shp %>% right_join(lsoa11) 
lsoa <- bind_rows(
  lsoa_2001 %>% dplyr::select(LSOA01CD, LSOA01NM, LSOA11CD, LSOA11NM, CHGIND, LAD11CD, LAD11NM, 
                           starts_with("segrlsoa_simpson"), starts_with("segrlsoa_fraction_ethgrouped_")) %>% 
    mutate(year = 2001),
  lsoa_2011 %>% dplyr::select(LSOA01CD, LSOA01NM, LSOA11CD, LSOA11NM, CHGIND, LAD11CD, LAD11NM, 
                           starts_with("segrlsoa_simpson"), starts_with("segrlsoa_fraction_ethgrouped_"), 
                           -segrlsoa_simpson_ethgroupedses, -segrlsoa_simpson2_ethgroupedses) %>% 
    mutate(year = 2011)
)
lsoa_2001 <- lsoa %>% as_tibble() %>% 
  pivot_wider(id_cols = c(LSOA01CD, LSOA01NM, LSOA11CD, LSOA11NM, CHGIND, LAD11CD, LAD11NM, geometry), 
              names_from = year, values_from = starts_with("segr")) %>% 
  transmute(LSOA01CD, LSOA01NM, LSOA11CD, LSOA11NM, CHGIND, LAD11CD, LAD11NM,
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
lsoa_2001 <- lsoa_2011 %>% as_tibble() %>% dplyr::select(LSOA01CD, LSOA01NM, LSOA11CD, LSOA11NM, CHGIND, LAD11CD, LAD11NM, contains("segr")) %>% 
  rename_with(function(x) paste0(x,"_11"), .cols=contains("segr") & !contains("_11")) %>% 
  left_join(lsoa_2001) %>% 
  mutate(high = nssec1_1+nssec1_2+nssec2, mid = nssec3+nssec4,low = nssec5+nssec6+nssec7,
         nonvalidses = neverw+ltunemp+ftstudent+notclass) %>% 
  st_as_sf()
save(lsoa_2001,lsoa_2011,file = "R/LAD_lsoa_2001_lsoa_2011")
lsoa_2001 %>% as_tibble() %>% dplyr::select(-geometry) %>% write_csv("R/LAD_lsoa_2001.csv")
lsoa_2011 %>% as_tibble() %>% dplyr::select(-geometry) %>% write_csv("R/LAD_lsoa_2011.csv")
towns01 %>% as_tibble() %>% write_csv("R/LAD_towns_2001.csv")
towns11 %>% as_tibble() %>% write_csv("R/LAD_towns_2011.csv")
save(towns01, towns11, file = "R/LAD_towns01_towns11")

# Bradford
bradford_2001 <- lsoa_2001 %>% filter(LAD11NM == "Bradford")
bradford_2011 <- lsoa_2011 %>% filter(LAD11NM == "Bradford")
save(bradford_2001,bradford_2011,file = "R/LAD_bradford_2001_2011")
write_csv(bradford_2001, "R/LAD_bradford_2001.csv")
write_csv(bradford_2011, "R/LAD_bradford_2011.csv")


### DESCRIPTIVES TO SHOW
load("R/LAD_lsoa_2001_lsoa_2011")
load("R/LAD_towns01_towns11")
load("R/LAD_bradford_2001_2011")

towns <- bind_rows(
  towns01 %>% dplyr::select(LAD11NM, contains("segrtown_"), all=all_01) %>% mutate(year = 2001),
  towns11 %>% dplyr::select(LAD11NM, contains("segrtown_"), all=all_11) %>% mutate(year = 2011)
) %>% #select(-segrtown_avgsimpson2_ethgroupedses, -segrtown_simpson2_ethgroupedses) %>% 
  mutate(excess_avgsimpson2_ethgrouped = segrtown_avgsimpson2_ethgrouped - segrtown_simpson2_ethgrouped,
         excess_avgsimpson2_eth = segrtown_avgsimpson2_eth - segrtown_simpson2_eth) %>% 
  dplyr::select(LAD11NM, year, everything())
br <- towns %>% filter(LAD11NM=="Bradford")

br %>% dplyr::select(year,  
                     segrtown_fraction_ethgrouped_whiteb,
                     segrtown_fraction_ethgrouped_asian,  
                     segrtown_fraction_ethgrouped_black, 
                     segrtown_fraction_ethgrouped_other,
                     segrtown_fraction_ethgroupedses_whiteb,
                     segrtown_fraction_ethgroupedses_asian,  
                     segrtown_fraction_ethgroupedses_black, 
                     segrtown_fraction_ethgroupedses_other,  
                     segrtown_dissim_ethgrouped_whiteb,
                     segrtown_dissim_ethgrouped_asian,  
                     segrtown_dissim_ethgrouped_black, 
                     segrtown_dissim_ethgrouped_other,
                     segrtown_dissim_ethgroupedses_whiteb,
                     segrtown_dissim_ethgroupedses_asian,  
                     segrtown_dissim_ethgroupedses_black, 
                     segrtown_dissim_ethgroupedses_other,
                     segrtown_simpson2_ethgrouped, 
                     segrtown_avgsimpson2_ethgrouped,
                     segrtown_simpson2_ethgroupedses, 
                     segrtown_avgsimpson2_ethgroupedses,) %>% 
  pivot_longer(-year) %>% filter(!is.na(value)) %>% 
  mutate(base=word(name,3,3,sep="_"), 
         name = str_remove(name,"segrtown_") %>% str_remove("_ethgrouped") %>% str_remove("ses")) %>% 
  pivot_wider(names_from = c(base,year), values_from=value) %>% knitr::kable(digits=3)

towns %>% dplyr::select(LAD11NM, year, segrtown_avgsimpson2_ethgrouped, segrtown_simpson2_ethgrouped,segrtown_fraction_ethgrouped_asian,all) %>% 
  arrange(desc(segrtown_fraction_ethgrouped_asian)) %>% filter() %>% head(20)
towns %>% dplyr::select(LAD11NM, year, segrtown_avgsimpson2_ethgrouped, segrtown_simpson2_ethgrouped,segrtown_fraction_eth_asia_pakistani_11,all) %>% 
  arrange(desc(segrtown_fraction_eth_asia_pakistani_11)) %>% filter(year == 2011) %>% head(20)


towns %>% dplyr::select(LAD11NM, year, segrtown_avgsimpson_ethgrouped, segrtown_avgsimpson_eth) %>% 
  arrange(segrtown_avgsimpson_ethgrouped) %>% head(20)
towns %>% dplyr::select(LAD11NM, year, segrtown_simpson2_ethgrouped, segrtown_simpson2_eth) %>% 
  arrange(segrtown_simpson2_ethgrouped) %>% head(20)
towns %>% dplyr::select(LAD11NM, year, segrtown_fraction_ethgrouped_asian) %>% 
  arrange(desc(segrtown_fraction_ethgrouped_asian)) %>% head(20)


towns %>% dplyr::select(LAD11NM, year, segrtown_avgsimpson2_ethgrouped, segrtown_simpson2_ethgrouped) %>% 
  mutate(diff = segrtown_avgsimpson2_ethgrouped - segrtown_simpson2_ethgrouped) %>% 
  arrange(desc(diff)) %>% head(20)




## export LSOA of some towns including raw data to ESRI shapefile readable with NetLogo GIS extension
# for (TOWN in c("Southampton","Leeds", "Bradford", "Leicester", "Manchester", "Birmingham", "Brighton and Hove",
#                "Stoke-on-Trent", "Plymouth", "Derby", "Nottingham", "Newcastle upon Tyne","Leeds", "Sheffield",
#                "Coventry", "St Albans" , "Guildford", "Cambridge", "Middlesbrough", "Sunderland", "Oxford","Luton",
#                "Blackburn with Darwen", "Oldham","Wolverhampton","Slough","Walsall","Bristol, City of", "Liverpool", "Leeds", "Reading")) {
for (TOWN in towns11$LAD11NM) {
    if (!dir.exists(paste0("LAD_shp_NetLogo/",TOWN))) {
    town <- lsoa_2011 %>% filter(LAD11NM == TOWN) %>%
      dplyr::select(LSOA11CD, allvalidses_11, starts_with("ethgroupedses_"), 
             -ethgroupedses_whiteb, -ethgroupedses_asian, -ethgroupedses_black, -ethgroupedses_other)
    names(town) <- sub("ethgroupedses_","",names(town))
    town %>% st_write(dsn = paste0("LAD_shp_NetLogo/",TOWN), layer = TOWN, driver = "ESRI Shapefile", delete_dsn = TRUE)
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


# load("R/bradford_2001_2011")
# 
# bradford_2001 %>% ggplot(aes(fill = change_segrlsoa_simpson2_ethgrouped)) + geom_sf()
# bradford_2001 %>% ggplot(aes(fill = change_segrlsoa_fraction_ethgrouped_asian)) + geom_sf()
# bradford_2001 %>% ggplot(aes(fill = segrlsoa_simpson2_ethgrouped)) + geom_sf()
# bradford_2001 %>% ggplot(aes(fill = segrlsoa_fraction_ethgrouped_asian)) + geom_sf()
# 
# glm(change_segrlsoa_fraction_ethgrouped_asian ~ tenure_owned_01, data = bradford_2001)
# 
# 
# bradford_2001 %>% mutate(id=1:n()) %>% 
#   ggplot(aes(fill = change_segrlsoa_simpson2_ethgrouped, label = id)) + geom_sf() + 
#   geom_sf_text()
# 
# a <- st_intersects(bradford_2001)
