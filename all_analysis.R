# dir <- "D:\\PhD\\Projects\\CZ, JL, RP, SS and ARS - segregation\\css2019-master\\R"
# setwd(dir)
library(rgdal)
library(spdep)
library(ggplot2)
library(dplyr)
library(CARBayes)
library(tmap)
library(spatialreg)


lsoa_2001_lsoa_2011 <- load("R/lsoa_2001_lsoa_2011")

# SHAPE files
bradford <- readOGR(dsn = "shp_NetLogo/Bradford/", layer = "Bradford", stringsAsFactors=F)
bradford@data <- bradford@data %>% rename(LSOA11CD=LSOA11C)
bradford@data <- left_join(bradford@data, lsoa_2001, by = (GEOID = "LSOA11CD"))

#computing the shares for the different variables
bradford@data <- bradford@data %>% 
  mutate(housdepri_4d_01_p = (housdepri_4d_01/(housdepri_no_01 + housdepri_1d_01 + housdepri_2d_01 + housdepri_3d_01 + housdepri_4d_01))*100,
         housdepri_3d_01_p = (housdepri_3d_01/(housdepri_no_01 + housdepri_1d_01 + housdepri_2d_01 + housdepri_3d_01 + housdepri_4d_01))*100,
         housdepri_2d_01_p = (housdepri_2d_01/(housdepri_no_01 + housdepri_1d_01 + housdepri_2d_01 + housdepri_3d_01 + housdepri_4d_01))*100,
         housdepri_1d_01_p = (housdepri_1d_01/(housdepri_no_01 + housdepri_1d_01 + housdepri_2d_01 + housdepri_3d_01 + housdepri_4d_01))*100,
         housdepri_no_01_p = (housdepri_no_01/(housdepri_no_01 + housdepri_1d_01 + housdepri_2d_01 + housdepri_3d_01 + housdepri_4d_01))*100,
         tenure_socialh_p = (tenure_socialrent_01 + tenure_houseasoc_01)/total_tenure_01*100,
         tenure_socialrent_01_p = tenure_socialrent_01/total_tenure_01*100,
         tenure_private_01_p = tenure_private_01/total_tenure_01*100,
         owned_hh_01_p = (tenure_owned_01/total_tenure_01)*100,
         rented_hh_01_p = (tenure_rentfree_01/total_tenure_01)*100,
         socialhousing_hh_01_p = (tenure_socialrent_01/total_tenure_01)*100,
         private_01_p = (tenure_private_01/total_tenure_01)*100,
         compten_01_p = (total_compten_01/total_tenure_01)*100,
         quali_4n5_01_p = (quali_4n5_01/(quali_no_01 + quali_1_01 + quali_2_01 + quali_3_01 + quali_4n5_01 + quali_other_01))*100,
         quali_3_01_p = (quali_3_01/(quali_no_01 + quali_1_01 + quali_2_01 + quali_3_01 + quali_4n5_01 + quali_other_01))*100,
         quali_2_01_p = (quali_2_01/(quali_no_01 + quali_1_01 + quali_2_01 + quali_3_01 + quali_4n5_01 + quali_other_01))*100,
         quali_1_01_p = (quali_1_01/(quali_no_01 + quali_1_01 + quali_2_01 + quali_3_01 + quali_4n5_01 + quali_other_01))*100,
         quali_no_01_p = (quali_no_01/(quali_no_01 + quali_1_01 + quali_2_01 + quali_3_01 + quali_4n5_01 + quali_other_01))*100,
         age_0_to_4_01_p = (age_0_to_4_01/all_01)*100,
         single_01_p = (marstat_sing_01/all_01)*100,
         divorcedsep_01_p = ((marstat_sep_01+marstat_div_01)/all_01)*100,
         married_01_p = ((marstat_marr_01)/all1674_01)*100,
         nssec1_1_p = ((nssec1_1)/all1674_01)*100,
         nssec1_2_p = ((nssec1_2)/all1674_01)*100,
         nssec2_p = ((nssec2)/all1674_01)*100,
         nssec3_p = ((nssec3)/all1674_01)*100,
         nssec4_p = ((nssec4)/all1674_01)*100,
         nssec6_p = ((nssec6)/all1674_01)*100,
         nssec7_p = ((nssec7)/all1674_01)*100,
         high2_p = high/(high+mid+low)*100,
         mid2_p = mid/(high+mid+low)*100,
         low2_p = low/(high+mid+low)*100,
         highses_01_p = ((nssec1_1+nssec1_2)/all1674_01)*100,
         lowses_01_p = ((nssec5+nssec6+nssec7)/all1674_01)*100,
         partemp_01_p = (partemp/all1674_01)*100,
         fullemp_01_p = (fullemp/all1674_01)*100,
         selfemp_01_p = (selfemp/all1674_01)*100,
         unemp_01_p = (unemp/all1674_01)*100,
         activestud_01_p = (activestud/all1674_01)*100,
         retired_01_p = (retired/all1674_01)*100,
         ictivestud_01_p = (ictivestud/all1674_01)*100,
         caring_01_p = (caring/all1674_01)*100,
         sick_01_p = (sick/all1674_01)*100,
         otherict_01_p = (otherict/all1674_01)*100,
         unemp1624_01_p = (unemp1624/all1674_01)*100,
         unemp50plus_01_p = (unemp50plus/all1674_01)*100,
         neverw2_01_p = (neverw2/all1674_01)*100,
         ltunemp2_01_p = (ltunemp2/all1674_01)*100,
         familynochild_01_p = (hhcompten64/total_house_01)*100,
         familydepchild_01_p = (hhcompten73/total_house_01)*100,
         familynodepchild_01_p = (hhcompten82/total_house_01)*100,
         loneparentsdepchild_01_p = (hhcompten100/total_house_01)*100,
         loneparentsnodepchild_01_p = (hhcompten109/total_house_01)*100,
         otherhh_01_p = (hhcompten127/total_house_01)*100,
         students_01_p = (hhcompten136/total_house_01)*100,
         hhcompten19_p  =  hhcompten19/total_house_01*100,
         hhcompten46_p  =  hhcompten46/total_house_01*100,
         hhcompten73_p  =  hhcompten73/total_house_01*100,
         hhcompten100_p = hhcompten100/total_house_01*100,
         hhcompten136_p = hhcompten136/total_house_01*100,
         nonwhitemig_01_p = (migr11/migr4)*100,
         whitemig_01_p = 100 - (migr11/migr4)*100,
         accom02_p = accom02/total_accom_01*100,
         accom13_p = accom13/total_accom_01*100,
         accom29_p = accom29/total_accom_01*100,
         accom33_p = accom33/total_accom_01*100,
         vacanthouses_01_p = (accom04/total_accom_01)*100,
         detachedhouse_01_p = (accom13/total_accom_01)*100,
         semidetachedhouse_01_p = (accom17/total_accom_01)*100,
         terrace_01_p = (accom21/total_accom_01)*100,
         flatinblock_01_p = (accom29/total_accom_01)*100,
         flatinshared_01_p = (accom33/total_accom_01)*100,
         flatincommercial_01_p = (accom37/total_accom_01)*100,
         caravan_01_p = (accom41/total_accom_01)*100,
         shareddwelling_01_p = (accom45/total_accom_01)*100,
         turnover_01=(migr3+migr4)/all_01*100, # arrivals as a share of total
         netmig_01=all_01/(all_01+migr7-migr3-migr4), # net migration
         netmignw_01=all_01/(all_01+migr14-migr10-migr11), 
         netmigw_01=all_01/(all_01+migr21-migr17-migr18)) %>% 
  mutate(nonwhitemig_01_p=ifelse(is.na(nonwhitemig_01_p), 0, nonwhitemig_01_p),
         whitemig_01_p=ifelse(is.na(whitemig_01_p), 0, whitemig_01_p))

#Create neighborhood matrix in different formats for different functions
W.nb.con <- poly2nb(bradford, row.names = rownames(bradford@data), snap=0.005)
W_mat_con <- nb2mat(W.nb.con, style="B", zero.policy=TRUE)
W.list.con <- nb2listw(W.nb.con, style="B", zero.policy = TRUE)
# no_nb <- which(rowSums(W_mat_con) == 0)
# nonb_W <- W_mat_con[-no_nb, -no_nb]
# bradford <- bradford[-no_nb,]
#######bradford <- bradford
# nonb_Wnb <- poly2nb(bradford, row.names = rownames(bradford@data))
# W.mat_bradford <- nb2mat(nonb_Wnb, style="B")
# W.list_bradford <- nb2listw(nonb_Wnb, style="B")
W.mat_bradford <- W_mat_con
W.list_bradford <- W.list.con

# #formulas
# frml1 <- segrlsoa_simpson_ethgrouped ~ 
#   change_price_00_gr +
#   housdepri_4d_01_p + 
#   owned_hh_01_p + rented_hh_01_p + socialhousing_hh_01_p + 
#   quali_4n5_01_p + quali_no_01_p + 
#   age_0_to_4_01_p + single_01_p + divorcedsep_01_p + married_01_p + 
#   nssec1_1_p + nssec7_p + 
#   partemp_01_p  + fullemp_01_p + selfemp_01_p + retired_01_p + unemp1624_01_p + unemp50plus_01_p  + neverw2_01_p + ltunemp2_01_p + 
#   nonwhitemig_01_p +
#   vacanthouses_01_p + detachedhouse_01_p + flatinshared_01_p + shareddwelling_01_p +
#   familydepchild_01_p + loneparentsdepchild_01_p + students_01_p
# frml1_ch <- change_segrlsoa_simpson_ethgrouped ~ 
#   change_price_00_gr +
#   housdepri_4d_01_p + 
#   owned_hh_01_p + rented_hh_01_p + socialhousing_hh_01_p + 
#   quali_4n5_01_p + quali_no_01_p + 
#   age_0_to_4_01_p + single_01_p + divorcedsep_01_p + married_01_p + 
#   nssec1_1_p + nssec7_p + 
#   partemp_01_p  + fullemp_01_p + selfemp_01_p + retired_01_p + unemp1624_01_p + unemp50plus_01_p  + neverw2_01_p + ltunemp2_01_p + 
#   nonwhitemig_01_p +
#   vacanthouses_01_p + detachedhouse_01_p + flatinshared_01_p + shareddwelling_01_p +
#   familydepchild_01_p + loneparentsdepchild_01_p + students_01_p
# 
# frml2 <- segrlsoa_fraction_ethgrouped_asian ~ 
#   change_price_00_gr +
#   housdepri_4d_01_p + 
#   owned_hh_01_p + rented_hh_01_p + socialhousing_hh_01_p + 
#   quali_4n5_01_p + quali_no_01_p + 
#   age_0_to_4_01_p + single_01_p + divorcedsep_01_p + married_01_p + 
#   nssec1_1_p + nssec7_p + 
#   partemp_01_p  + fullemp_01_p + selfemp_01_p + retired_01_p + unemp1624_01_p + unemp50plus_01_p  + neverw2_01_p + ltunemp2_01_p + 
#   nonwhitemig_01_p + 
#   vacanthouses_01_p + detachedhouse_01_p + flatinshared_01_p + shareddwelling_01_p + 
#   familydepchild_01_p + loneparentsdepchild_01_p + students_01_p
# 
#   frml2_ch <- change_segrlsoa_fraction_ethgrouped_asian ~ 
#   change_price_00_gr +
#   housdepri_4d_01_p + 
#   owned_hh_01_p + rented_hh_01_p + socialhousing_hh_01_p + 
#   quali_4n5_01_p + quali_no_01_p + 
#   age_0_to_4_01_p + single_01_p + divorcedsep_01_p + married_01_p + 
#   nssec1_1_p + nssec7_p + 
#   partemp_01_p  + fullemp_01_p + selfemp_01_p + retired_01_p + unemp1624_01_p + unemp50plus_01_p  + neverw2_01_p + ltunemp2_01_p + 
#   nonwhitemig_01_p + 
#   vacanthouses_01_p + detachedhouse_01_p + flatinshared_01_p + shareddwelling_01_p + 
#   familydepchild_01_p + loneparentsdepchild_01_p + students_01_p
# 
# frml3 <- segrlsoa_fraction_ethgrouped_black ~ 
#   change_price_00_gr +
#   housdepri_4d_01_p + 
#   owned_hh_01_p + rented_hh_01_p + socialhousing_hh_01_p + 
#   quali_4n5_01_p + quali_no_01_p + 
#   age_0_to_4_01_p + single_01_p + divorcedsep_01_p + married_01_p + 
#   nssec1_1_p + nssec7_p + 
#   partemp_01_p  + fullemp_01_p + selfemp_01_p + retired_01_p + unemp1624_01_p + unemp50plus_01_p + neverw2_01_p + ltunemp2_01_p + 
#   nonwhitemig_01_p + 
#   vacanthouses_01_p + detachedhouse_01_p + flatinshared_01_p + shareddwelling_01_p + 
#   familydepchild_01_p + loneparentsdepchild_01_p + students_01_p
# 
# frml4 <- segrlsoa_fraction_ethgrouped_whiteb ~ 
#   change_price_00_gr +
#   housdepri_4d_01_p + 
#   owned_hh_01_p + rented_hh_01_p + socialhousing_hh_01_p + 
#   quali_4n5_01_p + quali_no_01_p + 
#   age_0_to_4_01_p + single_01_p + divorcedsep_01_p + married_01_p + 
#   nssec1_1_p + nssec7_p + 
#   partemp_01_p  + fullemp_01_p + selfemp_01_p + retired_01_p + unemp1624_01_p + unemp50plus_01_p  + neverw2_01_p + ltunemp2_01_p + 
#   nonwhitemig_01_p + 
#   vacanthouses_01_p + detachedhouse_01_p + flatinshared_01_p + shareddwelling_01_p + 
#   familydepchild_01_p + loneparentsdepchild_01_p + students_01_p
# 
# frml4_ch <- change_segrlsoa_fraction_ethgrouped_whiteb ~ 
#   change_price_00_gr +
#   housdepri_4d_01_p + 
#   owned_hh_01_p + rented_hh_01_p + socialhousing_hh_01_p + 
#   quali_4n5_01_p + quali_no_01_p + 
#   age_0_to_4_01_p + single_01_p + divorcedsep_01_p + married_01_p + 
#   nssec1_1_p + nssec7_p + 
#   partemp_01_p  + fullemp_01_p + selfemp_01_p + retired_01_p + unemp1624_01_p + unemp50plus_01_p  + neverw2_01_p + ltunemp2_01_p + 
#   nonwhitemig_01_p + 
#   vacanthouses_01_p + detachedhouse_01_p + flatinshared_01_p + shareddwelling_01_p + 
#   familydepchild_01_p + loneparentsdepchild_01_p + students_01_p

# New formulas 
predictor_str <- "high2_p + mid2_p + # NSSEC 
  netmignw_01 + netmigw_01 + # Migration
  age_0_to_4_01_p + # Fertility
  quali_1_01_p + quali_2_01_p + # Education
  partemp_01_p + unemp1624_01_p + unemp50plus_01_p + # Activity
  housdepri_1d_01_p + # House deprivation
  tenure_socialh_p + tenure_private_01_p + # Tenure
  accom02_p + # Vacant houses
  hhcompten19_p + hhcompten46_p + hhcompten73_p + hhcompten100_p + hhcompten136_p + # Houshold compostion
  accom13_p + accom29_p + accom33_p # Type of housing"
frml1 <- as.formula(paste("segrlsoa_simpson2_ethgrouped ~", predictor_str))
frml2 <- as.formula(paste("segrlsoa_fraction_ethgrouped_asian ~", predictor_str))
frml4 <- as.formula(paste("segrlsoa_fraction_ethgrouped_whiteb ~", predictor_str))
frml1_ch <- as.formula(paste("change_segrlsoa_simpson2_ethgrouped ~", predictor_str))
frml2_ch <- as.formula(paste("change_segrlsoa_fraction_ethgrouped_asian ~", predictor_str))
frml4_ch <- as.formula(paste("change_segrlsoa_fraction_ethgrouped_whiteb ~", predictor_str))

lm.mod1 <- lm(formula=frml1, data=bradford@data)
lm.mod1_ch <- lm(formula=frml1_ch, data=bradford@data)
lm.mod2 <- lm(formula=frml2, data=bradford@data)
lm.mod2_ch <- lm(formula=frml2_ch, data=bradford@data)
lm.mod4 <- lm(formula=frml4, data=bradford@data)
lm.mod4_ch <- lm(formula=frml4_ch, data=bradford@data)
lagsarlm.mod1 <-    lagsarlm(formula=frml1   , data=bradford@data, listw = W.list_bradford)
lagsarlm.mod1_ch <- lagsarlm(formula=frml1_ch, data=bradford@data, listw = W.list_bradford)
lagsarlm.mod2 <-    lagsarlm(formula=frml2   , data=bradford@data, listw = W.list_bradford)
lagsarlm.mod2_ch <- lagsarlm(formula=frml2_ch, data=bradford@data, listw = W.list_bradford)
lagsarlm.mod4 <-    lagsarlm(formula=frml4   , data=bradford@data, listw = W.list_bradford)
lagsarlm.mod4_ch <- lagsarlm(formula=frml4_ch, data=bradford@data, listw = W.list_bradford)
resid.lm.mod1 <- residuals(lm.mod1)
resid.lm.mod1_ch <- residuals(lm.mod1_ch)
resid.lm.mod2 <- residuals(lm.mod2)
resid.lm.mod2_ch <- residuals(lm.mod2_ch)
resid.lm.mod4 <- residuals(lm.mod4)
resid.lm.mod4_ch <- residuals(lm.mod4_ch)
resid.lagsarlm.mod1 <-    residuals(lagsarlm.mod1)
resid.lagsarlm.mod1_ch <- residuals(lagsarlm.mod1_ch)
resid.lagsarlm.mod2 <-    residuals(lagsarlm.mod2)
resid.lagsarlm.mod2_ch <- residuals(lagsarlm.mod2_ch)
resid.lagsarlm.mod4 <-    residuals(lagsarlm.mod4)
resid.lagsarlm.mod4_ch <- residuals(lagsarlm.mod4_ch)

#moran's I
moran1 <- moran.mc(x=resid.lm.mod1, listw=W.list_bradford, nsim=10000)
moran2 <- moran.mc(x=resid.lm.mod2, listw=W.list_bradford, nsim=10000)
moran4 <- moran.mc(x=resid.lm.mod4, listw=W.list_bradford, nsim=10000)
moran1_ch <- moran.mc(x=resid.lm.mod1_ch, listw=W.list_bradford, nsim=10000)
moran2_ch <- moran.mc(x=resid.lm.mod2_ch, listw=W.list_bradford, nsim=10000)
moran4_ch <- moran.mc(x=resid.lm.mod4_ch, listw=W.list_bradford, nsim=10000)
moran1.test <- moran.test(x=resid.lm.mod1, listw=W.list_bradford)
moran2.test <- moran.test(x=resid.lm.mod2, listw=W.list_bradford)
moran4.test <- moran.test(x=resid.lm.mod4, listw=W.list_bradford)
moran1_ch.test <- moran.test(x=resid.lm.mod1_ch, listw=W.list_bradford)
moran2_ch.test <- moran.test(x=resid.lm.mod2_ch, listw=W.list_bradford)
moran4_ch.test <- moran.test(x=resid.lm.mod4_ch, listw=W.list_bradford)
spmoran1    <- moran.mc(x=resid.lagsarlm.mod1,    listw=W.list_bradford, nsim=10000)
spmoran2    <- moran.mc(x=resid.lagsarlm.mod2,    listw=W.list_bradford, nsim=10000)
spmoran4    <- moran.mc(x=resid.lagsarlm.mod4,    listw=W.list_bradford, nsim=10000)
spmoran1_ch <- moran.mc(x=resid.lagsarlm.mod1_ch, listw=W.list_bradford, nsim=10000)
spmoran2_ch <- moran.mc(x=resid.lagsarlm.mod2_ch, listw=W.list_bradford, nsim=10000)
spmoran4_ch <- moran.mc(x=resid.lagsarlm.mod4_ch, listw=W.list_bradford, nsim=10000)
spmoran1.test <- moran.test(x=resid.lagsarlm.mod1, listw=W.list_bradford)
spmoran2.test <- moran.test(x=resid.lagsarlm.mod2, listw=W.list_bradford)
spmoran4.test <- moran.test(x=resid.lagsarlm.mod4, listw=W.list_bradford)
spmoran1_ch.test <- moran.test(x=resid.lagsarlm.mod1_ch, listw=W.list_bradford)
spmoran2_ch.test <- moran.test(x=resid.lagsarlm.mod2_ch, listw=W.list_bradford)
spmoran4_ch.test <- moran.test(x=resid.lagsarlm.mod4_ch, listw=W.list_bradford)

moransI <-  data.frame(
  index=c("Local Simpson","Fraction Asians", "Fraction White British"),
  statistic=c(moran1$statistic, 
              moran2$statistic, 
              moran4$statistic),
  p_value=c(moran1$p.value, 
            moran2$p.value, 
            moran4$p.value))
moransI_ch <-  data.frame(
  index=c("Local Simpson","Fraction Asians", "Fraction White British"),
  statistic=c(moran1_ch$statistic, 
              moran2_ch$statistic, 
              moran4_ch$statistic),
  p_value=c(moran1_ch$p.value, 
            moran2_ch$p.value, 
            moran4_ch$p.value))
spmoransI <-  data.frame(
  index=c("Local Simpson","Fraction Asians", "Fraction White British"),
  statistic=c(spmoran1$statistic, 
              spmoran2$statistic, 
              spmoran4$statistic),
  p_value=c(spmoran1$p.value, 
            spmoran2$p.value, 
            spmoran4$p.value))
spmoransI_ch <-  data.frame(
  index=c("Local Simpson","Fraction Asians", "Fraction White British"),
  statistic=c(spmoran1_ch$statistic, 
              spmoran2_ch$statistic, 
              spmoran4_ch$statistic),
  p_value=c(spmoran1_ch$p.value, 
            spmoran2_ch$p.value, 
            spmoran4_ch$p.value))
moransI.test <-  data.frame(
  index=c("Local Simpson","Fraction Asians", "Fraction White British"),
  statistic=c(moran1.test$estimate[1], 
              moran2.test$estimate[1], 
              moran4.test$estimate[1]),
  p_value=c(moran1.test$p.value, 
            moran2.test$p.value, 
            moran4.test$p.value))
moransI_ch.test <-  data.frame(
  index=c("Local Simpson","Fraction Asians", "Fraction White British"),
  statistic=c(moran1_ch.test$estimate[1], 
              moran2_ch.test$estimate[1], 
              moran4_ch.test$estimate[1]),
  p_value=c(moran1_ch.test$p.value, 
            moran2_ch.test$p.value, 
            moran4_ch.test$p.value))
spmoransI.test <-  data.frame(
  index=c("Local Simpson","Fraction Asians", "Fraction White British"),
  statistic=c(spmoran1.test$estimate[1], 
              spmoran2.test$estimate[1], 
              spmoran4.test$estimate[1]),
  p_value=c(spmoran1.test$p.value, 
            spmoran2.test$p.value, 
            spmoran4.test$p.value))
spmoransI_ch.test <-  data.frame(
  index=c("Local Simpson","Fraction Asians", "Fraction White British"),
  statistic=c(spmoran1_ch.test$estimate[1], 
              spmoran2_ch.test$estimate[1], 
              spmoran4_ch.test$estimate[1]),
  p_value=c(spmoran1_ch.test$p.value, 
            spmoran2_ch.test$p.value, 
            spmoran4_ch.test$p.value))

print(summary(lm.mod1), signif.stars = TRUE)
print(summary(lm.mod2), signif.stars = TRUE)
print(summary(lm.mod4), signif.stars = TRUE)
moransI
print(summary(lm.mod1_ch), signif.stars = TRUE)
print(summary(lm.mod2_ch), signif.stars = TRUE)
print(summary(lm.mod4_ch), signif.stars = TRUE)
moransI_ch
print(summary(lagsarlm.mod1), signif.stars = TRUE)
print(summary(lagsarlm.mod2), signif.stars = TRUE)
print(summary(lagsarlm.mod4), signif.stars = TRUE)
spmoransI
print(summary(lagsarlm.mod1_ch), signif.stars = TRUE)
print(summary(lagsarlm.mod2_ch), signif.stars = TRUE)
print(summary(lagsarlm.mod4_ch), signif.stars = TRUE)
spmoransI_ch

plot(bradford@data$partemp_01_p,bradford@data$change_segrlsoa_simpson2_ethgrouped)
plot(bradford@data$partemp_01_p,bradford@data$segrlsoa_simpson2_ethgrouped)
plot(bradford@data$netmignw_01,bradford@data$change_segrlsoa_fraction_ethgrouped_asian)
plot(bradford@data$netmignw_01,bradford@data$segrlsoa_fraction_ethgrouped_asian)



#Bayesian CARleroux spatial models

model.spatial1 <- S.CARleroux(formula=frml1, data=bradford@data,
                              family="gaussian",W=W.mat_bradford,
                              burnin=20000,n.sample=100000,thin=10)
model.spatial2 <- S.CARleroux(formula=frml2, data=bradford@data,
                              family="gaussian",W=W.mat_bradford,
                              burnin=20000,n.sample=100000,thin=10)
model.spatial2_ch <- S.CARleroux(formula=frml2_ch, data=bradford@data,
                                 family="gaussian",W=W.mat_bradford,
                                 burnin=20000,n.sample=100000,thin=10)
model.spatial3 <- S.CARleroux(formula=frml3, data=bradford@data,
                              family="gaussian",W=W.mat_bradford,
                              burnin=20000,n.sample=100000,thin=10)
model.spatial4 <- S.CARleroux(formula=frml4, data=bradford@data,
                              family="gaussian",W=W.mat_bradford,
                              burnin=20000,n.sample=100000,thin=10)
#models
m_bradford_new <- cbind(model.spatial1$summary.results[,1:3],
                    model.spatial2$summary.results[,1:3], 
                    model.spatial3$summary.results[,1:3],
                    model.spatial4$summary.results[,1:3])
saveRDS(m_bradford_new, file="D:\\PhD\\Projects\\CZ, JL, RP, SS and ARS - segregation\\Abstract\\m_bradford_new.RDS")

#store results of models back into the map
model.fitted_bradford01 <- data.frame(LSOA11CD = names(table(bradford@data$LSOA11CD)), 
                                      predicted_LS_simpson_01=fitted(model.spatial1),
                                      predicted_LQ_asians_01 = fitted(model.spatial2),
                                      predicted_LQ_blacks_01 = fitted(model.spatial3),
                                      predicted_LQ_whiteb_01 = fitted(model.spatial4))
model.fitted_bradford01 <- model.fitted_bradford01 %>% mutate(LSOA11CD=as.character(LSOA11CD))
bradford@data <- left_join(bradford@data, model.fitted_bradford01, by = (GEOID = "LSOA11CD"))

#save predictions
preds_new <- bradford@data
saveRDS(preds_new, file="D:\\PhD\\Projects\\CZ, JL, RP, SS and ARS - segregation\\Abstract\\preds_new.RDS")


vars <- c("change_segrlsoa_simpson_ethgrouped",        
          "change_segrlsoa_simpson_eth",               
          "change_segrlsoa_simpson2_ethgrouped",       
          "change_segrlsoa_simpson2_eth",              
          "change_segrlsoa_fraction_ethgrouped_whiteb",
          "change_segrlsoa_fraction_ethgrouped_asian", 
          "change_segrlsoa_fraction_ethgrouped_black", 
          "change_segrlsoa_fraction_ethgrouped_other", 
          "segrlsoa_simpson_ethgrouped",               
          "segrlsoa_simpson_eth",                      
          "segrlsoa_simpson2_ethgrouped",              
          "segrlsoa_simpson2_eth",                     
          "segrlsoa_fraction_ethgrouped_whiteb",       
          "segrlsoa_fraction_ethgrouped_asian",        
          "segrlsoa_fraction_ethgrouped_black",        
          "segrlsoa_fraction_ethgrouped_other",        
          "segrlsoa_fraction_eth_white_brit_01",       
          "segrlsoa_fraction_eth_white_irish_01",      
          "segrlsoa_fraction_eth_white_other_01",      
          "segrlsoa_fraction_eth_mix_carib_01",        
          "segrlsoa_fraction_eth_mix_africa_01",       
          "segrlsoa_fraction_eth_mix_asia_01",         
          "segrlsoa_fraction_eth_mix_other_01",        
          "segrlsoa_fraction_eth_asia_indian_01",      
          "segrlsoa_fraction_eth_asia_pakistani_01",   
          "segrlsoa_fraction_eth_asia_bangla_01",      
          "segrlsoa_fraction_eth_asia_chi_01",         
          "segrlsoa_fraction_eth_asia_other_01",       
          "segrlsoa_fraction_eth_black_caribbean_01",  
          "segrlsoa_fraction_eth_black_african_01",   
          "segrlsoa_fraction_eth_black_other_01",      
          "segrlsoa_fraction_eth_other_01")


#LISA
pval <- 0.05

#local simpson
locm01 <- localmoran(bradford$segrlsoa_simpson_ethgrouped, W.list_bradford)  #calculate the local moran's I
summary(locm01)
# manually make a moran plot standarize variables
bradford$ssegrlsoa_simpson_ethgrouped <- scale(bradford$segrlsoa_simpson_ethgrouped)  #save to a new column
# create a lagged variable
bradford$lag_ssegrlsoa_simpson_ethgrouped <- lag.listw(W.list_bradford, bradford$ssegrlsoa_simpson_ethgrouped)
plot(x = bradford$ssegrlsoa_simpson_ethgrouped, y = bradford$lag_ssegrlsoa_simpson_ethgrouped, main = " Moran Scatterplot PPOV", ylim=c(-10,10), xlim=c(-2,2))
abline(h = 0, v = 0)
abline(lm(bradford$lag_ssegrlsoa_simpson_ethgrouped ~ bradford$ssegrlsoa_simpson_ethgrouped), lty = 3, lwd = 4, col = "red")

# identify the moran plot quadrant for each observation
bradford$quad_sig_ls <- NA
bradford@data[(bradford$ssegrlsoa_simpson_ethgrouped >= 0 & bradford$lag_ssegrlsoa_simpson_ethgrouped >= 0) & (locm01[, 5] <= pval), "quad_sig_ls"] <- 1
bradford@data[(bradford$ssegrlsoa_simpson_ethgrouped <= 0 & bradford$lag_ssegrlsoa_simpson_ethgrouped <= 0) & (locm01[, 5] <= pval), "quad_sig_ls"] <- 2
bradford@data[(bradford$ssegrlsoa_simpson_ethgrouped >= 0 & bradford$lag_ssegrlsoa_simpson_ethgrouped <= 0) & (locm01[, 5] <= pval), "quad_sig_ls"] <- 3
bradford@data[(bradford$ssegrlsoa_simpson_ethgrouped >= 0 & bradford$lag_ssegrlsoa_simpson_ethgrouped <= 0) & (locm01[, 5] <= pval), "quad_sig_ls"] <- 4
bradford@data[(bradford$ssegrlsoa_simpson_ethgrouped <= 0 & bradford$lag_ssegrlsoa_simpson_ethgrouped >= 0) & (locm01[, 5] <= pval), "quad_sig_ls"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

#fraction asians
locm01 <- localmoran(bradford$segrlsoa_fraction_ethgrouped_asian, W.list_bradford)  #calculate the local moran's I
summary(locm01)
# manually make a moran plot standarize variables
bradford$ssegrlsoa_fraction_ethgrouped_asian <- scale(bradford$segrlsoa_fraction_ethgrouped_asian)  #save to a new column
# create a lagged variable
bradford$lag_ssegrlsoa_fraction_ethgrouped_asian <- lag.listw(W.list_bradford, bradford$ssegrlsoa_fraction_ethgrouped_asian)
plot(x = bradford$ssegrlsoa_fraction_ethgrouped_asian, y = bradford$lag_ssegrlsoa_fraction_ethgrouped_asian, main = " Moran Scatterplot PPOV", ylim=c(-10,10), xlim=c(-2,2))
abline(h = 0, v = 0)
abline(lm(bradford$lag_ssegrlsoa_fraction_ethgrouped_asian ~ bradford$ssegrlsoa_fraction_ethgrouped_asian), lty = 3, lwd = 4, col = "red")

# identify the moran plot quadrant for each observation
bradford$quad_sig_lqa <- NA
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_asian >= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_asian >= 0) & (locm01[, 5] <= pval), "quad_sig_lqa"] <- 1
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_asian <= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_asian <= 0) & (locm01[, 5] <= pval), "quad_sig_lqa"] <- 2
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_asian >= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_asian <= 0) & (locm01[, 5] <= pval), "quad_sig_lqa"] <- 3
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_asian >= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_asian <= 0) & (locm01[, 5] <= pval), "quad_sig_lqa"] <- 4
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_asian <= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_asian >= 0) & (locm01[, 5] <= pval), "quad_sig_lqa"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

#fraction blacks
locm01 <- localmoran(bradford$segrlsoa_fraction_ethgrouped_black, W.list_bradford)  #calculate the local moran's I
summary(locm01)
# manually make a moran plot standarize variables
bradford$ssegrlsoa_fraction_ethgrouped_black <- scale(bradford$segrlsoa_fraction_ethgrouped_black)  #save to a new column
# create a lagged variable
bradford$lag_ssegrlsoa_fraction_ethgrouped_black <- lag.listw(W.list_bradford, bradford$ssegrlsoa_fraction_ethgrouped_black)
plot(x = bradford$ssegrlsoa_fraction_ethgrouped_black, y = bradford$lag_ssegrlsoa_fraction_ethgrouped_black, main = " Moran Scatterplot PPOV", ylim=c(-10,10), xlim=c(-2,2))
abline(h = 0, v = 0)
abline(lm(bradford$lag_ssegrlsoa_fraction_ethgrouped_black ~ bradford$ssegrlsoa_fraction_ethgrouped_black), lty = 3, lwd = 4, col = "red")

# identify the moran plot quadrant for each observation
bradford$quad_sig_lqb <- NA
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_black >= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_black >= 0) & (locm01[, 5] <= pval), "quad_sig_lqb"] <- 1
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_black <= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_black <= 0) & (locm01[, 5] <= pval), "quad_sig_lqb"] <- 2
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_black >= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_black <= 0) & (locm01[, 5] <= pval), "quad_sig_lqb"] <- 3
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_black >= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_black <= 0) & (locm01[, 5] <= pval), "quad_sig_lqb"] <- 4
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_black <= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_black >= 0) & (locm01[, 5] <= pval), "quad_sig_lqb"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

#fraction whites
locm01 <- localmoran(bradford$segrlsoa_fraction_ethgrouped_whiteb, W.list_bradford)  #calculate the local moran's I
summary(locm01)
# manually make a moran plot standarize variables
bradford$ssegrlsoa_fraction_ethgrouped_whiteb <- scale(bradford$segrlsoa_fraction_ethgrouped_whiteb)  #save to a new column
# create a lagged variable
bradford$lag_ssegrlsoa_fraction_ethgrouped_whiteb <- lag.listw(W.list_bradford, bradford$ssegrlsoa_fraction_ethgrouped_whiteb)
plot(x = bradford$ssegrlsoa_fraction_ethgrouped_whiteb, y = bradford$lag_ssegrlsoa_fraction_ethgrouped_whiteb, main = " Moran Scatterplot PPOV", ylim=c(-10,10), xlim=c(-2,2))
abline(h = 0, v = 0)
abline(lm(bradford$lag_ssegrlsoa_fraction_ethgrouped_whiteb ~ bradford$ssegrlsoa_fraction_ethgrouped_whiteb), lty = 3, lwd = 4, col = "red")

# identify the moran plot quadrant for each observation
bradford$quad_sig_lqw <- NA
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_whiteb >= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_whiteb >= 0) & (locm01[, 5] <= pval), "quad_sig_lqw"] <- 1
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_whiteb <= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_whiteb <= 0) & (locm01[, 5] <= pval), "quad_sig_lqw"] <- 2
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_whiteb >= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_whiteb <= 0) & (locm01[, 5] <= pval), "quad_sig_lqw"] <- 3
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_whiteb >= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_whiteb <= 0) & (locm01[, 5] <= pval), "quad_sig_lqw"] <- 4
bradford@data[(bradford$ssegrlsoa_fraction_ethgrouped_whiteb <= 0 & bradford$lag_ssegrlsoa_fraction_ethgrouped_whiteb >= 0) & (locm01[, 5] <= pval), "quad_sig_lqw"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
# see ?findInterval - This is necessary for making a map
bradford@data$np01_ls <- as.character(findInterval(bradford$quad_sig_ls, breaks))
bradford@data$np01_lqa <- as.character(findInterval(bradford$quad_sig_lqa, breaks))
bradford@data$np01_lqb <- as.character(findInterval(bradford$quad_sig_lqb, breaks))
bradford@data$np01_lqw <- as.character(findInterval(bradford$quad_sig_lqw, breaks))
colors <- c("red", "blue", "lightpink", "skyblue2", "white")

pdf("lisa_bradford.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) +  
  tm_fill(col="np01_ls",
          colorNA = "gray89",
          textNA = "n.s.",
          style="cat",
          palette=colors,
          labels=c("High-High","Low-Low", "n.s."),
          title = "",
          legend.show = FALSE) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top')) +
  tm_credits('LISA Local Simpson 2011', position = 'right') + 
  tm_scale_bar(color.dark = "gray60", # Customize scale bar and north arrow
               position = c(0.005, 0.005),
               lwd=0.5) +  # set position of the scale bar
  tm_compass(type = "4star", 
             size = 1.5, # set size of the compass
             text.size = 0.5, # set font size of the compass
             color.dark = "gray60", # color the compass
             text.color = "gray60", # color the text of the compass
             position = c(0.1, 0.9))
dev.off()

pdf("fractionasian_bradford.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) +  
  tm_fill(col="np01_lqa",
          colorNA = "gray89",
          textNA = "n.s.",
          style="cat",
          palette=colors,
          labels=c("High-High","Low-Low", "n.s."),
          title = "",
          legend.show = FALSE) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top')) +
  tm_credits('LISA Fraction South Asians 2011', position = 'right') + 
  tm_scale_bar(color.dark = "gray60", # Customize scale bar and north arrow
               position = c(0.005, 0.005),
               lwd=0.5) +  # set position of the scale bar
  tm_compass(type = "4star", 
             size = 1.5, # set size of the compass
             text.size = 0.5, # set font size of the compass
             color.dark = "gray60", # color the compass
             text.color = "gray60", # color the text of the compass
             position = c(0.1, 0.9))
dev.off()

pdf("fractionblack_bradford.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) +  
  tm_fill(col="np01_lqb",
          colorNA = "gray89",
          textNA = "n.s.",
          style="cat",
          palette=colors,
          labels=c("High-High","Low-Low", "n.s."),
          title = "",
          legend.show = FALSE) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top')) +
  tm_credits('LISA Fraction Blacks 2011', position = 'right') + 
  tm_scale_bar(color.dark = "gray60", # Customize scale bar and north arrow
               position = c(0.005, 0.005),
               lwd=0.5) +  # set position of the scale bar
  tm_compass(type = "4star", 
             size = 1.5, # set size of the compass
             text.size = 0.5, # set font size of the compass
             color.dark = "gray60", # color the compass
             text.color = "gray60", # color the text of the compass
             position = c(0.1, 0.9))
dev.off()

pdf("fractionwhite_bradford.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) +  
  tm_fill(col="np01_lqw",
          colorNA = "gray89",
          textNA = "n.s.",
          style="cat",
          palette=colors,
          labels=c("High-High","Low-Low", "n.s."),
          title = "",
          legend.show = FALSE) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top')) +
  tm_credits('LISA Fraction Whites 2011', position = 'right') + 
  tm_scale_bar(color.dark = "gray60", # Customize scale bar and north arrow
               position = c(0.005, 0.005),
               lwd=0.5) +  # set position of the scale bar
  tm_compass(type = "4star", 
             size = 1.5, # set size of the compass
             text.size = 0.5, # set font size of the compass
             color.dark = "gray60", # color the compass
             text.color = "gray60", # color the text of the compass
             position = c(0.1, 0.9))
dev.off()

pdf("legend_bradford.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) +  
  tm_fill(col="np01_lqw",
          colorNA = "gray89",
          textNA = "n.s.",
          palette=colors,
          labels=c("High-High","Low-Low", "n.s."),
          title = "",
          legend.show = T) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top'), legend.only = T)
dev.off()

#local indices
#segrlsoa_simpson_ethgrouped 
#segrlsoa_fraction_ethgrouped_asian 
#segrlsoa_fraction_ethgrouped_black 
#segrlsoa_fraction_ethgrouped_whiteb

pdf("local_simpson_des.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) + 
  tm_fill(col = "segrlsoa_simpson_ethgrouped", 
          palette="RdBu",
          title = "",
          breaks=seq(0, 1, by=0.2),
          legend.show = FALSE) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top')) +
  tm_credits('2001', position = 'right') + 
  tm_scale_bar(color.dark = "gray60", # Customize scale bar and north arrow
               position = c(0.005, 0.005),
               lwd=0.5) +  # set position of the scale bar
  tm_compass(type = "4star", 
             size = 1.5, # set size of the compass
             text.size = 0.5, # set font size of the compass
             color.dark = "gray60", # color the compass
             text.color = "gray60", # color the text of the compass
             position = c(0.1, 0.9))
dev.off()

pdf("fraction_asian_des.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) + 
  tm_fill(col = "segrlsoa_fraction_ethgrouped_asian", 
          palette="RdBu",
          title = "",
          breaks=seq(0, 1, by=0.2),
          legend.show = FALSE) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top')) +
  tm_credits('2001', position = 'right') + 
  tm_scale_bar(color.dark = "gray60", # Customize scale bar and north arrow
               position = c(0.005, 0.005),
               lwd=0.5) +  # set position of the scale bar
  tm_compass(type = "4star", 
             size = 1.5, # set size of the compass
             text.size = 0.5, # set font size of the compass
             color.dark = "gray60", # color the compass
             text.color = "gray60", # color the text of the compass
             position = c(0.1, 0.9))
dev.off()

pdf("fraction_black_des.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) + 
  tm_fill(col = "segrlsoa_fraction_ethgrouped_black", 
          palette="RdBu",
          title = "",
          breaks=seq(0, 1, by=0.2),
          legend.show = T) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top')) +
  tm_credits('2001', position = 'right') + 
  tm_scale_bar(color.dark = "gray60", # Customize scale bar and north arrow
               position = c(0.005, 0.005),
               lwd=0.5) +  # set position of the scale bar
  tm_compass(type = "4star", 
             size = 1.5, # set size of the compass
             text.size = 0.5, # set font size of the compass
             color.dark = "gray60", # color the compass
             text.color = "gray60", # color the text of the compass
             position = c(0.1, 0.9))
dev.off()

pdf("fraction_white_des.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) + 
  tm_fill(col = "segrlsoa_fraction_ethgrouped_whiteb", 
          palette="RdBu",
          title = "",
          breaks=seq(0, 1, by=0.2),
          legend.show = T) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top')) +
  tm_credits('2001', position = 'right') + 
  tm_scale_bar(color.dark = "gray60", # Customize scale bar and north arrow
               position = c(0.005, 0.005),
               lwd=0.5) +  # set position of the scale bar
  tm_compass(type = "4star", 
             size = 1.5, # set size of the compass
             text.size = 0.5, # set font size of the compass
             color.dark = "gray60", # color the compass
             text.color = "gray60", # color the text of the compass
             position = c(0.1, 0.9))
dev.off()

pdf("legend_des.pdf",width=6,height=4,paper='special') 
tm_shape(bradford) + 
  tm_fill(col = "segrlsoa_fraction_ethgrouped_whiteb", 
          palette="RdBu",
          title = "",
          breaks=seq(0, 1, by=0.2),
          legend.show = T) + 
  tm_borders(col = 'white', lwd = .5) +
  tm_layout(legend.position = c('left', 'top'), legend.only = T)
dev.off()



