# Prepare data for predicting water quality - same as code on Cont Limno git but
# do NOT want to remove entries with NO resonponse variables - instead save for training data


# ----load_packages ----
library(dplyr)
library(ggplot2)

# ----load_lagos ----
library(LAGOSNE)
lg <- lagosne_load("1.087.1")

# ----select_epi_nutr_variables ----
# variables: TP, TN, chlorophyll, Secchi
epi_nutr <- lg$epi_nutr
secchi   <- lg$secchi

# ----filter_to_common_time_period----
# time period: 1990 - 2011, using samples collected between Jun 15 and Sep 15 (stratification period)
# define time parameters
first_year <- 1990
last_year  <- 2011
first_day  <- '0615' #i.e., '0615' for Jun 15
last_day   <- '0915'

# convert sampledate to date format and create "monthday" column to filter by day
epi_nutr$sampledate <- as.Date(epi_nutr$sampledate, format="%m/%d/%Y")
secchi$sampledate   <- as.Date(secchi$sampledate, format="%m/%d/%Y")

epi_nutr$monthday   <- format(epi_nutr$sampledate, format="%m%d")
secchi$monthday     <- format(secchi$sampledate, format="%m%d")

# ---- fix_N_data ----
# from WISCONSIN dataset Noah Lottig's code 2018-04-25

#Fix TKN
epi_nutr$tkn[which(epi_nutr$programname=="WI_LKLS")] <-
  epi_nutr$tkn[which(epi_nutr$programname=="WI_LKLS")]*1000

#Fix NO2NO3
epi_nutr$no2no3[which(epi_nutr$programname=="WI_LKLS")] <-
  epi_nutr$no2no3[which(epi_nutr$programname=="WI_LKLS")]*1000

#Look at summary of data to ensure we appropriately fixed the issue
# epi_nutr %>%
#   filter(programname=="WI_LKLS") %>%
#   select(lagoslakeid,sampledate,tkn,no2no3) %>%
#   summary()

# ----calculate_tn ----
# from Sam Oliver
# from componenent variables and join with tn into a new column called "tn_combined"
epi_nutr$tn_calculated <- epi_nutr$tkn + epi_nutr$no2no3
epi_nutr$tn_combined   <- epi_nutr$tn
epi_nutr$tn_combined[which(is.na(epi_nutr$tn_combined) == TRUE)] <-
  epi_nutr$tn_calculated[which(is.na(epi_nutr$tn_combined) == TRUE)]

# subset by sample date and year cutoffs specified above
epi_nutr_subset <- epi_nutr[epi_nutr$monthday >= first_day & epi_nutr$monthday <= last_day,]
epi_nutr_subset <- subset(epi_nutr_subset, sampleyear >= first_year & sampleyear <= last_year)

secchi_subset <- secchi[secchi$monthday >= first_day & secchi$monthday <= last_day,]
secchi_subset <- subset(secchi_subset, sampleyear >= first_year & sampleyear <= last_year)

# get rid of duplicate values from MPCA
epi_nutr_dTN <- epi_nutr_subset[
  !duplicated(paste(epi_nutr_subset$lagoslakeid,
                    epi_nutr_subset$sampledate,
                    epi_nutr_subset$tn_combined)),]
epi_nutr_dTP <- epi_nutr_dTN[
  !duplicated(paste(epi_nutr_dTN$lagoslakeid,
                    epi_nutr_dTN$sampledate,
                    epi_nutr_dTN$tp)),]
epi_nutr_dchl <- epi_nutr_dTP[
  !duplicated(paste(epi_nutr_dTP$lagoslakeid,
                    epi_nutr_dTP$sampledate,
                    epi_nutr_dTP$chla)),]
secchi_subset_d <- secchi_subset[
  !duplicated(paste(secchi_subset$lagoslakeid,
                    secchi_subset$sampledate,
                    secchi_subset$secchi)),]

#select only useful columns for analysis (this is a final dataset for WQ1, 
# all observations retained for temporal study)
epi_nutr_tptnchl <- select(epi_nutr_dchl,
                           lagoslakeid, tp, tn_combined, chla, sampledate,
                           sampleyear, samplemonth, monthday, eventida1087,
                           programname)
sec              <- select(secchi_subset_d,
                           lagoslakeid, sampledate, secchi)

#merge tp,tn,chla to the secchi table
limno_data_WQ1   <- full_join(epi_nutr_tptnchl, sec,
                              by = c("lagoslakeid","sampledate" ))

#add a column that counts up the number of variables present
limno_data_WQ1$var_num <- apply(limno_data_WQ1, 1, function(x) {
  sum(!is.na(x['tp']), !is.na(x['tn_combined']),
      !is.na(x['chla']), !is.na(x['secchi']))
})

hu4_lagoslakeid_table <- data.frame(lagoslakeid = lg$lakes.geo$lagoslakeid,
                                    hu4_zoneid  = lg$lakes.geo$hu4_zoneid)

# save only observations with at least 1 variable
limno_vars_WQ0 <- filter(limno_data_WQ1, var_num == 0) #MLB added - want to save these for training data
limno_vars_WQ1 <- filter(limno_data_WQ1, var_num >= 1)

# limit lakes to those >= 4ha
greater_than_4ha <- lg$locus %>%
  filter(lake_area_ha >= 4) %>%
  select(lagoslakeid)
limno_vars_WQ1   <- filter(limno_vars_WQ1, lagoslakeid %in% greater_than_4ha$lagoslakeid)
limno_vars_WQ0   <- filter(limno_vars_WQ0, lagoslakeid %in% greater_than_4ha$lagoslakeid)


study_lakes <- limno_data_WQ1$lagoslakeid
# missing_lakes <- limno_


limno_data_WQ1_final <- left_join(limno_vars_WQ1, hu4_lagoslakeid_table,
                                  by = "lagoslakeid")
limno_data_WQ0_final <- left_join(limno_vars_WQ0, hu4_lagoslakeid_table, 
                                  by = "lagoslakeid")

# write.csv(limno_data_WQ1_final, "data/wq1_temporal.csv", row.names = FALSE)
  write.csv(limno_data_WQ0_final, file = "./csv-data/wq0_temporal.csv", row.names = FALSE)

# subset of data to get one date per lake with the most variables for WQ2
# give preference to the sample date that has the most variables, if there is more than one date,
# it chooses the first. #10,561 observations
limno_vars_WQ2       <- limno_vars_WQ1 %>%
  group_by(lagoslakeid) %>% slice(which.max(var_num))
limno_data_WQ2_final <- left_join (limno_vars_WQ2, hu4_lagoslakeid_table,
                                   by = "lagoslakeid")

limno_vars_WQ0.2       <- limno_vars_WQ0 %>%
  group_by(lagoslakeid) %>% slice(which.max(var_num))
limno_data_WQ0.2_final <- left_join (limno_vars_WQ0.2, hu4_lagoslakeid_table,
                                   by = "lagoslakeid") #643 observations

write.csv(limno_data_WQ0.2_final, "./csv-data/wq0_single.csv", row.names = FALSE)


# ----pull_local_predictors (for all lakes)----

# lulc (2001 NLCD) #updated all to 2006
#11=openwater
#21=developed open space
#22=developed low intensity
#23=developed medium intensity
#24=developed high intensity
#31=bare rock
#41=deciduous
#42=evergreen
#43=mixed forest
#52=scrub/shrub
#71=grasslands/herbaceous
#81=pasture/hay
#82=row crops
#90=woody wetlands
#95=emergent herbaceous wetlands
# Watershed: IWS

IWS_LULC <- lg$iws.lulc
IWS_LULC <- data.frame(lagoslakeid=IWS_LULC$lagoslakeid,
                       openwater2006_pct=IWS_LULC$iws_nlcd2006_pct_11,
                       developed_open2006_pct=IWS_LULC$iws_nlcd2006_pct_21,
                       developed_low2006_pct=IWS_LULC$iws_nlcd2006_pct_22,
                       developed_med2006_pct=IWS_LULC$iws_nlcd2006_pct_23,
                       developed_high2006_pct=IWS_LULC$iws_nlcd2006_pct_24,
                       barerock2006_pct=IWS_LULC$iws_nlcd2006_pct_31,
                       deciduous2006_pct=IWS_LULC$iws_nlcd2006_pct_41,
                       evergreen2006_pct=IWS_LULC$iws_nlcd2006_pct_42,
                       mixedforest2006_pct=IWS_LULC$iws_nlcd2006_pct_43,
                       scrubshrub2006_pct=IWS_LULC$iws_nlcd2006_pct_52,
                       grasslands_herbaceous2006_pct=IWS_LULC$iws_nlcd2006_pct_71,
                       pasture_hay2006_pct=IWS_LULC$iws_nlcd2006_pct_81,
                       rowcrops2006_pct=IWS_LULC$iws_nlcd2006_pct_82,
                       topo_rough_index_mean=IWS_LULC$iws_tri_mean,
                       topo_rough_index_max=IWS_LULC$iws_tri_max,
                       roaddensity_mperha=IWS_LULC$iws_roaddensity_density_mperha,
                       wetland_woody2006_pct=IWS_LULC$iws_nlcd2006_pct_90,
                       wetland_emergent2006_pct=IWS_LULC$iws_nlcd2006_pct_95)

IWS_LULC$iws_nlcd2006_urb <- IWS_LULC$developed_open2006_pct +
  IWS_LULC$developed_low2006_pct + IWS_LULC$developed_med2006_pct +
  IWS_LULC$developed_high2006_pct

IWS_LULC$iws_nlcd2006_for <- IWS_LULC$deciduous2006_pct +
  IWS_LULC$evergreen2006_pct + IWS_LULC$mixedforest2006_pct

IWS_LULC$iws_nlcd2006_agr <- IWS_LULC$pasture_hay2006_pct +
  IWS_LULC$rowcrops2006_pct

IWS_LULC$iws_nlcd2006_wet <- IWS_LULC$wetland_woody2006_pct +
  IWS_LULC$wetland_emergent2006_pct

# Local buffer around lakes (100 m)

Buff100_LULC <- lg$buffer100m.lulc
Buff100_LULC <- data.frame(lagoslakeid=Buff100_LULC$lagoslakeid,
                           buff_openwater2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_11,
                           buff_developed_open2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_21,
                           buff_developed_low2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_22,
                           buff_developed_med2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_23,
                           buff_developed_high2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_24,
                           buff_barerock2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_31,
                           buff_deciduous2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_41,
                           buff_evergreen2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_42,
                           buff_mixedforest2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_43,
                           buff_scrubshrub2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_52,
                           buff_grasslands_herbaceous2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_71,        buff_pasture_hay2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_81,
                           buff_rowcrops2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_82,
                           buff_topo_rough_index_mean=Buff100_LULC$buffer100m_tri_mean,
                           buff_topo_rough_index_max=Buff100_LULC$buffer100m_tri_max,
                           buff_roaddensity_mperha=Buff100_LULC$buffer100m_roaddensity_density_mperha,
                           buff_wetland_woody2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_90,
                           buff_wetland_emergent2006_pct=Buff100_LULC$buffer100m_nlcd2006_pct_95)

Buff100_LULC$buff_nlcd2006_urb <- Buff100_LULC$buff_developed_open2006_pct +
  Buff100_LULC$buff_developed_low2006_pct +
  Buff100_LULC$buff_developed_med2006_pct +
  Buff100_LULC$buff_developed_high2006_pct

Buff100_LULC$buff_nlcd2006_for <- Buff100_LULC$buff_deciduous2006_pct +
  Buff100_LULC$buff_evergreen2006_pct + Buff100_LULC$buff_mixedforest2006_pct

Buff100_LULC$buff_nlcd2006_agr<-Buff100_LULC$buff_pasture_hay2006_pct +
  Buff100_LULC$buff_rowcrops2006_pct

Buff100_LULC$buff_nlcd2006_wet<-Buff100_LULC$buff_wetland_woody2006_pct +
  Buff100_LULC$buff_wetland_emergent2006_pct

# watershed/lake morphometry and connectivity
# max and mean depth
depth <- data.frame(lagoslakeid=lg$lakes_limno$lagoslakeid,
                    maxdepth_m=lg$lakes_limno$maxdepth,
                    meandepth_m=lg$lakes_limno$meandepth)

# bring in expanded depth dataset - MLB: need lakesnodepth file!
# expanded_depth <- read.csv("./csv-data/lakesnodepth_6.15.18 - lakesnodepth_27Jun17-EDIT.csv",
#                            stringsAsFactors = FALSE)
# expanded_depth <- select(expanded_depth, lagoslakeid, ex_maxdepth_m = NEW.maxdepth..meters.)
# 
# depth <- dplyr::left_join(depth, expanded_depth, by = "lagoslakeid")
# depth$maxdepth_m[is.na(depth$maxdepth_m) & !is.na(depth$ex_maxdepth_m)] <-
#   depth$ex_maxdepth_m[is.na(depth$maxdepth_m) & !is.na(depth$ex_maxdepth_m)]
# depth <- dplyr::select(depth, -ex_maxdepth_m)

#lake area, perim, sdf, lat, long, elevation, and all ids
lake_morphometry <- data.frame(lagoslakeid=lg$locus$lagoslakeid,
                               nhd_lat=lg$locus$nhd_lat,
                               nhd_long=lg$locus$nhd_long,
                               lakearea_ha=lg$locus$lake_area_ha,
                               lakeperim_m=lg$locus$lake_perim_meters,
                               elevation_m=lg$locus$ elevation_m,
                               iws_zoneid=lg$locus$iws_zoneid,
                               hu4_zoneid=lg$locus$hu4_zoneid,
                               hu6_zoneid=lg$locus$hu6_zoneid,
                               hu8_zoneid=lg$locus$hu8_zoneid,
                               hu12_zoneid=lg$locus$hu12_zoneid,
                               edu_zoneid=lg$locus$edu_zoneid,
                               county_zoneid=lg$locus$county_zoneid,
                               state_zoneid=lg$locus$state_zoneid)

#calculate shoreline development: convert lake area to km2; convert lake perim to km
#lake_sdf = lake_perim_km / 2 * (pi * lake_area_km2)^1/2
lake_morphometry$lakearea_km2 <- lake_morphometry$lakearea_ha / 100
lake_morphometry$lakeperim_km <- lake_morphometry$lakeperim_m / 1000
lake_morphometry$lake_sdf     <- lake_morphometry$lakeperim_km/(2 *
                                                                  (pi*lake_morphometry$lakearea_km2)^0.5)

# iws area, perim, sdf
iws_morphometry             <- data.frame(lagoslakeid=lg$iws$lagoslakeid,
                                          iwsarea_ha=lg$iws$iws_ha,
                                          iwsperim_km=lg$iws$iws_perimkm)
iws_morphometry$iwsarea_km2 <- iws_morphometry$iwsarea_ha / 100
iws_morphometry$iws_sdf     <- iws_morphometry$iwsperim_km/(2 *
                                                              (pi * iws_morphometry$iwsarea_km2)^0.5)

# lakes with no iws data
# unique(limno_data_WQ1_final[!(limno_data_WQ1_final$lagoslakeid %in% iws_morphometry$lagoslakeid), "lagoslakeid"])

#merge data tables
local_morphometry <- left_join(lake_morphometry, iws_morphometry,
                               by="lagoslakeid")

#calculate iws/lake ratio
local_morphometry$IWS_lk_ratio <- local_morphometry$iwsarea_ha /
  local_morphometry$lakearea_ha

# connectivity
lake_conn <- lg$lakes.geo
lake_conn <- data.frame(lagoslakeid=lake_conn$lagoslakeid,
                        glaciation=lake_conn$latewisconsinglaciation_glacial,
                        conn_class=lake_conn$lakeconnection,
                        wlconnections_shoreline_km=lake_conn$wlconnections_allwetlands_shoreline_km)

# calculate percent wetland shoreline

pct_wetland_shoreline <- dplyr::left_join(
  dplyr::select(lake_conn, lagoslakeid),
  dplyr::select(lg$lakes.geo, lagoslakeid, wlconnections_allwetlands_shoreline_km))

pct_wetland_shoreline <- dplyr::left_join(
  pct_wetland_shoreline,
  dplyr::select(lg$locus, lagoslakeid, lake_perim_meters))

pct_wetland_shoreline <- dplyr::mutate(pct_wetland_shoreline,
                                       pct_wetland_shoreline = (wlconnections_allwetlands_shoreline_km /
                                                                  (lake_perim_meters * 0.001)) * 100)

lake_conn$lakeconn_v2<-sapply(lake_conn$conn_class, function(x) {
  if(x == 'DR_LakeStream') {'DR_LakeStream'}
  else {
    if(x == 'Headwater') {'HW_ISO'}
    else {
      if(x == 'DR_Stream') {'DR_Stream'}
      else {
        if(x== 'Isolated') {'HW_ISO'}
      }}}})

IWS_conn <- lg$iws.conn
IWS_conn <- data.frame(lagoslakeid=IWS_conn$lagoslakeid,
                       streamdensity_mperha=IWS_conn$iws_streamdensity_streams_density_mperha,
                       lakes_overlapping_area_pct=IWS_conn$iws_lakes_overlapping_area_pct,
                       wetlands_overlapping_area_pct=IWS_conn$iws_wl_allwetlandsdissolved_overlapping_area_pct)

#merge all local tables #51,065 observations
#includes all zone ids

res <- left_join(local_morphometry, depth, by = "lagoslakeid") %>%
   filter(lagoslakeid %in% study_lakes) %>%
  left_join(Buff100_LULC, by = "lagoslakeid") %>%
  left_join(IWS_LULC, by = "lagoslakeid") %>%
  left_join(lake_conn, by = "lagoslakeid") %>%
  left_join(IWS_conn, by = "lagoslakeid")

write.csv(res, "data/local_predictors_.csv", row.names = TRUE)

# ---- pull_regional_predictors ----
# Regional watershed (HU4)
# get table of HU4 and lagoslakeid
hu4_lagoslakeid_table <- data.frame(lagoslakeid=lg$lakes.geo$lagoslakeid,
                                    hu4_zoneid=lg$lakes.geo$hu4_zoneid)
hu4_lagoslakeid_table <- dplyr::filter(hu4_lagoslakeid_table,
                                       hu4_zoneid %in% limno_data_WQ1_final$hu4_zoneid)

HU4_LULC <- lg$hu4.lulc
HU4_LULC <- data.frame(hu4_zoneid=HU4_LULC$hu4_zoneid,
                       openwater2006_pct=HU4_LULC$hu4_nlcd2006_pct_11,
                       developed_open2006_pct=HU4_LULC$hu4_nlcd2006_pct_21,
                       developed_low2006_pct=HU4_LULC$hu4_nlcd2006_pct_22,
                       developed_med2006_pct=HU4_LULC$hu4_nlcd2006_pct_23,
                       developed_high2006_pct=HU4_LULC$hu4_nlcd2006_pct_24,
                       barerock2006_pct=HU4_LULC$hu4_nlcd2006_pct_31,
                       deciduous2006_pct=HU4_LULC$hu4_nlcd2006_pct_41,
                       evergreen2006_pct=HU4_LULC$hu4_nlcd2006_pct_42,
                       mixedforest2006_pct=HU4_LULC$hu4_nlcd2006_pct_43,
                       scrubshrub2006_pct=HU4_LULC$hu4_nlcd2006_pct_52,
                       grasslands_herbaceous2006_pct=HU4_LULC$hu4_nlcd2006_pct_71,
                       pasture_hay2006_pct=HU4_LULC$hu4_nlcd2006_pct_81,
                       rowcrops2006_pct=HU4_LULC$hu4_nlcd2006_pct_82,
                       topo_rough_index_mean=HU4_LULC$hu4_tri_mean,
                       topo_rough_index_max=HU4_LULC$hu4_tri_max,
                       roaddensity_mperha=HU4_LULC$hu4_roaddensity_density_mperha,
                       wetland_woody2006_pct=HU4_LULC$hu4_nlcd2006_pct_90,
                       wetland_emergent2006_pct=HU4_LULC$hu4_nlcd2006_pct_95)

HU4_LULC$hu4_nlcd2006_urb <- HU4_LULC$developed_open2006_pct +
  HU4_LULC$developed_low2006_pct + HU4_LULC$developed_med2006_pct +
  HU4_LULC$developed_high2006_pct

HU4_LULC$hu4_nlcd2006_for <- HU4_LULC$deciduous2006_pct +
  HU4_LULC$evergreen2006_pct + HU4_LULC$mixedforest2006_pct

HU4_LULC$hu4_nlcd2006_agr <- HU4_LULC$pasture_hay2006_pct +
  HU4_LULC$rowcrops2006_pct

HU4_LULC$hu4_nlcd2006_wet <- HU4_LULC$wetland_woody2006_pct +
  HU4_LULC$wetland_emergent2006_pct

# hydrology and deposition (at HU 4)
HU4_hydro_dep <- lg$hu4.chag
HU4_hydro_dep <- data.frame(hu4_zoneid=HU4_hydro_dep$hu4_zoneid,
                            baseflow_mean=HU4_hydro_dep$hu4_baseflowindex_mean,
                            runoff_mean=HU4_hydro_dep$hu4_runoff_mean,
                            groundwater_recharge=HU4_hydro_dep$hu4_groundwaterrecharge_mean,
                            so4dep_1990_mean=HU4_hydro_dep$hu4_dep_so4_1990_mean,
                            so4dep_2010_mean=HU4_hydro_dep$hu4_dep_so4_2010_mean,
                            totalNdep_1990_mean=HU4_hydro_dep$hu4_dep_totaln_1990_mean,
                            totalNdep_2010_mean=HU4_hydro_dep$hu4_dep_totaln_2010_mean,
                            no3dep_1990_mean=HU4_hydro_dep$hu4_dep_no3_1990_mean,
                            no3dep_2010_mean=HU4_hydro_dep$hu4_dep_no3_2010_mean,
                            prism_ppt_mean=HU4_hydro_dep$hu4_prism_ppt_30yr_normal_800mm2_annual_mean,
                            prism_tmean_mean=HU4_hydro_dep$hu4_prism_tmean_30yr_normal_800mm2_annual_mean,
                            prism_tmax_mean=HU4_hydro_dep$hu4_prism_tmax_30yr_normal_800mm2_annual_mean,
                            prism_tmin_mean=HU4_hydro_dep$hu4_prism_tmin_30yr_normal_800mm2_annual_mean,
                            alluvial_pct=HU4_hydro_dep$hu4_surficialgeology_alluv_pct,
                            colluvial_pct=HU4_hydro_dep$hu4_surficialgeology_colluv_pct,
                            glaciofluvial_pct=HU4_hydro_dep$hu4_surficialgeology_gf_out_pct,
                            till_loam_pct=HU4_hydro_dep$hu4_surficialgeology_till_loam_pct,
                            till_sand_pct=HU4_hydro_dep$hu4_surficialgeology_till_sand_pct,
                            till_clay_pct=HU4_hydro_dep$hu4_surficialgeology_till_clay_pct,
                            till_oth_pct=HU4_hydro_dep$hu4_surficialgeology_till_oth_pct)

# calculate deposition differences 1990-2010
HU4_hydro_dep$so4dep_19902010_diff <- HU4_hydro_dep$so4dep_1990_mean -
  HU4_hydro_dep$so4dep_2010_mean

HU4_hydro_dep$totalNdep_19902010_diff <- HU4_hydro_dep$totalNdep_1990_mean -
  HU4_hydro_dep$totalNdep_2010_mean

HU4_hydro_dep$no3dep_19902010_diff <- HU4_hydro_dep$no3dep_1990_mean -
  HU4_hydro_dep$no3dep_2010_mean

# HU4 connectivity
HU4_conn <- lg$hu4.conn
HU4_conn <- data.frame(hu4_zoneid=HU4_conn$hu4_zoneid,
                       streamdensity_mperha=HU4_conn$hu4_streamdensity_streams_density_mperha,
                       lakes_overlapping_area_pct=HU4_conn$hu4_lakes_overlapping_area_pct,
                       wetlands_overlapping_area_pct=HU4_conn$hu4_wl_allwetlandsdissolved_overlapping_area_pct,
                       hu4_lakes_drstream_overlapping_area_pct=HU4_conn$hu4_lakes_drstream_overlapping_area_pct,
                       hu4_lakes_drlakestream_overlapping_area_pct=HU4_conn$hu4_lakes_drlakestream_overlapping_area_pct,
                       hu4_lakes_headwater_overlapping_area_pct=HU4_conn$hu4_lakes_headwater_overlapping_area_pct,
                       hu4_lakes_isolated_overlapping_area_pct=HU4_conn$hu4_lakes_isolated_overlapping_area_pct)

HU4_conn$hu4_lakes_HWisolated_overlapping_area_pct <- HU4_conn$hu4_lakes_isolated_overlapping_area_pct +
  HU4_conn$hu4_lakes_headwater_overlapping_area_pct

# merge all HU4 data sets and lagoslakeid
HU4_hydro_dep  <- merge(HU4_hydro_dep, hu4_lagoslakeid_table, by='hu4_zoneid', all.x=FALSE)
HU4_hydro_LULC <- left_join(HU4_hydro_dep, HU4_LULC, by='hu4_zoneid')
HU4_predictors <- left_join(HU4_hydro_LULC, HU4_conn, by='hu4_zoneid')

write.csv(HU4_predictors, "data/regional_predictors.csv", row.names = FALSE)

# ---- add_holdout_ids ----

set.seed(364)

# randomly holdout 25% of the data
lagoslakeid_keep <-
  data.frame(dplyr::select(limno_data_WQ2_final, lagoslakeid)) %>%
  sample_frac(0.25) %>%
  .$lagoslakeid

limno_data_WQ2_final <- mutate(limno_data_WQ2_final,
                               random25_holdout = lagoslakeid %in% lagoslakeid_keep)

# sum(limno_data_WQ2_final$random25_holdout) / nrow(limno_data_WQ2_final)

# randomly holdout 75% of the data
lagoslakeid_keep <-
  data.frame(dplyr::select(limno_data_WQ2_final, lagoslakeid)) %>%
  sample_frac(0.75) %>%
  .$lagoslakeid

limno_data_WQ2_final <- mutate(limno_data_WQ2_final,
                               random75_holdout = lagoslakeid %in% lagoslakeid_keep)

# sum(limno_data_WQ2_final$random75_holdout) / nrow(limno_data_WQ2_final)

# holdout lakes that are within a HU4 in the top 50% of ag landuse cover
lower_50_ag <- HU4_predictors %>%
  dplyr::filter(!duplicated(hu4_zoneid)) %>%
  arrange(desc(hu4_nlcd2006_agr)) %>%
  slice(-1:-round(nrow(.) * 0.5)) %>%
  .$hu4_zoneid

limno_data_WQ2_final <- mutate(limno_data_WQ2_final,
                               hu4_ag50_holdout = !(hu4_zoneid %in% lower_50_ag))

# holdout lakes that are located in 50% of randomly chosen hucs
hu4_keep <- HU4_predictors %>%
  dplyr::filter(!duplicated(hu4_zoneid)) %>%
  sample_frac(0.50) %>%
  .$hu4_zoneid

limno_data_WQ2_final <- mutate(limno_data_WQ2_final,
                               hu4_random50_holdout = hu4_zoneid %in% hu4_keep)

# holdout 75% of lakes in each huc
lagoslakeid_strat_keep <- limno_data_WQ2_final %>%
  group_by(hu4_zoneid) %>%
  sample_frac(0.75) %>%
  .$lagoslakeid

# length(lagoslakeid_strat_keep) / nrow(limno_data_WQ2_final)

limno_data_WQ2_final <- mutate(limno_data_WQ2_final,
                               hu4_strat75_holdout = lagoslakeid %in% lagoslakeid_strat_keep)
limno_data_WQ2_final <- dplyr::filter(limno_data_WQ2_final,
                                      hu4_zoneid != "OUT_OF_HU4")

write.csv(limno_data_WQ2_final, "data/wq2_single.csv", row.names = FALSE)
write.csv(limno_data_WQ0.3_final, "./csv-data/wq0_single.csv", row.names = FALSE)