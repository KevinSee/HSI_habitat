# Author: Kevin See
# Purpose: Compile HSI estimates of fish capacities and CHaMP data
# Created: 5/10/16
# Last Modified: 5/10/16
# Notes: HSI estimates provided by Sara Bangen

#-----------------------------------------------------------------
library(readr)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
# library(WriteXLS)
library(readxl)

#-----------------------------------------------------------------
# get HSI estimates
hsi_data = read_csv('Data/capacityEstimates_fuzzyHSI.csv',
                    col_types = 'iccdddccicdddd')

xtabs(~ Lifestage + Species, hsi_data)

#-----------------------------------------------------------------
# get CHaMP habitat data, data dictionary. Then write it to a file in this directory
## only run once ##
# download.date = '20150916'
# 
# cm_data = left_join(inner_join(x = read.csv(paste0('/Users/kevin/Dropbox/ISEMP/Data/Habitat/CHaMP/CMorg/CHaMP_ProgramMetrics_', download.date, '/MetricAndCovariates.csv')), 
#                                y = read.csv(paste0('/Users/kevin/Dropbox/ISEMP/Data/Habitat/CHaMP/CMorg/CHaMP_ProgramMetrics_', download.date, '/MetricVisitInformation.csv'))),
#                     read.csv(paste0('/Users/kevin/Dropbox/ISEMP/Data/Habitat/CHaMP/CMorg/CHaMP_ProgramMetrics_', download.date, '/StreamTempSummer7dAM.csv'))) %>% 
#   tbl_df %>%
#   mutate(SampleDate = ymd_hms(SampleDate)) %>%
#   arrange(SiteName, VisitYear, SampleDate)
# 
# table(hsi_data$VisitID %in% cm_data$VisitID)
# 
# # data dictionary
# hab_dict = read.csv(paste0('/Users/kevin/Dropbox/ISEMP/Data/Habitat/CHaMP/CMorg/CHaMP_ProgramMetrics_', download.date, '/Definitions.csv')) %>% tbl_df()
# 
# 
# WriteXLS(c('cm_data', 'hab_dict'),
#          'Data/CHaMP_data.xlsx',
#          SheetNames = c('CHaMP_data', 'data_dictionary'))


#-----------------------------------------------------------------
# get CHaMP habitat data and data dictionary
cm_data = read_excel('Data/CHaMP_data.xlsx', 1)
hab_dict = read_excel('Data/CHaMP_data.xlsx', 2)

#-----------------------------------------------------------------
# combine HSI and habitat data
hsi_habitat = hsi_data %>%
  select(-SiteName, -WatershedName, -VisitYear, -Stream) %>%
  inner_join(cm_data)

# normalize data within watershed and year groups
hsi_habitat %<>%
  group_by(Lifestage, Species, WatershedName, VisitYear) %>%
  mutate(cap_mean = mean(capacity),
         cap_sd = sd(capacity),
         cap_z = (capacity - cap_mean) / cap_sd) %>%
  ungroup() %>%
  select(VisitID:capacity, cap_z, everything())

#-----------------------------------------------------------------
# put metrics in categories
hab_catg = filter(hab_dict, 
                  MetricGroupName %in% c('Visit Metric', 'Stream Temp Summer 7dAM'),
                  !grepl('^GCD', ShortName), 
                  !grepl('GeoDatabase$', ShortName),
                  !grepl('Img$', ShortName),
                  !grepl('^HydraModel', ShortName),
                  !grepl('RBT Outputs', ShortName),
                  !grepl('ResultsXML', ShortName),
                  !grepl('LogFile', ShortName)) %>%
  select(ShortName, Name, DescriptiveText, UnitOfMeasure, UnitOfMeasureAbbrv) %>%
  mutate(MetricCategory = ifelse(grepl('^Sub', ShortName), 'Substrate',
                                 ifelse(grepl('^LW', ShortName), 'Wood',
                                        ifelse((grepl('^FishCov', ShortName) | 
                                                  grepl('Ucut', ShortName)), 'Cover',
                                               ifelse(grepl('^RipCov', ShortName), 'Riparian',
                                                      ifelse((grepl('SlowWater', ShortName) | 
                                                                grepl('FstTurb', ShortName) | 
                                                                grepl('FstNT', ShortName) |
                                                                grepl('PoolToTurbulentAreaRatio', ShortName)), 'ChannelUnit',
                                                             ifelse((grepl('Island', ShortName) | 
                                                                       grepl('Sin', ShortName) | 
                                                                       grepl('_CV$', ShortName) | 
                                                                       grepl('DpthWet_SD', ShortName) |
                                                                       grepl('DetrendElev_SD', ShortName) |
                                                                       grepl('Braid', ShortName) |
                                                                       grepl('Side Channel', Name) |
                                                                       grepl('Lgth_ThlwgCLRat', ShortName)), 'Complexity',
                                                                    ifelse((grepl('temperature', DescriptiveText) | 
                                                                              grepl('SolarSummr_Avg', ShortName) | 
                                                                              grepl('7dAM', ShortName)), 'Temperature', 
                                                                           ifelse((grepl('Cond', ShortName) |
                                                                                     grepl('Alk', ShortName) |
                                                                                     grepl('DriftBioMass', ShortName)), 'WaterQuality', 'Size'))))))))) %>%
  # add a couple other metrics
  bind_rows(data.frame(ShortName = c('DistPrin1', 'NatPrin1', 'NatPrin2', 'mean_JulAug_temp', 'CUMDRAINAG', 'BraidChannelRatio', 'PoolToTurbulentAreaRatio'),
                       Name = c('Disturbance Index', 'Natural PC 1', 'Natural PC 2', 'Mean Summer Temperature', 'Cummulative Drainage Area', 'Braid to Channel Ratio', 'Pool To Turbulent Area Ratio'),
                       DescriptiveText = NA,
                       UnitOfMeasure = NA,
                       UnitOfMeasureAbbrv = NA,
                       MetricCategory = c(rep('Disturbance', 3), 'Temperature', 'Size', 'Complexity', 'ChannelUnit'))) %>%
  bind_rows(data.frame(ShortName = c('VisitYear', 'ValleyClass', 'ChannelType', 'Ppt', 'MeanU', 'SiteLength', 'AverageBFWidth'),
                       Name = c('Year', 'Valley Class', 'Beechie Channel Type', 'Precipitation', 'Mean Annual Discharge', 'Site Length', 'Average Bankfull Width'),
                       DescriptiveText = NA,
                       UnitOfMeasure = NA,
                       UnitOfMeasureAbbrv = NA,
                       MetricCategory = c(rep('Categorical', 3), rep('Size', 4)))) %>%
  mutate(ShortName = as.factor(ShortName),
         MetricCategory = as.factor(MetricCategory))
hab_catg$ShortName = gsub('7dAMG', 'X7dAMG', hab_catg$ShortName)


#-----------------------------------------------------------------
# save data files
save(hsi_data, cm_data, hab_dict, hab_catg, hsi_habitat, 
     file = 'Data/HSI_habitat_prepped.rda')

