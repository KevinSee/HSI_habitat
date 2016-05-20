# Author: Kevin See
# Purpose: Use MINE to determine metrics in various groups that are most associated with HSI fish density
# Created: 5/10/2016
# Last Modified: 5/10/2016
# Notes: HSI estimates provided by Sara Bangen

#----------------------------------------------------------------
library(stringr)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

setwd('MINE')
source('MINE.R')

#----------------------------------------------------------------
# load fish and habitat data
load('../Data/HSI_habitat_prepped.rda')

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

# remove categorical variables - can't run MINE on them
hab_catg = filter(hab_catg,
                  MetricCategory != 'Categorical')

xtabs(~ MetricCategory, hab_catg, drop.unused.levels = T) %>% addmargins()
filter(hab_catg, is.na(MetricCategory)) %>%
  select(ShortName, Name, MetricCategory) %>% as.data.frame()

select(hab_catg, ShortName, Name, MetricCategory, UnitOfMeasureAbbrv) %>% as.data.frame()

select(hab_catg, ShortName, Name, MetricCategory, UnitOfMeasureAbbrv) %>% 
  filter(grepl('^Area_', ShortName)) %>%
  # filter(grepl('m2', UnitOfMeasureAbbrv)) %>%
  as.data.frame()

# which possible metrics aren't in data set?
hab_catg$ShortName[!hab_catg$ShortName %in% names(hsi_habitat)]
hab_catg %<>%
  filter(ShortName %in% names(hsi_habitat))

# vector of all possible habitat metrics
all_hab_mets = hab_catg$ShortName[hab_catg$ShortName %in% names(hsi_habitat)]
length(all_hab_mets)

# # modify some to be scaled by site-length
# hsi_habitat = hsi_habitat %>%
#   mutate_each(funs(ScaleSiteLength = . / ModeledArea), matches('LWVol'), matches('_Area'), matches('^Area_'), BfVol)


#----------------------------------------------------------------
# run MINE on chinook and steelhead adult and juvenile HSI capacity estimates
mine_list = hsi_habitat %>%
  select(Lifestage, 
         Species,
         capacity,
         cap_z,
         one_of(hab_catg$ShortName)) %>%
  dlply(.(Lifestage, Species))

# how many NAs are in each metric?
non_NA_df = ldply(mine_list, .id = 'Model', .fun = function(x) {
  gather(x, variable, value, -(Lifestage:Species)) %>%
    group_by(Lifestage, Species, variable) %>%
    summarise(non_NA = sum(!is.na(value)),
              is_NA = sum(is.na(value)),
              perc_NA = is_NA / (non_NA + is_NA)) %>%
    left_join(select(hab_catg, ShortName, MetricCategory),
              by = c('variable' = 'ShortName')) %>%
    filter(!variable %in% c('capacity', 'cap_z'))
}) %>% tbl_df() %>%
  select(Lifestage, Species, MetricCategory, variable, non_NA, is_NA, perc_NA)

arrange(non_NA_df, desc(is_NA))


# run MINE on normalized response
lapply(mine_list, function(x) {
  var_nm = select(x, 2,1) %>%
    distinct() %>%
    as.matrix() %>% as.character() %>%
    paste(collapse = '_')
  x %>%
    select(-Lifestage, -Species, -capacity) %>%
    as.matrix() %>%
    t() %>%
    rMINE(paste0('MINE_Results/', var_nm, '_Normalized'), style = 'master.variable', var1.id = 0)
})

# run MINE on non-normalized response
lapply(mine_list, function(x) {
  var_nm = select(x, 2,1) %>%
    distinct() %>%
    as.matrix() %>% as.character() %>%
    paste(collapse = '_')
  x %>%
    select(-Lifestage, -Species, -cap_z) %>%
    as.matrix() %>%
    t() %>%
    rMINE(paste0('MINE_Results/', var_nm, '_Untransformed'), style = 'master.variable', var1.id = 0)
})



#----------------------------------------------------------------
# examine results
file_nms = list.files('MINE_Results')
file_nms = file_nms[grepl('Results.csv$', file_nms)]

all_res = data.frame(file_name = file_nms) %>% tbl_df() %>%
  mutate(var_nm = sapply(file_name, function(x) str_split(x, ',', 2)[[1]][1]),
         Species = sapply(var_nm, function(x) str_split(x, '_', 3)[[1]][1]),
         Lifestage = sapply(var_nm, function(x) str_split(x, '_', 3)[[1]][2]),
         Type = sapply(var_nm, function(x) str_split(x, '_', 3)[[1]][3])) %>%
  ddply(.(Species, Lifestage, Type), .fun = function(x) {
    res = read.csv(paste0('MINE_Results/', x$file_name)) %>% tbl_df()
    names(res) = gsub('\\.$', '', names(res))
    names(res) = gsub('\\.\\.', '_', names(res))
    names(res) = gsub('\\.', '_', names(res))
    return(res)
  }) %>% tbl_df() %>%
  mutate(X_var = ifelse(Type == 'Normalize', 'cap_z', 'capacity'),
         Y_num = as.integer(gsub('variable ', '', Y_var)),
         Y_var = hab_catg$ShortName[Y_num - 1]) %>%
  left_join(hab_catg %>%
              select(Y_var = ShortName, Name, MetricCategory)) %>%
  select(Species:Type, MetricCategory, Y_var, Name, 
         MIC = MIC_strength, 
         MAS = MAS_non_monotonicity, 
         MEV = MEV_functionality, 
         MCN = MCN_complexity, 
         non_linear = MIC_p_2_nonlinearity, 
         Linear_p = Linear_regression_p) %>%
  left_join(non_NA_df %>%
              rename(Y_var = variable))
  

table(hab_catg$ShortName %in% all_res$Y_var)
hab_catg$ShortName[!hab_catg$ShortName %in% all_res$Y_var]
which(!hab_catg$ShortName %in% all_res$Y_var)

non_NA_df %>%
  select(variable:perc_NA) %>%
  distinct() %>%
  select(perc_NA) %>%
  as.matrix() %>% as.numeric() %>%
  hist(30)

all_res %>%
  filter(perc_NA < 0.6) %>%
  filter(MetricCategory != 'Size',
         !grepl('Volume', Name),
         !grepl('Area', Name)) %>%
  filter(Type == 'Normalized') %>%
  group_by(Species, Lifestage, Type) %>%
  arrange(desc(MIC)) %>%
  slice(1:5) %>%
  as.data.frame()


# top few metrics from each category
top_mets = all_res %>%
  filter(perc_NA < 0.6) %>%
  filter(MetricCategory != 'Size',
         !grepl('Volume', Name),
         !grepl('Area', Name)) %>%
  # filter(Type == 'Normalized') %>%
  filter(Type == 'Untransformed') %>%
  group_by(Species, Lifestage, MetricCategory) %>%
  mutate(deltaMIC = max(MIC) - MIC) %>%
  arrange(desc(MIC)) %>%
  filter(deltaMIC < 0.05) %>%
  select(Y_var, Name, perc_NA, deltaMIC, MIC:Linear_p) %>%
  # slice(1:5) %>%
  # filter(MetricCategory == 'Temperature') %>%
  as.data.frame()

top_mets %>%
  select(Species, Lifestage, MetricCategory, ShortName = Y_var, Name, MIC, deltaMIC)

# how many metrics are in top_mets for each category?
top_mets %>%
  group_by(Species, Lifestage, MetricCategory) %>%
  summarize(n_metrics = n_distinct(Y_var))

spp = c('Chinook', 'Steelhead')[1]
stage = c('Juvenile', 'Spawner')[1]

top_mets %>%
  filter(Species == spp,
         Lifestage == stage) %>%
  select(Species, Lifestage, MetricCategory, Y_var, MIC, deltaMIC, MAS:Linear_p)

# top metric in each category
hab_mets = top_mets %>%
  filter(Species == spp,
         Lifestage == stage) %>%
  filter(deltaMIC == 0) %>%
  select(Y_var) %>%
  as.matrix() %>% as.character()

hsi_habitat %>%
  filter(Species == spp,
         Lifestage == stage) %>%
  select(Species, Lifestage, WatershedName, capacity, cap_z, one_of(hab_mets)) %>%
  gather(metric, value, -(Species:cap_z)) %>%
  ggplot(aes(x = value,
             y = capacity)) +
  geom_point(aes(color = WatershedName)) +
  geom_smooth(method = loess) +
  facet_wrap(~ metric, scales = 'free') +
  labs(x = 'Habitat Metric',
       y = 'Capacity',
       title = paste(spp, stage))

# correlation between top metrics
corr_list = hsi_habitat %>%
  dlply(.(Species, Lifestage), function(x) {
    spp = unique(x$Species)
    stage = unique(x$Lifestage)
    hab_mets = top_mets %>%
      filter(Species == spp,
             Lifestage == stage) %>%
      filter(deltaMIC == 0) %>%
      select(Y_var) %>%
      as.matrix() %>% as.character()
    select(x, one_of(hab_mets)) %>%
      cor(use = 'pairwise.complete.obs')
  }) 

corr_list[[3]] %>%
  corrplot(method = 'ellipse', type = 'lower')

