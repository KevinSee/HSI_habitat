# Author: Kevin See
# Purpose: fit random forest models to HSI estimates of capacity and habitat data
# Created: 5/17/2016
# Last Modified: 5/20/2016
# Notes: HSI estimates provided by Sara Bangen

#----------------------------------------------------------------
library(stringr)
library(lubridate)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(missForest)
library(corrplot)

theme_set(theme_bw())

#----------------------------------------------------------------
# load HSI and habitat data
load('Data/HSI_habitat_prepped.rda')

# which habitat metrics are going into the model?
hab_mets_list =list('Chinook.Juvenile' = c('SlowWater_Freq', 
                                           'DpthWet_SD',
                                           'FishCovTotal',
                                           'NatPrin1',
                                           'RipCovBigTree',
                                           'SubLT6', 
                                           # 'SubD50',
                                           'SolarSummr_Avg',
                                           # 'AvgHourly',
                                           'Alk',
                                           'LWFreq_Bf'),
                    'Chinook.Spawner' = c('SlowWater_Freq',
                                          'DpthThlwg_UF_CV',
                                          'FishCovTotal',
                                          'NatPrin1',
                                          'DistPrin1',
                                          'RipCovUstory',
                                          # 'RipCovBigTree',
                                          'SubLT6',
                                          # 'SubD50',
                                          'AvgHourly',
                                          # 'SolarSummr_Avg',
                                          'Alk',
                                          'LWFreq_Wet'),
                    'Steelhead.Juvenile' = c('FstNT_Freq',
                                             'DpthThlwg_UF_CV',
                                             # 'DpthWet_SD',
                                             'FishCovTotal',
                                             'NatPrin1',
                                             'DistPrin1',
                                             'RipCovCanNone',
                                             'SubD50',
                                             # 'SubLT2',
                                             'AvgHourly',
                                             # 'SolarSummr_Avg',
                                             'Alk',
                                             'LWFreq_Bf'),
                    'Steelhead.Spawner' = c('FstTurb_Freq',
                                            'DpthThlwg_UF_CV',
                                            'FishCovTotal',
                                            'NatPrin1',
                                            'RipCovBigTree',
                                            # 'RipCovCanNone',
                                            'SubLT6',
                                            # 'SubEstCbl',
                                            # 'SubD50',
                                            'AvgHourly',
                                            'SolarSummr_Avg',
                                            'X7dAMGtr22',
                                            'Alk',
                                            'LWFreq_Bf'))


hab_mets_df = hab_mets_list %>% ldply(.fun = function(x) data.frame(Metric = x),
                        .id = 'Model') %>% tbl_df() %>%
  mutate(Species = sapply(Model, function(x) str_split(x, '\\.')[[1]][1]),
         Lifestage = sapply(Model, function(x) str_split(x, '\\.')[[1]][2])) %>%
  select(Species, Lifestage, Metric) %>%
  left_join(hab_catg %>%
              select(ShortName, Name, MetricCategory),
            by = c('Metric' = 'ShortName'))



# how correlated are habitat metrics?
all_hab_catg = hab_mets_df %>%
  select(metric = Metric, Name, MetricCategory) %>%
  distinct() %>%
  arrange(MetricCategory)

all_hab_mets = all_hab_catg %>%
  select(metric) %>%
  as.matrix() %>% as.character()

corr_matrix = cm_data %>%
  filter(Primary.Visit == 'Yes') %>%
  select(VisitID, one_of(all_hab_mets)) %>%
  gather(metric, value, -VisitID) %>%
  mutate(metric = factor(metric, levels = all_hab_catg$metric)) %>%
  spread(metric, value) %>%
  select(-VisitID) %>%
  cor(use = 'pairwise.complete.obs')

# rownames(corr_matrix) = colnames(corr_matrix) = all_hab_catg %>%
#   select(Name) %>%
#   as.matrix() %>% as.character()

pdf('Figures/Covariate_Correlation.pdf',
    width = 8,
    height = 8)
corrplot(corr = corr_matrix,
         method = 'ellipse', 
         type = 'lower', 
         # order = 'AOE',
         # title = 'Habitat Metric Correlations',
         tl.col = 'black',
         diag = F)
par(mar = c(5,4,4,2) + 0.1)
dev.off()


# additional metrics to be used for data imputation
impute_mets = c('WatershedName', 'VisitYear', 'Elev_M', 'Sin', 'CUMDRAINAG')
impute_mets = impute_mets[!impute_mets %in% hab_mets_df$Metric]


#----------------------------------------------------------------
# fit random forest models
set.seed(17)
resp_met = 'capacity'

spp = c('Chinook', 'Steelhead')[2]
stage = c('Juvenile', 'Spawner')[2]

for(spp in c('Chinook', 'Steelhead')) {
  for(stage in c('Juvenile', 'Spawner')) {

    hab_mets = hab_mets_df %>%
      filter(Species == spp,
             Lifestage == stage) %>%
      select(Metric) %>%
      as.matrix() %>% as.character()
    
    mod_data = hsi_habitat %>%
      filter(Species == spp,
             Lifestage == stage) %>%
      select(Lifestage, 
             Species,
             SiteName,
             capacity,
             one_of(hab_mets),
             one_of(impute_mets))
    
    # how much missing data?
    mod_data %<>%
      select(-one_of(hab_mets)) %>%
      bind_cols(mod_data %>%
                  select(one_of(hab_mets)) %>%
                  mutate(n_miss = rowSums(is.na(.)))) %>%
      tbl_df()
    
    table(mod_data$n_miss)
    
    # remove any row with more than 3 missing habitat covariates
    mod_data %<>%
      filter(n_miss <= 3) %>%
      select(-n_miss)
    
    # imputed missing values with missForest package
    mod_df = mod_data %>% as.data.frame()
    impute_test = missForest(mod_df[,c(hab_mets, impute_mets)], variablewise = T, verbose = T)
    # data.frame(Metric = c(hab_mets, impute_mets), OOBerror = round(impute_test$OOBerror,3))
    imputed_df = bind_cols(mod_data[,resp_met], impute_test$ximp)
    
    # nrow(imputed_df) / length(hab_mets)
    rf_mod = randomForest(x = imputed_df[,hab_mets], 
                          y = imputed_df[,resp_met],
                          ntree = 2000)
    
    #----------------------------------------------------------------
    # Make plots
    #----------------------------------------------------------------
    # importance plot
    imp_df = data.frame(Metric = rownames(importance(rf_mod)),
                        importance(rf_mod)) %>%
      left_join(hab_mets_df %>%
                  filter(Species == spp,
                         Lifestage == stage) %>%
                  select(Metric, Name)) %>%
      mutate(Name = factor(Name, levels = Name[order(IncNodePurity)])) %>%
      arrange(desc(IncNodePurity))
    imp_p = imp_df %>%
      ggplot(aes(x = Name, y = IncNodePurity)) +
      geom_bar(stat = 'identity', fill = 'gray40') +
      coord_flip() +
      labs(title = paste(spp, stage, 'Capacity\nRelative Importance')) +
      theme(axis.title = element_blank())
    # imp_p
    
    #----------------------------------------------------------------
    # partial dependence plots
    # get means and ranges of all covariates
    covar_range = select(imputed_df, one_of(hab_mets)) %>%
      gather(covar, value) %>%
      group_by(covar) %>%
      summarise(mean_value = mean(value),
                median_value = median(value),
                min_value = min(value),
                max_value = max(value))
    
    # how many points to plot?
    n_pts = 200
    
    pdp_list = dlply(covar_range, .(covar), function(x) cbind(seq(x$min_value, x$max_value, length.out = n_pts),
                                                              matrix(covar_range$median_value, nrow = n_pts, ncol = nrow(covar_range), byrow = T, dimnames = list(NULL, covar_range$covar)))) 
    
    for(i in 1:length(pdp_list)) {
      pdp_list[[i]] = pdp_list[[i]][,-match(names(pdp_list)[i], colnames(pdp_list[[i]]))]
      colnames(pdp_list[[i]])[1] = names(pdp_list)[i]
    }
    
    pdp_df = ldply(pdp_list) %>% tbl_df %>%
      mutate(rf_pred = predict(rf_mod, newdata = select(., one_of(hab_mets)))) %>%
      gather(Metric, value, -covar, -rf_pred) %>%
      filter(covar == Metric) %>%
      select(covar, value, rf_pred) %>%
      left_join(hab_mets_df %>%
                  filter(Species == spp,
                         Lifestage == stage) %>%
                  select(covar = Metric, covar_label = Name, MetricCategory)) %>%
      mutate(covar_label = factor(covar_label, levels = imp_df$Name))
    
    rug_df = select(imputed_df, WatershedName, one_of(hab_mets)) %>%
      gather(covar, value, -WatershedName) %>% tbl_df %>%
      left_join(hab_mets_df %>%
                  filter(Species == spp,
                         Lifestage == stage) %>%
                  select(covar = Metric, covar_label = Name, MetricCategory))
    
    # all variables
    pdp_p = ggplot(pdp_df, aes(x = value, y = rf_pred)) +
      geom_smooth(method = 'loess', se=F, col=1) +
      geom_rug(data = rug_df, aes(x = value, y=NULL, color = WatershedName)) +
      scale_color_brewer(palette = 'Set1') +
      facet_wrap(~ covar_label, scales = 'free') +
      theme(legend.position = 'bottom') +
      guides(color = guide_legend(nrow = 2)) +
      labs(y = 'HSI Capacity', x = 'Covariate Value', title = 'Partial Dependence Plots')
    # pdp_p
    
    #----------------------------------------------------------------
    # observed vs. predicted
    obs_pred_df = imputed_df %>% tbl_df %>%
      select(obs = capacity,
             WatershedName:CUMDRAINAG) %>%
      bind_cols(data.frame(pred = predict(rf_mod))) %>% tbl_df
    
    # r2
    # with(obs_pred_df, cor(obs, pred))^2
    # summary(lm(pred ~ obs, obs_pred_df))$r.squared
    # summary(lm(pred ~ obs, obs_pred_df))$adj.r.squared
    
    # plot
    obs_pred_p1 = ggplot(obs_pred_df,
                         aes(x = obs,
                             y = pred)) +
      geom_smooth(method = lm,
                  # formula = y ~ -1 + x,
                  alpha = 0.2) +
      geom_point(aes(color = WatershedName)) +
      scale_color_brewer(palette = 'Set1') +
      geom_abline(slope = 1,
                  intercept = 0,
                  color = 'red',
                  linetype = 2) +
      coord_cartesian(ylim = c(0, max(obs_pred_df$pred) + 20)) +
      labs(x = 'Observed HSI',
           y = 'Predicted',
           title = paste(spp, stage, 'Capacity'),
           color = 'Watershed')
    
    obs_pred_p2 = ggplot(obs_pred_df,
                         aes(x = log(obs),
                             y = log(pred))) +
      geom_smooth(method = lm,
                  # formula = y ~ -1 + x,
                  alpha = 0.2) +
      geom_point(aes(color = WatershedName)) +
      scale_color_brewer(palette = 'Set1') +
      geom_abline(slope = 1,
                  intercept = 0,
                  color = 'red',
                  linetype = 2) +
      labs(x = 'Log Observed HSI',
           y = 'Log Predicted',
           title = paste(spp, stage, 'Capacity'),
           color = 'Watershed')
    
    # obs_pred_p1
    # obs_pred_p2
    
    ggsave(paste0('Figures/', spp, '_', stage, '_RelImp.pdf'),
           imp_p,
           width = 7,
           height = 7,
           units = 'in')
    
    ggsave(paste0('Figures/', spp, '_', stage, '_PartialDependence.pdf'),
           pdp_p,
           width = 7,
           height = 7,
           units = 'in')
    
    ggsave(paste0('Figures/', spp, '_', stage, '_ObsVsPred.pdf'),
           obs_pred_p1,
           width = 7,
           height = 7,
           units = 'in')
    
    ggsave(paste0('Figures/', spp, '_', stage, '_ObsVsPred_log.pdf'),
           obs_pred_p2,
           width = 7,
           height = 7,
           units = 'in')
    
    save(hab_mets, mod_data, imputed_df, rf_mod, imp_df, imp_p, pdp_df, rug_df, pdp_p, obs_pred_df, obs_pred_p1, obs_pred_p2,
         file = paste0('ModelFits/', spp, '_', stage, '_model.rda'))
  }
}
