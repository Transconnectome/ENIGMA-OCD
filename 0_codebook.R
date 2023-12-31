library(tidyverse)
library(magrittr)

setwd("../ENIGMA-OCD/0.Data/Analysis/2_z-scaled/1.adult")

df <- read_csv('T.Dx_Adult_S.Test_267_v.z.norm.211123.csv')
df %>% names()
# start: ACR.FA
# end: UNC.R.AD

df_dti <- df %>% 
  select(ACR.FA: UNC.R.AD) %>% names() %>% as_tibble() 

df %>% names()

df_dti %>% 
  mutate(Feature = ifelse(str_detect(df_dti$value, 'ACR'), 'Anterior corona radiata', 
                          ifelse(str_detect(df_dti$value, 'ALIC'), 'Anterior limb of internal capsule', 
                                 ifelse(str_detect(df_dti$value, 'AverageFA'), 'Average FA', 
                                        ifelse(str_detect(df_dti$value, 'BCC'), 'Body of corpus callosum', 
                                               ifelse(str_detect(df_dti$value, 'CC'), 'Corpus callosum',
                                                      ifelse(str_detect(df_dti$value, 'CGC'), 'Cingulum  (cingulate gyrus)', 
                                                             ifelse(str_detect(df_dti$value, 'CGH'), 'Cingulum (hippocampus)', ifelse(str_detect(df_dti$value, 'CR'), 'Corona radiata',  ifelse(str_detect(df_dti$value, 'CST'), 'Corticospinal tract',
                                                                                                                                                                                                ifelse(str_detect(df_dti$value, 'EC'), 'External capsule', ifelse(str_detect(df_dti$value, 'FX.ST'), 'Fornix (cres)/Stria terminalis', NA))))))))))))
