# packages
library(tidyverse); library(lubridate); library(stargazer)

ed<-read.csv("~/Documents/full2000.csv")

# financial crisis sub-sample
event <- ymd("2008")

# create before data set
est_data <- ed %>% filter(Year >= 2001 & Year <= 2007)
  
# create during and following data set
obs_data <- ed %>%
  filter(Year >= 2008 & Year <= 2014)

# pooled OLS regressions for before
panel.LFPMBFC <- plm(LFPM ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR,  data = est_data, model="pooling")
summary(panel.LFPMBFC)

panel.LFPMFC <- plm(LFPM ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR,  data = obs_data, model="pooling")
summary(panel.LFPMFC)

# pooled OLS regressions for during and following
panel.LFPFBFC <- plm(LFPF ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR,  data = est_data, model="pooling")
summary(panel.LFPFBFC)

panel.LFPFFC <- plm(LFPF ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR,  data = obs_data, model="pooling")
summary(panel.LFPFFC)

stargazer(panel.LFPMBFC, panel.LFPMFC, panel.LFPFBFC, panel.LFPFFC)





# before covid data set
bef_data <- ed %>% filter(Year >= 2015 & Year <= 2019)

# including covid data set
dur_data <- ed %>%
  filter(Year >= 2015 & 
           Year <= 2021)

# pooled OLS regressions for before
panel.LFPMBC <- plm(LFPM ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR,  data = bef_data, model="pooling")
summary(panel.LFPBC)

panel.LFPMC <- plm(LFPM ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR,  data = dur_data, model="pooling")
summary(panel.LFPC)

# pooled OLS regressions including covid
panel.LFPFBC <- plm(LFPF ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR,  data = bef_data, model="pooling")
summary(panel.LFPBC)

panel.LFPFC <- plm(LFPF ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR,  data = dur_data, model="pooling")
summary(panel.LFPC)

stargazer(panel.LFPMBC, panel.LFPMC, panel.LFPFBC, panel.LFPFC)



