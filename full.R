full<-read.csv("~/Documents/newdata.csv")
View(full)

library(ggplot2)
library(ggthemes) # to access theme_hc()


ggplot(data = full, mapping = aes(x = Year, y = LFPF, color = Country)) + # specify data, x-axis, y-axis and grouping variable
  geom_line() + # a line per group
  geom_point() + # points per group
  labs(title = "Female Labour Force Participation, 1990-2021") + # plot caption
  theme(legend.position = "right", # move legend to the right hand side of the plot
        axis.title.x = element_text(), # remove x axis title
        axis.title.y = element_text(), # remove y axis title
        legend.title = element_text(), # remove legend title
        plot.title = element_text(size = 20, color = "gray40"), # change size and color of plot title
        plot.subtitle = element_text(color = "gray40"), # change color of subtitle
        plot.caption = element_text(color = "gray40", hjust = 0)) + # change color of caption and left-align
  scale_y_continuous(breaks = seq(0, 100, by = 20)) + # specify min, max and break distance for y axis
  scale_x_continuous(breaks = seq(1990, 2021, by = 5)) + # specify min, max and break distance for x axis
  expand_limits(y = c(0, 100))

ggplot(data = full, mapping = aes(x = Year, y = LFPM, color = Country)) + # specify data, x-axis, y-axis and grouping variable
  geom_line() + # a line per group
  geom_point() + # points per group
  labs(title = "Male Labour Force Participation, 1990-2021") + # plot caption
  theme(legend.position = "right", # move legend to the right hand side of the plot
        axis.title.x = element_text(), # remove x axis title
        axis.title.y = element_text(), # remove y axis title
        legend.title = element_text(), # remove legend title
        plot.title = element_text(size = 20, color = "gray40"), # change size and color of plot title
        plot.subtitle = element_text(color = "gray40"), # change color of subtitle
        plot.caption = element_text(color = "gray40", hjust = 0)) + # change color of caption and left-align
  scale_y_continuous(breaks = seq(0, 100, by = 20)) + # specify min, max and break distance for y axis
  scale_x_continuous(breaks = seq(1990, 2021, by = 5)) + # specify min, max and break distance for x axis
  expand_limits(y = c(0, 100))

bp<-read.csv("~/Documents/bp.csv")
View(bp)

ggplot(bp, aes(x=Gender, y=LFP, color=Country)) + ggtitle("Box Plot for Labour Force Participation") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_boxplot()

ggplot(full, aes(x=Country, y=LFPF, color=Country)) + ggtitle("Box Plot for Female Labour Force Participation") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_boxplot()

ggplot(full, aes(x=Country, y=LFPM, color=Country)) + ggtitle("Box Plot for Male Labour Force Participation") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_boxplot()


library(stargazer)

# full, pooling

full<-read.csv("~/Documents/full.csv")
View(full)

panel.LFP <- plm(LFP ~ log(BOP+ 200000000000) + FDI + FER + log(GDP) + CPI + INF + SOC + POP + HOUR, data = full, model="pooling" )
summary(panel.LFP)

panel.LFPM <- plm(LFPM ~ log(BOP+ 200000000000) + FDI + FER + log(GDP) + CPI + INF + SOC  + POP + HOUR, data = full, model="pooling" )
summary(panel.LFPM)

panel.LFPF <- plm(LFPF ~ log(BOP+ 200000000000) + FDI + FER + log(GDP) + CPI + INF + SOC  + POP + HOUR, data = full, model="pooling" )
summary(panel.LFPF)

stargazer(panel.LFP, panel.LFPM, panel.LFPF)


panel.LFP <- plm(LFP ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR, data = full, model="pooling" )
summary(panel.LFP)

panel.LFPM <- plm(LFPM ~ log(BOP+ 200000000000) + FER + log(GDP) + INF + SOC  + POP + HOUR, data = full, model="pooling" )
summary(panel.LFPM)

panel.LFPF <- plm(LFPF ~ log(BOP+ 200000000000) + FER + log(GDP) + INF + SOC  + POP + HOUR, data = full, model="pooling" )
summary(panel.LFPF)

stargazer(panel.LFP, panel.LFPM, panel.LFPF)


# ed, pooling

ed<-read.csv("~/Documents/full2000.csv")

panel.LFP <- plm(LFP ~ log(BOP+ 200000000000) + FDI + FER + log(GDP) + CPI + INF + SOC + EDU + POP + HOUR, data = ed, model="pooling" )
summary(panel.LFP)

panel.LFPM <- plm(LFPM ~ log(BOP+ 200000000000) + FDI + FER + log(GDP) + CPI + INF + SOC + EDU + POP + HOUR, data = ed, model="pooling" )
summary(panel.LFPM)

panel.LFPF <- plm(LFPF ~ log(BOP+ 200000000000) + FDI + FER + log(GDP) + CPI + INF + SOC + EDU + POP + HOUR, data = ed, model="pooling" )
summary(panel.LFPF)

stargazer(panel.LFP, panel.LFPM, panel.LFPF)

# FE

Within.LFPED <- plm(LFP ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR, data = full, model="within")
summary(Within.LFPED)
fixef(Within.LFPED)

Within.LFPMED <- plm(LFPM ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR, data = full, model="within")
summary(Within.LFPMED)
fixef(Within.LFPMED)

Within.LFPFED <- plm(LFPF ~ log(BOP+ 200000000000)  + FER + log(GDP) + INF + SOC + POP + HOUR, data = full, model="within")
summary(Within.LFPFED)
fixef(Within.LFPFED)

stargazer(Within.LFPED, Within.LFPMED, Within.LFPFED)
coef(Within.LFPED, Within.LFPMED, Within.LFPFED)
fixef(Within.LFPED, Within.LFPMED, Within.LFPFED)



library(tidyverse)
library(dotwhisker)
library(gapminder)


dwplot(list(panel.LFP, panel.LFPM, panel.LFPF))
dwplot(list(panel.LFPED, panel.LFPMED, panel.LFPFED))
dwplot(list(Within.LFP, Within.LFPM, Within.LFPF)) 
dwplot(list(Within.LFPED, Within.LFPMED, Within.LFPFED)) 


