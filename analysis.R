# set the working directory to the folder on your computer where the files live 
setwd("~/Desktop/symexp/")

# load any packages you want to use when running your script
library(tidyverse) # for plotting in ggplot2, data wrangling, and loading in files tidy-like
library(summarytools) # generating summary plot

# load in date file
count_data <- read_csv("Symbiont Counts for Nitrogen Experiment - Final data.csv")

# examine your data
# head shows you the first 6 lines
head(count_data)

# summary shows you a summary of your data
summary(count_data)

# generate summary figure, you can pop it out as a webpage
view(dfSummary(count_data))

# data wrangling/tidying
# "Type" indicates the name of the tube
# Make variable called "sample_name" that indicates species, treatment, and tube number
# Make variable called "treatment" that indicates treatment

# %>% ... those symbols together are a "pipe", it lets you perform functions in serial

count_data2 <- count_data %>% 
  mutate(sample_name = paste(Species, Type, sep = "_")) %>% # makes a new column called "sample_name"
  rename(treat = Type) %>% # renames the 'Type' column to be "treat" 
  group_by(sample_name, Date) %>%
  mutate(sum_Counts = sum(Count))

count_data2$treat <- gsub('[0-9]+', '', count_data2$treat) # remove numbers in treatment names

# separate by species
data_occ <- count_data2 %>% filter(Species=="Ocu")
head(data_occ)

data_mac <- count_data2 %>% filter(Species=="Mac")
head(data_mac)

# plot histogram
hist(data_mac$sum_Counts)
hist(data_occ$sum_Counts)

# make boxplot
boxplot(sum_Counts ~ treat, data = data_mac)
boxplot(sum_Counts ~ treat, data = data_occ)

# quick linear model (stats)
lm_mac <- lm(sum_Counts ~ treat, data = data_mac)
aov_mac <- aov(lm_mac)
aov_mac
TukeyHSD(aov_mac)

lm_oc <- lm(sum_Counts ~ treat, data = data_occ)
aov_oc <- aov(lm_oc)
TukeyHSD(aov_oc)

# summarize across bio reps
head(data_mac)
summ_mac <- data_mac %>%
  ungroup() %>% #because we had grouped it previously
  group_by(treat, Date) %>% # group the bio reps (tubes) of same treatment and day together
  summarise(mean.count = mean(sum_Counts), sd = sd(sum_Counts)) # find the mean and SD of counts
summ_mac

summ_occ <- data_occ %>%
  ungroup() %>% #because we had grouped it previously
  group_by(treat, Date) %>% # group the bio reps (tubes) of same treatment and day together
  summarise(mean.count = mean(sum_Counts), sd = sd(sum_Counts)) # find the mean and SD of counts
summ_occ

# plot over time
limits <- aes(ymax = mean.count + sd, ymin=mean.count - sd) # set the limits for plotting error bars

gg1 <- ggplot(summ_mac, aes(x=Date, y=mean.count, color=treat)) +
  geom_point() +
  geom_line(aes(group=treat))+
  geom_errorbar(limits, width=0.2)+
  ylab("Symbiont Count")+
  xlab("Date")+
  ggtitle("Clade D1a Growth")+
  scale_color_manual(values=c("black","orange", "green"))+
  theme_bw()
gg1

gg2 <- ggplot(summ_occ, aes(x=Date, y=mean.count, color=treat)) +
  geom_point() +
  geom_line(aes(group=treat))+
  geom_errorbar(limits, width=0.2)+
  ylab("Symbiont Count per mL")+
  xlab("Date")+
  ggtitle("Oculina Growth")+
  scale_color_manual(values=c("black","orange", "green"))+
  theme_bw()
gg2


