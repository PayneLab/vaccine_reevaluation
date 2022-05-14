###################################
# Covariate analysis              # 
# Updated 5/14/2022                # 

##### Data set up and cleaning #####
# Set directory path 
my.dir <-rstudioapi::getActiveDocumentContext()$path
vaccine <- sub("(.*vaccine_reevaluation\\/).*", "\\1", my.dir)

# load relevant libraries 
library(tidyverse)
library(readxl)
library(lmtest)
library(car)

# read in data 
imr = read_xlsx(paste0(vaccine, "data/UNIGME-2020-Country-Sex-specific_U5MR-CMR-and-IMR-2.xlsx"), sheet = 2)
summary = read_xlsx(paste0(vaccine, "data/data HDI health care and gini.xlsx"), sheet = "summary", skip = 1)
gini = read_xlsx(paste0(vaccine, "data/data HDI health care and gini.xlsx"), sheet = "gini index")
haq = read_xlsx(paste0(vaccine, "data/data HDI health care and gini.xlsx"), sheet = "HAQ index 2010")
imr_dose = read.csv(paste0(vaccine, "data/Figure_1_Data.csv"))

# clean up IMR data structure 
imr = imr %>% rename('Country_code' = 'Child Mortality Estimates', 'Country' = 2, 'Uncertainty_bounds' = 3, '1990' = 4, 
                                       '1991' = 5, '1992' = 6, '1993' = 7, '1994' = 8, '1995' = 9, '1996' = 10, '1997' = 11, '1998' = 12, '1999' = 13, 
                                       '2000' = 14, '2001' = 15, '2002' = 16, '2003' = 17, '2004' = 18, '2005' = 19, '2006' = 20, 
                                       '2007' = 21, '2008' = 22, '2009' = 23, '2010' = 24, '2011' = 25, '2012' = 26, '2013' = 27,
                                       '2014' = 28, '2015' = 29, '2016' = 30, '2017' = 31, '2018' = 32, '2019' = 33, 
                                       'f1990' = 34, 'f1991' = 35, 'f1992' = 36, 'f1993' = 37, 'f1994' = 38, 'f1995' = 39, 'f1996' = 40, 
                                       'f1997' = 41, 'f1998' = 42, 'f1999' = 43, 'f2000' = 44, 'f2001' = 45, 'f2002' = 46, 'f2003' = 47, 
                                       'f2004' = 48, 'f2005' = 49, 'f2006' = 50, 'f2007'= 51, 'f2008' = 52, 'f2009' = 53, 'f2010' = 54, 
                                       'f2011' = 55, 'f2012' = 56, 'f2013' = 57, 'f2014' = 58, 'f2015' = 59, 'f2016' = 60, 'f2017' = 61, 
                                       'f2018' = 62, 'f2019' = 63) 
imr_2010 = filter(imr, Uncertainty_bounds == 'Median') %>% 
  select(Country_code, Country, '2010', 'f2010') %>%
  rename("m2010" = "2010")

# calculate mean IMR (average male and female) for 2010
imr_2010$mean_imr_2010 = rowMeans(imr_2010[,3:4])

# summary data - add in country codes 
summary = left_join(summary, haq %>% select(Entity, Code), by = c("...1" = "Entity"))
summary = left_join(summary, imr_2010 %>% select(Country, Country_code), by = c("...1" = "Country"))
summary$Code[is.na(summary$Code)] = summary$Country_code[is.na(summary$Code)]
summary = subset(summary, select = -Country_code)
summary$Code[summary$...1 == "Liechtenstein"] = "LIE"
summary$Code[summary$...1 == "Hong Kong"] = "HKG"

# dose data - add in country codes 
imr_dose = left_join(imr_dose, haq %>% select(Entity, Code), by = c("Country" = "Entity"))
imr_dose = left_join(imr_dose, imr_2010 %>% select(Country, Country_code), by = c("Country" = "Country"))
imr_dose$Code[is.na(imr_dose$Code)] = imr_dose$Country_code[is.na(imr_dose$Code)]
imr_dose = subset(imr_dose, select = -Country_code)

# dose data - specific countries 
imr_dose$Code[imr_dose$Country == "Korea, South"] = "KOR"
imr_dose$Code[imr_dose$Country == "Liechtenstein"] = "LIE"
imr_dose$Code[imr_dose$Country == "Hong Kong"] = "HKG"
imr_dose$Code[imr_dose$Country == "Russia"] = "RUS"

##### Individual covariates by imr #####
# join imr with haq data
#haq_imr = left_join(haq, imr_2010 %>% select(Country, Country_code, mean_imr_2010), 
 #                   by = c("Code" = "Country_code"))

# join imr dose data with haq data 
haq_imr = left_join(haq, imr_dose %>% select(Country, Code, cia.gov_IMR), 
                    by = c("Code" = "Code"))

# plot - haq by IMR
haq_imr %>% 
  ggplot(aes(x= `HAQ Index (IHME (2017))`, y = cia.gov_IMR)) + 
  geom_point(stat='identity') + theme_classic()  + 
  labs(y = "Mean IMR 2010")
ggsave("~/Downloads/haq_imr.png", height = 5, width = 5)

# join imr with gini index data 
#gini_imr = left_join(gini %>% filter(!is.na(`2009 most recent value`)), imr_2010 %>% select(Country, Country_code, mean_imr_2010), 
 #                    by = c("Country Code" = "Country_code"))

gini_imr = left_join(gini %>% filter(!is.na(`2009 most recent value`)), imr_dose %>% select(Country, Code, cia.gov_IMR),
                     by = c("Country Code" = "Code"))

# plot - gini by IMR
gini_imr %>% 
  ggplot(aes(x= `2009 most recent value`, y = cia.gov_IMR)) + 
  geom_point(stat='identity') + theme_classic() 
ggsave("~/Downloads/gini_imr.png", height = 5, width = 5)

# t-test - top 1/2 & bottom 1/2 of gini index 
gini_top = gini_imr %>% 
  filter(`2009 most recent value` > 36.5)
gini_bottom = gini_imr %>% 
  filter(`2009 most recent value` <= 36.5)

t.test(gini_top$mean_imr_2010, gini_bottom$mean_imr_2010)


##### Individual covariates by imr - Only highly/very highly developed countries #####

# join imr with summary data 
#sum_imr = left_join(summary, imr_2010 %>% select(Country_code, mean_imr_2010), by = c("Code" = "Country_code"))
sum_imr = left_join(summary, imr_dose %>% select(Country, Code, cia.gov_IMR), 
                    by = c("Code" = "Code"))

# plot - HDI 2009 by IMR 
sum_imr %>% 
  ggplot(aes(x=`HDI 2009`, y = cia.gov_IMR)) + 
  geom_point(stat='identity') + theme_classic() 
ggsave("~/Downloads/hdi_sum_imr.png", height = 5, width = 5)

# plot - gini by IMR 
sum_imr %>% 
  ggplot(aes(x= `Gini index 2009 or earlier (income inequality)`, y = cia.gov_IMR)) + 
  geom_point(stat='identity') + theme_classic() 
ggsave("~/Downloads/gini_sum_imr.png", height = 5, width = 5)

# t-test - top 1/2 & bottom 1/2 of gini index 
gini_sum_top = sum_imr %>% 
  filter(`Gini index 2009 or earlier (income inequality)` >= 33.8)
gini_sum_bottom = sum_imr %>% 
  filter(`Gini index 2009 or earlier (income inequality)` < 33.8)

t.test(gini_sum_top$mean_imr_2010, gini_sum_bottom$mean_imr_2010)

# plot - haq by IMR 
sum_imr %>% 
  ggplot(aes(x = `Healthcare access and quality index 2010`, y = cia.gov_IMR)) +
  geom_point(stat='identity') + theme_classic()
ggsave("~/Downloads/haq_sum_imr.png", height = 5, width = 5)

# plot - universal healthcare by IMR 
sum_imr %>% 
  filter(!is.na(`Universal Healthcare?`)) %>%
  ggplot(aes(x = `Universal Healthcare?`, y = cia.gov_IMR)) + 
  geom_point(stat='identity') + theme_classic()
ggsave("~/Downloads/healthcare_sum_imr.png", height = 5, width = 5)

# single payer dataset
sum_imr_single_payer = sum_imr %>% 
  filter(`Universal Healthcare?` == "Single Payer")

# NOT single payer dataset
sum_imr_other_payer = sum_imr %>% 
  filter(`Universal Healthcare?` != "Single Payer", 
         !is.na(`Universal Healthcare?`))

# t-test Universal Healthcare
t.test(sum_imr_single_payer$mean_imr_2010, sum_imr_other_payer$mean_imr_2010)


##### Multiple linear regression - calculated dose data #####
sum_imr_dose = left_join(sum_imr, imr_dose %>% select(Code, CalculatedDoseNum), by = c("Code" = "Code")) %>% 
  unique()

mlr_hdi = lm(cia.gov_IMR ~ `HDI 2009` + CalculatedDoseNum, data = sum_imr_dose)

summary(mlr_hdi)

mlr_all = lm(cia.gov_IMR ~ `HDI 2009` + `Gini index 2009 or earlier (income inequality)` + 
        `Healthcare access and quality index 2010` + CalculatedDoseNum, data = sum_imr_dose)

summary(mlr_all)

##### Check MLR assumptions ##### 
# MLR HDI
plot(mlr_hdi, 1) # 1_linear
durbinWatsonTest(mlr_hdi) # 2_independent
plot(mlr_hdi, 3) # 3_constant_variance
ncvTest(mlr_hdi) # 4_constant_variance_test


# MLR All
plot(mlr_all, 1) # 1_linear
durbinWatsonTest(mlr_all) # 2_independent
plot(mlr_all, 3) # 3_constant_variance
ncvTest(mlr_all) # 4_constant_variance_test

