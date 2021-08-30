library(tidyverse)
library(readxl)

# IMR Data
imr_estimates = read_xlsx("../data/UNIGME-2020-Country-Sex-specific_U5MR-CMR-and-IMR-2.xlsx", sheet=2)
imr = tibble(imr_estimates) %>% rename('Country_code' = 'Child Mortality Estimates', 'Country' = 2, 'Uncertainty_bounds' = 3, '1990' = 4, 
                                       '1991' = 5, '1992' = 6, '1993' = 7, '1994' = 8, '1995' = 9, '1996' = 10, '1997' = 11, '1998' = 12, '1999' = 13, 
                                       '2000' = 14, '2001' = 15, '2002' = 16, '2003' = 17, '2004' = 18, '2005' = 19, '2006' = 20, 
                                       '2007' = 21, '2008' = 22, '2009' = 23, '2010' = 24, '2011' = 25, '2012' = 26, '2013' = 27,
                                       '2014' = 28, '2015' = 29, '2016' = 30, '2017' = 31, '2018' = 32, '2019' = 33, 
                                       'f1990' = 34, 'f1991' = 35, 'f1992' = 36, 'f1993' = 37, 'f1994' = 38, 'f1995' = 39, 'f1996' = 40, 
                                       'f1997' = 41, 'f1998' = 42, 'f1999' = 43, 'f2000' = 44, 'f2001' = 45, 'f2002' = 46, 'f2003' = 47, 
                                       'f2004' = 48, 'f2005' = 49, 'f2006' = 50, 'f2007'= 51, 'f2008' = 52, 'f2009' = 53, 'f2010' = 54, 
                                       'f2011' = 55, 'f2012' = 56, 'f2013' = 57, 'f2014' = 58, 'f2015' = 59, 'f2016' = 60, 'f2017' = 61, 
                                       'f2018' = 62, 'f2019' = 63) 
imr = imr[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),] 
imr = filter(imr, Uncertainty_bounds == 'Median')

# HebB Data
hepBdata = read_xls('../data/HepBdata.xls')
hepBdata = tibble(hepBdata) 
hepBdata = select(hepBdata, Country, '1993':'2019')

# Male IMR vs HepB %

male_imr = select(imr, 'Country', '1993':'2019')
imr_all = pivot_longer(male_imr, cols='1993':'2019', names_to = 'Year', values_to = 'IMR')
hepB_all = pivot_longer(hepBdata, cols='2019':'1993', names_to = 'Year', values_to = 'HepB')
all_data = left_join(hepB_all, imr_all, by = c('Country', 'Year')) %>% na.omit(all_data)

us = filter(all_data, Country == 'United States of America')
ggplot(us) + geom_text(aes(x = HepB, y = IMR, label = Year), hjust = 1.25, vjust = 1, check_overlap = TRUE) + geom_point(aes(x = HepB, y = IMR, label = Year)) +
  theme_bw(base_size = 16) + labs(x = "1-year-old children vaccinated for HepB (%)", y = 'Male Infant Mortality Rate (deaths/1000)') + 
  scale_x_continuous(expand = c(0,0), lim = c(0, 102)) + scale_y_continuous(expand = c(0,0), lim = c(4.74, 9.5))


# Female IMR vs HepB %
female_imr = select(imr, 'Country', 'f1993':'f2019')
female_imr = rename(female_imr, '1993'= 'f1993', '1994' = 'f1994', '1995' = 'f1995', '1996' = 'f1996', '1997' = 'f1997', 
                    '1998' = 'f1998', '1999' = 'f1999', '2000' = 'f2000', '2001' = 'f2001', '2002' = 'f2002', '2003' = 'f2003',
                    '2004' = 'f2004', '2005' = 'f2005', '2006'='f2006', '2007' = 'f2007', '2008' = 'f2008', '2009' = 'f2009', 
                    '2010' = 'f2010', '2011' = 'f2011', '2012' = 'f2012', '2013' = 'f2013', '2014' = 'f2014', '2015' = 'f2015', 
                    '2016' = 'f2016', '2017' = 'f2017', '2018' = 'f2018', '2019' = 'f2019')
female_imr_all = pivot_longer(female_imr, cols='1993':'2019', names_to = 'Year', values_to = 'IMR')
all_f = left_join(hepB_all, female_imr_all, by = c('Country', 'Year')) %>% na.omit(all_f)

usa = filter(all_f, Country == 'United States of America')
ggplot(usa) + geom_text(aes(x = HepB, y = IMR, label = Year), hjust = 1.25, vjust = 1, check_overlap = TRUE) + geom_point(aes(x = HepB, y = IMR, label = Year)) +
  theme_bw(base_size = 16) + labs(x = "1-year-old children vaccinated for HepB (%)", y = 'Female Infant Mortality Rate (deaths/1000)') + 
  scale_x_continuous(expand = c(0,0), lim = c(0, 102)) + scale_y_continuous(expand = c(0,0), lim = c(4.75, 9.5))

# Statistical Test
cor.test(us$HepB, us$IMR, method='spearman', exact = FALSE)
cor.test(usa$HepB, usa$IMR, method = 'spearman', exact = FALSE)

