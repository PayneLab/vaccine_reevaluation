library(tidyverse)
library(cowplot)

df = read_tsv("./GLOBAL_DATAFLOW_2010-2020.txt")

# this includes several rows that are about continents like 'Africa' or other stuff like "Australia and New Zealand"
#and I'm ONLY interested in the country stuff

non_countries = c("Africa", "Americas", "Australia and New Zealand", "Central Asia", "Central and Southern Asia",
                 "East Asia and Pacific", "Eastern Asia", "Eastern Europe and Central Asia",
                 "Eastern Mediterranean", "Eastern and South-Eastern Asia", "Eastern and Southern Africa", "Europe",
                 "Europe and Central Asia", "Europe and Northern America", "Landlocked developing countries (LLDCs)",
                 "Latin America & the Caribbean", "Latin America and Caribbean", "Least developed countries (LDCs)",
                 "Middle East and North Africa", "North America", "Northern Africa", "Northern Africa and Western Asia",
                 "Oceania", "Oceania excluding Australia and New Zealand", "Small Island Developing States (SIDS)",
                 "South Asia", "South-Eastern Asia", "Southeast Asia", "Southern Asia",
                 "UNICEF reporting regions - Global", "West and Central Africa", "Western Asia", "Western Europe",
                 "Western Pacific", "World", "World Bank (high income)", "World Bank (low income)",
                 "World Bank (lower middle income)", "World Bank (upper middle income)",
                 "World Health Organisation regions - Global", "sub-Saharan Africa")

# select for infant mortality data 
IMR_df = df %>% filter(Indicator == "Infant mortality rate" & Sex == "Total") 
# remove non-countries 
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  IMR_df = IMR_df[IMR_df$`Geographic area` != area,]
}

#select for child mortality data 
CMR_df = df %>% filter(Indicator == "Child mortality rate (aged 1-4 years)" & Sex == "Total")
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  CMR_df = CMR_df[CMR_df$`Geographic area` != area,]
}

# MMR 
mmr_str = "Percentage of surviving infants who received the first dose of measles-containing vaccine"
mmr_df = df %>% filter(Indicator == mmr_str)
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  mmr_df = mmr_df[mmr_df$`Geographic area` != area,]
}

# DTP
dtp_str = "Percentage of surviving infants who received the third dose of DTP-containing vaccine"
dtp_df = df %>% filter(Indicator == dtp_str)
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  dtp_df = dtp_df[dtp_df$`Geographic area` != area,]
}

# Polio
polio_str = "Percentage of surviving infants who received the third dose of inactivated polio-containing vaccine"
polio_df = df %>% filter(Indicator == polio_str)
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  polio_df = polio_df[polio_df$`Geographic area` != area,]
}

# HIB
hib_str = "Percentage of surviving infants who received the third dose of Hib-containing vaccine"
hib_df = df %>% filter(Indicator == hib_str)
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  hib_df = hib_df[hib_df$`Geographic area` != area,]
}

# HEP B
hepb_str = "Percentage of surviving infants who received the third dose of hep B-containing vaccine"
hepb_df = df %>% filter(Indicator == hepb_str)
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  hepb_df = hepb_df[hepb_df$`Geographic area` != area,]
}

# PCV 
pcv_str = "Percentage of surviving infants who received the third dose of pneumococcal conjugate-containing vaccine (PCV)"
pcv_df = df %>% filter(Indicator == pcv_str)
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  pcv_df = pcv_df[pcv_df$`Geographic area` != area,]
}

# Rotavirus 
rota_str = "Percentage of surviving infants who received the last dose of rotavirus-containing vaccine (2nd or 3rd dose depending on vaccine used)"
rota_df = df %>% filter(Indicator == rota_str)
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  rota_df = rota_df[rota_df$`Geographic area` != area,]
}

# TB 
tb_str = "Percentage of live births who received bacille Calmette-Guerin (vaccine against tuberculosis)"
tb_df = df %>% filter(Indicator == tb_str)
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  tb_df = tb_df[tb_df$`Geographic area` != area,]
}

# Rubella
rubella_str = "Percentage of surviving infants who received the first dose of rubella-containing vaccine"
rubella_df = df %>% filter(Indicator == rubella_str)
for (i in 1:length(non_countries)) {
  area = non_countries[i]
  rubella_df = rubella_df[rubella_df$`Geographic area` != area,]
}


# Downsample and rename 
imr_slim = IMR_df %>% select(`Geographic area`, OBS_VALUE)
imr_slim = imr_slim %>% rename("IMR" = OBS_VALUE)

mmr_slim = mmr_df %>% select(`Geographic area`, OBS_VALUE)
mmr_slim = mmr_slim %>% rename("measles" = OBS_VALUE)

dtp_slim = dtp_df %>% select(`Geographic area`, OBS_VALUE)
dtp_slim = dtp_slim %>% rename("dtp" = OBS_VALUE)

polio_slim = polio_df %>% select(`Geographic area`, OBS_VALUE)
polio_slim = polio_slim %>% rename("polio" = OBS_VALUE)

hib_slim = hib_df %>% select(`Geographic area`, OBS_VALUE) 
hib_slim = hib_slim %>% rename("hib" = OBS_VALUE)

hepb_slim = hepb_df %>% select(`Geographic area`, OBS_VALUE) 
hepb_slim = hepb_slim %>% rename("hepb" = OBS_VALUE)

pcv_slim = pcv_df %>% select(`Geographic area`, OBS_VALUE)
pcv_slim = pcv_slim %>% rename("pcv" = OBS_VALUE)

rota_slim = rota_df %>% select(`Geographic area`, OBS_VALUE)
rota_slim = rota_slim %>% rename("rotavirus" = OBS_VALUE)

rubella_slim = rubella_df %>% select(`Geographic area`, OBS_VALUE)
rubella_slim = rubella_slim %>% rename("rubella" = OBS_VALUE)

tb_slim = tb_df %>% select(`Geographic area`, OBS_VALUE)
tb_slim = tb_slim %>% rename("tb" = OBS_VALUE)


imr_mmr = left_join(mmr_slim, imr_slim, by = c("Geographic area" = "Geographic area"))

imr_mmr_plot = ggplot(data = imr_mmr, aes(x = measles, y = IMR)) + 
  geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = 'lm') + 
  labs(x = "Measles vaccination rate", y = "Infant mortality rate")
print(imr_mmr_plot)

imr_dtp = left_join(dtp_slim, imr_slim, by = c("Geographic area" = "Geographic area"))

imr_dtp_plot = ggplot(data = imr_dtp, aes(x = dtp, y = IMR)) + 
  geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = 'lm') + 
  labs(x = "DTP vaccination rate", y = "Infant mortality rate")
print(imr_dtp_plot)

imr_polio = left_join(polio_slim, imr_slim, by = c("Geographic area" = "Geographic area"))

imr_polio_plot = ggplot(data = imr_polio, aes(x = polio, y = IMR)) + 
  geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = 'lm') + 
  labs(x = "Polio vaccination rate", y = "Infant mortality rate")
print(imr_polio_plot)

imr_hib = left_join(hib_slim, imr_slim, by =c("Geographic area" = "Geographic area"))

imr_hib_plot = ggplot(data = imr_hib, aes(x = hib, y = IMR)) + 
  geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = 'lm') + 
  labs(x = "Hib vaccination rate", y = "Infant mortality rate")
print(imr_hib_plot)

imr_hepb = left_join(hepb_slim, imr_slim, by = c("Geographic area" = "Geographic area"))

imr_hepb_plot = ggplot(data = imr_hepb, aes(x = hepb, y = IMR)) + 
  geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = 'lm') + 
  labs(x = "Hepatitis B vaccination rate", y = "Infant mortality rate")
print(imr_hepb_plot)

imr_pcv = left_join(pcv_slim, imr_slim, by = c("Geographic area" = "Geographic area"))

imr_pcv_plot = ggplot(data = imr_pcv, aes(x = pcv, y = IMR)) + 
  geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = 'lm') + 
  labs(x = "Pneumococcal conjugate vaccination rate", y = "Infant mortality rate")
print(imr_pcv_plot)

imr_rota = left_join(rota_slim, imr_slim, by = c("Geographic area" = "Geographic area")) 

imr_rota_plot = ggplot(data = imr_rota, aes(x = rotavirus, y = IMR)) + 
  geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = 'lm') + 
  labs(x = "Rotavirus vaccination rate", y = "Infant mortality rate")
print(imr_rota_plot)

imr_rubella = left_join(rubella_slim, imr_slim, by = c("Geographic area" = "Geographic area")) 

imr_rubella_plot = ggplot(data = imr_rubella, aes(x = rubella, y = IMR)) + 
  geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = 'lm') + 
  labs(x = "Rubella vaccination rate", y = "Infant mortality rate")
print(imr_rubella_plot)

imr_tb = left_join(tb_slim, imr_slim, by = c("Geographic area" = "Geographic area")) 

imr_tb_plot = ggplot(data = imr_tb, aes(x = tb, y = IMR)) + 
  geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = 'lm') + 
  labs(x = "Tuberculosis vaccination rate", y = "Infant mortality rate")
print(imr_tb_plot)

plot_grid(imr_dtp_plot, imr_hib_plot, imr_hepb_plot, imr_polio_plot, imr_rota_plot, imr_tb_plot, imr_pcv_plot, 
          imr_mmr_plot, labels = "AUTO")

grid = plot_grid(imr_dtp_plot, imr_hib_plot, imr_hepb_plot, imr_polio_plot, 
                 imr_rubella_plot, imr_rota_plot, imr_tb_plot, imr_pcv_plot, 
                 imr_mmr_plot)

