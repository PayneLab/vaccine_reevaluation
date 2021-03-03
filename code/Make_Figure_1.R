library(tidyverse)

data_full = read_csv("https://raw.githubusercontent.com/tkmika/BIO465_Vaccine/main/data/Figure_1_Data.csv")

data_full$UsedByOriginalStudy = factor(data_full$UsedByOriginalStudy)
data_full = mutate(data_full, UsedByOriginalStudy = fct_recode(UsedByOriginalStudy,
                            "Excluded from original study" = "0",
                            "Included in original study" = "1"))

data_plot = ggplot(data = data_full) +
  geom_point(aes(x=CalculatedDoseNum, y=cia.gov_IMR, color=UsedByOriginalStudy), size=3) +
  geom_point(aes(x=OriginalDoseNum, y=Original_IMR, color=UsedByOriginalStudy), size=3) +
  labs(x="Country's recommended number of vaccine doses by age one", y="Infant mortality rate (deaths/1000)", color="Data Use") +
  theme_bw(base_size=16) +
  ggtitle("Figure 1: 2009 Vaccine Dose and IMR Data") +
  scale_color_manual(values=c("red", "black")) +
  xlim(0,NA) +
  theme(legend.position=c(0.2,0.9), legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))
print(data_plot)

# Number of Countries in the Two Groups
num_excluded_countries = nrow(data_full)
num_included_countries = length(which(data_full$UsedByOriginalStudy=="Included in original study"))
print(num_excluded_countries)
print(num_included_countries)

# F statistic, R^2 val, slope coefficients for linear regressions
lm_full_data_result = lm(CalculatedDoseNum~cia.gov_IMR, data=data_full)
print("Summary of linear regression for all available data:\n")
print(summary(lm_full_data_result))

lm_originally_used_data_result = lm(OriginalDoseNum~Original_IMR, data=data_full)
print("Summary of linear regression for data included in original study:\n")
print(summary(lm_originally_used_data_result))
