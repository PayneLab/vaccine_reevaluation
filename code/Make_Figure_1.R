library(tidyverse)

data_full = read_csv("../data/Figure_1_Data.csv")

data_full$UsedByOriginalStudy = factor(data_full$UsedByOriginalStudy)
data_full = mutate(data_full, UsedByOriginalStudy = fct_recode(UsedByOriginalStudy,
                                                               "Excluded from original study" = "0",
                                                               "Included in original study" = "1"))

data_plot = ggplot(data = data_full, aes(color=UsedByOriginalStudy, shape=UsedByOriginalStudy)) +
  geom_point(aes(x=CalculatedDoseNum, y=cia.gov_IMR), size=3) +
  geom_point(aes(x=OriginalDoseNum, y=Original_IMR), size=3) +
  labs(x="Country's recommended number of vaccine doses by age one", y="Infant mortality rate (deaths/1000)", color="Countries") +
  theme_bw(base_size=16) +
  scale_color_manual(name="Countries", values=c("red", "black")) +
  scale_shape_manual(name="Countries", values=c(19, 17)) +
  xlim(0,NA) +
  theme(legend.position=c(0.2,0.9), 
        legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))
print(data_plot)

# Number of Countries in the Two Groups
num_countries = nrow(data_full)
num_included_countries = length(which(data_full$UsedByOriginalStudy=="Included in original study"))
print(num_countries)
print(num_included_countries)

# F statistic, R^2 val, slope coefficients for linear regressions
lm_full_data_result = lm(CalculatedDoseNum~cia.gov_IMR, data=data_full)
print("Summary of linear regression for all available data:\n")
print(summary(lm_full_data_result))

data_original_study = read_csv("../data/2009_Data_Used_by_Original_Study.csv")
lm_originally_used_data_result = lm(OriginalDoseNum~Original_IMR, data=data_original_study)
print("Summary of linear regression for data included in original study:\n")
print(summary(lm_originally_used_data_result))

# Check linear regression assumptions 
#plot(lm_full_data_result, 1) # 1_linear
#durbinWatsonTest(lm_full_data_result) # 2_independent
#plot(lm_full_data_result, 3) # 3_constant_variance
#ncvTest(lm_full_data_result) # 4_constant_variance_test

#plot(lm_originally_used_data_result, 1) # 1_linear
#durbinWatsonTest(lm_originally_used_data_result) # 2_independent
#plot(lm_originally_used_data_result, 3) # 3_constant_variance
#ncvTest(lm_originally_used_data_result) # 4_constant_variance_test

# t test of coefficients 
library("lmtest")
library("sandwich")

coeftest(lm_full_data_result, vcov = vcovHC(lm_full_data_result, type = "HC0"))
coeftest(lm_originally_used_data_result, vcov = vcovHC(lm_originally_used_data_result, type = "HC0"))


