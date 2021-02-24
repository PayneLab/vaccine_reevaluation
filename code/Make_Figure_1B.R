library(tidyverse)

data = read_csv("https://raw.githubusercontent.com/tkmika/BIO465_Vaccine/main/data/Figure_1B_Data.csv?token=AEB2B6DCNZB7HTJHEYRSBWLAGWJSO")

lm_result = lm(CalculatedDoseNum~cia.gov_IMR, data=data)
print(summary(lm_result))

predictions = predict.lm(lm_result, interval="predict")
plot_data = cbind(data, as_tibble(predictions))

overall_plot = ggplot(data, aes(x=CalculatedDoseNum, y=cia.gov_IMR)) +
  geom_point(size=2) +
  geom_smooth(method=lm, se=TRUE, color="black") +
  labs(x="Number of vaccine doses", y="Infant mortality rate\n(deaths/1000)") +
  theme_bw() +
  ggtitle("Figure 1b: Linear Regression of All the Available Data")
print(overall_plot)
