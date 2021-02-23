library(tidyverse)

data = read_csv("https://raw.githubusercontent.com/tkmika/BIO465_Vaccine/main/data/Figure_1A_Data.csv?token=AEB2B6FDTG2FYQXNS7IK7PDAGWJP6")

lm_result = lm(OriginalDoseNum~Original_IMR, data=data)
print(summary(lm_result))

predictions = predict.lm(lm_result, interval="predict")
plot_data = cbind(data, as_tibble(predictions))

original_plot = ggplot(data, aes(x=OriginalDoseNum, y=Original_IMR)) +
  geom_point(size=2) +
  geom_smooth(method=lm, se=TRUE, color="black") +
  labs(x="Number of vaccine doses", y="Infant mortality rate\n(deaths/1000)") +
  theme_bw() +
  ggtitle("Figure 1a: Linear Regression Matching the Original Study's Data")
print(original_plot)

