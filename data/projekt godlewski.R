#START
library(ggplot2)
library(scales)
library(tidyverse)
df_brand <- Car_sale_ads_cleaned_drive_transmission_imputed %>%
  count(Vehicle_brand, name = "n") %>%
  mutate(perc = n / sum(n)) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20)

ggplot(df_brand, aes(x = reorder(Vehicle_brand, n), y = n, fill = n)) +
  geom_col() +
  geom_text(aes(label = paste0(round(perc * 100, 1), "%")),
            hjust = -0.1, size = 4.5, fontface = "bold") +
  coord_flip() +
  scale_fill_gradient(low = "#6ECFF6", high = "#005B96") +
  labs(
    title = "TOP 20 najpopularniejszych marek samochodów",
    subtitle = "Z procentowym udziałem w całej bazie ogłoszeń",
    x = "Marka pojazdu",
    y = "Liczba modeli",
    fill = "Liczba"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 11),
    legend.position = "none"
  ) +
  expand_limits(y = max(df_brand$n) * 1.15)
#test