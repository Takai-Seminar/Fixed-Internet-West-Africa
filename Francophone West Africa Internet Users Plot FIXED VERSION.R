# ============================================
# Francophone West Africa Internet Users Plot
# FIXED VERSION
# ============================================

# 0. パッケージ
packages <- c("WDI", "dplyr", "ggplot2", "ggrepel")

installed <- packages %in% installed.packages()
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(WDI)
library(dplyr)
library(ggplot2)
library(ggrepel)

# 1. 国コード（ISO2）
countries <- c("BJ","BF","CI","GN","ML","MR","NE","SN","TG")

# 2. データ取得
df <- WDI(
  country = countries,
  indicator = "IT.NET.USER.ZS",
  start = 1995,
  end = 2024
)

# 3. 整形
df_clean <- df %>%
  filter(!is.na(IT.NET.USER.ZS)) %>%
  rename(
    country_name = country,
    internet = IT.NET.USER.ZS
  ) %>%
  select(country_name, year, internet)

# 4. ラベル用（最新年）
label_df <- df_clean %>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup()

# 5. グラフ
p <- ggplot(df_clean, aes(x = year, y = internet, group = country_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  geom_text_repel(
    data = label_df,
    aes(label = country_name),
    nudge_x = 1,
    direction = "y",
    hjust = 0,
    segment.color = "grey70"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = "Internet Users in Francophone West Africa",
    x = "Year",
    y = "Internet users (% of population)",
    caption = "Source: World Bank (WDI)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

print(p)

# 6. 保存
ggsave("internet_users_west_africa.png", p, width = 10, height = 6, dpi = 300)
ggsave("internet_users_west_africa.pdf", p, width = 10, height = 6)

# ============================================

