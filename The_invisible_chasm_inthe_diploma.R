# REQUIRED PACKAGES
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("scales")
install.packages("ggrepel")
install.packages("stringr")
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(ggrepel)
library(stringr)


# DATA
all_ages <- read.csv("all-ages.csv")

#COLORS
category_colors <- c(
  "Engineering" = "#E69F00",
  "Business" = "#56B4E9",
  "Physical Sciences" = "#6699CC",
  "Biology & Life Science" = "#A6D854",
  "Health" = "#0072B2",
  "Social Science" = "#D55E00",
  "Arts" = "#CC79A7",
  "Humanities & Liberal Arts" = "#999999",
  "Education" = "#A6761D",
  "Industrial Arts & Consumer Services" = "#1B9E77",
  "Psychology & Social Work" = "#7570B3",
  "Law & Public Policy" = "#E7298A",
  "Computers & Mathematics" = "#66A61E",
  "Agriculture & Natural Resources" = "#F0E442",
  "Interdisciplinary" = "#FFD92F",
  "Communications & Journalism" = "grey"
)

# FOR THE COLORS
all_ages$Major_category <- factor(
  all_ages$Major_category,
  levels = names(category_colors)
)

# BAR CHARTS
lower_unemp <- all_ages %>%
  arrange(Unemployment_rate) %>%
  slice_head(n = 15)

ggplot(lower_unemp,
       aes(x = Unemployment_rate,
           y = reorder(Major, Unemployment_rate),
           fill = Major_category)) +
  
  geom_col() +
  
  geom_text(aes(label = percent(Unemployment_rate, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = category_colors, drop = TRUE)+
  
  labs(
    title = "Majors with the Lowest Unemployment Rates",
    x = "Unemployment Rate",
    y = "",
    fill = "Major Category"
  ) + 
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.box = "horizontal"
  ) +
  guides(
    fill = guide_legend(ncol = 3)
  )



highest_unemp <- all_ages %>%
  
  arrange(desc(Unemployment_rate)) %>%
  slice_head(n = 15)

ggplot(highest_unemp,
       aes(x = Unemployment_rate,
           y = reorder(Major, Unemployment_rate),
           fill = Major_category)) +
  
  geom_col() +
  
  geom_text(aes(label = percent(Unemployment_rate, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = category_colors,
                    drop = TRUE,
                    na.translate = FALSE) +
  
  labs(
    title = "Majors with the Highest Unemployment Rates",
    x = "Unemployment Rate",
    y = "",
    fill = "Major Category"
  ) +
  
  theme_minimal() +
  
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom"
  ) +
  guides(
    fill = guide_legend(ncol = 3) )


highest_earn <- all_ages %>%
  arrange(desc(Median)) %>%
  slice_head(n = 15)

ggplot(highest_earn,
       aes(x = Median,
           y = reorder(Major, Median),
           fill = Major_category)) +
  
  geom_col() +
  
  geom_text(aes(label = dollar(Median)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = dollar_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = category_colors, drop = TRUE) +
  
  
  labs(
    title = "Majors with the Highest Median Earnings",
    x = "Median Earnings (USD)",
    y = "",
    fill = "Major Category"
  ) +
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.box = "horizontal"
  ) +
  guides(
    fill = guide_legend(ncol = 3)
  )

lowest_earn <- all_ages %>%
  arrange(Median) %>%
  slice_head(n = 15)

ggplot(lowest_earn,
       aes(x = Median,
           y = reorder(Major, Median),
           fill = Major_category)) +
  
  geom_col() +
  
  geom_text(aes(label = dollar(Median)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(
    labels = dollar_format(),
    limits = c(0, 125000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_fill_manual(values = category_colors) +
  
  labs(
    title = "Majors with the Lowest Median Earnings",
    x = "Median Earnings (USD)",
    y = "",
    fill = "Major Category"
  ) +
  
  theme_minimal() +
  
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom"
  ) +
  
  guides(
    fill = guide_legend(ncol = 3)
  )
```

# VIOLIN GRAPH (This graph provides extra insight into STEM categories (not included in final poster due to space))
stem_categories <- c(
  "Engineering",
  "Computers & Mathematics",
  "Physical Sciences",
  "Biology & Life Science"
)

stem_data <- all_ages %>%
  filter(!is.na(Major_category)) %>%
  mutate(
    STEM_group = ifelse(
      Major_category %in% stem_categories,
      "STEM",
      "Non-STEM"
    )
  )

ggplot(stem_data,
       aes(x = STEM_group,
           y = Median,
           fill = STEM_group)) +
  
  geom_violin(
    alpha = 0.8,
    trim = FALSE
  ) +
  
  geom_boxplot(
    width = 0.15,
    fill = "white",
    outlier.shape = NA
  ) +
  
  scale_fill_manual(
    values = c(
      "STEM" = "#E69F00",
      "Non-STEM" = "#999999"
    )
  ) +
  
  scale_y_continuous(
    labels = scales::dollar_format()
  ) +
  
  labs(
    title = "Do STEM Majors Provide Better Earnings Outcomes?",
    subtitle = "Distribution of median earnings across STEM and non-STEM majors",
    x = "",
    y = "Median Earnings (USD)"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

# SCATTER PLOT
all_ages <- all_ages %>%
  mutate(
    score = scale(Median) - scale(Unemployment_rate)
  )

important_majors <- bind_rows(
  
  # Highest earn 3
  all_ages %>% arrange(desc(Median)) %>% slice_head(n = 3),
  
  # lowest unemployed 3
  all_ages %>% arrange(Unemployment_rate) %>% slice_head(n = 3),
  
  # lowest unemployed 2
  all_ages %>% arrange(desc(Unemployment_rate)) %>% slice_head(n = 2),
  
  # better balance (score)
  all_ages %>% arrange(desc(score)) %>% slice_head(n = 2)
  
) %>%
  distinct(Major, .keep_all = TRUE)



ggplot(all_ages,
       aes(x = Unemployment_rate,
           y = Median)) +
  
  geom_point(
    color = "grey80",
    alpha = 0.5,
    size = 2
  ) +
  
  geom_point(
    data = important_majors,
    aes(color = Major_category),
    size = 3
  ) +
  
  geom_text_repel(
    data = important_majors,
    aes(label = stringr::str_to_title(Major), color = Major_category),
    size = 2.5,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.6,
    
    segment.color = "grey60",
    segment.size = 0.5,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = category_colors,
    drop = TRUE
  ) +
  
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title = "Earnings vs Unemployment Risk by Major",
    subtitle = "Highlighted majors represent extreme and optimal labor-market outcomes",
    x = "Unemployment Rate",
    y = "Median Earnings ($)",
    color = "Major Category"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.margin = margin(20, 40, 20, 20)
  ) +
  guides(
    color = guide_legend(ncol = 3)
  )

# DOWNLOAD PHOTO

lowerunemp<- ggplot(lower_unemp,
                    aes(x = Unemployment_rate,
                        y = reorder(Major, Unemployment_rate),
                        fill = Major_category)) +
  
  
  
  
  geom_col() +
  
  geom_text(aes(label = percent(Unemployment_rate, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = category_colors, drop = TRUE)+
  
  labs(
    title = "Majors with the Lowest Unemployment Rates",
    x = "Unemployment Rate",
    y = "",
    fill = "Major Category"
  ) + 
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.box = "horizontal"
  ) +
  guides(
    fill = guide_legend(ncol = 3)
  )

ggsave(
  "lunemp.png",
  plot = lowerunemp,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 300
)

highunemp <- ggplot(highest_unemp,
                    aes(x = Unemployment_rate,
                        y = reorder(Major, Unemployment_rate),
                        fill = Major_category)) +
  
  geom_col() +
  
  geom_text(aes(label = percent(Unemployment_rate, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = category_colors,
                    drop = TRUE,
                    na.translate = FALSE) +
  
  labs(
    title = "Majors with the Highest Unemployment Rates",
    x = "Unemployment Rate",
    y = "",
    fill = "Major Category"
  ) +
  
  theme_minimal() +
  
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom"
  ) +
  guides(
    fill = guide_legend(ncol = 3) )

ggsave(
  "hunemp.png",
  plot = highunemp,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 300
)

hearn <- ggplot(highest_earn,
                aes(x = Median,
                    y = reorder(Major, Median),
                    fill = Major_category)) +
  
  geom_col() +
  
  geom_text(aes(label = dollar(Median)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = dollar_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = category_colors, drop = TRUE) +
  
  
  labs(
    title = "Majors with the Highest Median Earnings",
    x = "Median Earnings (USD)",
    y = "",
    fill = "Major Category"
  ) +
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.box = "horizontal"
  ) +
  guides(
    fill = guide_legend(ncol = 3)
  )

ggsave(
  "hearn.png",
  plot = hearn,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 300
)

learn <- ggplot(lowest_earn,
                aes(x = Median,
                    y = reorder(Major, Median),
                    fill = Major_category)) +
  
  geom_col() +
  
  geom_text(aes(label = dollar(Median)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(
    labels = dollar_format(),
    limits = c(0, 125000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_fill_manual(values = category_colors) +
  
  labs(
    title = "Majors with the Lowest Median Earnings",
    x = "Median Earnings (USD)",
    y = "",
    fill = "Major Category"
  ) +
  
  theme_minimal() +
  
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom"
  ) +
  
  guides(
    fill = guide_legend(ncol = 3) )

ggsave(
  "learn.png",
  plot = learn,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 300
)

stem <- ggplot(stem_data,
               aes(x = STEM_group,
                   y = Median,
                   fill = STEM_group)) +
  
  geom_violin(
    alpha = 0.8,
    trim = FALSE
  ) +
  
  geom_boxplot(
    width = 0.15,
    fill = "white",
    outlier.shape = NA
  ) +
  
  scale_fill_manual(
    values = c(
      "STEM" = "#E69F00",
      "Non-STEM" = "#999999"
    )
  ) +
  
  scale_y_continuous(
    labels = scales::dollar_format()
  ) +
  
  labs(
    title = "Do STEM Majors Provide Better Earnings Outcomes?",
    subtitle = "Distribution of median earnings across STEM and non-STEM majors",
    x = "",
    y = "Median Earnings (USD)"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )


ggsave(
  "stem.png",
  stem,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 300,
  bg = "white"
)

scatter <- ggplot(all_ages,
                  aes(x = Unemployment_rate,
                      y = Median)) +
  
  geom_point(
    color = "grey80",
    alpha = 0.5,
    size = 2
  ) +
  
  geom_point(
    data = important_majors,
    aes(color = Major_category),
    size = 3
  ) +
  
  geom_text_repel(
    data = important_majors,
    aes(label = stringr::str_to_title(Major), color = Major_category),
    size = 2.5,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.6,
    
    segment.color = "grey60",
    segment.size = 0.5,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = category_colors,
    drop = TRUE
  ) +
  
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title = "Earnings vs Unemployment Risk by Major",
    subtitle = "Highlighted majors represent extreme and optimal labor-market outcomes",
    x = "Unemployment Rate",
    y = "Median Earnings ($)",
    color = "Major Category"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.margin = margin(20, 40, 20, 20)
  ) +
  guides(
    color = guide_legend(ncol = 3)
  )

ggsave(
  "scatter.png",
  plot = scatter,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 300
)