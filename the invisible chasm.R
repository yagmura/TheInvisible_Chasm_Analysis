# Installing and loading the required packages

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("scales")
#install.packages("ggrepel")
#install.packages("stringr") 
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(ggrepel)
library(stringr)

#You can find the data file separately within the repository and download it to run this code.

all_ages <- read.csv("all-ages.csv")

#Coloring according to Major Category

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

all_ages$Major_category <- factor(
  all_ages$Major_category,
  levels = names(category_colors)
)
#Drawing graphs

## Lowest Unemployment Rates graph

lower_unemp <- all_ages %>%
  arrange(Unemployment_rate) %>%
  slice_head(n = 10)

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
  scale_y_discrete(labels = function(x) str_to_title(x))+
  scale_fill_manual(values = category_colors, drop = TRUE)+
  
  labs(
    title = "Majors with the Lowest Unemployment Rates",
    x = "Unemployment Rate",
    y = "",
    fill = "Major Category"
  ) + 
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 24 ),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = c(1, -0.2),
        legend.box = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
  ) +
  guides(
    fill = guide_legend(ncol = 3)
  )

###Download the graph

lowerunemp<-ggplot(lower_unemp,
                   aes(x = Unemployment_rate,
                       y = reorder(Major, Unemployment_rate),
                       fill = Major_category)) +
  
  geom_col() +
  
  geom_text(aes(label = percent(Unemployment_rate, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(labels = function(x) str_to_title(x))+
  scale_fill_manual(values = category_colors, drop = TRUE)+
  
  labs(
    title = "Majors with the Lowest Unemployment Rates",
    x = "Unemployment Rate",
    y = "",
    fill = "Major Category"
  ) + 
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 24 ),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = c(1, -0.2),
        legend.box = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
  ) +
  guides(
    fill = guide_legend(ncol = 3)
  )

ggsave(
  "lunemp.png",
  plot = lowerunemp,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300
)

##Highest Unemployment Rates graph

highest_unemp <- all_ages %>%
  
  arrange(desc(Unemployment_rate)) %>%
  slice_head(n = 10)

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
  scale_y_discrete(labels = function(x) str_to_title(x))+
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
        plot.title = element_text(hjust = 0.5, face = "bold", size = 24 ),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = c(1, -0.2),
        legend.box = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
  ) +
  guides(
    fill = guide_legend(ncol = 3) )

###Download the graph

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
  scale_y_discrete(labels = function(x) str_to_title(x))+
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
        plot.title = element_text(hjust = 0.5, face = "bold", size = 24 ),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = c(1, -0.2),
        legend.box = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
  ) +
  guides(
    fill = guide_legend(ncol = 3) )

ggsave(
  "hunemp.png",
  plot = highunemp,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300
)

##Comparison of Median Earnings graph

hearn <- all_ages %>%
  arrange(desc(Median)) %>%
  slice_head(n = 10) %>%
  mutate(type = "High Earnings")

learn <- all_ages %>%
  arrange(Median) %>%
  slice_head(n = 10) %>%
  mutate(
    Median = -Median,
    type = "Low Earnings"
  )


earn_compare <- bind_rows(hearn, learn) %>%
  mutate(
    Major = fct_reorder(Major, Median)
  )

ggplot(
  earn_compare,
  aes(
    x = Median,
    y = Major,
    fill = Major_category
  )
) +
  geom_col(width = 0.7, show.legend = TRUE) +
  geom_text(
    aes(label = scales::dollar(Median),
        hjust = ifelse(Median > 0, -0.1, 1.1)),
    size = 5,
    show.legend = FALSE)+
  scale_x_continuous(
    labels = label_dollar(),
    limits = c(-125000, 125000))+
  scale_y_discrete(labels = function(x) str_to_title(x))+
  scale_fill_manual(values = category_colors,
                    drop = TRUE,
                    na.translate = FALSE) +
  
  labs(
    title = "Comparison of Majors with Highest and Lowest Median Earnings",
    subtitle = "Right: Top 10 highest-paying sections - Left: Bottom 10 lowest-paying sections",
    x = "",
    y = "",
    fill = "Major Category"
  ) +
  theme_minimal() +
  
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30 ),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = c(-0.5, -0.2),
        legend.box = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
        
  ) +
  guides(
    fill = guide_legend(ncol = 3) )

###Download the graph

earn <-ggplot(
  earn_compare,
  aes(
    x = Median,
    y = Major,
    fill = Major_category
  )
) +
  geom_col(width = 0.7, show.legend = TRUE) +
  geom_text(
    aes(label = scales::dollar(Median),
        hjust = ifelse(Median > 0, -0.1, 1.1)),
    size = 5,
    show.legend = FALSE)+
  scale_x_continuous(
    labels = label_dollar(),
    limits = c(-125000, 125000))+
  scale_y_discrete(labels = function(x) str_to_title(x))+
  scale_fill_manual(values = category_colors,
                    drop = TRUE,
                    na.translate = FALSE) +
  
  labs(
    title = "Comparison of Majors with Highest and Lowest Median Earnings",
    subtitle = "Right: Top 10 highest-paying sections - Left: Bottom 10 lowest-paying sections",
    x = "",
    y = "",
    fill = "Major Category"
  ) +
  theme_minimal() +
  
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30 ),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "bottom",
        legend.justification = c(-0.5, -0.2),
        legend.box = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
        
  ) +
  guides(
    fill = guide_legend(ncol = 3) )


ggsave(
  "earn.png",
  plot = earn,
  width = 75,
  height = 35,
  units = "cm",
  dpi = 300
)

##Earnings vs Unemployment Risks graph

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
    size = 3
  ) +
  
  geom_point(
    data = important_majors,
    aes(color = Major_category),
    size = 4
  ) +
  
  geom_text_repel(
    data = important_majors,
    aes(label = stringr::str_to_title(Major), color = Major_category),
    size = 6,
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
  
  theme_minimal(base_size = 20) +
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

###Download the graph
scatter <- ggplot(all_ages,
                  aes(x = Unemployment_rate,
                      y = Median)) +
  
  geom_point(
    color = "grey80",
    alpha = 0.5,
    size = 3
  ) +
  
  geom_point(
    data = important_majors,
    aes(color = Major_category),
    size = 4
  ) +
  
  geom_text_repel(
    data = important_majors,
    aes(label = stringr::str_to_title(Major), color = Major_category),
    size = 6,
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
  
  theme_minimal(base_size = 20) +
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
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300
)


