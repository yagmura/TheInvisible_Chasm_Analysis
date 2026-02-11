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

#Coloring 

main_color <- "#00607A"

#Drawing graphs

## Lowest Unemployment Rates graph

lower_unemp <- all_ages %>%
  arrange(Unemployment_rate) %>%
  slice_head(n = 10)

ggplot(lower_unemp,
       aes(x = Unemployment_rate,
           y = reorder(Major, Unemployment_rate),
           fill = Major_category)) +
  
  geom_col(fill = main_color, width = 0.7) +
  
  geom_text(aes(label = percent(Unemployment_rate, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(labels = function(x) str_to_title(x))+
  
  
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
                       y = reorder(Major, Unemployment_rate)
                   )) +
  
  geom_col(fill = main_color, width = 0.7) +
  
  geom_text(aes(label = percent(Unemployment_rate, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(labels = function(x) str_to_title(x))+
  
  
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
  "lounemp.png",
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
  
  geom_col(fill = main_color, width = 0.7) +
  
  geom_text(aes(label = percent(Unemployment_rate, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(labels = function(x) str_to_title(x))+
  
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
  
  geom_col(fill = main_color, width = 0.7) +
  
  geom_text(aes(label = percent(Unemployment_rate, accuracy = 0.1)),
            hjust = -0.1,
            size = 3) +
  
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(labels = function(x) str_to_title(x))+
  
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
  "hiunemp.png",
  plot = highunemp,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300
)

##Comparison of Median Earnings graph

avg_earnings <- mean(all_ages$Median, na.rm = TRUE)

hearn <- all_ages %>% arrange(desc(Median)) %>% slice_head(n = 10)
learn <- all_ages %>% arrange(Median) %>% slice_head(n = 10)

earn_compare <- bind_rows(hearn, learn) %>%
  mutate(
    diff_from_avg = Median - avg_earnings, 
    Major = fct_reorder(Major, diff_from_avg)
  )

ggplot(earn_compare, aes(x = diff_from_avg, y = Major)) +
  
  geom_col(aes(fill = diff_from_avg > 0), width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(
      label = scales::comma(Median, big.mark = ","),
      hjust = ifelse(diff_from_avg > 0, -0.1, 1.1)
    ),
    size = 3.5,
    color = "gray30"
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.8) +
  
  scale_fill_manual(values = c("TRUE" = main_color, "FALSE" = "gray70")) +
  
  scale_x_continuous(
    
    labels = function(x) paste0(round((x + avg_earnings) / 1000), "k"),
    expand = expansion(mult = 0.05) 
  ) +
  
  scale_y_discrete(labels = function(x) str_to_title(x)) +
  
  labs(
    title = "Income Disparity: Deviation from Average",
    
    x = "Median Earnings", 
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22, color = main_color),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, color = "gray60") 
  )

###Download the graph

earn <- ggplot(earn_compare, aes(x = diff_from_avg, y = Major)) +
  
  geom_col(aes(fill = diff_from_avg > 0), width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(
      label = scales::comma(Median, big.mark = ","),
      hjust = ifelse(diff_from_avg > 0, -0.1, 1.1)
    ),
    size = 3.5,
    color = "gray30"
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.8) +
  
  scale_fill_manual(values = c("TRUE" = main_color, "FALSE" = "gray70")) +
  
  scale_x_continuous(
    
    labels = function(x) paste0(round((x + avg_earnings) / 1000), "k"),
    expand = expansion(mult = 0.05) 
  ) +
  
  scale_y_discrete(labels = function(x) str_to_title(x)) +
  
  labs(
    title = "Income Disparity: Deviation from Average",
    
    x = "Median Earnings", 
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22, color = main_color),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, color = "gray60") 
  )

ggsave(
  "tearn.png",
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
    color = main_color,
    size = 4
  ) +
  
  geom_text_repel(
    data = important_majors,
    aes(label = stringr::str_to_title(Major)),
    color = main_color,
    size = 6,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.6,
    
    segment.color = "grey60",
    segment.size = 0.5,
    max.overlaps = Inf,
    show.legend = FALSE
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
    y = "Median Earnings ($)"
    
  ) +
  
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
    legend.position = "none",
    plot.margin = margin(20, 40, 20, 20)
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
    color = main_color,
    size = 4
  ) +
  
  geom_text_repel(
    data = important_majors,
    aes(label = stringr::str_to_title(Major)),
    color = main_color,
    size = 6,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.6,
    
    segment.color = "grey60",
    segment.size = 0.5,
    max.overlaps = Inf,
    show.legend = FALSE
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
    y = "Median Earnings ($)"
    
  ) +
  
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
    legend.position = "none",
    plot.margin = margin(20, 40, 20, 20)
  ) 

ggsave(
  "sscatter.png",
  plot = scatter,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300
)
    
    
