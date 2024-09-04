library(ggplot2)
library(tidyverse)
#read the csv file
col_df <- read.csv("data/raw/Cost_of_Living_Index_by_Country_2024.csv")

#check the class of the data
class(col_df)

#get the structure information
str(col_df)

#get the summary of each variable
summary(col_df)

#check new structure
str(col_df)

#check the top 50 countries
col_df_subset <- head(col_df, 50)

#plotting cost of living index for each country
#NOTE : re-factoring country column using unique(country) in order to preserve the level as it appears in the data.
#NOTE : Grouping is needed for different lines in geom_line plot
final_plot <- 
  col_df_subset %>% 
  mutate(Country = factor(Country, levels = unique(Country))) %>% 
  ggplot(mapping = aes(x = Country, y = Cost.of.Living.Index, colour = Country, group = 1)) +
  geom_point() +
  theme_bw() +
  geom_line(colour = "black", size = .5, alpha = 0.5) +
  scale_x_discrete(labels = label_wrap_gen(width = 5)) +
  theme(axis.title = element_text(size = 14), panel.grid.major.x = element_blank(), legend.position = "none", 
        plot.title = element_text(face = "bold", size = 20)) +
  labs(title = "COL Index across countries.",
       subtitle = "A visualization",
       x = "Country",
       y = "COL Index") 

#saving the plot
ggsave(filename = "images/country_col_index.jpg", plot = final_plot, height = 6, width = 8)

#TODO
# Fix X titles.
# Add values for each dot.
# Visualise difference ratio between index.