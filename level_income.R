library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(reshape2)
library(scales)
library(gridExtra)

# Subset with levels of learning 

Levels <- Stats_gender |>
  select(c(age, gender, year, p5_log_inc, p25_log_inc, p50_log_inc, p75_log_inc, p95_log_inc))

Level<- subset(Levels, age != "25-55")

Earning_level <- subset(Level, gender != "All genders")


# Young female subset 

female_inc_lev <- subset(Earning_level, gender == "Female")

male_inc_lev <-subset(Earning_level, gender == "Male")


young_female <- subset(female_inc_lev, age == "25-34")


# Maxi plot all together 

# Point graph 

ggplot(data = Earning_level, aes(x = year, y = p25_log_inc)) +
  geom_point(aes(color = "p25_log_inc")) +
  geom_point(aes(x = year, y = p50_log_inc, color = "p50_log_inc")) +
  geom_point(aes(x = year, y = p75_log_inc, color = "p75_log_inc")) +
  geom_point(aes(x = year, y = p95_log_inc, color = "p95_log_inc")) +
  facet_grid(rows = vars(gender), cols = vars(age)) +
  scale_color_manual(values = c("p25_log_inc" = "red", 
                                "p50_log_inc" = "blue", 
                                "p75_log_inc" = "green", 
                                "p95_log_inc" = "purple")) +
  labs(x = "Year", y = "Income level") +
  theme_economist()

# Plot only young womens

post_crisis_young_fem <- subset(young_female, year > 2000)


ggplot(data = post_crisis_young_fem, aes(x = year, y = p25_log_inc)) +
  geom_line(aes(color = "p25_log_inc")) +
  geom_line(aes(x = year, y = p95_log_inc, color = "p95_log_inc")) +
  facet_grid(rows = vars(age), cols = vars(gender)) +
  scale_color_manual(values = c("p25_log_inc" = "red", 
                                "p95_log_inc" = "purple")) +
  labs(x = "Year", y = "Income level") +
  theme_economist()

ggplot(data = Earning_level, aes(x = year, y = p25_log_inc, color = p25_log_inc)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red", name = "Log of Annual Earnings (EUR)") +
  labs(title = "Income Levels by Age and Gender",
       subtitle = "Log of Annual Earnings (EUR) by Percentile, Gender, and Age Group",
       x = "Year of Survey",
       y = "Log of Annual Earnings (EUR)") +
  facet_grid(rows = vars(gender), cols = vars(age)) +
  theme_economist() +
  theme(legend.position = "bottom")


ggplot(data = Earning_level, aes(x = year)) +
  geom_point(aes(y = `p25_log_inc`), color = "#e41a1c") +
  geom_point(aes(y = `p50_log_inc`), color = "#377eb8") +
  geom_point(aes(y = `p75_log_inc`), color = "#4daf4a") +
  geom_point(aes(y = `p95_log_inc`), color = "#984ea3") +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), 
                     name = "Percentile") +
  labs(title = "Income Levels by Age and Gender",
       subtitle = "Log of Annual Earnings (EUR) by Percentile, Gender, and Age Group",
       x = "Year of Survey",
       y = "Log of Annual Earnings EUR") +
  facet_grid(rows = vars(gender), cols = vars(age)) +
  theme_economist() +
  theme(legend.position = "bottom")


# Violin Plots

ggplot(data = Earning_level, aes(x = gender))+
  geom_violin(aes(y = p25_log_inc, fill = age)) +
  geom_violin(aes(y = p50_log_inc, fill = age)) +
  geom_violin(aes(y = p75_log_inc, fill = age)) +
  geom_violin(aes(y = p95_log_inc, fill = age)) +
  scale_fill_viridis_d() + 
  facet_grid(rows = vars(age), cols = vars(gender))+
  labs(title = "Distribution of Annual Earnings by Gender and Age Group",
        subtitle = "Log of Annual Earnings (EUR) by Age Group and Gender",
        x = "Gender",
        y = "Log of Annual Earnings EUR") +
  theme_economist()


# Generate IQR for standard error bars 

Earning_Levels <- enl_level %>%
  group_by(year, gender, age) %>%
  mutate(iqr_p25_log_inc = IQR(p25_log_inc),
         iqr_p50_log_inc = IQR(p50_log_inc),
         iqr_p75_log_inc = IQR(p75_log_inc),
         iqr_p95_log_inc = IQR(p95_log_inc))

# Graph with error bars

ggplot(data = enl_level, aes(x = year)) +
  geom_point(aes(y = p25_log_inc, color = "25th Percentile"), size = 1, shape = 21, fill = "lightgreen") +
  geom_errorbar(aes(ymin = p25_log_inc - iqr_p25_log_inc, ymax = p25_log_inc + iqr_p25_log_inc, color = "25th Percentile"), width = 0) +
  geom_point(aes(y = p50_log_inc, color = "50th Percentile"), size = 1, shape = 21, fill = "pink") +
  geom_errorbar(aes(ymin = p50_log_inc - iqr_p50_log_inc, ymax = p50_log_inc + iqr_p50_log_inc, color = "50th Percentile"), width = 0) +
  geom_point(aes(y = p75_log_inc, color = "75th Percentile"), size = 1, shape = 21, fill = "lightblue") +
  geom_errorbar(aes(ymin = p75_log_inc - iqr_p75_log_inc, ymax = p75_log_inc + iqr_p75_log_inc, color = "75th Percentile"), width = 0) +
  geom_point(aes(y = p95_log_inc, color = "95th Percentile"), size = 1, shape = 21, fill = "yellow") +
  geom_errorbar(aes(ymin = p95_log_inc - iqr_p95_log_inc, ymax = p95_log_inc + iqr_p95_log_inc, color = "95th Percentile"), width = 0) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  labs(title = "Income Levels by Age and Gender",
       subtitle = "Log of Annual Earnings (EUR) by Percentile, Gender, and Age Group",
       x = "Year of Survey",
       y = "Log of Annual Earnings EUR") +
  facet_grid(rows = vars(age), cols = vars(gender))


enlarged_stats <- Stats_gender |>
  select(c(age, gender, year, p5_log_inc, p25_log_inc, p50_log_inc, p75_log_inc, p95_log_inc, std_log_inc))

enl_lev<- subset(enlarged_stats, age != "25-55")

enl_level <- subset(enl_lev, gender != "All genders")


ggplot(data = Earning_Levels, aes(x = as.numeric(year))) +
  geom_point(aes(y = p25_log_inc, color = "25th Percentile"), size = 3, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = p25_log_inc - std_log_inc, ymax = p25_log_inc + std_log_inc, color = "25th Percentile"), width = 0.2) +
  geom_point(aes(y = p50_log_inc, color = "50th Percentile"), size = 3, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = p50_log_inc - std_log_inc, ymax = p50_log_inc + std_log_inc, color = "50th Percentile"), width = 0.2) +
  geom_point(aes(y = p75_log_inc, color = "75th Percentile"), size = 3, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = p75_log_inc - std_log_inc, ymax = p75_log_inc + std_log_inc, color = "75th Percentile"), width = 0.2) +
  geom_point(aes(y = p95_log_inc, color = "95th Percentile"), size = 3, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = p95_log_inc - std_log_inc, ymax = p95_log_inc + std_log_inc, color = "95th Percentile"), width = 0.2) +
  scale_color_gradient(low = "#e41a1c", high = "#984ea3", name = "Percentile") +
  facet_grid(rows = vars(age), cols = vars(gender)) +
  labs(title = "Income Levels by Age and Gender",
       subtitle = "Log of Annual Earnings (EUR) by Percentile, Gender, and Age Group",
       x = "Year of Survey",
       y = "Log of Annual Earnings (EUR)") 
  


ggplot(data = Earning_Levels, aes(x = as.numeric(year))) +
  geom_point(aes(y = p25_log_inc, color = "25th Percentile"), size = 1, shape = 22, fill = "white") +
  geom_errorbar(aes(ymin = p25_log_inc - std_log_inc, ymax = p25_log_inc + std_log_inc, color = "25th Percentile"), width = 0.2) +
  geom_point(aes(y = p95_log_inc, color = "95th Percentile"), size = 1, shape = 22, fill = "white") +
  geom_errorbar(aes(ymin = p95_log_inc - std_log_inc, ymax = p95_log_inc + std_log_inc, color = "95th Percentile"), width = 0.2) +
  scale_color_manual(values = c("25th Percentile" = "#e41a1c",
                                "95th Percentile" = "#984ea3")) +
  facet_grid(rows = vars(age), cols = vars(gender)) +
  labs(title = "Income Levels by Age and Gender",
       subtitle = "Log of Annual Earnings (EUR) by Percentile, Gender, and Age Group",
       x = "Year of Survey",
       y = "Log of Annual Earnings (EUR)") 

#boxplot male/female


ggplot(data = Earning_Levels, aes(x = as.factor(age), y = p50_log_inc, fill = gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1b9e77", "#d95f02"), name = "Gender") +
  labs(title = "Median Income by Age and Gender",
       subtitle = "Annual Earnings (EUR) by Gender and Age Group",
       x = "Age Group",
       y = "Annual Earnings (EUR)")

# violin plot smaller version 

ggplot(Earning_Levels, aes(x = factor(age), y = p50_log_inc, fill = gender)) +
  geom_violin(scale = "width", trim = FALSE, alpha = 0.8) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  labs(title = "Income Levels by Age and Gender",
       subtitle = "Log of Annual Earnings (EUR) by Gender and Age Group",
       x = "Age Group",
       y = "Log of Annual Earnings (EUR)",
       fill = "Gender")


# fancier 

#p25

p25 <- ggplot(data = Earning_level, aes(x = p25_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#377eb8", "pink"), name = "Gender") +
  facet_grid(rows = vars(age)) +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender and Age Group",
       x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 6, face = "bold"),
        strip.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

#p50

p50 <- ggplot(data = Earning_level, aes(x = p50_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("black", "yellow"), name = "Gender") +
  facet_grid(rows = vars(age)) +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender and Age Group",
       x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 6, face = "bold"),
        strip.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

#p75 

p75 <- ggplot(data = Earning_level, aes(x = p50_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("darkred", "lightpink"), name = "Gender") +
  facet_grid(rows = vars(age)) +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender and Age Group",
       x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 6, face = "bold"),
        strip.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

#p95
p95 <- ggplot(data = Earning_level, aes(x = p95_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("grey", "lightgreen"), name = "Gender") +
  facet_grid(rows = vars(age)) +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender and Age Group",
       x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 6, face = "bold"),
        strip.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 4),
        legend.position = "bottom")












# Heatmap

ggplot(data = Earning_Levels, aes(x = gender, y = age, fill = p95_log_inc)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender and Age Group",
       x = "Gender",
       y = "Age Group") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")



# stacked bar 


# convert gender into factor 
Earning_Levels$gender <- as.factor(Earning_Levels$gender)

ggplot(data = Earning_Levels, aes(x = age, fill = p95_log_inc, group = gender)) +
  geom_bar(position = "identity") +
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), 
                    name = "Income Level", 
                    labels = c("Lowest 25%", "25-50%", "50-75%", "Highest 25%")) +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Proportion of Different Income Levels by Gender and Age Group",
       x = "Age Group",
       y = "Proportion") +
  theme_economist_white() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

# Create a stacked bar graph of earnings by gender and age group
ggplot(data = Earning_Levels, aes(x = gender, y = p95_log_inc, fill = age)) +
  geom_bar(stat = "identity") +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender and Age Group",
       x = "Age Group",
       y = "Log of Annual Earnings (EUR)") +
  theme_economist_white() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")




# line plot 

ggplot(data = Earning_Levels, aes(x = year, y = p25_log_inc, color = gender)) +
  geom_line() +
  scale_color_manual(values = c("#377eb8", "pink"), name = "Gender") +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender over Time",
       x = "Year",
       y = "Log of Annual Earnings (EUR)") +
  theme_economist_white() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")


# grouped stack chart 

ggplot(data = Earning_Levels, aes(x = age, y = p25_log_inc, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender and Age Group",
       x = "Gender",
       y = "Log of Annual Earnings (EUR)") +
  scale_fill_brewer(palette = "Set1") +
  theme_economist_white() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

#series of density plots
#p25

ggplot(data = Earning_Levels, aes(x = p25_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("green", "darkgreen"), name = "Gender") +
  facet_grid(~age, scales = "free_y") +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender, Age and Percentile",
       x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

#p50 

ggplot(data = Earning_Levels, aes(x = p50_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "yellow"), name = "Gender") +
  facet_grid(~age, scales = "free_y") +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender, Age and Percentile",
       x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")


#p75
ggplot(data = Earning_Levels, aes(x = p75_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("purple", "black"), name = "Gender") +
  facet_grid(~age, scales = "free_y") +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender, Age and Percentile",
       x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")


#p95
ggplot(data = Earning_Levels, aes(x = p95_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("pink", "lightblue"), name = "Gender") +
  facet_grid(~age, scales = "free_y") +
  labs(title = "Income Distribution by Gender and Age",
       subtitle = "Log of Annual Earnings (EUR) by Gender, Age and Percentile",
       x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")



# combine the four density plots in a faced grid
big_graph <- grid.arrange(p25, p50, p75, p95, ncol =2)





library(ggplot2)
library(gridExtra)

# set the overall title
main_title <- "Income Distribution by Gender and Age\nLog of Annual Earnings (EUR) by Gender, Age and Percentile"

# define the plot size
plot_width <- 6
plot_height <- 5

# create the four density plots
p25 <- ggplot(data = Earning_level, aes(x = p25_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("green", "darkgreen"), name = "Gender") +
  facet_grid(age ~ ., scales = "free_x") +
  labs(x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10))

p50 <- ggplot(data = Earning_level, aes(x = p50_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "yellow"), name = "Gender") +
  facet_grid(age ~ ., scales = "free_x") +
  labs(x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10))

p75 <- ggplot(data = Earning_level, aes(x = p75_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("purple", "black"), name = "Gender") +
  facet_grid(age ~ ., scales = "free_x") +
  labs(x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10))

p95 <- ggplot(data = Earning_level, aes(x = p95_log_inc, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("pink", "lightblue"), name = "Gender") +
  facet_grid(age ~ ., scales = "free_x") +
  labs(x = "Log of Annual Earnings (EUR)",
       y = "Density") +
  theme_fivethirtyeight(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10))

# Combine the four plots into a single grid
big_graph <- grid.arrange(p25, p50, p75, p95, ncol = 2)















        
        
        
        

