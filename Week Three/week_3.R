library(tidyverse)
library(janitor)
library(lubridate)
library(ggdist)
library(performance)
library(gt)

benchmark <- read_csv("Week Three/Benchmark.csv")

colnames(benchmark)

benchmark <- benchmark %>% 
  rename(timestamp = 1,
         ID = 2,
         preferred_hand = 3,
         react_1 = 4,
         react_2 = 5,
         react_3 = 6,
         react_4 = 7,
         react_5 = 8,
         verbal_score = 9,
         number_score = 10,
         visual_score = 11,
         activity = 12)


benchmark %>% 
  select("ID") %>% 
  duplicated() %>% 
  sum()




benchmark <- benchmark %>% 
  mutate(react_avg=(rowSums(across(4:8)))/5)

benchmark %>% 
  select(react_avg) %>% 
  is.na() %>% 
  sum()



ggplot(aes(x=react_avg), 
       data=benchmark)+
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(benchmark$sum, na.rm = TRUE),
                            sd = sd(benchmark$sum, na.rm = TRUE)))


benchmark %>% 
  select(verbal_score) %>% 
  is.na() %>% 
  sum()


color <- c("dodgerblue", "dark orange", "purple")

benchmark <-  benchmark %>% 
  mutate(activity=factor(activity, 
             levels=c("Sedentary",
              "Moderately Active",
              "Very active")))

benchmark %>% 
  group_by(activity) %>% 
  summarise(mean=mean(react_avg, na.rm=T))

plot <- ggplot(benchmark, aes(x = activity, y = react_avg, fill = activity)) +
ggdist::stat_halfeye(
  ## custom bandwidth
  adjust = .5, 
  ## adjust height
  width = .6, 
  ## move geom to the right
  justification = -.2, 
  ## remove slab interval
  .width = 0, 
  point_colour = NA,
  alpha=0.6
)+
  geom_point(aes(fill = activity), 
             position = position_jitter(width = .07), 
             size = 2, 
             stroke=1, 
             shape = 21, 
             colour="black")+
  geom_boxplot(outlier.shape = NA, 
               alpha = .5, 
               width = .1, 
               colour = "black")+
  scale_color_manual(values=color)+ 
  scale_fill_manual(values=color)+
  theme_classic()+
  theme(legend.position="none")+
  labs(y="Average reaction time \n in milliseconds",
       x= "Self reported activity levels",
       title="Figure 1: Self reported high levels of physical activity \n are correlated with modest improvements in reaction times")+
  ## remove white space on the left
  coord_cartesian(xlim = c(1.2, NA),
                  ylim=c(200,400))



activity_model <- lm(react_avg ~ activity, data=benchmark)

check_model(activity_model)


benchmark_2 <- benchmark %>% 
  filter(react_avg < 600)

benchmark_3 <- benchmark %>% 
  filter(react_avg < 400)

activity_model <- lm(react_avg ~ activity, data=benchmark_3)

check_model(activity_model)

summary(activity_model)

anova_test(react_avg~activity, data=benchmark_3)


augmented_model <- broom::augment(activity_model,
                                  se_fit=TRUE)


plot+geom_line(data=augmented_model, 
               aes(x=activity, y=.fitted, group=1), 
               linetype="dashed")


augmented_model %>% 
  distinct(activity, .keep_all = TRUE) %>% 
  select(activity, .fitted, .se.fit) %>% 
  gt() %>% 
  cols_label(activity = "Self reported activity levels", 
             .fitted = "Average reaction time (ms)", 
             .se.fit="S.E.") %>% 
  tab_spanner(label="Model summary", 
              columns=vars(.fitted, #
                           .se.fit)) %>%   
  tab_header(
  title = "Table 1. Average reaction times grouped by self-reported activity levels") %>% 
  opt_align_table_header(align="left")
# %>% gtsave("table.png")
