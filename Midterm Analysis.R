devtools::install_github("crsh/papaja")
library(devtools)

library(data.table)
midterm_data <- fread("https://raw.githubusercontent.com/CrumpLab/statisticsLab/master/data/Jamesetal2015Experiment2.csv")

library(summarytools)
view(dfSummary(midterm_data))

library(dplyr)

library(ggplot2)

# get means and SEs
descriptive_df <- midterm_data %>% 
  group_by(Condition) %>% 
  summarise(means= mean(Days_One_to_Seven_Number_of_Intrusions),
            SEs = sd(Days_One_to_Seven_Number_of_Intrusions)/sqrt(length(Days_One_to_Seven_Number_of_Intrusions)))

# Make the plot
ggplot(descriptive_df, aes(x=Condition, y=means))+ 
  geom_bar(stat="identity", aes(fill=Condition))+ 
  geom_errorbar(aes(ymin=means-SEs,             
                    ymax=means+SEs), width=.1) +
  geom_point(data=midterm_data, aes(x=Condition, y=Days_One_to_Seven_Number_of_Intrusions), alpha=.5)+
  geom_point(alpha=.25)+
  ylab("Intrusive Memories (Mean for Week)")

anova_data <- aov(Days_One_to_Seven_Number_of_Intrusions ~ Condition, midterm_data)

summary_data <- summary(anova_data)

library(xtable)

knitr::kable(xtable(summary_data))
