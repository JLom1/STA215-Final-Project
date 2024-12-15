## Project: STA 215, Fall 2024, Final Project
# Located: sta215 Folder
# File Name: Final_Template
# Date: 2024_12_2
# Who: Jake LoMonaco

#Set Working Directory
setwd("c:/rstudio")

#Load Packages
# NOTE: Run Base.R if these commands return an error!
library(haven)
library(ggplot2)

# Load Data
data <- read.csv("data.csv")

##################################################################################
############### Table 1: descriptive statistics ##################################
##################################################################################

#Qualitative Variables
table(data$simplicity)

table(data$lyrical_tone)

table(data$lyrical_tone.1)

#Quantitative Variables
table(data$speed)
mean(data$speed)
sd(data$speed)

table(data$monthly_listeners)
mean(data$monthly_listeners)
sd(data$monthly_listeners)

table(data$merch_sold)
mean(data$merch_sold)
sd(data$merch_sold)

##################################################################################
############### Figure 1: boxplot ################################################
##################################################################################

#Create Objects
Speed <- data$speed
Simplicity <- factor(data$simplicity)

#ANOVA
anova <- aov(Speed ~ Simplicity, data=data)
summary(anova)

#Create Boxplot
box_plotc3 <- ggplot(data, aes(x=Simplicity, y=speed)) +
  geom_boxplot(
    fill = "grey",     
    color = "black",     
    outlier.color = "darkblue",  
    outlier.shape = 16,     
    outlier.size = 3,       
    width = 0.5 
  ) +
  labs(title="Relationship Between Simplicity and Speed of Songs", x="Simplicity", y="Speed")
theme_minimal()
print(box_plotc3)

##################################################################################
############### Figure 2: scatter plot  ##########################################
##################################################################################

#Create Objects
Listeners <- data$monthly_listeners
Merch <- data$merch_sold

#Create Scatter Plot
scatter_plot <- ggplot(data, aes(x=Listeners, y=Merch)) + 
  geom_point(fill="white",
             shape=21,
             color= "black",
             stroke= 1.5) + 
  ggtitle("Effect of Artistis Monthly Listeners on the Amount of Merchandise Sold") + 
  xlab("Monthly Listeners") + 
  ylab("Merchandise Sold")
print(scatter_plot)

##################################################################################
############### Figure 3: residual plot  #########################################
##################################################################################

#Create Object
model <- lm(Merch ~ Listeners)

# Summarize Object
summary(model)

# Create a residual plot
ggplot(data, aes(x = fitted(model), y = resid(model))) +
  geom_point(fill = "white", shape = 21, color = "black", stroke = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "Fitted values", y = "Residuals") +
  theme_minimal() +
  ggtitle("Residual Plot")

##################################################################################
############### Table 2: contingency table  ######################################
##################################################################################

#Create Tables
table(data$lyrical_tone, data$lyrical_tone.1)
chisq.test(table(data$lyrical_tone, data$lyrical_tone.1))
