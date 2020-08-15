library(mosaic)
library(dplyr)
library(ggplot2)

setwd("C:/Users/katel/OneDrive/Desktop/R")

# get rid of scientific notation
options(scipen=999)

# read in dataset
greenData <- read.csv("greenbuildings.csv")

# separate into green and non-green buildings
greenBuildings <- greenData %>% filter(green_rating == 1)
nonGreenBuildings <- greenData %>% filter(green_rating == 0)

# create subset of similar buildings based on what we know:
# mixed use building so look at buildings with amenities = 1
# new building so filter on buildings <10 yrs or renovated
# 15 stories so filter on buildings between 10-20 stories
# Austin, TX so filter on buildings with num cooling days > median of 966

subset_All <- greenData %>% filter(amenities==1,
                                   cd_total_07>=966,
                                   stories<=20,
                                   stories>=10,
                                   age<=10 | renovated==1)

subset_Green <- subset_All %>% filter(green_rating==1)
subset_Nongreen <- subset_All %>% filter(green_rating==0)

#####################################################
# plot leasing rate vs size, all data
#####################################################

# plot leasing rate vs size (all data)
ggplot(data = greenData) + 
  geom_point(aes(x = leasing_rate, y = size, color=factor(green_rating))) +
  labs(color = "Green Building") +
  # add colors and legend labels
  scale_colour_manual(values = c("#01148A","#0AD626"),labels = c("No","Yes")) +
  ggtitle("Leasing Rate vs. Size") +
  # center plot title
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Leasing Rate",y = "Size (Sq. ft)")

#####################################################
# rent distribution box plots
#####################################################

boxplot(greenBuildings$Rent, main="Rent Distribution (All Green Buildings)", xlab ="Rent Per Sq.Ft.",
        col = "#B4FCEA",horizontal = TRUE)
boxplot(nonGreenBuildings$Rent, main="Rent Distribution (All Non-Green Buildings)", xlab ="Rent Per Sq.Ft.",
        col = "#BAD6FA",horizontal = TRUE)
boxplot(subset_Green$Rent, main="Rent Distribution (Subset, Green Buildings)", xlab ="Rent Per Sq.Ft.",
        col = "#23CF8E",horizontal = TRUE)
boxplot(subset_Nongreen$Rent, main="Rent Distribution (Subset, Non-green Buildings)t", xlab ="Rent Per Sq.Ft.",
        col = "#4A9AFF",horizontal = TRUE)

#####################################################
# distribution of rental prices, all buildings
#####################################################

plot(density(greenBuildings$Rent), main="Rent Distribution, All Buildings", ylim=c(0,.045), col = "#0AD626", xlab = "Rent Per Square Foot")
lines(density(nonGreenBuildings$Rent),col = "#01148A")
legend(100, .035, legend=c("Green", "Not Green"),
       col=c("#0AD626", "#01148A"), lty=1:2, cex=0.8)

plot(density(subset_Green$Rent), main="Rent Distribution, Subset of Buildings", ylim=c(0,.06), col = "#0AD626", xlab = "Rent Per Square Foot")
lines(density(subset_Nongreen$Rent),col = "#01148A")
legend(42, .05, legend=c("Green", "Not Green"),
       col=c("#0AD626", "#01148A"), lty=1:2, cex=0.8)

# green buildings compared (all to subset)
plot(density(greenBuildings$Rent), main="Rent Distribution, Green Buildings", ylim=c(0,.06), xlim=c(0,100), col = "#69E878", xlab = "Rent Per Square Foot")
lines(density(subset_Green$Rent),col = "#155B2F")
legend(70, .05, legend=c("All Data", "Subset"),
       col=c("#69E878", "#155B2F"), lty=1:2, cex=0.8)

# nongreen buildings compared (all to subset)
plot(density(nonGreenBuildings$Rent), main="Rent Distribution, Non-Green Buildings", ylim=c(0,.06), xlim=c(0,100), col = "skyblue", xlab = "Rent Per Square Foot")
lines(density(subset_Nongreen$Rent),col = "#01148A")
legend(70, .05, legend=c("All Data", "Subset"),
       col=c("skyblue", "#01148A"), lty=1:2, cex=0.8)

#####################################################
# plot dist. of rent prices for subset
#####################################################

green.denplot <- function(df,TITLE){
  attach(df)
  median.column = round(median(df$Rent),digits=2)
  graph = df %>%
    ggplot(aes(x=Rent))+
    geom_density(fill="#0AD626",alpha=0.5)+
    scale_x_log10()+
    geom_vline(xintercept = median.column,size=1,color="black")+
    geom_label(aes(x = median.column, y = 0.5, label = paste("Median Rent: $", median.column)), fill = "white")+
    labs(y="Density", x ="Rent Per Sq.Ft.", title = TITLE) +
    theme(plot.title = element_text(hjust = 0.5))
  return(graph) 
}

green.denplot(subset_Green, "Rent Distribution (Subset, Green Buildings)")

nongreen.denplot <- function(df,TITLE){
  attach(df)
  median.column = round(median(df$Rent),digits=2)
  graph = df %>%
    ggplot(aes(x=Rent))+
    geom_density(fill="#01148A",alpha=0.5)+
    scale_x_log10()+
    geom_vline(xintercept = median.column,size=1,color="black")+
    geom_label(aes(x = median.column, y = 0.5, label = paste("Median Rent: $", median.column)), fill = "white")+
    labs(y="Density", x ="Rent Per Sq.Ft.", title = TITLE) +
    theme(plot.title = element_text(hjust = 0.5))
  return(graph) 
}

nongreen.denplot(subset_Nongreen, "Rent Distribution (Subset, Non-Green Buildings)")

#####################################################
# plot break even analysis
#####################################################

# fixed costs
fixed_cost <- 5000000

# annual electricity costs
# average kwh for commercial buildings is 22.5
# https://www.iotacommunications.com/blog/benchmarking-commercial-building-energy-use-per-square-foot/#:~:text=According%20to%20the%20Department%20of,are%20consumed%20by%20refrigeration%20%26%20equipment.
annual_kwh_persqf <- 22.5
building_size_sqf <- 250000
elec_cost <- median(subset_All$Electricity_Costs)
ann_elec_usage_other <- annual_kwh_persqf*building_size_sqf
ann_elec_costs_other <- ann_elec_usage_other*elec_cost
# average 25% reduction in energy usage for green buildings
# https://www.iotacommunications.com/blog/green-building-benefits/
ann_elec_usage_green <- ann_elec_usage_other*0.75
ann_elec_costs_green <- ann_elec_usage_green*elec_cost
delta_elec_costs <- ann_elec_costs_green - ann_elec_costs_other

# annual revenue
occ_rate_other <- median(subset_Nongreen$leasing_rate)/100
rent_other <- median(subset_Nongreen$Rent)
ann_revenue_other <- building_size_sqf*occ_rate_other*rent_other
occ_rate_green <- median(subset_Green$leasing_rate)/100
rent_green <- median(subset_Green$Rent)
ann_revenue_green <- building_size_sqf*occ_rate_green*rent_green
delta_revenue <- ann_revenue_green - ann_revenue_other

years <- c(seq(0, 15))
fixed_cost <- c(rep(5000000, length(years)))
variable_cost <- delta_elec_costs*years
total_cost <- fixed_cost + variable_cost
revenue <- delta_revenue*years
contribution_margin <- delta_revenue - delta_elec_costs
break_even_years <- round(fixed_cost[1]/contribution_margin,digits=1)
breakeven <- data.frame(years, fixed_cost, variable_cost, total_cost, revenue)

# plot break-even analysis
ggplot(data = breakeven, aes(x = years)) +
  geom_line(aes(y = fixed_cost,
                col = "Fixed Cost")) +
  geom_line(aes(y = variable_cost,
                col = "Variable Cost")) +
  geom_line(aes(y = total_cost,
                col = "Total Cost")) +
  geom_line(aes(y = revenue,
                col = "Revenue")) +
  geom_segment(aes(x = break_even_years, xend = break_even_years, 
                   y = 0, yend = break_even_years*delta_revenue),
               linetype = "dashed") +
  geom_segment(aes(x = 0, xend = break_even_years, 
                   y = break_even_years*delta_revenue, yend = break_even_years*delta_revenue),
               linetype = "dashed") +
  geom_point(aes(x = break_even_years, 
                 y= break_even_years*delta_revenue), 
             colour = "black", size = 4) +
  annotate("text", x = break_even_years, y = 0, label = paste("Break Even Point:", break_even_years,"years")) +
  scale_color_manual(labels = c("Fixed Cost", "Delta Revenue", "Total Cost", "Delta Variable Cost"),
                     values = c("Fixed Cost" = "black", "Variable Cost" = "blue", "Total Cost" = "red", "Revenue" = "green")) +
  geom_ribbon(data = breakeven[breakeven$total_cost >= breakeven$revenue, ], aes(x = years, ymin = revenue, ymax = total_cost), fill = "red", alpha = 0.15) +
  geom_ribbon(data = breakeven[breakeven$total_cost <= breakeven$revenue, ], aes(x = years, ymin = total_cost, ymax = revenue), fill = "green", alpha = 0.15) +
  labs(title = "Break-Even Analysis (Green Building)",
       x = "Year",
       y = "Dollars",
       color = NULL) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels=scales::dollar_format())

#####################################################
# plot rent distribution for class a buildings (all)
#####################################################

classABuildings <- greenData %>% filter(class_a == 1)
otherClassBuildings <- greenData %>% filter(class_a == 0)

classa.denplot <- function(df,TITLE){
  attach(df)
  median.column = round(median(df$Rent),digits=2)
  graph = df %>%
    ggplot(aes(x=Rent))+
    geom_density(fill="#45CCE4",alpha=0.5)+
    scale_x_log10()+
    geom_vline(xintercept = median.column,size=1,color="black")+
    geom_label(aes(x = median.column, y = 0.5, label = paste("Median Rent: $", median.column)), fill = "white")+
    labs(y="Density", x ="Rent Per Sq.Ft.", title = TITLE) +
    theme(plot.title = element_text(hjust = 0.5))
  return(graph) 
}

classa.denplot(classABuildings, "Rent Distribution (All Class A Buildings)")
classa.denplot(otherClassBuildings, "Rent Distribution (All Non-Class A Buildings)")
