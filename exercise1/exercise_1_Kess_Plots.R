library(mosaic)
library(dplyr)
library(ggplot2)



greenData <- read.csv('greenbuildings.csv')



# Subset into just green buildings (Green Buildings: Rent)
greenBuildings <- greenData %>% filter(green_rating == 1)

# Subset Green Buildings, ~15 stories (+-5 stories), new (built within last 5 years)
greenBuildings_Ideal <- greenData %>% filter(green_rating == 1,
                                             stories <= 20,
                                             stories >= 10,
                                             age >= 0,
                                             age <= 10,
                                             amenities==1,
                                             cd_total_07>=966)

nonGreenBuildings_Ideal <- greenData %>% filter(green_rating != 1,
                                                stories <= 20,
                                                stories >= 10,
                                                age >= 0,
                                                age <= 10,
                                                amenities==1,
                                                cd_total_07>=966)


# Histogram (Green Buildings: Rent)-------------------------------DONE
hist(greenBuildings$Rent, 25, xlab = 'Rent Per Square Foot', main = 'Histogram of Rent Prices')



#density plot of one df (parameter 1) with mean line -------------------------------FIX
#Parameter 1: dataframe to use Ex: green_only
#Parameter 2: title of the grpah as a string Ex: "Title"
#add mean line from entire dataset
rent.denplot <- function(df,TITLE){
  attach(df)
  mean.column = mean(df$Rent)
  graph = df %>%
      ggplot(aes(x=Rent))+
        geom_density(fill="blue",alpha=0.5)+
        scale_x_log10()+
        geom_vline(xintercept = mean.column,size=1,color="black")+
        labs(y="Density", title = TITLE)
  return(graph) 
}
rent.denplot(greenBuildings_Ideal, "Density Plot of Ideal Green Building's Rent")
rent.denplot(nonGreenBuildings_Ideal, "Density Plot of Ideal Non-Green Building's Rent")

#density plot of green and non green on the same graph -------------------------------FIX
#add mean line of rent from both groups of buildings
plot(density(greenBuildings_Ideal$Rent), col = "darkgreen", xlab = "Rent Per Square Foot")
lines(density(nonGreenBuildings_Ideal$Rent),col = "red")



# Box Plot (Green Buildings: Rent) -------------------------------FIX
# put them side by side
boxplot(greenBuildings$Rent, main="Boxplot of Green Building Rent", ylab ="Rent Per Square Foot",
        col = "seagreen1",horizontal = TRUE)
boxplot(nonGreenBuildings_Ideal$Rent, main="Boxplot of Non-Green Building Rent", ylab ="Rent Per Square Foot",
        col = "turquoise",horizontal = TRUE)
boxplot(greenBuildings_Ideal$Rent, main="Boxplot of Ideal Green Building Rent", ylab ="Rent Per Square Foot",
        col = "darkgreen",horizontal = TRUE)



# Scatter plot matrix 
pairs(greenBuildings_Ideal[,3:10], pch = 19, lower.panel = NULL)



##ESSENTIALLY THE SAME AS A DENSITY PLOT
# Histogram (Green Buildings (Subset): Rent)
hist(greenBuildings_Ideal$Rent)

# Histogram (Non-Green Buildings (Subset): Rent)
hist(nonGreenBuildings_Ideal$Rent)







# Rent Comparison
mean(nonGreenBuildings_Ideal$Rent) - mean(greenBuildings_Ideal$Rent)















