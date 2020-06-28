#This project is to show how much I spent my dining dollars on certain items and at certain retail places in Stony Brook
#During Spring 2019 Semester

# Lines 4 - 19 is about how many times I went to each place during SP 19 Semester
Spending_Dining_Dollars <- read.csv("C:/Users/.../..../.../.../Spending Dining Dollars_Spring19.csv")
options(max.print = 999999)
Spending_Dining_Dollars
Location_DF <- data.frame(Location = Spending_Dining_Dollars$Location)
Location_DF #shows a "list" of places I went to 
Location_Count <- table(Location_DF)
Location_Count #counts the total visits I had for each place
Location_Vector <- c("Cha-Time", "Cocina Fresca", "Delancey's", "Emporium", "Jasmine", "SAC",
                     "Starbucks", "West \nConvenience \nStore") 
#Line 12: vector that will be used as the labels for the horizontial axis of the graph
library(ggplot2)
Location_Graph <- barplot(Location_Count, names.arg = Location_Vector, main = "How Often Iftiar Went to Each Retail During SP '19 Semester", 
                          xlab = "Location", ylab = "Frequency", col = "red", ylim = c(0,20), las = 2, cex.names = .6, cex.lab = .9, 
                          cex.main = .75)
text(Location_Graph, Location_Count + 2, paste(" ", Location_Count), cex = 1)
print(Location_Graph)
# Use las = 2, cex.names, cex.lab, and cex.main to increase/reduce sizes of the labels, axes, and title respectively
#use ylim() to set vertical axis boundaries
#text shows the Frequency of visits

#This part of the project is to show how many dining dollars I spent at each place during SP '19

DD_DF <- data.frame(Location_DF, Dining_Dollars = Spending_Dining_Dollars$Cost)
DD_DF #shows where I spent my dining dollars and how much for all transactions
X <- table(DD_DF)
X
library(dplyr)
Tabled_Spent <- dplyr::group_by(Spending_Dining_Dollars, Location)%>%dplyr::summarize(sum(Cost))
#Line 31 is so there is only 2 columns, one with Location and one with total dining dollars spent at each place
Tabled_Spent
DF_TS <- data.frame(Tabled_Spent)
Spent <- ggplot(data = DF_TS, aes(Location_Vector, DF_TS$sum.Cost.)) + xlab("Location") + ylab("Dining Dollars Spent")+
  geom_col(data = DF_TS, inherit.aes = TRUE, position = "stack", col = "black", fill = "white") +
  ggtitle("Where Did My Dining Dollars Go?: \nSpring 2019")
Spent <- Spent + geom_label(label = DF_TS$sum.Cost.)
Spent

#Now to show how much dining dollars spent per week
Weekly_Spent <- dplyr::group_by(Spending_Dining_Dollars, Week)%>%dplyr::summarize(sum(Cost))
Weekly_Spent
Budget <- 16.76 #max amount of dining dollars I should spent per week, cap
#to get line 45, I divided my starting balance by the number of weeks of school there is
DF_Weekly <- data.frame(Weekly_Spent)
Weekly_Track <- ggplot(data = DF_Weekly, aes(DF_Weekly$Week, DF_Weekly$sum.Cost.)) + xlab("Week") + ylab("Dining Dollars Spent") +
  ggtitle("Spring 19: Dining Dollars Spent By Week") + geom_line() + scale_x_continuous(breaks = seq(1,15,1)) + 
  scale_y_continuous(breaks = seq(0,35,5)) + geom_hline(yintercept = Budget, linetype = "dashed", color = "blue") + 
  geom_text(aes( 0, Budget, label = Budget, vjust = -1), size = 3)
Weekly_Track <- Weekly_Track + geom_point() + geom_text(label = DF_Weekly$sum.Cost., vjust = -0.25) 
Weekly_Track
