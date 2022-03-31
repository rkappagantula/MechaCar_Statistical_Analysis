### CHALLENGE 15: DELIVERABLE 1

#1.Use the library() function to load the dplyr package
library(dplyr)

#2.Import and read in the MechaCar_mpg.csv file as a dataframe.
library(tidyverse)

#3.Import and read in the MechaCar_mpg.csv file as a dataframe.
MechCarChallenge_Table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#4.Perform linear regression using the lm() function
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechCarChallenge_Table) #generate multiple linear regression model

#5.Using the summary() function, determine the p-value and the r-squared value for the linear regression model.
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechCarChallenge_Table)) #generate summary statistics


### CHALLENGE 15: DELIVERABLE 2

#1. Import and read in the Suspension_Coil.csv file as a table
suspension_coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#2 #create summary table with multiple columns
total_summary <- suspension_coil_table %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Var_PSI=var(PSI),Std_Dev_PSI=sd(PSI),
Num_Coil=n(), .groups = 'keep') 

#3 Create a lot_summary dataframe using the group_by() and the summarize() functions to group each manufacturing lot.                                                                
lot_summary <- suspension_coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),
                                                                         Median_PSI=median(PSI),
                                                                         Var_PSI=var(PSI),
                                                                         Std_Dev_PSI=sd(PSI),
                                                                         Num_Coil=n(), .groups = 'keep')


#box plot: PSI Whole lot
#import dataset into ggplot2
plt1 <- ggplot(suspension_coil_table,aes(y=PSI))
plt1 + geom_boxplot() #add boxplot

#box plot: PSI each individual Lot
#import dataset into ggplot2
plt2 <- ggplot(suspension_coil_table,aes(x=Manufacturing_Lot,y=PSI)) 
plt2 + geom_boxplot()

### CHALLENGE 15: DELIVERABLE 3

#1. use t.test() to determine if the PSI across ALL lots is statistically different from the pop. mean of 1,500 PSI.
t.test(suspension_coil_table$PSI,mu=1500)


#2. Use t.test() function 3 more times with subset() to determine if PSI for each manufacturing lot is statistically different from the pop. mean of 1,500 PSI
lot1 <- subset(suspension_coil_table, Manufacturing_Lot=="Lot1")
lot2 <- subset(suspension_coil_table, Manufacturing_Lot=="Lot2")
lot3 <- subset(suspension_coil_table, Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)

