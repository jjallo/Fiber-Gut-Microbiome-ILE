#LOAD BigDataMerge Script first

# FINAL TABLE 1 for analysis


#subsetting to baseline mean values 
fiber_merge_b <- subset(fiber_merge, fiber_merge$Phase.x == 1)

# mean values for each persons 3 baseline samples
mean_shan_b <- aggregate(fiber_merge_b$Shannon, by = list(fiber_merge_b$UserName), mean)
colnames(mean_shan_b) <- c("UserName", "Mean_Shannon_Baseline")

# find the mean of means
cutpoint <- mean(mean_shan_b$Mean_Shannon_Baseline) # 2.78

#make the high low cutpoint variable
mean_shan_b$BaselineShan <- ifelse(mean_shan_b$Mean_Shannon_Baseline <= cutpoint, "Low", "High")

#Histogram of Shannon Values
hist(mean_shan_b$Mean_Shannon_Baseline, col='steelblue', main='Distribution of Shannon Diversity', 
     xlab = 'Shannon Index Values', 
     ylab= 'Participants')

# Add line at point 2.78
abline(v = 2.78, col = 'red', lwd = 2)

#renaming variables to fit table descriptions 

# Change the variable name from Gender to Sex
colnames(mytable2)[grep("Gender", colnames(mytable2))] <- "Sex"

#Change the variable name from Usual_bristol to Bristol
colnames(mytable2)[grep("Usual_bristol", colnames(mytable2))] <- "Bristol"
grep("Bristol", colnames(mytable2))

#Change the variable name from physact to Physical Activity
colnames(mytable2)[grep("Physact", colnames(mytable2))] <- "Physical Activity"

#Change the variable name to Baseline Weight (kg)
colnames(mytable2)[grep("Baseline_Weight_kg", colnames(mytable2))] <- "Baseline Weight (kg)"
table(mytable2$`Baseline Weight (kg)`)

#Change the variable name to Movements per week
colnames(mytable2)[grep("Movements_per_week", colnames(mytable2))] <- "Movements per week"

#recode gas_leakage to Binary leakage
#check this
mytable2$Leakage <- ifelse(grepl("f", mytable2$Gas_leakage), "No", "Yes")


#recode Physical Activity to binary 
mytable2$`Physical Activity` <- ifelse(mytable2$`Physical Activity` >= 3, "≥ 3 days", "< 3 days")

table(mytable2$`Physical Activity`)



#get baseline Shannon values for table 2 group variable
baseshancat <- mean_shan_b[,colnames(mean_shan_b) %in% c("UserName", "BaselineShan")]
colnames(baseshancat) <- c("StudyID","BaselineShan")
mytable2 <- merge(mytable2, baseshancat)

mytable2$BaselineShan <- as.factor(mytable2$BaselineShan)

submytable <- mytable2[mytable2$Fiber == "Normal",]

#add dietary fiber variable
diets <- read.delim("/Users/juliajallo/Documents/Spring 2023 Grad School/ILE/R Work/Datasets/diet_visit.txt")

dietssub <- subset(diets, diets$Visit == 1)

dietssub <- subset(dietssub, select= c("StudyID", "KCAL", "FIBE"))

submytable <- merge(submytable, dietssub, by= "StudyID", all=TRUE)


submytable$Fiber_grams <- submytable$FIBE/(submytable$KCAL/1000)

submytable$Daily_fiber <- ifelse(submytable$Fiber_grams >= 14, "met", "insufficient")


table(submytable$FIBE)

table2 <-gtsummary::tbl_summary(submytable, 
                                include = c(  "Sex", 
                                              "Age", 
                                              "Leakage",
                                              "Baseline Weight (kg)",
                                              "Bristol", 
                                              "Movements per week", 
                                              "Physical Activity",
                                              "Daily_fiber"),
                                by = BaselineShan,
                                type = list(Sex ~ "categorical", 
                                            Leakage ~ "categorical", 
                                            Daily_fiber ~ "categorical",
                                            `Physical Activity` ~ "categorical",
                                            Bristol ~ "continuous", 
                                            `Movements per week` ~ "continuous"),
                                statistic = all_continuous() ~ "{mean}, ({min}, {max})", 
                                digits = list(Bristol ~ c(1, 0, 0), 
                                              `Movements per week` ~ c(2, 0, 0)))                  

table2

flextable::save_as_docx(as_flex_table(table2), path = "/Users/juliajallo/Documents/Spring 2023 Grad School/ILE/R Work/Output/table2.docx")


