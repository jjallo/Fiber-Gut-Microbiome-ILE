# merge fiber aliquot data with tax_map
setwd("/Users/juliajallo/Documents/Spring 2023 Grad School/ILE")


# load tax_map
tax_map <- read.delim("R Work/Datasets/mb_sample_metadata_with_alpha_div.txt", row.names = 1)
tax_map$SampleID
fiber_ali <- read.delim("R Work/Datasets/Fiber Aliquots Metadata.tsv")


fiber_ali$sampleid[fiber_ali$sampleid == "FS28p1s1" & fiber_ali$Date == "07/27/2018"] <- "FS29p1s1"

row.names(fiber_ali) <- fiber_ali$sampleid

fiber_merge <- merge(fiber_ali, tax_map, by = 0, all.y = T)

#merge trial 


fiber_merge_b <- subset(fiber_merge, fiber_merge$Phase.x == 1)

# mean values for each persons 3 baseline samples
mean_shan_b <- aggregate(fiber_merge_b$Shannon, by = list(fiber_merge_b$UserName), mean)
colnames(mean_shan_b) <- c("UserName", "Mean_Shannon_Baseline")

# find the mean of means
cutpoint <- mean(mean_shan_b$Mean_Shannon_Baseline) # 2.78

#make the high low cutpoint variable
mean_shan_b$BaselineShan <- ifelse(mean_shan_b$Mean_Shannon_Baseline < cutpoint, "Low", "High")


# put the high low variable back on the big spreadsheet to make life easier
fiber_merge <- merge(fiber_merge, mean_shan_b, by.x = "UserName", by.y = "UserName", all = T)

#create variable for histogram for any fiber 
fiber_merge$Anyfiber <- ifelse(fiber_merge$Phase.x == 1, "Baseline", "Fiber")


#Test for NumStool24

#Check for Normality Number Stools 
hist(fiber_merge$NumStool24, col='steelblue', main='Normal')
shapiro.test(fiber_merge$NumStool24)

# Subset for baseline subset Stool
base_stool <- subset(fiber_merge, fiber_merge$Phase.x == 1)
mean_stool <- aggregate(base_stool$NumStool24, by = list(base_stool$UserName, 
                                                            base_stool$fiber1), mean)
colnames(mean_stool) <- c("UserName", "Fiber1", "Mean_Stool")
mean_stool <- merge(mean_stool, mean_shan_b, by.x = "UserName")
mean_stool$BaselineShan <- as.factor(mean_stool$BaselineShan)

mean_stool <- na.omit(mean_stool)

#GroupMeans Baseline Stool
mymeans_bstool <- aggregate(mean_stool$Mean_Stool, 
                     by = list(mean_stool$BaselineShan, 
                               mean_stool$Fiber1), 
                     mean, na.rm = T)
mymeans_bstool

#Wilcox test Stool Pre
stoolwilcoxbase <- wilcox.test(mean_stool$Mean_Stool ~ mean_stool$BaselineShan, 
                      exact=FALSE, alternative="two.sided")
stoolwilcoxbase

#Subset for Stool post

post_stool <- subset(fiber_merge, fiber_merge$Phase.x %in% c(2,4))
mean_stoolp <- aggregate(post_stool$NumStool24, by = list(post_stool$UserName, 
                                                          post_stool$fiber1), mean)
colnames(mean_stoolp) <- c("UserName", "Fiber1", "Mean_Stool")
mean_stoolp <- merge(mean_stoolp, mean_shan_b, by.x = "UserName")
mean_stoolp$BaselineShan <- as.factor(mean_stoolp$BaselineShan)

#remove missing values 
mean_stoolp<- na.omit(mean_stoolp)

#Group Means Post Stool
mymeans_pstool <- aggregate(mean_stoolp$Mean_Stool, 
                            by = list(mean_stoolp$BaselineShan, 
                                      mean_stoolp$Fiber1), 
                            mean, na.rm = T)
mymeans_pstool

# Wilcox test Stool Post
stoolwilcoxpost <- wilcox.test(mean_stoolp$Mean_Stool ~ mean_stoolp$BaselineShan, 
                               exact=FALSE, alternative="two.sided")
stoolwilcoxpost


#GGplot representing data 

dailymerge <- merge(mean_stool, mean_stoolp, all=TRUE)

#High Baseline Shannon Mean Shannon differences Low vs high intervention 
ggplot(dailymerge, aes(x = Fiber1, y = Mean_Stool, group = UserName)) + 
  geom_point() + geom_line(aes(col=`BaselineShan`)) + 
  labs(x = "Intervention", y = "Daily Stool Values") +
  theme_bw() +
  theme(panel.background = element_rect(fill="white"), panel.grid=element_blank())



#STOOL Test merge rename values for visual 
colnames(mymeans_bstool)[grep("Group.1", colnames(mymeans_bstool))] <- "Alpha Diversity"
grep("Alpha Diversity", colnames(mymeans_bstool))
colnames(mymeans_pstool)[grep("Group.1", colnames(mymeans_pstool))] <- "Alpha Diversity"
grep("Alpha Diversity", colnames(mymeans_pstool))

colnames(mymeans_bstool)[grep("Group.2", colnames(mymeans_bstool))] <- "Intervention"
grep("Intervention", colnames(mymeans_bstool))
colnames(mymeans_pstool)[grep("Group.2", colnames(mymeans_pstool))] <- "Intervention"
grep("Intervention", colnames(mymeans_pstool))

colnames(mymeans_bstool)[grep("x", colnames(mymeans_bstool))] <- "Mean Daily Stool"
grep("Mean Daily Stool", colnames(mymeans_bstool))
colnames(mymeans_pstool)[grep("x", colnames(mymeans_pstool))] <- "Mean Daily Stool"
grep("Mean Daily Stool", colnames(mymeans_pstool))



dailystool <- merge(mymeans_bstool, mymeans_pstool, all=TRUE)


#NOT USED 
#Visual For mean Daily Stool pre and post intervention by Alpha Diversity

install.packages("ggplot2")
require(ggplot2)

ggplot(dailystool, aes(x =Intervention, y= `Mean Daily Stool`, group = `Alpha Diversity`)) +
  geom_point() +
  geom_line(aes(col=`Alpha Diversity`)) +
  theme_bw() +
  theme(panel.background = element_rect(fill="white"), panel.grid=element_blank())


# Test for Bristol Stool baseline

base_bristol <- subset(fiber_merge, fiber_merge$Phase.x == 1)
mean_bristol <- aggregate(base_bristol$BristolStoolScale, by = list(base_bristol$UserName, 
                                                                  base_bristol$fiber1), mean)
colnames(mean_bristol) <- c("UserName", "Fiber1", "Mean_Bristol")
mean_bristol <- merge(mean_bristol, mean_shan_b, by.x = "UserName")
mean_bristol$BaselineShan <- as.factor(mean_bristol$BaselineShan)

#remove missing values 
mean_bristol<- na.omit(mean_bristol)


t.test(mean_bristol$Mean_Bristol ~ mean_bristol$BaselineShan)

mymeans_bbristol <- aggregate(mean_bristol$Mean_Bristol, 
                            by = list(mean_bristol$BaselineShan, 
                                      mean_bristol$Fiber1), 
                            mean, na.rm = T)
mymeans_bbristol

#Test for Bristol Stool Post

post_bristol <- subset(fiber_merge, fiber_merge$Phase.x %in% c(2,4))
mean_bristolp <- aggregate(post_bristol$BristolStoolScale, by = list(post_bristol$UserName, 
                                                                post_bristol$fiber1), mean)
colnames(mean_bristolp) <- c("UserName", "Fiber1", "Mean_Bristol")
mean_bristolp <- merge(mean_bristolp, mean_shan_b, by.x = "UserName")
mean_bristolp$BaselineShan <- as.factor(mean_bristolp$BaselineShan)


mean_bristolp<- na.omit(mean_bristolp)



mymeans_pbristol <- aggregate(mean_bristolp$Mean_Bristol, 
                              by = list(mean_bristolp$BaselineShan, 
                                        mean_bristolp$Fiber1), 
                              mean, na.rm = T)
mymeans_pbristol


t.test(mean_bristolp$Mean_Bristol ~ mean_bristolp$BaselineShan)



bristolmerge <- merge(mean_bristol, mean_bristolp, all=TRUE)

#USED
#High Baseline Shannon Mean Shannon differences Low vs high intervention 
ggplot(bristolmerge, aes(x = Fiber1, y = Mean_Bristol, group = UserName)) + 
  geom_point() + geom_line(aes(col=`BaselineShan`)) + 
  labs(x = "Intervention", y = "Bristol Stool Values") +
  theme_bw() +
  theme(panel.background = element_rect(fill="white"), panel.grid=element_blank())

#BRISTOL Test merge rename values for visual 
colnames(mymeans_bbristol)[grep("Group.1", colnames(mymeans_bbristol))] <- "Alpha Diversity"
grep("Alpha Diversity", colnames(mymeans_bbristol))
colnames(mymeans_pbristol)[grep("Group.1", colnames(mymeans_pbristol))] <- "Alpha Diversity"
grep("Alpha Diversity", colnames(mymeans_pbristol))

colnames(mymeans_bbristol)[grep("Group.2", colnames(mymeans_bbristol))] <- "Intervention"
grep("Intervention", colnames(mymeans_bbristol))
colnames(mymeans_pbristol)[grep("Group.2", colnames(mymeans_pbristol))] <- "Intervention"
grep("Intervention", colnames(mymeans_pbristol))

colnames(mymeans_bbristol)[grep("x", colnames(mymeans_bbristol))] <- "Mean Bristol Stool"
grep("Mean Bristol Stool", colnames(mymeans_bbristol))
colnames(mymeans_pbristol)[grep("x", colnames(mymeans_pbristol))] <- "Mean Bristol Stool"
grep("Mean Bristol Stool", colnames(mymeans_pbristol))


bristolstool <- merge(mymeans_bbristol, mymeans_pbristol, all=TRUE)

ggplot(bristolstool, aes(x =Intervention, y= `Mean Bristol Stool`, group = `Alpha Diversity`)) +
  geom_point() + 
  geom_line(aes(col=`Alpha Diversity`)) +
  theme_bw() +
  theme(panel.background = element_rect(fill="white"), panel.grid=element_blank())

