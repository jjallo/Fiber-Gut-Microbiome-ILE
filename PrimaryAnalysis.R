
#Load BigDataMerge First 

Prime <- mytable2

diets <- read.delim("/Users/juliajallo/Documents/Spring 2023 Grad School/ILE/R Work/Datasets/diet_visit.txt")

table(diets$FIBE)
table(diets$KCAL)

#only variable I want is FIBE and I am merging on STUDYID
diets_subset <- subset(diets, select= c("StudyID","KCAL", "FIBE"))

library(dplyr)

PrimeA <- merge(diets_subset, Prime, by=0, all= TRUE)

PrimeA <- PrimeA %>% select(-Row.names, -StudyID.y) %>% rename (StudyID = StudyID.x)

#delete missing values for FIBE
PrimeA <- subset(PrimeA, !is.na(FIBE))

#binary fiber with recommended intake level as cutoff

PrimeA$Fiber_grams <- PrimeA$FIBE/(PrimeA$KCAL/1000)

PrimeA$Daily_fiber <- ifelse(PrimeA$Fiber_grams >= 14, "met", "insufficient")

#make tertile cutoffs
tertile_cutoffs <- quantile(PrimeA$Fiber_grams, probs = c(0, 1/3, 2/3, 1), na.rm= TRUE)


PrimeA$tertiles_fiber <- cut(PrimeA$Fiber_grams, breaks=tertile_cutoffs, 
                             labels = c("low", "medium", "high"), include.lowest= TRUE)

#tertiles of habitual fiber intake
PrimeA$Daily_fiber <- ifelse(PrimeA$tertiles_fiber == "high", "met", "insufficient")

table(PrimeA$Daily_fiber)

#first creating variables needed for model 

# add visit level
PrimeA$Visit[PrimeA$Fiber == "Normal"] <- 1
PrimeA$Visit[PrimeA$Group == 1 & PrimeA$Fiber == "Low"] <- 3
PrimeA$Visit[PrimeA$Group == 1 & PrimeA$Fiber == "High"] <- 5
PrimeA$Visit[PrimeA$Group == 2 & PrimeA$Fiber == "Low"] <- 5
PrimeA$Visit[PrimeA$Group == 2 & PrimeA$Fiber == "High"] <- 3



PrimeA$Int_timing <- ifelse(PrimeA$Visit == 1, "Baseline", 
                            ifelse(PrimeA$Visit == 2, "Pre", 
                                   ifelse(PrimeA$Visit == 3, "Post",
                                          ifelse(PrimeA$Visit == 4,"Pre",
                                                 ifelse(PrimeA$Visit == 5, "Post", 
                                                        NA)))))
#intervention level = treatment effect

PrimeA$Int_level <- ifelse(PrimeA$Visit == 1, "Baseline", 
                           ifelse(PrimeA$Visit == 2, as.character(PrimeA$Phase_2_dose), 
                                  ifelse(PrimeA$Visit == 3, as.character(PrimeA$Phase_2_dose),
                                         ifelse(PrimeA$Visit == 4, as.character(PrimeA$Phase_4_dose),
                                                ifelse(PrimeA$Visit == 5, as.character(PrimeA$Phase_4_dose), 
                                                       NA)))))

#Period= Period effect

PrimeA$Period <- ifelse(PrimeA$Visit == 1, "1st",
                        ifelse(PrimeA$Visit == 3, "2nd", 
                        ifelse(PrimeA$Visit == 5, "3rd",
                               NA)))

#add sequence variable for sequence effect 

PrimeA$Sequence <- paste0(PrimeA$Phase_2_dose, PrimeA$Phase_4_dose)

#these dummy variables also test for treatment effect
# add a high (yes/no) variable
PrimeA$HighFiber <- ifelse(PrimeA$Int_level == "High", 1, 0)

#add a low (yes/no) variable 
PrimeA$LowFiber <- ifelse(PrimeA$Int_level == "Low", 1, 0)


#LME ANALYSIS

#remove those with compliance issues
primary <- subset(PrimeA, PrimeA$Study_Status %in% c("Complete", "Missingv4")) 
primary <- droplevels(primary)


#Don't run bc we want all 3 levels
# show High Low comparison with Low on the left, change factor order
#primary$Int_level <- factor(primary$Int_level, levels = c("Low", "High"))

primary$BMI <- primary$`Baseline Weight (kg)`/(primary$Height_cm/100)^2

#find which rows have NA's for Mean_Bristol so you can omit them
na_rows <- which(is.na(primary$Mean_Bristol))
df_with_na <- primary[na_rows,]
print(df_with_na)

#omitted rows 83 and 8 due to NA's 
bristoltest <- primary[-78, ]
bristoltest <- bristoltest[-82,]

bristoltest <- subset(primary, !is.na(Mean_Bristol))

install.packages("nlme")
library(nlme)

allbristol <- lme(Mean_Bristol ~ BaselineShan + Int_level + Sequence + Age + BMI + Sex + Fiber_grams
                  , random= ~1|StudyID, data=bristoltest)
summary(allbristol)

car::Anova(allbristol, type="3")

myallbristol <- lsmeans(allbristol, "BaselineShan")
mybdif <- contrast(myallbristol, interaction = "pairwise")

allbres <- summary(mybdif)

myallbristol
allbres

bristolmean_frame <- as.data.frame(myallbristol)
ggplot(bristolmean_frame, aes(x = BaselineShan, y = lsmean, color = BaselineShan)) +
  geom_point() +
  geom_errorbar(aes(ymin = lsmean - SE, ymax = lsmean + SE), linewidth = 0.2, size = 0.8) +
  labs(x = "Shannon Diversity", y = "Bristol Stool Lsmeans") + 
  theme_bw() +
  theme(panel.background = element_rect(fill="white"), panel.grid=element_blank())


#plotting data 



#need to subset to do high and low baseline value differences 

LowDiversity <- subset(primary, primary$BaselineShan %in% c("Low"))

# model for low diversity individuals

#crude
crudelow <- lme(Mean_Shannon ~ Int_level + Sequence, random= ~1|StudyID, data=LowDiversity)
summary(crudelow)

#adjusted
Low <- lme(Mean_Shannon ~ Int_level + Sequence + Age + BMI+ Sex + FIBE, random= ~1|StudyID, data=LowDiversity)
summary(Low)


install.packages("car")
library(car)


car::Anova(Low, type="3")
Lowlsmeans <- lsmeans(Low, "Int_level")
Lowlsmeansdif <- contrast(Lowlsmeans, interaction = "pairwise")

Lowres <- summary(Lowlsmeansdif)

Lowlsmeans
Lowres

#CORRECT
#LOW Diversity LSMEANS GGPLOT
Lowmean_frame <- as.data.frame(Lowlsmeans)
ggplot(Lowmean_frame, aes(x = Int_level, y = lsmean, color = Int_level)) +
  geom_point() +
  geom_errorbar(aes(ymin = lsmean - SE, ymax = lsmean + SE), linewidth = 0.2, size = 0.8) +
  labs(x = "Intervention Level", y = "Shannon Diversity Lsmeans") + 
  scale_color_manual(values = c("darkblue", "darkgreen", "salmon")) +
  theme_bw() +
  theme(panel.background = element_rect(fill="white"), panel.grid=element_blank())

#NA for the moment
#ageLow <- lme(Mean_Shannon ~ Age, random= ~1|StudyID, data=LowDiversity)
#summary(ageLow)
#car::Anova(reduced_Low, type="3")
#reducedlsmeans <- lsmeans(reduced_Low, "Int_level")
#reducedlsmeansdif <- contrast(reducedlsmeans, interaction = "pairwise")
#reducedres <- summary(reducedlsmeansdif)
#reducedres

table(LowDiversity$Int_level, LowDiversity$Period)

table(LowDiversity$Int_level, LowDiversity$Sequence)

table(LowDiversity$Period, LowDiversity$Int_timing)

#Low Baseline Shannon Mean Shannon differences Low vs high intervention 

ggplot(LowDiversity, aes(x = Int_level, y = Mean_Shannon, group = StudyID)) + geom_point() + geom_line()


#High Diversity LME model 

HighDiversity <- subset(primary, primary$BaselineShan %in% c("High"))

High <- lme(Mean_Shannon ~ Int_level + Sequence + BMI + Sex + Age + FIBE, random= ~1|StudyID, data=HighDiversity)
summary(High)

#Type III effects predictors 
car::Anova(High, type="3")

#Pairwise test for treatment effects 
Highlsmeans <- lsmeans(High, "Int_level")
Highlsmeansdif <- contrast(Highlsmeans, interaction = "pairwise")

highres <- summary(Highlsmeansdif)

highres

#Correct
#HIGH Diversity LSMEANS GGPLOT
Highmean_frame <- as.data.frame(Highlsmeans)
ggplot(Highmean_frame, aes(x = Int_level, y = lsmean, color = Int_level)) +
  geom_point() +
  geom_errorbar(aes(ymin = lsmean - SE, ymax = lsmean + SE), linewidth = 0.2, size = 0.8) +
  labs(x = "Intervention Level", y = "Shannon Diversity Lsmeans") + 
  scale_color_manual(values = c("darkblue", "darkgreen", "salmon")) +
  theme_bw() +
  theme(panel.background = element_rect(fill="white"), panel.grid=element_blank()) + 
  scale_y_continuous(breaks = seq(floor(min(Highmean_frame$lsmean)), ceiling(max(Highmean_frame$lsmean)), by = 0.1))



#High Baseline Shannon Mean Shannon differences Low vs high intervention 
ggplot(HighDiversity, aes(x = Int_level, y = Mean_Shannon, group = StudyID)) + geom_point() + geom_line()


