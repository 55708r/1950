##----------------------------------------------------------##
## PSY1950: Intermediate Statistical Analysis in Psychology ##
## Midterm Project by Zoe Beatty and Izabelė Jonušaitė      ##
## Submitted on October 22nd, 2023                          ##
##----------------------------------------------------------##

# We have worked on separate data sets and performed analyses individually. We then combined our scripts into this joint script. 
# Part 1 is by Zoe Beatty, Part 2 is by Izabelė Jonušaitė.

## INITIALIZE: Need to use this to make sure R finds files in the same folder as this script:

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}


########################### MIDTERM PROJECT PART 1 (Zoe Beatty) ####################

#Midterm Project Work in Progress
#Zoe G. Beatty
#October 22nd, 2023 

#WORKING DIRECTORY NEEDS TO BE SET TO FOLDER DATASET IS IN FOR THIS TO RUN!

#The dataset I chose for my project is that used for the 2011 Nature paper
#'Post-traumatic stress disorer is associated with PACAP and the PAC1 
#receptor' by Ressler and colleagues. Dr. Kerry Ressler is my thesis advisor
#and is allowing me to analyze this dataset. 
#The paper is linked (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3046811/)

#Saliva samples were collected from patients in Atlanta, GA hospitals (either in primary
#care or OBG clinics). A smaller subset of participants also donated blood. 

#Participants information that was collected: age, income, employment, substance abuse info, 
#childhood traumatic questionairre, PTSD symptom scale (PSS) score, Beck 
#Depression Inventory score. Additionally, there is genetic (on gene variants and methylation) 
#and biological data (from blood samples). 

#Index for Code

#Lines 67 - 670 are EDA of entire data set 
#Lines 670 - 980 are analysis of entire data set (regressions, hypothesis testing)
#Lines 980 - 1110 are EDA of the subset of the dataset with a PTSD diagnosis (as this is of interest in the field)
#Remaining lins are analysis of PTSD subset of dataset (regressions, hypothesis testing)

#all needed packages are called up here! 
library(haven)
library("see")              
library("car")
library("sjPlot")              
library("boot")                
library("estimatr")             
library("vioplot")
library("car")             
library("polycor")  
library("corrplot") 
library("MASS")  
library("see") 
library("sjPlot")                 
library("effects") 
library("ggstatsplot")
library("emmeans") 
library("foreign") 
library("ggplot2")

#import dataset
#dataset and R file are both in folder titled MidtermZGB
PACAP_dataset_raw <- read_sav("PSY1950_Beatty_Dataset.sav")

#Clean up dataset, this will just involve getting rid of any variables that I am not interested in 
#for my analysis to make the dataset more manageable. 

PACAP_dataset <- PACAP_dataset_raw[, c(1:3, 6:10, 17:22, 29:35, 41:44, 81, 87, 90:94, 96)]

dim(PACAP_dataset)
#dataset has 1457 rows and 34 columns. This is much more managable than the original dataset
#and includes the variables i want to look at. 

#I will start exploring the dataset by just looking at it!
View(PACAP_dataset)
head(PACAP_dataset)    

#We can see a few pieces of demographic data at the beginning. These will be helpful
#for controls and assessing interactions. 

#The cleaned up dataset has demographic info in columns 2-6, psychiatric examination info
#in columns 7-26 and genetic/biological info in 27-34. 

#Changing necessary variables to factors
sex <- factor(PACAP_dataset$sex, labels = c("male", "female"))
employment <- factor(PACAP_dataset$employment, labels = c("no", "yes"))
subs_abuse_past <- factor(PACAP_dataset$subs_abuse_past, labels = c("no", "yes"))
subs_abuse_now <- factor(PACAP_dataset$subs_abuse_now, labels = c("no", "yes"))
suicide_attempt <- factor(PACAP_dataset$suicide_attempt, labels = c("no", "yes"))
PSS_clinical_sig_sxs_01 <- factor(PACAP_dataset$PSS_clinical_sig_sxs_01, labels = c("no", "yes"))
PTSD_diagnosis <- factor(PACAP_dataset$PTSD_diagnosis, labels = c("no", "yes"))
BDI_CAT <- factor(PACAP_dataset$BDI_CAT, labels = c("no", "yes")) 
PACAP38pM_01 <- factor(PACAP_dataset$PACAP38pM_01, labels = c("low", "high")) 
PACAP38pM01 <- factor(PACAP_dataset$PACAP38pM01, labels = c("low", "high")) 
PACAPpM01 <- factor(PACAP_dataset$PACAPpM01, labels = c("low", "high")) 
income <- factor(PACAP_dataset$income, labels = c("$0-249", "$250-499", "$500-999", "$1000-1999", "$2000+"))
#STARTING EDA

#I will start off by orienting myself to the data using location measures, dispersion measures and histograms. This will
#give me a sense of the distribution. I will do this for my metric variables and then move on. 

#LOCATION/DISPERSION AND HISTOGRAM EDA FOR METRIC (or quasi-metric) VARIABLES

#AGE
#location and dispersion measures for age of participants
mean(PACAP_dataset$age, na.rm = TRUE)
median(PACAP_dataset$age, na.rm = TRUE)
var(PACAP_dataset$age, na.rm = TRUE)
sd(PACAP_dataset$age, na.rm = TRUE)
range(PACAP_dataset$age, na.rm = TRUE)

#histogram showing distribution of age of participants
hist(PACAP_dataset$age, main = "Age of Participants")
#we can see that age is left skewed relatively normal with a median of 42 and mean of ~39.5. 
#the youngest participant is 18 and the oldest is 77. 

#CHILDHOOD TRAUMA QUESTIONNAIRE TOTAL SCORES

#location and dispersion measures for age of participants
mean(PACAP_dataset$CTQTOT, na.rm = TRUE)
median(PACAP_dataset$CTQTOT, na.rm = TRUE)
var(PACAP_dataset$CTQTOT, na.rm = TRUE)
sd(PACAP_dataset$CTQTOT, na.rm = TRUE)
range(PACAP_dataset$CTQTOT, na.rm = TRUE)

#histogram showing CTQ (childhood trauma questionnaire) total scores among participants
hist(PACAP_dataset$CTQTOT, main = 'Childhood Trauma Questionnaire (CTQ) Scores') 

#we can see that CTQ total scores are strongly skewed to the right with mean of 41.5 and median of 36. 
#the lowest score is 25 and the highest score is 120. 

#CHILDHOOD TRAUMA QUESTIONNAIRE SEXUAL ABUSE SCORES

mean(PACAP_dataset$CTQ_SEX_AB, na.rm = TRUE)
median(PACAP_dataset$CTQ_SEX_AB, na.rm = TRUE)
var(PACAP_dataset$CTQ_SEX_AB, na.rm = TRUE)
sd(PACAP_dataset$CTQ_SEX_AB, na.rm = TRUE)
range(PACAP_dataset$CTQ_SEX_AB, na.rm = TRUE)
hist(PACAP_dataset$CTQ_SEX_AB, main = 'CTQ Sexual Abuse Scores') 

#we can see that CTQ sex abuse scores are also strongly skewed right with mean of 7.6 and median of 5
#the lowest score is 5 and the highest score is 25

#CHILDHOOD TRAUMA QUESTIONNAIRE EMOTIONAL ABUSE SCORES

mean(PACAP_dataset$CTQ_EMOT_AB, na.rm = TRUE)
median(PACAP_dataset$CTQ_EMOT_AB, na.rm = TRUE)
var(PACAP_dataset$CTQ_EMOT_AB, na.rm = TRUE)
sd(PACAP_dataset$CTQ_EMOT_AB, na.rm = TRUE)
range(PACAP_dataset$CTQ_EMOT_AB, na.rm = TRUE)
hist(PACAP_dataset$CTQ_EMOT_AB, main = 'CTQ Emotional Abuse Scores') 

#we can see that CTQ emotional abuse scores are strongly skewed right iwth mean of 9.08 and median of 7
#the lowest score is 5 and the highest score is 25

#CHILDHOOD TRAUMA QUESTIONNAIRE PHYSICAL ABUSE SCORES

mean(PACAP_dataset$CTQ_PHYS_AB, na.rm = TRUE)
median(PACAP_dataset$CTQ_PHYS_AB, na.rm = TRUE)
var(PACAP_dataset$CTQ_PHYS_AB, na.rm = TRUE)
sd(PACAP_dataset$CTQ_PHYS_AB, na.rm = TRUE)
range(PACAP_dataset$CTQ_PHYS_AB, na.rm = TRUE)
hist(PACAP_dataset$CTQ_PHYS_AB, main = 'CTQ Physical Abuse Scores') 

#we can see that CTQ physical abuse scores are strongly skewed right with mean of 8.3 and median of 7
#the lowest score is 5 and the highest score is 25

#CHILDHOOD TRAUMA QUESTIONNAIRE EMOTIONAL NEGLECT SCORES

mean(PACAP_dataset$CTQ_EMOT_NEG, na.rm = TRUE)
median(PACAP_dataset$CTQ_EMOT_NEG, na.rm = TRUE)
var(PACAP_dataset$CTQ_EMOT_NEG, na.rm = TRUE)
sd(PACAP_dataset$CTQ_EMOT_NEG, na.rm = TRUE)
range(PACAP_dataset$CTQ_EMOT_NEG, na.rm = TRUE)
hist(PACAP_dataset$CTQ_EMOT_NEG, main = 'CTQ Emotional Neglect Scores') 

#we can see that CTQ emotional neglect scores are strongly skewed right with mean of 9.3 and median of 7
#the lowest score is 5 and the highest score is 25

#CHILDHOOD TRAUMA QUESTIONNAIRE PHYSICAL NEGLECT SCORES

mean(PACAP_dataset$CTQ_PHYS_NEG, na.rm = TRUE)
median(PACAP_dataset$CTQ_PHYS_NEG, na.rm = TRUE)
var(PACAP_dataset$CTQ_PHYS_NEG, na.rm = TRUE)
sd(PACAP_dataset$CTQ_PHYS_NEG, na.rm = TRUE)
range(PACAP_dataset$CTQ_PHYS_NEG, na.rm = TRUE)
hist(PACAP_dataset$CTQ_PHYS_NEG, main = 'CTQ Physical Neglect Scores') 

#we can see that CTQ physical neglect scores are strongly skewed right with mean of 7.3 and median of 6
#the lowest score is 5 and the highest score is 23

#Overall, looking at CTQ totals and subscores, we see a heavy right skew but the full range of potential values
#up to 25 are met. SDs hovered between 4 and 5 for all subsets. 

#PTSD SYMPTOM SCALE TOTAL SCORES

mean(PACAP_dataset$PSStotal, na.rm = TRUE)
median(PACAP_dataset$PSStotal, na.rm = TRUE)
var(PACAP_dataset$PSStotal, na.rm = TRUE)
sd(PACAP_dataset$PSStotal, na.rm = TRUE)
range(PACAP_dataset$PSStotal, na.rm = TRUE)

#histogram showing PTSD symptom scale (PSS) total scores among participants
hist(PACAP_dataset$PSStotal, main = 'PTSD Symptom Scale (PSS) Scores') 

#we see that total PSS scores are strongly right skewed with mean of 12.8 and median of 9
#the lowest score is 0 and the highest score is 51

#histogram showing PTSD symptom scale (PSS) score by category

#PTSD SYMPTOM SCALE INTRUSION SUBSCORES

mean(PACAP_dataset$PSS_Intrusive, na.rm = TRUE)
median(PACAP_dataset$PSS_Intrusive, na.rm = TRUE)
var(PACAP_dataset$PSS_Intrusive, na.rm = TRUE)
sd(PACAP_dataset$PSS_Intrusive, na.rm = TRUE)
range(PACAP_dataset$PSS_Intrusive, na.rm = TRUE)
hist(PACAP_dataset$PSS_Intrusive, main = 'PTSD Symptom Scale (PSS) Intrusion Scores') 

#we see that total PSS intrusion subscores are strongly right skewed with mean of 3.2 and median of 2
#the lowest score is 0 and the highest score is 15 

#PTSD SYMPTOM SCALE AVOIDANCE/NUMBING SUBSCORES

mean(PACAP_dataset$PSS_avoidnumb, na.rm = TRUE)
median(PACAP_dataset$PSS_avoidnumb, na.rm = TRUE)
var(PACAP_dataset$PSS_avoidnumb, na.rm = TRUE)
sd(PACAP_dataset$PSS_avoidnumb, na.rm = TRUE)
range(PACAP_dataset$PSS_avoidnumb, na.rm = TRUE)
hist(PACAP_dataset$PSS_avoidnumb, main = 'PTSD Symptom Scale (PSS) Avoidance/Numbing Scores') 

#we see that total PSS avoidance/numbing subscores are strongly right skewed with mean of 5.1 and median of 3
#the lowest score is 0 and the highest score is 21

#PTSD SYMPTOM SCALE HYPERAROUSAL SUBSCORES

mean(PACAP_dataset$PSS_hyperarousal, na.rm = TRUE)
median(PACAP_dataset$PSS_hyperarousal, na.rm = TRUE)
var(PACAP_dataset$PSS_hyperarousal, na.rm = TRUE)
sd(PACAP_dataset$PSS_hyperarousal, na.rm = TRUE)
range(PACAP_dataset$PSS_hyperarousal, na.rm = TRUE)
hist(PACAP_dataset$PSS_hyperarousal, main = 'PTSD Symptom Scale (PSS) Hyperarousal Scores') 

#we see that total PSS avoidance/numbing subscores are strongly right skewed with mean of 4.5 and median of 3
#the lowest score is 0 and the highest score is 15 

#Overall we see that PSS scores are heavily skewed, similarly to the CTQ scores and as we would expect
#anecdotally. 

#BECK DEPRESSION INVENTORY SCORE

#histogram showing Beck Depression Inventory score among participants 

mean(PACAP_dataset$BDItotalscore, na.rm = TRUE)
median(PACAP_dataset$BDItotalscore, na.rm = TRUE)
var(PACAP_dataset$BDItotalscore, na.rm = TRUE)
sd(PACAP_dataset$BDItotalscore, na.rm = TRUE)
range(PACAP_dataset$BDItotalscore, na.rm = TRUE)
hist(PACAP_dataset$BDItotalscore, main = 'Beck Depression Inventory (BDI) score') 

#we see that Beck Depression Inventory scores are right skewed with mean of 14.3 and median of 11
#the lowest score is 0 and the highest score is 56 

#EDA FOR SINGLE CATEGORICAL VARIABLES

#Frequency Tables and Barplots for Sex 
table(PACAP_dataset$sex)  
prop.table(table(PACAP_dataset$sex))           
barplot(table(PACAP_dataset$sex), xlab = "Sex", main = "Sex Bar Chart")

#The sample is ~62% female and 37% male. This may have been done because PTSD diagnosis 
#is much more common in women. Either way, good to note that these are not nearly even. 

#Frequency Tables and Barplots for Employment 
table(PACAP_dataset$employment)              
prop.table(table(PACAP_dataset$employment))            
barplot(table(PACAP_dataset$employment), xlab = "Employment", main = "Employment Bar Chart")

#~71% of the sample is currently unemployed.. so this is a little bit odd and good to note. 

#Frequency Tables and Barplots for Past Substance Use
table(PACAP_dataset$subs_abuse_past)                  
prop.table(table(PACAP_dataset$subs_abuse_past))           
barplot(table(PACAP_dataset$subs_abuse_past), xlab = "Past Substance Abuse", main = "Past Substance Abuse Bar Chart")

#~30% of sample has abused substances in the past (self reported)

#Frequency Tables and Barplots for Current Substance Use
table(PACAP_dataset$subs_abuse_now)                  
prop.table(table(PACAP_dataset$subs_abuse_now))           
barplot(table(PACAP_dataset$subs_abuse_now), xlab = "Current Substance Abuse", main = "Current Substance Abuse Bar Chart")

# ~5% of sample abuses substances currently (self reported)

#Frequency Tables and Barplots for Suicide Attempt History
table(PACAP_dataset$suicide_attempt)                  
prop.table(table(PACAP_dataset$suicide_attempt))           
barplot(table(PACAP_dataset$suicide_attempt), xlab = "Suicide Attempt History", main = "Suicide Attempt History Bar Chart")

# ~16% of sample has attempted suicide in the past 

#Frequency Tables and Barplots for PTSD Diagnosis 
table(PACAP_dataset$PTSD_diagnosis)                  
prop.table(table(PACAP_dataset$PTSD_diagnosis))           
barplot(table(PACAP_dataset$PTSD_diagnosis), xlab = "PTSD Diagnosis", main = "PTSD Diagnosis Bar Chart")

# ~33% of sample has a PTSD diagnosis 

#Frequency Tables and Barplots for Depression Diagnosis 
table(PACAP_dataset$BDI_CAT)                  
prop.table(table(PACAP_dataset$BDI_CAT))           
barplot(table(PACAP_dataset$BDI_CAT), xlab = "Depression Diagnosis", main = "Depression Diagnosis Bar Chart")

# ~32% of sample has a depression diagnosis 

#Frequency Tables and Barplots for Income
table(PACAP_dataset$income)                  
prop.table(table(PACAP_dataset$income))           
barplot(table(PACAP_dataset$income), xlab = "Income", main = "Income Bar Chart")

#The sample is relatively low income 

#Frequency Tables and Barplots for PACAP38 Levels
table(PACAP_dataset$PACAP38pM_01)                  
prop.table(table(PACAP_dataset$PACAP38pM_01))           
barplot(table(PACAP_dataset$PACAP38pM_01), xlab = "PACAP 38 Levels", main = "PACAP 38 Levels Bar Chart")
#Half of the sample (that had blood drawn) is considered to have "high" PACAP 38 blood levels


#Frequency Tables and Barplots for Total PACAP Levels
table(PACAP_dataset$PACAPpM01)                  
prop.table(table(PACAP_dataset$PACAPpM01))           
barplot(table(PACAP_dataset$PACAPpM01), xlab = "PACAP Levels", main = "PACAP Levels Bar Chart")

#Half of the sample (that had blood drawn) is considered to have "high" overall PACAP blood levels
#my guess is that it was split by the average by design. 

#Now that I have done EDA on individual variables (categorical and metric), I am going to do some EDA on multiple 
#variables. 

#I will start by making a correlation matrix of all the metric variables to see which ones
#might be interesting to look at further

head(PACAP_dataset[, c(3, 9:18, 24, 27, 29)])
cormat <- cor(PACAP_dataset[, c(2:3, 9:18, 24, 27, 29)],  use = "pairwise.complete.obs")
cormat
round(cormat, 3)

corrplot(cormat)                       ## from corrplot package
corrplot(cormat, method = "ellipse")
corrplot.mixed(cormat, upper = "ellipse", lower = "number", lower.col = "gray")

#we see from the matrix that CTQ scores are moderately positively correlated with PSS scores
#we also see BDI sore is highly correlated with PSS scores and moderately correlated with CTQ scores
#we see that PACAP38 levels are moderately positively correlated with PSS scores. 
#there doesn't appear to be strong correlations between age and any of the given variables 

#We also see a positive correlation between childhood sexual abuse scores and sex. 
#This might be interesting to look at and appears to show a correlation between increased sexual abuse and 
#being female. 

#Now we will make scatterplots for the metric variables that look particularly interesting together 

#SCATTERPLOTS

#I am interested in scatterplots between scales (PSS vs. CTQ vs BDI) so I will start with those

#scatterplot of PTSD symptom scale (PSS) and Beck Depression Inventory (BDI) total scores
scatterplot(BDItotalscore ~ PSStotal, main = "PTSD (PSS) vs. Depression (BDI) Scores", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'BDI Score', ylab = 'PSS Score') 

#scatterplot of PTSD symptom scale (PSS) and CTQ total scores
scatterplot(CTQTOT ~ PSStotal, main = "PTSD (PSS) vs. CTQ Scores", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'CTQ Score', ylab = 'PSS Score') 

#scatterplot of Beck Depression Inventory (BDI) total scores and CTQ total scores
scatterplot(CTQTOT ~ BDItotalscore, main = "BDI vs. CTQ Scores", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'CTQ Score', ylab = 'BDI Score') 

#We see that all of these scores are positively correlated with each other as expected. 

#I am also interested whether types of abuse and neglect are correlated. I will plot these below. 

#scatterplot CTQ Emotional Abuse x CTQ Physical Abuse scores
scatterplot(CTQ_PHYS_AB ~ CTQ_EMOT_AB, main = "Emotional vs. Physical Abuse CTQ Scores", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'Emotional Abuse CTQ Score', ylab = 'Physical Abuse CTQ Score') 

#scatterplot CTQ Emotional Abuse x CTQ Sexual Abuse scores
scatterplot(CTQ_SEX_AB ~ CTQ_EMOT_AB, main = "Emotional vs. Sexual Abuse CTQ Scores", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'Emotional Abuse CTQ Score', ylab = 'Sexual Abuse CTQ Score') 

#scatterplot CTQ Sexual Abuse x CTQ Physical Abuse scores
scatterplot(CTQ_PHYS_AB ~ CTQ_SEX_AB, main = "Sexual vs. Physical Abuse CTQ Scores", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'Sexual Abuse CTQ Score', ylab = 'Physical Abuse CTQ Score') 

#scatterplot of CTQ Emotional Neglect vs. Physical Neglect Scores 
scatterplot(CTQ_EMOT_NEG ~ CTQ_PHYS_NEG, main = "CTQ Physical Neglect vs. Emotional Neglect Scores", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'CTQ Physical Neglect Score', ylab = 'CTQ Emotional Neglect Score') 

#scatterplot of CTQ Emotional Neglect vs Emotional Abuse Scores
scatterplot(CTQ_EMOT_NEG ~ CTQ_EMOT_AB, main = "CTQ Emotional Abuse vs. Emotional Neglect Scores", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'CTQ Emotional Abuse Score', ylab = 'CTQ Emotional Neglect Score') 

#scatterplot of CTQ Physical Neglect vs. Physical Abuse Scores 
scatterplot(CTQ_PHYS_AB ~ CTQ_PHYS_NEG, main = "CTQ Physical Neglect vs. Physical Abuse Scores", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'CTQ Physical Neglect Score', ylab = 'CTQ Physical Abuse Score') 

#Lastly, I am interested in the scatterplot between PACAP and PSS scores 
scatterplot(PACAP38pM ~ PSStotal, main = "PTSD (PSS) Scores vs. PACAP38 Concentration", col = "black", smooth = FALSE, data = PACAP_dataset, xlab = 'PACAP38 Concentration', ylab = 'PSS Score') 

#Now I will make some tables to compare sets of categorical variables. I will stick to ones that seem
#particularly interesting in relation to each other. 

#Sex vs. PACAP38 (high vs. low)
sex_PACAP38 <- table(sex = PACAP_dataset$sex, PACAP_levels = PACAP_dataset$PACAP38pM_01)
sex_PACAP38

#we see that high vs low PACAP levels are pretty evenly distributed across sex from what
#we can tell

#PTSD Diagnosis vs. PACAP38
PTSD_PACAP38 <- table(PTSD_diagnosis = PACAP_dataset$PTSD_diagnosis, PACAP_levels = PACAP_dataset$PACAP38pM_01)
PTSD_PACAP38

#Depression Diagnosis vs. PACAP38
Depress_PACAP38 <- table(Depression_diagnosis = PACAP_dataset$BDI_CAT, PACAP_levels = PACAP_dataset$PACAP38pM_01)
Depress_PACAP38

#we see that the proportion of people with high PACAP levels is higher in people
#with a PTSD diagnosis than without one. 

#Sex vs. PTSD Diagnosis
sex_PTSD <- table(sex = PACAP_dataset$sex, PTSD_Diagonosis = PACAP_dataset$PTSD_diagnosis)
sex_PTSD

#proportions of PTSD diagnosis are quite close between sexes

#Sex vs. Depression Diagnosis 
sex_Depress <- table(sex = PACAP_dataset$sex, BDI_CAT = PACAP_dataset$BDI_CAT)
sex_Depress

#It appears the proportion of those who meet critera for depression are higher in females

#Sex vs. Suicide Attempt
sex_suicideA <- table(sex = PACAP_dataset$sex, suicide_attempt = PACAP_dataset$suicide_attempt)
sex_suicideA

#it appears that suicide attempt history was of a higher proportion for females in the sample

#PTSD Diagnosis vs. Suicide Attempt
PTSD_suicideA <- table(PTSD_diagnosis = PACAP_dataset$PTSD_diagnosis, suicide_attempt = PACAP_dataset$suicide_attempt)
PTSD_suicideA

#The proportion who have had a suicide attempt is much higher in the population diagnosed with PTSD

#Depression Diagnosis vs. Suicide Attempt
Depress_suicideA <- table(Depression_diagnosis = PACAP_dataset$BDI_CAT, suicide_attempt = PACAP_dataset$suicide_attempt)
Depress_suicideA

#the proportion who have had a suicide attempt is much higher in the population diagnosed with depression 

#PTSD Diagnosis vs. Income
PTSD_income <- table(PTSD_diagnosis = PACAP_dataset$PTSD_diagnosis, income = PACAP_dataset$income)
PTSD_income

#interesting to see the distribution of participants across income
#this sample has a lot of people in low income categories
#it may be interesting to see if income has an impact on PTSD diagnosis later 

#Income vs. Depression Diagnosis 
Income_Depress <- table(income = PACAP_dataset$income, BDI_CAT = PACAP_dataset$BDI_CAT)
Income_Depress

#same comments as for PTSD... it may be interesting to see if income has an 
#impact on depression later 

#PTSD Diagnosis vs. Depression Diagnosis
PTSD_Depress <- table(PTSD_diagnosis = PACAP_dataset$PTSD_diagnosis, Depression_Diagnosis = PACAP_dataset$BDI_CAT)
PTSD_Depress

#proportion of those with a depression diagnosis is much higher in those with a PTSD diagnosis and vice versa
#this is consistent with seeing a positive correlation between BDI total score and PSS total score 

#Now I will do violin plots to compare metric variables between categorical variables I am particularly
#interested in 

#I am interested in the relationship between sex and several of my metric variables
#The violin plots for sex vs. those variables are below. 

#For these, I will note any observations of interest! 

#violin plot of PTSD symptom scale score across sexes
vioplot(PSStotal ~ sex, data = PACAP_dataset, col = "bisque", ylab = "PSS Total Score")

#violin plot of PTSD symptom scale intrusion category score across sexes
vioplot(PSS_Intrusive ~ sex, data = PACAP_dataset, col = "bisque", ylab = "PSS Intrusion Score")

#the variation in shape of the distribution is interesting here. 
#means in intrusion scores are similar but the distribution for females
#is much larger at the higher PSS intrusion scores 

#violin plot of PTSD symptom scale avoidance/numbing category score across sexes
vioplot(PSS_avoidnumb ~ sex, data = PACAP_dataset, col = "bisque", ylab = "PSS Avoidance and Numbing Score")

#this variation in shape of the distribution is similar to that for intrusion scores. 
#similar means but the distribution for females is larger at higher scores 

#violin plot of PTSD symptom scale score across sexes
vioplot(PSS_hyperarousal ~ sex, data = PACAP_dataset, col = "bisque", ylab = "PSS Hyperarousal Score")

#this is more evenly distributed across sexes 

#violin plot of CTQ (childhood trauma questionnaire) score across sexes 
vioplot(CTQTOT ~ sex, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Score')

#violin plot of CTQ Sexual Abuse score across sexes
vioplot(CTQ_SEX_AB ~ sex, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Sexual Abuse Score')

#violin plot of CTQ Physical Abuse score across sexes
vioplot(CTQ_PHYS_AB ~ sex, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Physical Abuse Score')

#violin plot of CTQ Emotional Abuse score across sexes
vioplot(CTQ_EMOT_AB ~ sex, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Emotional Abuse Score')

#violin plot of CTQ Physical Neglect score across sexes
vioplot(CTQ_PHYS_NEG ~ sex, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Physical Neglect Score')

#violin plot of CTQ Emotional Neglect score across sexes
vioplot(CTQ_EMOT_NEG ~ sex, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Emotional Neglect Score')

#the CTQ distribution shapes look pretty similar between sexes
#the one thing I noticed was that the female group had a notably higher 
#maximum for a few of the subcategories. 

#violin plot of Beck Depression Inventory across sexes
vioplot(BDItotalscore ~ sex, data = PACAP_dataset, col = "bisque", ylab = 'BDI Total Score')

#mean depression scores are a little bit higher in females

#I am also interested in the relationship between income and these metric variables 

#violin plot of PTSD symptom scale score across income
vioplot(PSStotal ~ income, data = PACAP_dataset, col = "bisque", ylab = "PSS Total Score")

#this is mostly interested in that the maximum PSS scores vary with income
#the two highest income groups have notable lower maximum PSS scores
#this doesnt necessarily mean anything but is interesting to observe 
#and I might look more directly at the contribution of income to PSS scores 

#violin plot of PTSD symptom scale intrusion category score across income
vioplot(PSS_Intrusive ~ income, data = PACAP_dataset, col = "bisque", ylab = "PSS Intrusion Score")

#violin plot of PTSD symptom scale avoidance/numbing category score across income
vioplot(PSS_avoidnumb ~ income, data = PACAP_dataset, col = "bisque", ylab = "PSS Avoidance and Numbing Score")

#violin plot of PTSD symptom scale hypearousal score across income
vioplot(PSS_hyperarousal ~ income, data = PACAP_dataset, col = "bisque", ylab = "PSS Hyperarousal Score")

#distribution shape varies by income 

#violin plot of CTQ (childhood trauma questionnaire) score across income
vioplot(CTQTOT ~ income, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Score')

#violin plot of CTQ Sexual Abuse score across income
vioplot(CTQ_SEX_AB ~ income, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Sexual Abuse Score')

#violin plot of CTQ Physical Abuse score across income
vioplot(CTQ_PHYS_AB ~ income, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Physical Abuse Score')

#violin plot of CTQ Emotional Abuse score across income
vioplot(CTQ_EMOT_AB ~ income, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Emotional Abuse Score')

#violin plot of CTQ Physical Neglect score across income
vioplot(CTQ_PHYS_NEG ~ income, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Physical Neglect Score')

#violin plot of CTQ Emotional Neglect score across income
vioplot(CTQ_EMOT_NEG ~ income, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Emotional Neglect Score')

#violin plot of Beck Depression Inventory across income
vioplot(BDItotalscore ~ income, data = PACAP_dataset, col = "bisque", ylab = 'BDI Total Score')

#I am also interested in the relationship between a suicide attempt history and these metric variables 

#violin plot of PTSD symptom scale score across suicide attempt history
vioplot(PSStotal ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = "PSS Total Score")

#we see two modes for PSS total scores for those who have had a suicide attempt

#violin plot of PTSD symptom scale intrusion category score across suicide attempt history
vioplot(PSS_Intrusive ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = "PSS Intrusion Score")

#violin plot of PTSD symptom scale avoidance/numbing category score across suicide attempt history
vioplot(PSS_avoidnumb ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = "PSS Avoidance and Numbing Score")

#again we see the two modes for those who have had a suicide attempt
#its notable that we dont see these two modes as strongly for the intrusion 
#category as we do for the avoidance and numbing category. 

#violin plot of PTSD symptom scale score across suicide attempt history
vioplot(PSS_hyperarousal ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = "PSS Hyperarousal Score")

#two modes for those who have had a suicide attempt again. We see that there is a large 
#proportion of those who have attempted suicide with very high (almost maximum)
#hyperarousal scores

#it might be interesting to see the contribution of different types of PTSD
#symptoms to whether someone has attempted suicide. 

#violin plot of CTQ (childhood trauma questionnaire) score across suicide attempt history
vioplot(CTQTOT ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Score')

#distribution of CTQ scores is much different for those that have attempted suicide vs not
#mean CTQ scores are much higher in those who have attempted suicide. 
#distribution is much fatter at higher CTQ scores for those who have attempted suicide
#and there is a higher maximum. 

#violin plot of CTQ Sexual Abuse score across suicide attempt history
vioplot(CTQ_SEX_AB ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Sexual Abuse Score')

#violin plot of CTQ Physical Abuse score across suicide attempt history
vioplot(CTQ_PHYS_AB ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Physical Abuse Score')

#violin plot of CTQ Emotional Abuse score across suicide attempt history
vioplot(CTQ_EMOT_AB ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Emotional Abuse Score')

#two modes for emotional abuse scores for those who have attempted suicide
#mean is much higher for emotional abuse scores in those who have a suicide attempt 

#violin plot of CTQ Physical Neglect score across suicide attempt history
vioplot(CTQ_PHYS_NEG ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Physical Neglect Score')

#violin plot of CTQ Emotional Neglect score across suicide attempt history
vioplot(CTQ_EMOT_NEG ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Emotional Neglect Score')

#two modes for emotional neglect scores for those who have attempted suicide

#it might be interesting to see the contribution of different types of childhood abuse and suicide attempts later

#violin plot of Beck Depression Inventory across suicide attempt history
vioplot(BDItotalscore ~ suicide_attempt, data = PACAP_dataset, col = "bisque", ylab = 'BDI Total Score')

#very high mode depression score for those who have attempted suicide

#I am also interested in the relationship between PACAP 38 levels (low or high) and these metric variables

vioplot(PSStotal ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = "PSS Total Score")

#two modes, large proportion of those with high PACAP scores have high PSS total scores 

#violin plot of PTSD symptom scale intrusion category score across PACAP 38 Levels
vioplot(PSS_Intrusive ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = "PSS Intrusion Score")

#violin plot of PTSD symptom scale avoidance/numbing category score across PACAP 38 Levels
vioplot(PSS_avoidnumb ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = "PSS Avoidance and Numbing Score")

#two modes 

#violin plot of PTSD symptom scale score across PACAP 38 Levels
vioplot(PSS_hyperarousal ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = "PSS Hyperarousal Score")

#two modes 

#violin plot of CTQ (childhood trauma questionnaire) score across PACAP 38 Levels
vioplot(CTQTOT ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Score')

#violin plot of CTQ Sexual Abuse score across PACAP 38 Levels
vioplot(CTQ_SEX_AB ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Sexual Abuse Score')

#violin plot of CTQ Physical Abuse score across PACAP 38 Levels
vioplot(CTQ_PHYS_AB ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Physical Abuse Score')

#violin plot of CTQ Emotional Abuse score across PACAP 38 Levels
vioplot(CTQ_EMOT_AB ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Emotional Abuse Score')

#Emotional abuse scores are much higher in those with high PACAP levels 
#It might be interesting to look at emotional abuse specifically later. 

#violin plot of CTQ Physical Neglect score across PACAP 38 Levels
vioplot(CTQ_PHYS_NEG ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Physical Neglect Score')

#violin plot of CTQ Emotional Neglect score across PACAP 38 Levels
vioplot(CTQ_EMOT_NEG ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = 'CTQ Emotional Neglect Score')

#violin plot of Beck Depression Inventory across PACAP 38 Levels
vioplot(BDItotalscore ~ PACAP38pM_01, data = PACAP_dataset, col = "bisque", ylab = 'BDI Total Score')

#two modes 

#EDA ENDS HERE

#imported for regression analysis 

#SIMPLE REGRESSION

#I will now make simple linear regression models for variables of interest

# I will first look at the relationship between PSS Scores, Depression Scores, and CTQ Scores

#simple linear regression between PTSD and Depression total scores 
PTSDtoDepression <- lm(PSStotal ~ BDItotalscore, data = PACAP_dataset)
PTSDtoDepression         
coefficients(PTSDtoDepression)              
summary(PTSDtoDepression)     
plot(PACAP_dataset$PSStotal, PACAP_dataset$BDItotalscore, pch = 20, main = "Regression Fit", xlab = "PSS Score", ylab = "Depression Score")
abline(PTSDtoDepression, col = "red", lwd = 2) 
confint(PTSDtoDepression)
plot_model(PTSDtoDepression, type = "eff", terms = c("BDItotalscore"))

#Y intercept is telling us the expected BDI total score given a PSS score of 0 is 2.86. 
#We see that we would expect a person with a 1 point higher PSS score than another 
#person to have a 0.684 point higher BDI score 
#these are clearly highly positively correlated 
#we see an R squared value of 0.45 

#I will check each of my simple regressions for constant variance using the residuals plot. 
plot(PTSDtoDepression, which = 1)  
#The variance does appear to me to be a funnel.. so that is suggesting a lack of constant variance.

#I will check each of my simple regressions for normality using a QQ plot
plot(PTSDtoDepression, which = 2) 
#This looks great!

#simple linear regression between childhood trauma experience and Depression total scores 
CTQtoDepression <- lm(CTQTOT ~ BDItotalscore, data = PACAP_dataset)
CTQtoDepression         
coefficients(CTQtoDepression)              
summary(CTQtoDepression)   
plot(PACAP_dataset$CTQTOT, PACAP_dataset$BDItotalscore, pch = 20, main = "Regression Fit", xlab = "CTQ Score", ylab = "Depression Score")
abline(CTQtoDepression, col = "red", lwd = 2) 
confint(CTQtoDepression)
plot_model(CTQtoDepression, type = "eff", terms = c("BDItotalscore"))

#Y intercept is telling us the expected BDI total score given a CTQ score of 0 is 33.5. 
#We see that we would expect a person with a 1 point higher CTQ score than another 
#person to have a 0.56 point higher BDI score 
#we see an R squared value of 0.15 

#We see that a lot more of the variance in a BDI total score can be predicted
#by ones PSS total score than by their childhood trauma score, which is kind of 
#interesting!

plot(CTQtoDepression, which = 1)  
#This one is a little bit harder for me to tell but I think it passes the constant variance assumption check. 

plot(CTQtoDepression, which = 2) 
#this looks light tailed

#simple linear regression between childhood trauma experience and PTSD symptoms
CTQtoPTSD <- lm(CTQTOT ~ PSStotal, data = PACAP_dataset)
CTQtoPTSD       
coefficients(CTQtoPTSD)              
summary(CTQtoPTSD)  
plot(PACAP_dataset$CTQTOT, PACAP_dataset$PSStotal, pch = 20, main = "Regression Fit", xlab = "CTQ Score", ylab = "PSS Score")
abline(CTQtoPTSD, col = "red", lwd = 2) 
confint(CTQtoPTSD)
plot_model(CTQtoPTSD, type = "eff", terms = c("PSStotal"))

plot(CTQtoPTSD, which = 1) 
#This one I will also say passes the constant variance assumption check 

plot(CTQtoPTSD, which = 2) 
#light-tailed again

#Y intercept is telling us the expected PSS total score given a CTQ score of 0 is 34.4. 
#We see that we would expect a person with a 1 point higher CTQ score than another 
#person to have a 0.56 point higher BDI score 
#we see an R squared value of 0.17 

#Next I will look at the relationship that each of these scores has with PACAP38 concentration

#simple linear regression between PACAP 38 concentration and PSS total
PACAPtoPTSD <- lm(PACAP38pM ~ PSStotal, data = PACAP_dataset)
PACAPtoPTSD       
coefficients(PACAPtoPTSD)              
summary(PACAPtoPTSD)  
plot(PACAP_dataset$PACAP38pM, PACAP_dataset$PSStotal, pch = 20, main = "Regression Fit", xlab = "PACAP 38", ylab = "PSS Score")
abline(PACAPtoPTSD, col = "red", lwd = 2)    
confint(PACAPtoPTSD)
plot_model(PACAPtoPTSD, type = "eff", terms = c("PSStotal"))

plot(PACAPtoPTSD, which = 1) 
#I would say this one passes as well but there is a lot less data so its harder to tell. 

plot(PACAPtoPTSD, which = 2) 
#This is a little bit harder to tell because there are less data points
#but it looks reasonable 

#Y intercept is telling us the expected PSS score given a PACAP38 concentration of 0 is 19.8. 
#We see that we would expect a person with a 1 unit higher PACAP38 concentration 
#to have a 0.2 point higher PSS score 
#we see an R squared value of 0.065

#simple linear regression between PACAP 38 concentration and BDI total
PACAPtoDepress <- lm(PACAP38pM ~ BDItotalscore, data = PACAP_dataset)
PACAPtoDepress      
coefficients(PACAPtoDepress)              
summary(PACAPtoDepress)  
plot(PACAP_dataset$PACAP38pM, PACAP_dataset$BDItotalscore, pch = 20, main = "Regression Fit", xlab = "PACAP 38", ylab = "Depression Score")
abline(PACAPtoDepress, col = "red", lwd = 2) 
confint(PACAPtoDepress)
plot_model(PACAPtoDepress, type = "eff", terms = c("BDItotalscore"))

plot(PACAPtoDepress, which = 2) 
#Again, a little bit harder to tell because there is less data but it looks reasonable. It goes quite a bit higher
#then theoretical values for those 3 data points with largest standardized residuals so this may mean that its 
#right skewed

plot(PACAPtoDepress, which = 1) 
#I would say this passes the constant variance check as well. 

#Y intercept is telling us the expected BDI score given a PACAP38 concentration of 0 is 18.7.  
#We see that we would expect a person with a 1 unit higher PACAP38 concentration 
#to have a 0.26 point higher BDI score 
#we see an R squared value of 0.065

#simple linear regression between PACAP 38 concentration and CTQ total
PACAPtoTrauma <- lm(PACAP38pM ~ CTQTOT, data = PACAP_dataset)
PACAPtoTrauma      
coefficients(PACAPtoTrauma)              
summary(PACAPtoTrauma)  
plot(PACAP_dataset$PACAP38pM, PACAP_dataset$CTQTOT, pch = 20, main = "Regression Fit", xlab = "PACAP 38", ylab = "CTQ Score")
abline(PACAPtoTrauma, col = "red", lwd = 2) 
plot_model(PACAPtoTrauma, type = "eff", terms = c("CTQTOT"))

plot(PACAPtoTrauma, which = 1) 
#I would say this passes the constant variance check as well. 

plot(PACAPtoTrauma, which = 2)
#this is also a small number of data points but it looks right skewed 

#The R squared is super small.. essentially none of the variance in CTQ score can be explained by 
#PACAP levels 
        
#Now, I am going to do multiple regression analysis where we look at the difference between scores
#based on income. We have 5 income groups. These are based on monthly income and the actual
#values are in the labels. 

ggbetweenstats(data = PACAP_dataset, x = income, y = BDItotalscore, messages = FALSE, results.subtitle = FALSE, 
               pairwise.comparisons = FALSE) 

#We can see a nice plot of BDI total scores by income. The lowest income bracket has the highest 
#BDI total scores, and this trend is followed throughout (more income corresponds to lower BDI total score mean)
#except for the top income bracket, which has a slightly higher mean than the second highest income bracket. 

#Now, I will do the same thing but look at PSS total scores
ggbetweenstats(data = PACAP_dataset, x = income, y = PSStotal, messages = FALSE, results.subtitle = FALSE, 
               pairwise.comparisons = FALSE)   

#this plot is interesting but perhaps more difficult to interpret. I will look into the relationship between
#income and PSS total more later. 

#Now, I will look at the relationship between income and trauma scores
ggbetweenstats(data = PACAP_dataset, x = income, y = CTQTOT, messages = FALSE, results.subtitle = FALSE, 
               pairwise.comparisons = FALSE)   

#this one again shows a less stark pattern than the BDI total score pattern, but is still interesting to visualize

#Now, I will try fitting models in which I control for other relevant variables 

#Ill start by fitting a model for effect of PSS total on BDI total score, I will control 
#for potentially relevant other factors
PSS_Effects <- lm(PSStotal ~ BDItotalscore + age + sex + income + CTQTOT + suicide_attempt + PACAP38pM, data = PACAP_dataset)  
round(coef(PSS_Effects), 5)
summary(PSS_Effects)   

#we can see from our summary that, when we control for other relevant factors, we would expect for a person 
#with a 1 point higher PSS total score to have a 0.88 point higher BDI total score. 
#We will look further into this relationship when we do hypothesis testing! 

#We can also see information about how PSS total is related to age, sex, income, CTQ total, and suicide attempt history
#but the relationship between BDI and PSS total seems particularly correlated. 

#Overall we can explain 70% of variance in PSS scores from the mentioned variables 

#I want to also look at different variables measured on suicide attempt history 

Suicide_Effects <- lm(suicide_attempt ~ BDItotalscore + age + sex + income + CTQTOT + PSStotal + PACAP38pM, data = PACAP_dataset)  
round(coef(Suicide_Effects), 5)
summary(Suicide_Effects)

#It doesn't look like any of the variables have a super clear impact on whether someone has a suicide attempt 
#overall we can explain ~40% of variance from these variables 

BDI_Effects <- lm(BDItotalscore ~ PSStotal + age + sex + income + CTQTOT + suicide_attempt + PACAP38pM, data = PACAP_dataset)  
round(coef(BDI_Effects), 5)
summary(BDI_Effects)

#The R squared is interesting... based on all our variables included we can predict 
#74.89% of variance in BDI scores. 

#I want to see how much of the variance in BDI effects we can explain from variables in the dataset
#since we see we have about 75% explained right now. I will make a more complicated
#BDI effects regression 

BDI_Effects_Complex <- lm(BDItotalscore ~ PSStotal + age + sex + income + CTQTOT + suicide_attempt + employment + subs_abuse_past + subs_abuse_now + PACAP38pM + PTSD_diagnosis + SEQ_TAQ_COMB, data = PACAP_dataset)  
round(coef(BDI_Effects_Complex), 5)
summary(BDI_Effects_Complex)

#I want to see what variance in PACAP38 concentration can be explained by the variables listed 

PACAP_Effects <- lm(PACAP38pM ~ PSStotal + BDItotalscore + age + sex + income + CTQTOT + suicide_attempt + employment + subs_abuse_past + PTSD_diagnosis + SEQ_TAQ_COMB, data = PACAP_dataset)  
round(coef(PACAP_Effects), 5)
summary(PACAP_Effects)

#~24% of variance in PACAP 38 concentration can be explained by the variables given

#Now I want to see the variance in substance use that can be explained 
Subs_Abuse_Effects <- lm(subs_abuse_past ~ PSStotal + BDItotalscore + age + income + CTQTOT + suicide_attempt + employment + subs_abuse_past + PTSD_diagnosis + SEQ_TAQ_COMB, data = PACAP_dataset)  
round(coef(Subs_Abuse_Effects), 5)
summary(Subs_Abuse_Effects)

#~24% of variance in substance abuse history can be explained by the variables given

#Now I want to try to analyze using interaction effects. I will do this based on interactions 
#that I think could be interesting 

#One that I thought might be interesting is the interaction between sex and trauma scores on PTSD symptom scores. 
#This is fitted below. 
lm_PSS_Trauma <- lm(PSStotal ~ sex*CTQTOT, data = PACAP_dataset)
summary(lm_PSS_Trauma) 
Anova(lm_PSS_Trauma)      

#I will also see the interaction between BDI scores and sex on PSS total. This gets
#at the question of whether the sexes have similar correlations between depression scores
#and PTSD scores 

lm_PSS_BDI <- lm(PSStotal ~ sex*BDItotalscore, data = PACAP_dataset)
summary(lm_PSS_BDI) 
Anova(lm_PSS_BDI) 

#I will next see the interaction between CTQ scores and sex on PSS total. 
#This is getting at the question of whether the sexes are similarly likely to have
#higher PTSD symptom scores after trauma 

lm_PSS_Sex_CTQ <- lm(PSStotal ~ sex*CTQTOT, data = PACAP_dataset)
summary(lm_PSS_Sex_CTQ) 
Anova(lm_PSS_Sex_CTQ) 

#I will next see the interaction between BDI scores and income on PSS total. This gets at 
#the question of whether the relationship between depression scores and PSS scores vary
#by income level. 

lm_PSS_BDI_Income <- lm(PSStotal ~ income*BDItotalscore, data = PACAP_dataset)
summary(lm_PSS_BDI_Income) 
Anova(lm_PSS_BDI_Income) 

#I will next see the interaction between BDI scores and CTQ scores on PSS total. This gets at
#the question of whether the relationship between depression scores and PSS scores varies based
#on the level of childhood trauma exposure 

lm_PSS_BDI_CTQ <- lm(PSStotal ~ CTQTOT*BDItotalscore, data = PACAP_dataset)
summary(lm_PSS_BDI_CTQ) 
Anova(lm_PSS_BDI_CTQ) 

#I will next see the interaction between BDI scores and CTQ scores on PSS total. This gets at
#the question of whether the relationship between depression scores and PSS scores varies based
#on the level of childhood trauma exposure 

lm_PSS_PACAP_Sex <- lm(PSStotal ~ sex*PACAP38pM, data = PACAP_dataset)
summary(lm_PSS_PACAP_Sex) 
Anova(lm_PSS_PACAP_Sex) 
#We see a significant interaction effect between sex and PACAP38pM concentrations of PSS total scores. 

#I will next see the interaction between age and CTQ scores on PSS total. This gets at
#the question of whether the relationship between childhood trauma scores and PSS scores varies based
#on the age of the participant. 

lm_PSS_CTQ_Age <- lm(PSStotal ~ age*CTQTOT, data = PACAP_dataset)
summary(lm_PSS_CTQ_Age) 
Anova(lm_PSS_CTQ_Age) 

#I will do the same with CTQ scores and age on BDI total 
lm_BDI_CTQ_Age <- lm(BDItotalscore ~ age*CTQTOT, data = PACAP_dataset)
summary(lm_BDI_CTQ_Age) 
Anova(lm_BDI_CTQ_Age)

#I will see now the interaction between sex and current substance abuse on depression
#and PTSD scores 
lm_SubsUse_BDI_Sex <- lm(BDItotalscore ~ sex*subs_abuse_now, data = PACAP_dataset)
summary(lm_SubsUse_BDI_Sex) 
Anova(lm_SubsUse_BDI_Sex)

lm_SubsUse_PSStotal_Sex <- lm(PSStotal ~ sex*subs_abuse_now, data = PACAP_dataset)
summary(lm_SubsUse_PSStotal_Sex) 
Anova(lm_SubsUse_PSStotal_Sex)

#There could be an interesting interaction between sex and current substance abuse on PSS total. Lets 
#flip this and also look at how sex and PSS total interact on substance abuse now. 

lm_PSStotal_SubsAbuse_Sex <- lm(subs_abuse_now ~ sex*PSStotal, data = PACAP_dataset)
summary(lm_PSStotal_SubsAbuse_Sex) 
Anova(lm_PSStotal_SubsAbuse_Sex)

#We dont see a particularly strong interaction here. 

#While most of my analysis involved the whole dataset, I wanted to also
#do a little bit of analysis on a subset that has a PTSD diagnosis

PTSD_Subset <- subset(PACAP_dataset, PACAP_dataset$PTSD_diagnosis >= 1)
dim(PTSD_Subset)
#PTSD_Subset contains the 399 people included that met criteria for PTSD diagnosis 

No_PTSD_Subset <- subset(PACAP_dataset, PACAP_dataset$PTSD_diagnosis == 0)
dim(No_PTSD_Subset)

#EDA for PTSD Subset

#CHILDHOOD TRAUMA QUESTIONNAIRE TOTAL SCORES

#location and dispersion measures for CTQ total score across PTSD diagnosed participants
mean(PTSD_Subset$CTQTOT, na.rm = TRUE)
median(PTSD_Subset$CTQTOT, na.rm = TRUE)
var(PTSD_Subset$CTQTOT, na.rm = TRUE)
sd(PTSD_Subset$CTQTOT, na.rm = TRUE)
range(PTSD_Subset$CTQTOT, na.rm = TRUE)

#histogram showing CTQ (childhood trauma questionnaire) total scores across PTSD diagnosed participants

hist(PTSD_Subset$CTQTOT, main = 'Childhood Trauma Questionnaire (CTQ) Scores') 

#right skew with mean of 49 and median of 44

#CHILDHOOD TRAUMA QUESTIONNAIRE SEXUAL ABUSE SCORES

mean(PTSD_Subset$CTQ_SEX_AB, na.rm = TRUE)
median(PTSD_Subset$CTQ_SEX_AB, na.rm = TRUE)
var(PTSD_Subset$CTQ_SEX_AB, na.rm = TRUE)
sd(PTSD_Subset$CTQ_SEX_AB, na.rm = TRUE)
range(PTSD_Subset$CTQ_SEX_AB, na.rm = TRUE)
hist(PTSD_Subset$CTQ_SEX_AB, main = 'CTQ Sexual Abuse Scores') 

#we can see that CTQ sex abuse scores are strongly skewed right for PTSD participants with mean of 8.9 and median of 5

#CHILDHOOD TRAUMA QUESTIONNAIRE EMOTIONAL ABUSE SCORES

mean(PTSD_Subset$CTQ_EMOT_AB, na.rm = TRUE)
median(PTSD_Subset$CTQ_EMOT_AB, na.rm = TRUE)
var(PTSD_Subset$CTQ_EMOT_AB, na.rm = TRUE)
sd(PTSD_Subset$CTQ_EMOT_AB, na.rm = TRUE)
range(PTSD_Subset$CTQ_EMOT_AB, na.rm = TRUE)
hist(PTSD_Subset$CTQ_EMOT_AB, main = 'CTQ Emotional Abuse Scores') 

#we can see that CTQ emotional abuse scores are strongly skewed right iwth mean of 11.3 and median of 10

#CHILDHOOD TRAUMA QUESTIONNAIRE PHYSICAL ABUSE SCORES

mean(PTSD_Subset$CTQ_PHYS_AB, na.rm = TRUE)
median(PTSD_Subset$CTQ_PHYS_AB, na.rm = TRUE)
var(PTSD_Subset$CTQ_PHYS_AB, na.rm = TRUE)
sd(PTSD_Subset$CTQ_PHYS_AB, na.rm = TRUE)
range(PTSD_Subset$CTQ_PHYS_AB, na.rm = TRUE)
hist(PTSD_Subset$CTQ_PHYS_AB, main = 'CTQ Physical Abuse Scores') 

#we can see that CTQ physical abuse scores are strongly skewed right with mean of 9.8 and median of 8

#CHILDHOOD TRAUMA QUESTIONNAIRE EMOTIONAL NEGLECT SCORES

mean(PTSD_Subset$CTQ_EMOT_NEG, na.rm = TRUE)
median(PTSD_Subset$CTQ_EMOT_NEG, na.rm = TRUE)
var(PTSD_Subset$CTQ_EMOT_NEG, na.rm = TRUE)
sd(PTSD_Subset$CTQ_EMOT_NEG, na.rm = TRUE)
range(PTSD_Subset$CTQ_EMOT_NEG, na.rm = TRUE)
hist(PTSD_Subset$CTQ_EMOT_NEG, main = 'CTQ Emotional Neglect Scores') 

#we can see that CTQ emotional neglect scores are strongly skewed right with mean of 10.8 and median of 9

#CHILDHOOD TRAUMA QUESTIONNAIRE PHYSICAL NEGLECT SCORES

mean(PTSD_Subset$CTQ_PHYS_NEG, na.rm = TRUE)
median(PTSD_Subset$CTQ_PHYS_NEG, na.rm = TRUE)
var(PTSD_Subset$CTQ_PHYS_NEG, na.rm = TRUE)
sd(PTSD_Subset$CTQ_PHYS_NEG, na.rm = TRUE)
range(PTSD_Subset$CTQ_PHYS_NEG, na.rm = TRUE)
hist(PTSD_Subset$CTQ_PHYS_NEG, main = 'CTQ Physical Neglect Scores') 

#we can see that CTQ physical neglect scores are strongly skewed right with mean of 8.1 and median of 7

#I want to look at the distribution of PSS scores across those with PTSD

#PTSD SYMPTOM SCALE TOTAL SCORES

mean(PTSD_Subset$PSStotal, na.rm = TRUE)
median(PTSD_Subset$PSStotal, na.rm = TRUE)
var(PTSD_Subset$PSStotal, na.rm = TRUE)
sd(PTSD_Subset$PSStotal, na.rm = TRUE)
range(PTSD_Subset$PSStotal, na.rm = TRUE)

#histogram showing PTSD symptom scale (PSS) total scores among participants with PTSD
hist(PTSD_Subset$PSStotal, main = 'PTSD Symptom Scale (PSS) Scores') 

#we see that total PSS scores are normally distirbuted (approx) with mean of 26.8 and median of 27

#histogram showing PTSD symptom scale (PSS) score by category for PTSD patients 

#PTSD SYMPTOM SCALE INTRUSION SUBSCORES

mean(PTSD_Subset$PSS_Intrusive, na.rm = TRUE)
median(PTSD_Subset$PSS_Intrusive, na.rm = TRUE)
var(PTSD_Subset$PSS_Intrusive, na.rm = TRUE)
sd(PTSD_Subset$PSS_Intrusive, na.rm = TRUE)
range(PTSD_Subset$PSS_Intrusive, na.rm = TRUE)
hist(PTSD_Subset$PSS_Intrusive, main = 'PTSD Symptom Scale (PSS) Intrusion Scores') 

#we see that total PSS intrusion subscores are right skewed with mean of 6.8 and median of 6

#PTSD SYMPTOM SCALE AVOIDANCE/NUMBING SUBSCORES

mean(PTSD_Subset$PSS_avoidnumb, na.rm = TRUE)
median(PTSD_Subset$PSS_avoidnumb, na.rm = TRUE)
var(PTSD_Subset$PSS_avoidnumb, na.rm = TRUE)
sd(PTSD_Subset$PSS_avoidnumb, na.rm = TRUE)
range(PTSD_Subset$PSS_avoidnumb, na.rm = TRUE)
hist(PTSD_Subset$PSS_avoidnumb, main = 'PTSD Symptom Scale (PSS) Avoidance/Numbing Scores') 

#we see that total PSS avoidance/numbing subscores are normally distribued (approx) with mean of 11 and median of 11

#PTSD SYMPTOM SCALE HYPERAROUSAL SUBSCORES

mean(PTSD_Subset$PSS_hyperarousal, na.rm = TRUE)
median(PTSD_Subset$PSS_hyperarousal, na.rm = TRUE)
var(PTSD_Subset$PSS_hyperarousal, na.rm = TRUE)
sd(PTSD_Subset$PSS_hyperarousal, na.rm = TRUE)
range(PTSD_Subset$PSS_hyperarousal, na.rm = TRUE)
hist(PTSD_Subset$PSS_hyperarousal, main = 'PTSD Symptom Scale (PSS) Hyperarousal Scores') 

#we see that PSS hyperarousal scores are normall dstributed (approx) with mean of 9 and median of 9

#ANALYSIS OF PTSD SUBSET 

#I will do a brief analysis of the PTSD subset as well now that I have done EDA. 

#I want to see the relationship between PACAP38 and PSS total levels in PTSD patients 

#I will set up a simple linear regression between PACAP 38 concentration and PSS total
PACAPtoPTSD_Subset <- lm(PACAP38pM ~ PSStotal, data = PTSD_Subset)
PACAPtoPTSD_Subset      
coefficients(PACAPtoPTSD_Subset)              
summary(PACAPtoPTSD_Subset)  
plot(PACAP_dataset$PACAP38pM, PACAP_dataset$PSStotal, pch = 20, main = "Regression Fit", xlab = "PACAP 38", ylab = "PSS Score")
abline(PACAPtoPTSD_Subset, col = "red", lwd = 2)    

#It doesn't appear that much of the variance in PSS scores among PTSD patients can be explained by PACAP 

#Now I want to look at PSS total scores by income in PTSD patients 

ggbetweenstats(data = PTSD_Subset, x = income, y = PSStotal, messages = FALSE, results.subtitle = FALSE, 
               pairwise.comparisons = FALSE)   

#This looks pretty interesting. 
#I am going to make a more formal model. 
lm(PSStotal ~ income, data = PTSD_Subset)

#The Y intercept tells us that we would expect a PSS total score of 29.3 for a person in the lowest 
#income bracket (this is the mean for that group). The slope shows that, for a person in a bracket one 
#lower than another person, we would expect the person in the lower bracket to have a 1.6 point higher PSS score. 


##################### MIDTERM PROJECT PART 2 (Izabelė Jonušaitė) ###################

# This data is from a computational model simulation about the interaction of implicit and explicit attitudes. 
# The model investigates metacognitive mechanisms in this interaction. It describes three profiles at different 
# levels of metacognition. Through interventions, we look into the effects these interventions have on the 
# interaction between implicit and explicit attitudes, depending on the metacognitive levels of a profile. 
# The model has a hierarchical architecture: the "Lower Level" represents implicit attitudes, and the "Higher Level" 
# is where the explicit attitude is formed.
#
# Originally, the model simulations played a qualitative role, and were used to demonstrate what sort of dynamics 
# the model can generate and account for. However, here I take the initial steps towards finding more qualitative 
# ways to capture simulation results.
#
# In this particular simulation, we examine how the higher level is affected by an intervention on the lower level 
# (implicit). The intervention occurs at t=150, where the values of the lower level nodes are set.
#
# For this midterm project, I had to decide on how to quantitatively capture the difference between higher and lower 
# level and how that divergence corresponds with different predictors like profiles and intervention status (post or pre).
# I decided to create a difference/divergence metric where I subtract the higher level (scaled up by multiplying it by 
# 10, as before it was from 0 to 1) from the lower level at every time step. Then, because this simulation data is 
# temporal, I split simulations into two parts: post and pre-intervention, then cut away the timesteps where the 
# simulation dynamics are still initiating (out of total 300 time steps, that's the first 74 time steps) and also 
# cut out the time steps where the intervention effect dissipates (last 74 timesteps). Then I was left with 75 time 
# steps pre-intervention and 75 time steps post-intervention. I 'collapsed' that temporal data by finding a mean for 
# pre and a mean of the difference metric for the post-intervention time window. Those two means are what I captured 
# at every simulation (in my function OneProfileOneRunSim1) and then ran them multiple times and aggregated that 
# data to be able to check the robustness of the dynamics (in my function RunMultipleSimulations).

# There are two main research questions under focus here: whether there is a difference in the divergence metric 
# pre and post-intervention; and whether different profiles are affected differently by the intervention.

## INITIALIZE PACKAGES
library("readxl")

######################## GENERATING THE SIMULATION DATA ########################

## Function to run a simulation for a given profile
OneProfileOneRunSim1 <- function(profileName){
  
  
  T = 300
  n_nodes = 10
  
  holdconstant = TRUE
  intervention1a = TRUE
  intervention1b = FALSE
  
  if(holdconstant){
    R_T_hc <- read_excel("R_Tdf.xlsx")
    R_T <- t(as.vector(R_T_hc))
    x = c(-1, -1, -1, -1, -1, 1, 1, 1, 1, 1)
    weights_hc <- read_excel("weightsdf.xlsx")
    weights <- as.matrix(weights_hc)
  }
  
  if(holdconstant==FALSE){
    x = sample(c(-1,1),n_nodes,replace=T)
    weights <- matrix(data = rnorm(100, mean = 0, sd = 0.01), n_nodes, n_nodes)
  }
  
  tau = array(.5,dim=c(n_nodes)) #lower level dispositions
  
  A = 2
  I = 1
  Cp = 0.001
  tw = 10
  epsilon = 0.001
  lambda = 0.001
  
  # Precision weighting profiles
  
  switch(profileName,
         "Profile1" = {
           gamma = 0.5
           gammaTAU = 0.01
           gammaE = 1
           gammaHtoL = 0.01
         },
         "Profile2" = {
           gamma = 3.5
           gammaTAU = 2
           gammaE = 1
           gammaHtoL = 1
         },
         "Profile3" = {
           gamma = 10
           gammaTAU = 5
           gammaE = 0.3
           gammaHtoL = 0.01 
         }
  )
  
  
  # AI initial values
  X = array(0,dim=c(2,T+1))
  X[,1] = c(.5,.5) # Higher level (HL) states: veg or non-veg attitude
  TAU = array(0,dim=c(n_nodes,2,T)) 
  TAU[,1,] = rep(.2,n_nodes)
  TAU[,2,] = rep(-.1,n_nodes)
  
  
  x_T = matrix(0,n_nodes,T+1)
  x_T[,1] = x
  weights_T = array(0,dim=c(n_nodes,n_nodes,T+1))
  weights_T[,,1] = weights
  weights_mean_T = array(0,dim=c(T+1)) #For plotting weights average per iteration
  weights_mean_T[1] = mean(weights[row(weights)!=col(weights)]) #Average of all weights, excl. diagonal
  tau_mean_T = array(0,dim=c(T+1))
  tau_mean_T[1] = mean(tau)
  tau2_mean_T = array(0,dim=c(T+1))
  tau2_mean_T[1] = mean(gammaTAU * (TAU[,1,1] * X[1,1] + TAU[,2,1] * X[2,1])) 
  A_T = array(0,dim=c(T+1))
  A_T[1] = A
  A_T2 = array(0,dim=c(T+1)) + gammaHtoL
  
  U_T = array(0,dim=c(T+1))
  U_T[1] = 0
  F_T = array(0,dim=c(T+1))
  sum_x_T = array(0,dim=c(T+1))
  sum_x_T[1] = sum(x)
  
  mean_TAU1 <- mean(TAU[,1,])
  mean_TAU2 <- mean(TAU[,2,])
  
  #### Active inference extension ####
  
  # HL transition
  B = array(0, dim=c(2,2))
  B[,] = c(0.7, 0.3, 0.3, 0.7)
  Bbar = array(0,dim=c(2,2))
  Bnorm = 0
  
  # Higher-level transition probability
  
  for(i in 1:2){
    for (j in 1:2){
      Bnorm = sum(B[,]**gamma)
      Bbar[i,j]=B[i,j]**gamma/sum(B[,j]**gamma)
    }
  }
  
  B[,] = Bbar
  
  #Measuring entropy
  Entropy_T = array(0, dim=c(2,T))
  FE_T = array(0, dim=c(2,T))
  
  #### Settings ####
  
  learning = TRUE
  AI = TRUE
  
  
  ######################## THE SIMULATION, Glauber dynamics & Metacognition
  
  for (t in 1:T) {
    
    # Intervention 1a
    if(intervention1a){
      if((t>=T/2) & t<(T/2+(T/5))){
        x = c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1)
      }}
    
    # Intervention 1b
    if(intervention1b){
      if((t>T/2) & t<(T/2+(T/5))){
        X[,t] = c(.1,.9) #First state is vegetarian attitude, second is non-veg attitude (as an example attitude)
      }}
    
    # Top-down updating
    if(AI){
      tau2 = gammaTAU * (TAU[,1,t] * X[1,t] + TAU[,2,t] * X[2,t]) 
    }
    
    E = c(0,0) #Vector for storing current state and opposite state energy
    E2 = c(0,0)
    
    i = sample(1:n_nodes,1) #Sample a random node i
    E2[1] = -tau2[i] * x[i] #Add disposition to stay
    E2[2] = -tau2[i] * -x[i] #Add disposition to flip
    
    E[1] = -tau[i] * x[i] #Add disposition to stay
    E[2] = -tau[i] * -x[i] #Add disposition to flip
    
    P0_flip = 1 / (1 + exp(-A_T[t] * (E[1] - E[2]) - A_T2[t] * (E2[1] - E2[2])))
    
    for (j in 1:10) {
      if (i != j) {
        E[1] = E[1] - weights[i,j] * x[i] * x[j]
        E[2] = E[2] - weights[i,j] * -x[i] * x[j]
      }
    }
    
    P_flip = 1 / (1 + exp(-A_T[t] * (E[1] - E[2]) - A_T2[t] * (E2[1] - E2[2])))
    
    Entropy_T[1,t] =   -1 * (P_flip * log(P_flip) + (1-P_flip) * log(1-P_flip))
    
    # The free energy of the lower level 
    FE_T[2,t] = (1 - P_flip) * E[1] + P_flip * E[2] 
    FE_T[2,t] =  FE_T[2,t] + P_flip * log(P_flip/P0_flip) + (1-P_flip) * log((1-P_flip)/(1-P0_flip))
    
    if(holdconstant==FALSE){
      R = runif(1)}
    
    if(holdconstant){
      R = R_T[t]}
    
    if (R <= P_flip){
      x[i] = -x[i]
      F = 1 } 
    if(R > P_flip){
      F = 0}
    
    F_T[t] = F
    
    x_T[,t+1] = x
    
    
    if(learning)
      for (i in 1:n_nodes){
        for(j in 1:n_nodes){
          if (i != j){
            weights[i,j] = weights[i,j] + (epsilon * (1 - abs(weights[i,j] * x[i] * x[j] - lambda * weights[i,j])))
          }}}
    
    for (i in 1:n_nodes){
      tau[i] = tau[i] + (epsilon * (1 - abs(tau[i]) * x[i] - lambda * tau[i]))
    }
    
    weights_T[1:n_nodes,1:n_nodes,t+1] = weights
    
    
    if(t>=tw){
      U = mean(F_T[t-(tw-1):t])
      U_T[t] = U
      A_T[t+1] = (1-Cp) * A_T[t] + Cp * (I + I * U_T[t] - A_T[t])
    }
    if(t<tw){
      A_T[t+1] = A
    }
    
    weights_mean_T[t+1] = mean(weights[row(weights)!=col(weights)])
    
    sum_x_T[t+1] = sum(x)
    
    tau_mean_T[t+1] = mean(tau)
    tau2_mean_T[t+1] = mean(tau2)
    
    # Bottom-up message
    
    if(AI){
      P_X1 = log(X[1,t]+10**-7) + gammaE * sum(TAU[,1,t] * x)
      P_X2 = log(X[2,t]+10**-7) + gammaE * sum(TAU[,2,t] * x)
      
      # Inserting the approximate posterior of time t in the state beliefs X
      X[1,t+1] = exp(P_X1)/sum(exp(P_X1),exp(P_X2))
      X[2,t+1] =  1 - X[1,t+1]
    }
    
    tau2 = gammaTAU * (TAU[,1,t] * X[1,t+1] + TAU[,2,t] * X[2,t+1]) 
    
    P_x = exp(tau2[i])/sum(exp(tau2[i]),exp(-1*tau2[i]))
    
    if(x[i]==1){
      FE_T[1,t] = -log(P_x)
    }
    if(x[i]==-1){
      FE_T[1,t] = -log(1-P_x)
    }
    
    # FE_T[1,t] = -2 * (tau2[i] * x[i])
    
    # FE of the higher level (the quantity that the higher level is minimizing
    # to optimize its beliefs)
    
    FE_T[1,t] = FE_T[1,t] + X[1,t+1] * log(X[1,t+1]/X[1,t]) + X[2,t+1] * log(X[2,t+1]/X[2,t])
    
    Entropy_T[2,t] = -1 * (X[1,t+1] * log(X[1,t+1]) + (X[2,t+1]) * log(X[2,t+1]))
    
    X[,t+1] = B[,]%*%X[,t+1]
    
  }
  
  HL <- (as.numeric(X[1,])) * 10
  
  simulationData <- data.frame(sum_x_T, HL)
  
  ## add a difference metric
  
  simulationData$LHDifference <- abs(simulationData[,1] - simulationData[,2])
  
  
  ## add pre-post intervention status column
  
  simulationData$InterventionStatus <- ifelse(1:nrow(simulationData) <= 150, 'Pre', 'Post')
  
  simulationData <- simulationData[76:225,]
  
  # Split data by intervention status
  preData <- subset(simulationData, InterventionStatus == "Pre")
  postData <- subset(simulationData, InterventionStatus == "Post")
  
  # Calculate mean LHDifference for both pre and post intervention
  preMean <- mean(preData$LHDifference)
  postMean <- mean(postData$LHDifference)
  
  # Combine results into a new dataframe
  resultDF <- data.frame(
    Profile = c(profileName, profileName),
    InterventionStatus = c("Pre", "Post"),
    MeanLHDifference = c(preMean, postMean)
  )
  
  return(resultDF)
  
}

## Function to run multiple simulations with all profiles to generate an aggregated dataset
RunMultipleSimulations <- function(num_iterations = 500) {
  # Profiles to iterate over
  profiles <- c("Profile1", "Profile2", "Profile3")
  
  # Initialize an empty dataframe to store the results
  aggregatedResults <- data.frame(
    Profile = character(),
    InterventionStatus = character(),
    MeanLHDifference = numeric()
  )
  
  # Iterate over each profile
  for (profile in profiles) {
    # Run the simulation for each profile for a selected or default (500) number of iterations
    for (i in 1:num_iterations) {
      resultDF <- OneProfileOneRunSim1(profile)
      aggregatedResults <- rbind(aggregatedResults, resultDF)
    }
  }
  
  return(aggregatedResults)
}

# Create aggregated simulation dataset
allResults <- RunMultipleSimulations()
head(allResults)

# Some rearrangement: reorder so that "Pre" intervention status precedes "Post"
allResults$InterventionStatus <- factor(allResults$InterventionStatus, levels = c("Pre", "Post"))



######################## EXPLORATORY ANALYSIS ########################


## Descriptive statistics: mean, variance, min/max values, quartiles

library(dplyr)

allResults %>%
  group_by(Profile, InterventionStatus) %>%
  summarize(
    mean_value = mean(MeanLHDifference),
    median_value = median(MeanLHDifference),
    variance = var(MeanLHDifference),
    min_value = min(MeanLHDifference),
    max_value = max(MeanLHDifference),
    first_quartile = quantile(MeanLHDifference, 0.25),
    third_quartile = quantile(MeanLHDifference, 0.75)
  )


library(tidyverse)

## Conditional histogram
ggplot(allResults, aes(x = MeanLHDifference)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
  facet_grid(rows = vars(Profile), cols = vars(InterventionStatus)) +
  labs(title = "Histogram of LHDifference",
       x = "LHDifference",
       y = "Count") +
  theme_minimal()

## Pirate plot (most relevant plot here; scatterplots are not applicable since I'm working with categorical variables and only one continuous)
library(yarrr)
pirateplot(MeanLHDifference ~ Profile * InterventionStatus, data = allResults)

## This pirate plot suggests that there's likely to be an interaction effect: comparing
## Pre and post, we can clearly see a difference, but it also looks like different profiles
## are affected differently by the intervention status 
## (in other words, the difference is not uniform across profiles). This informs my subsequent
## decision to fit an interaction model in addition to a regression model with two predictors without interaction

###############################  MODEL FITTING ###################################

# I want to fit two models here: a model with two predictors and an interaction model

mainModel <- lm(MeanLHDifference ~ Profile + InterventionStatus, data=allResults)
summary(mainModel)

## Model parameter interpretation:

## Intercept (3.56): Mean MeanLHDifference in the reference category (Profile 1)
## Slope parameters
## Profile2 (-3.22): Difference in mean MeanLHDifference between Profile 2 and reference profile (Profile 1). 
## This suggests that Profile 2 overall has less divergence (-3.22 units less) between Lower level and Higher level
## (as expressed by the MeanLHDifference divergence metric) than Profile 1
## Profile3 (-2.93): Difference in mean MeanLHDifference between Profile 3 and Profile 1.
## InterventionStatus Post (6.4): Difference in mean MeanLHDifference in post intervention, comparing to pre-intervention.
## This suggests that post intervention, divergence increases a lot. 


InteractionModel <- lm(MeanLHDifference ~ Profile + InterventionStatus + Profile:InterventionStatus, data=allResults)
summary(mainModel)

## Model parameter interpretation: Parameter values are largely the same as in mainModel, interpretataions of parameters
## themselves are identical to mainModel.
## (Maybe I'm missing somethign, but I thought there would be somethign different because I included an interaction?)

# To have anohter model for contrast, I thought I could also do a lm with only interventionstatus as a predictor
# and see how well that does in contrast to the others

OnlyInterventionStatusModel <- lm(MeanLHDifference ~ InterventionStatus, data=allResults)
summary(OnlyInterventionStatusModel)

# Model parameter interpretation:

# Intercept (1.51): Mean MeanLHDifference in the reference category (Pre-Intervention)
## Slope parameter InterventionStatusPost (6.4): difference in post intervention comparing to pre intervention.

## Comparing the three models:

AIC(mainModel, InteractionModel, OnlyInterventionStatusModel)
BIC(mainModel, InteractionModel, OnlyInterventionStatusModel)
## Interaction model has the lowest AIC/BIC


################ ANALYSING/INTERPRETING MODEL ESTIMATES ##############################

summary(InteractionModel)$coefficients

## (model parameter interpretations provided above, here focusing on the estimates)
## The p-values are very, very small for all the coefficients, suggesting that the probability of these coefficients under null hypothesis 
## (H0: no difference pre vs. post interaction and no differences in pairwise comparisons between profiles)
## is close to zero. 
## It is worth noting though that p-values are not very important here since the data is synthetic and generated by our model.
## What is more important and interesting in a context like this is the direction and magnitude of effects.
## (However, in my overall summary/interpretation I describe how this is actually a bit of a wrong set up
## to study effects that are actually of interest here. In the future, I want to figure out how to
## do a robustness analysis once I pin down more precisely what is the effect that we want to get at in this simulation)


## Effects plot

library(sjPlot)
plot_model(InteractionModel, type="int")
## This plot displays clearly significant interaction effects, showing that there's not only 
## a significant impact of intervention status (post vs pre), but different profiles are 
## affected differently by the intervention status. 
## Overall, the intervention increases MeanLHDifference in all profiles. Comparing the magnitudes
## of this effect on profiles, it looks like Profile 3 has the most change in MeanLHDifference post vs pre
## intervention.


########################## MODEL ASSUMPTION CHECKS #############################

# (Note, I'm now checkign the assumptions of only the Interaction Model since it had the best fit.
# but I don't know if I should be checking every model here)

## Visualizing error variance; homoscedasticity check
# Residuals vs fitted plot
plot(InteractionModel, which=1)
# Unfortunately, can't make much sense of this due to having categorical variables.
# The point of this check is to see whether the data is in a funnel shape. 
# I'm guessing this is not exactly funnel shape but I'm not sure.

## Q-Q plot for normality check, verifying if residuals have a normal distribution
plot(InteractionModel, which=2)

## Trying out a predictive check. Using bar charts due to categorical variables. I gather than this 
## shows fitted values against the actual values. Bars represent actual values and red points represent predicted
## values from the model.

allResults$Predicted <- predict(model, allResults)
ggplot(allResults, aes(x=interaction(Profile, InterventionStatus), y=MeanLHDifference)) +
  geom_bar(stat='identity', position='dodge') +
  geom_point(aes(y=Predicted), position=position_dodge(width=0.9), color="red")
## Interpretaton: the fitted and the actual values seem to be reasonably close, except for Profile 3 pre intervention.
## in that case, the predicted value is by one unit lower than the actual. But it's not by that much. I think in broad strokes,
## this looks good.

##################### SUMMARY/OVERALL INTERPRETATION FOR PART 2 ####################

# The original goal of this simulation was to display the role of metacognitive mechanisms 
# in implicit-explicit attitude interaction. Specifically, we wanted to show that if you have 
# less sensitive metacognition (Profile 1), intervention on the lower level is going to have 
# less of an effect on the higher level, comparing to functional/accurate metacognition 
# (Profile 2), and Profile 3 (overconfident or laggy sensititvity) will be somehwere in between. 
# It’s been a bit of a challenge to decide how exactly that should be expressed in more 
# concrete metrics like the divergence metric. Maybe this should translate into a comparison 
# of only the post-intervention divergence metric and compare that between profiles, and then 
# we’d expect to see the lowest score in Profile 2 and highest divergence in Profile 1 and 
# Profile 3 would fall in between. The reason for this is that only the intervention creates 
# an immediate change, and we’re interested in how good different profiles are in reflecting 
# that change on their higher (explicit) level. Instead of this, I compared pre and post 
# because I initially thought that would be meaningful here, but upon performing this analysis 
# and writing this general interpretation, I think exploring this other approach 
# (only post-intervention analysis) might make more sense.
#
# Moreover, another thing that is missing here is that my real goal with this statistical 
# analysis of model simulations is to actually check their robustness of our simulations. 
# So I think in my future work on this, I should focus a lot more on e.g. plotting distributions 
# of slope parameter estimates maybe, or whatever turns out to be the key metric 
# (depends on what I figure out for the previous point).

