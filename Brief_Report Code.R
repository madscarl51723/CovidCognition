library(tidyverse);library(dplyr)
library(ggplot2)
library(ggforce)
library(ggdist)
library(gghalves)


table(df4$gender)
#standardized scores
df$duplicates_chiZ <- scale(df$duplicates_chi, center = TRUE, scale = TRUE)
df4$Word_DefZ <- scale(df4$Word_Definitions.y, center = TRUE, scale = TRUE)
df4$AnalogiesZ <- scale(df4$Analogies.y, center = TRUE, scale = TRUE)
df4$Comp_ThinkZ <- scale(df4$Comp_Think.y, center = TRUE, scale = TRUE)
df$MoralizationZ <- scale(df$Moralization, center = TRUE, scale = TRUE)
df$RavensZ <- scale(df$Ravens, center = TRUE, scale = TRUE)
df$Paper_FoldZ <- scale(df$Paper_Folding, center = TRUE, scale = TRUE)
df$corsibZ <- scale(df$dcorsib, center = TRUE, scale = TRUE)
df$corsifZ <- scale(df$corsif, center = TRUE, scale = TRUE)
df$digitbZ <- scale(df$digitb, center = TRUE, scale = TRUE)
df$digitfZ <- scale(df$digitf, center = TRUE, scale = TRUE)
df$DSSTZ <- scale(df$DSST, center = TRUE, scale = TRUE)
df$mentalZ <- scale(df$mental, center = TRUE, scale = TRUE)
df$pairedZ <- scale(df$paired, center = TRUE, scale = TRUE)
df$TMAZ <- scale(df$TMA, center = TRUE, scale = TRUE)
df$TMBZ <- scale(df$TMB, center = TRUE, scale = TRUE)
df$verbal_countZ <- scale(df$verbal_count, center = TRUE, scale = TRUE)
df$duplicates_chiZ <- scale(df$duplicates_chi, center = TRUE, scale = TRUE)
df4$TypingZ <- scale(df4$Typing, center = TRUE, scale = TRUE)
df4$TMBTMAZ <- scale(df4$TMBTMA, center = TRUE, scale = TRUE)

#Experience with coronavirus code
df4 = df4 %>% mutate(Severity = case_when(
  df4$`Positive COVID-19 test` == 'No' ~ 'No COVID',
  df4$`Positive COVID-19 test` == 'Yes' & df4$`Experience with coronavirus` == 'I was asymptomatic (I had no symptoms of COVID).' ~ 'Asymptomatic',
  df4$`Positive COVID-19 test` == 'Yes' & df4$`Experience with coronavirus` == 'I was symptomatic and did not require medical care beyond standard treatment (i.e., I stayed at home and did not require emergency medical attention).' ~ 'Symptomatic, no medical attention',
  df4$`Positive COVID-19 test` == 'Yes' & df4$`Experience with coronavirus` == 'I was symptomatic and required emergency medical attention (e.g., I was worried for my safety so I called 911 or went to the hospital)' ~ 'Symptomatic, medical attention',
  df4$`Positive COVID-19 test` == 'Yes' & df4$`Experience with coronavirus` == 'I was symptomatic and was admitted to the hospital, but I did not require a ventilator (i.e., a machine that would help me breathe mechanically because my body could not breathe on its own)' ~ 'Symptomatic, admitted to the hospital',
  df4$`Positive COVID-19 test` == 'Yes' & df4$`Experience with coronavirus` == 'I was symptomatic and had to be put on a ventilator' ~ 'Symptomatic, admitted to the hospital'))

df4$Severity <- factor(df4$Severity,     # Reorder factor levels
                       c("No COVID", "Asymptomatic", "Symptomatic, no medical attention", 
                         "Symptomatic, medical attention", 
                         "Symptomatic, admitted to the hospital"))

#education code
df4 = df4 %>% mutate(education = case_when(
  df4$Eduacation == 'Some High school' ~ '0',
  df4$Eduacation == 'High School Diploma' ~ '1',
  df4$Eduacation == 'High school diploma' ~ '1',
  df4$Eduacation == 'Some college' ~ '2',
  df4$Eduacation == 'College degree' ~ '3',
  df4$Eduacation == 'Graduate School' ~ '4',
  df4$Eduacation == 'Graduate school' ~ '4'))

#centering education
df4$centerededucation <- scale(df4$edu_lvl, scale = TRUE)

#converting latitude longitude into zip
library(revgeo)
df4$zipcode <- revgeo(longitude = df4$Longitude, latitude = df4$Latitude, 
                provider = 'google', API = 'AIzaSyBR-zR55oM8d_I2s23EyAOfTBl9hJMigm8', 
                output = 'frame', item = 'zip')
zip <- zip_code_db
#log transformation on income
df.RAW$log_income <- log(df.RAW$median_household_income)
#centering income
df4$centeredses <- scale(df4$log_income, scale = TRUE)

#code for race/ethnicity, largest groups
df4 = df4 %>% mutate(race4 = case_when(
  df4$`race` == 'White' ~ 'White',
  df4$`race` == 'white' ~ 'White',
  df4$`race` == 'Black or African American' ~ 'African American',
  df4$`race` == 'Asian' ~ 'Asian',
  df4$race == 'hispanic' ~ 'Hispanic',
  df4$race == 'Hispanic' ~ 'Hispanic',
  df4$race == 'Hispanic/Latino' ~ 'Hispanic',
  df4$race == 'Hispanic/Latinx' ~ 'Hispanic',
  df4$race == 'latino' ~ 'Hispanic',
  df4$race == 'Latino' ~ 'Hispanic',
  df4$race == 'Latinx' ~ 'Hispanic',
  TRUE ~ 'Other'))

#relevel race
df4$race4 <- relevel(factor(df4$race4), ref = "White")
#create one variable
df4$race4 <- as.numeric(df4$race4)

#gender
df4 = df4 %>% mutate(gender = case_when(
  df4$Gender == 'Man' ~ 'Man',
  df4$Gender == 'Woman' ~ 'Woman',
  df4$Gender == 'woman' ~ 'Woman',
  df4$Gender == 'Nonbinary' ~ "Other",
  df4$Gender == 'Not listed' ~ "Other",
  df4$Gender == 'Transgender' ~ "Other"))

df4 = df4 %>% mutate(gendernum = case_when(
  df4$Gender == 'Man' ~ '1',
  df4$Gender == 'Woman' ~ '2',
  df4$Gender == 'woman' ~ '2',
  df4$Gender == 'Nonbinary' ~ "3",
  df4$Gender == 'Not listed' ~ "3",
  df4$Gender == 'Transgender' ~ "3"))

df4$gendernum <- as.numeric(df4$gendernum)

#centerage
df4$centeredAge <- scale(df4$Age, scale = TRUE)

#average time between two sessions
df4$average == ((df4$COVIDTIMESESSION1 + df4$COVIDTIMESESSION2)/2) #average between two sessions

#reverse PA1, so higher is better
df4$rev_PA1 <- 0 - df4$PA1_reg

library(tidyverse)
library(dplyr)

df4$Severity <- as.factor(df4$Severity)
table(df4$Severity)
#lm Severity PA1
fit1 <- lm(formula = rev_PA1 ~ Severity + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit1)
PA1pvalue <- summary(fit1)$coefficients[, "Pr(>|t|)"] #extract p values
PA1pvalue <- data.frame(PA1pvalue) #create data frame of  p values
dfPA1_add_padj <- PA1pvalue %>% 
  mutate("adjusted p-value" = p.adjust(PA1pvalue, method="fdr")) #correct p values

#lm Severity PA2
fit2 <- lm(formula = PA2_reg ~ Severity + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit2)
PA2pvalue <- summary(fit2)$coefficients[, "Pr(>|t|)"] #extract p values
PA2pvalue <- data.frame(PA2pvalue) #create data frame of  p values
dfPA2_add_padj <- PA2pvalue %>% 
  mutate("adjusted p-value" = p.adjust(PA2pvalue, method="fdr")) #correct p values
dfPA2_add_padj

#lm Severity PA3
fit3 <- lm(formula = PA3_reg ~ Severity + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit3)
PA3pvalue <- summary(fit3)$coefficients[, "Pr(>|t|)"] #extract p values
PA3pvalue <- data.frame(PA3pvalue) #create data frame of  p values
dfPA3_add_padj <- PA3pvalue %>% 
  mutate("adjusted p-value" = p.adjust(PA3pvalue, method="fdr")) #correct p values
dfPA3_add_padj
#lm Severity Word Def
fit4 <- lm(formula = Word_DefZ ~ Severity + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit4)
Wordpvalue <- summary(fit4)$coefficients[, "Pr(>|t|)"] #extract p values
Wordpvalue <- data.frame(Wordpvalue) #create data frame of  p values
dfWord_add_padj <- Wordpvalue %>% 
  mutate("adjusted p-value" = p.adjust(Wordpvalue, method="fdr")) #correct p values
dfWord_add_padj

#lingering symptoms PA1
fit5 <- lm(formula = rev_PA1 ~ `How severe are your symptoms` + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit5)
PA1pvalue_lingering <- summary(fit5)$coefficients[, "Pr(>|t|)"] #extract p values
PA1pvalue_lingering <- data.frame(PA1pvalue_lingering) #create data frame of  p values
dfPA1_add_padj_lingering <- PA1pvalue_lingering %>% 
  mutate("adjusted p-value" = p.adjust(PA1pvalue_lingering, method="fdr")) #correct p values

#lingering symptoms PA2
fit6 <- lm(formula = PA2_reg ~ `How severe are your symptoms` + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit6)
PA2pvalue_lingering <- summary(fit6)$coefficients[, "Pr(>|t|)"] #extract p values
PA2pvalue_lingering <- data.frame(PA2pvalue_lingering) #create data frame of  p values
dfPA2_add_padj_lingering <- PA2pvalue_lingering %>% 
  mutate("adjusted p-value" = p.adjust(PA2pvalue_lingering, method="fdr")) #correct p values

#lingering symptoms PA3
fit7 <- lm(formula = PA3_reg ~ `How severe are your symptoms` + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit7)
PA3pvalue_lingering <- summary(fit7)$coefficients[, "Pr(>|t|)"] #extract p values
PA3pvalue_lingering <- data.frame(PA3pvalue_lingering) #create data frame of  p values
dfPA3_add_padj_lingering <- PA3pvalue_lingering %>% 
  mutate("adjusted p-value" = p.adjust(PA3pvalue_lingering, method="fdr")) #correct p values

#lingering symptoms Word Def
fit8 <- lm(formula = Word_DefZ ~ `How severe are your symptoms` + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit8)
Wordpvalue_lingering <- summary(fit8)$coefficients[, "Pr(>|t|)"] #extract p values
Wordpvalue_lingering <- data.frame(Wordpvalue_lingering) #create data frame of  p values
dfWord_add_padj_lingering <- Wordpvalue_lingering %>% 
  mutate("adjusted p-value" = p.adjust(Wordpvalue_lingering, method="fdr")) #correct p values

#as numeric

df4$SOB_sev <- as.factor(df4$SOB_sev)
#relevel SOB
df4$SOB_sev <- relevel(factor(df4$SOB_sev), ref = "5")
table(df4$SOB_sev)
#lm SOB sev
fit9 <- lm(formula = rev_PA1 ~ SOB_sev + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit9)
PA1pvalue_SOB <- summary(fit9)$coefficients[, "Pr(>|t|)"] #extract p values
PA1pvalue_SOB <- data.frame(PA1pvalue_SOB) #create data frame of  p values
dfPA1_add_padj_SOB <- PA1pvalue_SOB %>% 
  mutate("adjusted p-value" = p.adjust(PA1pvalue_SOB, method="fdr")) #correct p values
dfPA1_add_padj_SOB
#lm SOB sev PA2
fit10 <- lm(formula = PA2_reg ~ SOB_sev + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit10)
PA2pvalue_SOB <- summary(fit10)$coefficients[, "Pr(>|t|)"] #extract p values
PA2pvalue_SOB <- data.frame(PA2pvalue_SOB) #create data frame of  p values
dfPA2_add_padj_SOB <- PA2pvalue_SOB %>% 
  mutate("adjusted p-value" = p.adjust(PA2pvalue_SOB, method="fdr")) #correct p values
dfPA2_add_padj_SOB
#lm SOB sev PA3
fit11 <- lm(formula = PA3_reg ~ SOB_sev + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit11)
PA3pvalue_SOB <- summary(fit11)$coefficients[, "Pr(>|t|)"] #extract p values
PA3pvalue_SOB <- data.frame(PA3pvalue_SOB) #create data frame of  p values
dfPA3_add_padj_SOB <- PA3pvalue_SOB %>% 
  mutate("adjusted p-value" = p.adjust(PA3pvalue_SOB, method="fdr")) #correct p values
dfPA3_add_padj_SOB
#lm SOB sev Word
fit12 <- lm(formula = Word_DefZ ~ SOB_sev + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit12)
Wordpvalue_SOB <- summary(fit12)$coefficients[, "Pr(>|t|)"] #extract p values
Wordpvalue_SOB <- data.frame(Wordpvalue_SOB) #create data frame of  p values
dfWord_add_padj_SOB <- Wordpvalue_SOB %>% 
  mutate("adjusted p-value" = p.adjust(Wordpvalue_SOB, method="fdr")) #correct p values
dfWord_add_padj_SOB

#time since COVID PA1
fit13 <- lm(formula = rev_PA1 ~ average + Severity + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit13)
PA1pvalue_time <- summary(fit13)$coefficients[, "Pr(>|t|)"] #extract p values
PA1pvalue_time <- data.frame(PA1pvalue_time) #create data frame of  p values
dfPA1pvalue_time <- PA1pvalue_time %>% 
  mutate("adjusted p-value" = p.adjust(PA1pvalue_time, method="fdr")) #correct p values
dfPA1pvalue_time

#time since COVID PA2
fit14 <- lm(formula = PA2_reg ~ average + Severity + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit14)
PA2pvalue_time <- summary(fit14)$coefficients[, "Pr(>|t|)"] #extract p values
PA2pvalue_time <- data.frame(PA2pvalue_time) #create data frame of  p values
dfPA2pvalue_time <- PA2pvalue_time %>% 
  mutate("adjusted p-value" = p.adjust(PA2pvalue_time, method="fdr")) #correct p values
dfPA2pvalue_time

#time since COVID PA3
fit15 <- lm(formula = PA3_reg ~ average + Severity + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit15)
PA3pvalue_time <- summary(fit15)$coefficients[, "Pr(>|t|)"] #extract p values
PA3pvalue_time <- data.frame(PA3pvalue_time) #create data frame of  p values
dfPA3pvalue_time <- PA3pvalue_time %>% 
  mutate("adjusted p-value" = p.adjust(PA3pvalue_time, method="fdr")) #correct p values
dfPA3pvalue_time

#time since COVID Word
fit16 <- lm(formula = Word_DefZ ~ average + Severity + centeredage + centeredses + centerededucation + race4 + gendernum, data = df4)
summary(fit16)
Wordpvalue_time <- summary(fit15)$coefficients[, "Pr(>|t|)"] #extract p values
Wordpvalue_time <- data.frame(Wordpvalue_time) #create data frame of  p values
dfWordpvalue_time <- Wordpvalue_time %>% 
  mutate("adjusted p-value" = p.adjust(Wordpvalue_time, method="fdr")) #correct p values
dfWordpvalue_time



#Memory Color
g1 <- 
  ggplot(df4, aes(Severity, PA3_reg)) + 
  geom_violin(fill = rgb(0/255,158/255,115/255), alpha =.3) + 
  geom_boxplot(aes(x = Severity, y = PA2_reg), width = 0.1, 
               fill = rgb(0/255,158/255,115/255), alpha = .3,
               outlier.colour = rgb(.1,.1,.1))

g1
g2 <- g1 + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  xlab("Severity of COVID-19") + ylab("Memory")+
  theme_bw()
g2 + theme(text = element_text(size = 15)) 
#General Reasoning Color
g1 <- 
  ggplot(df4, aes(Severity, PA2_reg)) + 
  geom_violin(fill = rgb(240/255,228/255,56/255), alpha =.3) + 
  geom_boxplot(aes(x = Severity, y = PA2_reg), width = 0.1, 
               fill = rgb(240/255,228/255,56/255), alpha = .3,
               outlier.colour = rgb(.1,.1,.1))

g1
g2 <- g1 + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  xlab("Severity of COVID-19") + ylab("General Reasoning")+
  theme_bw()
g2 + theme(text = element_text(size = 15)) 
#Processing Speed Color
g1 <- 
  ggplot(df4, aes(Severity, rev_PA1)) + 
  geom_violin(fill = rgb(86/255,180/255,233/255), alpha =.3) + 
  geom_boxplot(aes(x = Severity, y = rev_PA1), width = 0.1, 
               fill = rgb(86/255,180/255,233/255), alpha = .3,
               outlier.colour = rgb(.1,.1,.1))

g1
g2 <- g1 + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  xlab("Severity of COVID-19") + ylab("Processing Speed")+
  theme_bw()
g2 + theme(text = element_text(size = 15)) 
#Word Def Color
g1 <- 
  ggplot(df4, aes(Severity, Word_DefZ)) + 
  geom_violin(fill = rgb(0,0,0), alpha =.2) + 
  geom_boxplot(aes(x = Severity, y = Word_DefZ), width = 0.1, 
               fill = rgb(0,0,0), alpha = .2,
               outlier.colour = rgb(.1,.1,.1))

g1
g2 <- g1 + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  xlab("Severity of COVID-19") + ylab("Word Definitions")+
  theme_bw()

g2 + theme(text = element_text(size = 15)) 

#lingering symptoms Memory
library(ggplot2)
plot <- ggplot(df4, aes(x=`How severe are your symptoms`, y=PA3_reg))+
  geom_point(color = rgb(0/255,158/255,115/255), alpha = 1)+
  stat_smooth(method="lm", fill = rgb(0/255,158/255,115/255),
              color = "dimgrey", alpha = .3)

plot + 
  xlab("Subjective Severity of Lingering Symptoms") + 
  ylab("Memory")+
  theme_bw()+
  theme(text = element_text(size = 15)) 

#lingering Processing Speed
plot <- ggplot(df4, aes(x=`How severe are your symptoms`, y=rev_PA1))+
  geom_point(color = rgb(86/255,180/255,233/255), alpha = 1)+
  stat_smooth(method="lm", fill = rgb(86/255,180/255,233/255),
              color = "dimgrey", alpha = .3)

plot + 
  xlab("Subjective Severity of Lingering Symptoms") + 
  ylab("Processing Speed")+
  theme_bw()+
  theme(text = element_text(size = 15)) 

#lingering General Reasoning
plot <- ggplot(df4, aes(x=`How severe are your symptoms`, y=PA2_reg))+
  geom_point(color = rgb(240/255,228/255,56/255), alpha = 1)+
  stat_smooth(method="lm", fill = rgb(240/255,228/255,56/255),
              color = "dimgrey", alpha = .3)
plot + 
  xlab("Subjective Severity of Lingering Symptoms") + 
  ylab("General Reasoning")+
  theme_bw()+
  theme(text = element_text(size = 15)) 

#lingering Word Def
plot <- ggplot(df4, aes(x=`How severe are your symptoms`, y=Word_DefZ))+
  geom_point(color = rgb(0,0,0), alpha = 1)+
  stat_smooth(method="lm", fill = rgb(0,0,0),
              color = "dimgrey", alpha = .3)

plot + 
  xlab("Subjective Severity of Lingering Symptoms") + 
  ylab("Word Definitions")+
  theme_bw()+
  theme(text = element_text(size = 15))









#old analyses ANCOVA


#ancovas Severity PA1
ancova1 = aov(rev_PA1 ~ Severity + centeredage + centeredses + centerededucation + race_3 + gender, data = df4)
summary(ancova1, type='III')

emm = emmeans(ancova1, specs = trt.vs.ctrl ~ Severity)
summary(emm)

#ANCOVA PA2
ancova1 = aov(PA2_reg ~ Severity + centeredage + centeredses + centerededucation + race4 + gender, data = df4)
summary(ancova1, type='III')
#post hoc testing
library(emmeans)
emm = emmeans(ancova1, specs = trt.vs.ctrl ~ Severity)
summary(emm)

#ANCOVA PA3
ancova1 = aov(PA3_reg ~ Severity + centeredage + centeredses + centerededucation + race_3 + gender, data = df4)
summary(ancova1, type='III')

emm = emmeans(ancova1, specs = trt.vs.ctrl ~ Severity)
summary(emm)

#ANCOVA WORD DEF
ancova1 = aov(Word_DefZ ~ Severity + centeredage + centeredses + centerededucation + race_3 + gender, data = df4)
summary(ancova1, type='III')

emm = emmeans(ancova1, specs = trt.vs.ctrl ~ Severity)
summary(emm)


#ANCOVA SOB_sev PA2
ancova1 = aov(PA2_reg ~ SOB_sev + centeredage + centeredses + centerededucation + race_3 + gender, data = df4)
summary(ancova1, type='III')

emm = emmeans(ancova1, specs = trt.vs.ctrl ~ SOB_sev)
summary(emm)

#ANCOVA SOB_sev PA3
ancova1 = aov(PA3_reg ~ SOB_sev + centeredage + centeredses + centerededucation + race_3 + gender, data = df4)
summary(ancova1, type='III')

emm = emmeans(ancova1, specs = trt.vs.ctrl ~ SOB_sev)
summary(emm)

#ANCOVA SOB_sev Word Def
ancova1 = aov(Word_DefZ ~ SOB_sev + centeredage + centeredses + centerededucation + race_3 + gender, data = df4)
summary(ancova1, type='III')

emm = emmeans(ancova1, specs = trt.vs.ctrl ~ SOB_sev)
summary(emm)


