#vaccination stuff
dfv3 <- merge(dfv2, Moralization_Factors, by = 'Prolific ID', all.x = TRUE)
dfv2<- dfv3

#Vaccination status code
df4 = df4 %>% mutate(Vaccination_status = case_when(
  df4$`Vaccinated` == 'Yes, fully vaccinated' ~ '1',
  df4$`Vaccinated` == 'Yes, partially vaccinated' ~ '1',
  df4$`Vaccinated` == 'No' ~ '0'))
dfv <- df4 #create new data frame
dfv[!is.na(dfv$Vaccination_status),] #remove NAs
dfv2 <- dfv[complete.cases(dfv$Vaccination_status),]

#race code
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
#relevel factors
dfv2$race4 <- relevel(factor(dfv2$race4), ref = "White")

#z-scores
dfv2$Open_MindZ <- scale(dfv2$Open_Mind, center = TRUE, scale = TRUE)
dfv2$Close_MindZ <- scale(dfv2$Close_Mind, center = TRUE, scale = TRUE)
dfv2$Intuitive_ThinkZ <- scale(dfv2$Intuitive_Think, center = TRUE, scale = TRUE)
dfv2$Effortful_ThinkZ <- scale(dfv2$Effortful_Think, center = TRUE, scale = TRUE)

#t.tests
t.test(corsibZ~Vaccination_status, dfv2, var.equal=TRUE)

data_new <- dfv2                                     # Duplicate data
data_new[is.na(data_new) | data_new == "Inf"] <- NA 

#linear modeling
fit1 <- lm(formula = Intuitive_ThinkZ ~ Vaccination_status + centeredage + 
             centeredses + centerededucation + race4 + gender, data = dfv2)
summary(fit1)

fit1 <- lm(formula = PA3_reg ~ Vaccination_status + centeredage + 
             centeredses + centerededucation + race4 + gender + CompThinkZ, data = dfv2)
summary(fit1)

dfv2$race <- as.factor(dfv2$race)

table(df4$race)

#boxplots
library(ggplot2)
library(stringr)
plot2 <- ggplot(dfv2, mapping = aes(x = Vaccination_status, y = `corsifZ`)) +
  geom_boxplot(fill = rgb(86/255,180/255,233/255), alpha=0.2)+ 
  theme_bw()
plot2 + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  xlab("Vaccination Status") + ylab("Corsi Forwards")

p2 <- ggplot(dfv2, aes(x=Vaccination_status, y=MoralizationZ, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~variety, scale="free")

plot <- ggplot(Severity0_2, aes(x=`Age`, y=PA1_reg, color = covid))+
  geom_point()+
  stat_smooth(method="lm")

g1 <- 
  ggplot(dfv2, aes(Vaccination_status, MoralizationZ)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA, fill = rgb(86/255,180/255,233/255, alpha = .3)) +
  ggdist::stat_dots(side = "left", dotsize = .3, justification = 1.1, binwidth = .1)
g1 + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  xlab("Vacciantion Status") + ylab("Moralization")+
  theme_bw()

g1 <- 
  ggplot(dfv2, aes(Vaccination_status, corsifZ)) + 
  geom_violin(fill = rgb(86/255,180/255,233/255), alpha =.3)+ 
  geom_boxplot(aes(x = Vaccination_status, y = corsifZ), width = 0.1, 
               fill = rgb(86/255,180/255,233/255), alpha = .3,
               outlier.colour = rgb(.1,.1,.1))
g1 + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  xlab("Vaccination Status") + ylab("Corsi Forwards")
