

# Hunger and Beauty Perception


library(tidyverse)
library(psych)
library(lme4) 
library(lmerTest)
library(emmeans)
library(pbkrtest)
library(broom)


# Study 1A ####


## Data Management ====

df1a <- readxl::read_excel('Data_Study1A.xlsx')

# Rename and recode variables in raw data
df1a <- df1a %>% mutate(
  par_gender = case_when(male1 == 1 ~ 1,
                  male1 == 2 ~ 0),
  treat = case_when(hunger1 == 1 ~ 1,
                     hunger1 == 2 ~ 0),
  male1 = NULL,
  hunger1 = NULL
)

df1a$par_gender <- factor(df1a$par_gender, levels = c(0, 1),
                 labels = c('Women Participants', 'Men Participants'))

df1a$treat <- factor(df1a$treat, levels = c(0, 1),
                    labels = c('Satiation', 'Hunger'))

# Reshape data to long format
df1a_long <- df1a %>% 
  gather(W1:M40, key = 'pic', value = 'rating') %>% 
  arrange(id)

# Create dummy variable that identifies the gender of faces in the pictures
df1a_long <- df1a_long %>% mutate(
  pic_gender = case_when(str_detect(pic, '^W') ~ 0,
                     str_detect(pic, '^M') ~ 1),
)

df1a_long$pic_gender <- factor(df1a_long$pic_gender, levels = c(0, 1),
                          labels = c('Female Pictures', 'Male Pictures'))

## Analysis ====

# Manipulation check on felt hunger across different treatment conditions
describeBy(df1a$hungry, df1a$treat)

t.test(hungry ~ treat, df1a, var.equal = TRUE)

# Linear mixed model

# Showing significant Treat*Pic_Gender interaction at Par_Gender == Men
lmx1a_1 <- lmer(rating ~ treat*relevel(par_gender, ref = 'Men Participants')*pic_gender
               + (1 | id), df1a_long)

summary(lmx1a_1)

# Showing non-significant Treat*Pic_Gender interaction at Par_Gender == Women
lmx1a_2 <- lmer(rating ~ treat*par_gender*pic_gender + (1 | id), df1a_long)

summary(lmx1a_2)

## Figures ====

# Get mean attractiveness ratings in each cell

df1a_mean <- emmip(lmx1a_2, ~ treat | par_gender + pic_gender, CIs = TRUE,
      pbkrtest.limit = 10080, lmerTest.limit = 10080,
      plotit = FALSE)


# Study 1B ####


## Data Management ====

df1b <- readxl::read_excel('Data_Study1B.xlsx')

# Rename and recode variables in raw data
df1b <- df1b %>% mutate(
  par_gender = case_when(male1 == 1 ~ 1,
                        male1 == 2 ~ 0),
  treat = case_when(hunger1 == 1 ~ 1,
                    hunger1 == 2 ~ 0),
  pic_gender = case_when(picfemale1 == 1 ~ 0,
                         picfemale1 == 2 ~ 1),
  rating = rowMeans(select(., rating1, rating2), na.rm = TRUE),
  male1 = NULL,
  hunger1 = NULL,
  picfemale1 = NULL
)

df1b$par_gender <- factor(df1b$par_gender, levels = c(0, 1),
                          labels = c('Women Participants', 'Men Participants'))

df1b$treat <- factor(df1b$treat, levels = c(0, 1),
                     labels = c('Satiation', 'Hunger'))

df1b$pic_gender <- factor(df1b$pic_gender, levels = c(0, 1),
                               labels = c('Female Pictures', 'Male Pictures'))

## Analysis ====

# Correlation of two attractive rating items
cor.test(df1b$rating1, df1b$rating2)

# Linear regression

# Showing significant Treat*Pic_Gender interaction at Par_Gender == Men
lm1b_1 <- lm(rating ~ treat*relevel(par_gender, ref = 'Men Participants')*pic_gender, df1b)

tidy(lm1b_1)

# Showing non-significant Treat*Pic_Gender interaction at Par_Gender == Women
lm1b_2 <- lm(rating ~ treat*par_gender*pic_gender, df1b)

tidy(lm1b_2)

## Figures ====

# Get mean attractiveness ratings in each cell

df1b_mean <- emmip(lm1b_2, ~ treat | par_gender + pic_gender, CIs = TRUE,
                   plotit = FALSE)


# Study 2 ####


## Data Management ====

df2 <- readxl::read_excel('Data_Study2.xlsx')

# Rename and recode variables in raw data
df2 <- df2 %>% mutate(
  par_gender = case_when(male1 == 1 ~ 1,
                         male1 == 2 ~ 0),
  treat = case_when(hunger1 == 1 ~ 1,
                    hunger1 == 2 ~ 0),
  male1 = NULL,
  hunger1 = NULL
)

df2$par_gender <- factor(df2$par_gender, levels = c(0, 1),
                          labels = c('Women Participants', 'Men Participants'))

df2$treat <- factor(df2$treat, levels = c(0, 1),
                     labels = c('Satiation', 'Hunger'))

# Reshape data to long format
df2_long <- df2 %>% 
  gather('Neutral Woman@1':'Muscular Man @48', key = 'pic', value = 'duration') %>% 
  arrange(id)

# Create dummy variables that identify the category of the pictures
df2_long <- df2_long %>% mutate(
  pic_cue = case_when(str_detect(pic, '^Neutral') ~ 0,
                         !str_detect(pic, '^Neutral') ~ 1),
  pic_gender = case_when(str_detect(pic, 'Woman') ~ 0,
                         str_detect(pic, 'Man') ~ 1)
)

df2_long$pic_cue <- factor(df2_long$pic_cue, levels = c(0, 1),
                              labels = c('Sexual Cue-Absent', 'Sexual Cue-Present'))

df2_long$pic_gender <- factor(df2_long$pic_gender, levels = c(0, 1),
                               labels = c('Female Pictures', 'Male Pictures'))

## Analysis ====

# Manipulation check on felt hunger across different treatment conditions
describeBy(df2$hungry, df2$treat)

t.test(hungry ~ treat, df2, var.equal = TRUE)

# Linear mixed model

# Showing significant Treat*Pic_Gender interaction at Par_Gender == Men and
# at both Pic_Cue == Sexual Cue-Absent and Pic_Cue == Sexual Cue-Present
lmx2_1 <- lmer(
  duration ~ treat*relevel(par_gender, ref = 'Men Participants')*pic_gender*pic_cue
  + (1 | id), df2_long
)

summary(lmx2_1)

# Showing significant Treat*Pic_Cue interaction at Par_Gender == Women and
# at Pic_Gender == Male
lmx2_2 <- lmer(
  duration ~ treat*par_gender*relevel(pic_gender, ref = 'Male Pictures')*pic_cue
  + (1 | id), df2_long
)

summary(lmx2_2)

## Figures ====

# Get mean attractiveness ratings in each cell

lmx2 <- lmer(
  duration ~ treat*par_gender*pic_gender*pic_cue + (1 | id), df2_long
)

df2_mean <- emmip(lmx2, ~ treat | par_gender + pic_gender + pic_cue, CIs = TRUE,
                  pbkrtest.limit = 7872, lmerTest.limit = 7872,
                  plotit = FALSE)


# Study 3 ####


## Data Management ====

df3 <- readxl::read_excel('Data_Study3.xlsx')

# Rename and recode variables in raw data
df3 <- df3 %>% mutate(
  par_gender = case_when(male1 == 1 ~ 1,
                         male1 == 2 ~ 0),
  treat = case_when(hunger1 == 1 ~ 1,
                    hunger1 == 2 ~ 0),
  pic_cue = case_when(pic_cat %in% c(1, 3) ~ 0,
                      pic_cat %in% c(2, 4) ~ 1),
  pic_gender = case_when(pic_cat %in% c(1, 2) ~ 0,
                         pic_cat %in% c(3, 4) ~ 1),
  rst = rowMeans(select(., rst1:rst3), na.rm = TRUE),
  male1 = NULL,
  hunger1 = NULL
)

df3$par_gender <- factor(df3$par_gender, levels = c(0, 1),
                         labels = c('Women Participants', 'Men Participants'))

df3$treat <- factor(df3$treat, levels = c(0, 1),
                    labels = c('Satiation', 'Hunger'))

df3$pic_cue <- factor(df3$pic_cue, levels = c(0, 1),
                           labels = c('Sexual Cue-Absent', 'Sexual Cue-Present'))

df3$pic_gender <- factor(df3$pic_gender, levels = c(0, 1),
                              labels = c('Female Pictures', 'Male Pictures'))

# Reshape data to long format
df3_long <- df3 %>% 
  gather(rating_pic1:rating_pic3, key = 'pic', value = 'rating') %>% 
  arrange(id)

## Analysis ====

# Reliability of the rewarding-seeking tendency measures

alpha(df3[, 2:4])

# Linear mixed model

# Showing significant Treat*Pic_Gender interaction at Par_Gender == Men and
# at both Pic_Cue == Sexual Cue-Absent and Pic_Cue == Sexual Cue-Present
lmx3_1 <- lmer(
  rating ~ treat*relevel(par_gender, ref = 'Men Participants')*pic_gender*pic_cue
  + (1 | id), df3_long
)

summary(lmx3_1)

# Showing significant Treat*Pic_Cue interaction at Par_Gender == Women and
# at Pic_Gender == Male
lmx3_2 <- lmer(
  rating ~ treat*par_gender*relevel(pic_gender, ref = 'Male Pictures')*pic_cue
  + (1 | id), df3_long
)

summary(lmx3_2)

## Figures ====

# Get mean attractiveness ratings in each cell

lmx3 <- lmer(
  rating ~ treat*par_gender*pic_gender*pic_cue + (1 | id), df3_long
)

df3_mean <- emmip(lmx3, ~ treat | par_gender + pic_gender + pic_cue, CIs = TRUE,
                  pbkrtest.limit = 7872, lmerTest.limit = 7872,
                  plotit = FALSE)
