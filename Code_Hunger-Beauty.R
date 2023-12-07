
# Hunger and Beauty Perception

# Load packages

library(tidyverse)
library(psych)
library(lme4)
library(lmerTest)
library(emmeans)
library(pbkrtest)
library(broom)
library(bruceR)


# Study 1 ####

## Data Management ====

# Load data
df1 <- readxl::read_excel("Data_Study1.xlsx")

# Rename and recode variables in raw data
df1 <- df1 %>%
mutate(
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

# Factor variables

df1$par_gender <- factor(df1$par_gender, levels = c(0, 1),
                          labels = c("Women Participants", "Men Participants"))

df1$treat <- factor(df1$treat, levels = c(0, 1),
                     labels = c("Satiation", "Hunger"))

df1$pic_gender <- factor(df1$pic_gender, levels = c(0, 1),
                          labels = c("Female Pictures", "Male Pictures"))

## Analyses ====

# Correlation of two attractive rating items
cor.test(df1$rating1, df1$rating2)

# Linear regression

# Show significant treat * pic_gender interaction
# at par_gender == "Men Participants"
lm1_1 <- lm(rating ~ treat * relevel(par_gender,
ref = "Men Participants") * pic_gender, df1)

tidy(lm1_1)
tidy(lm1_1)[6, ]

# Show non-significant treat * pic_gender interaction
# at par_gender == "Women Participants"
lm1_2 <- lm(rating ~ treat * par_gender * pic_gender, df1)

tidy(lm1_2)
tidy(lm1_2)[6, ]

# Get mean attractiveness ratings in each condition
df1_mean <- emmip(lm1_2, ~ treat | par_gender + pic_gender,
CIs = TRUE, plotit = FALSE)

## Figure 1 ====

fig1 <- ggplot(df1_mean, aes(x = pic_gender, y = yvar, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LCL, ymax = UCL),
    width = 0.1, position = position_dodge(0.9)) +
  geom_text(aes(label = round(yvar, 2), y = UCL),
   family = "Helvetica Neue Light", vjust = -1, size = 5,
   position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  labs(title = "Figure 1. Study 1",
    y = "Mean Attractiveness Rating",
    fill = "Treatment \nCondition",
    caption = "Error bars represent 95% confidence intervals.") +
  lemon::facet_rep_wrap(~ fct_relevel(par_gender, "Men Participants"),
    nrow = 2, repeat.tick.labels = TRUE) +
  theme_light() +
  theme(
    text = element_text(family = "Helvetica Neue Light", size = 18),
    plot.title = element_text(margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0, margin = margin(30, 0, 0, 0)),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "black", size = 16),
    strip.text = element_text(size = 16, color = "black"),
    strip.background = element_rect(fill = "lightgrey"),
    panel.grid.major.y = element_line(linewidth = 1, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) + # horizontal line
  geom_segment(
    data = filter(df1_mean, par_gender == "Men Participants"),
    aes(x = 1 - 0.225, y = 4.94 + 1.5,
        xend = 1 + 0.225, yend = 4.94 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df1_mean, par_gender == "Men Participants"),
    aes(x = 1 - 0.225, y = 4.94 + 1.5 + 0.01,
        xend = 1 - 0.225, yend = 4.94 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df1_mean, par_gender == "Men Participants"),
    aes(x = 1 + 0.225, y = 4.94 + 1.5 + 0.01,
        xend = 1 + 0.225, yend = 4.94 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df1_mean, par_gender == "Men Participants"),
    aes(x = 1, y = 4.94 + 1.5 + 0.25, label = "***"),
    color = "black", size = 7
  )

# Study 2 ####

## Data Management ====

# Load data
df2 <- readxl::read_excel("Data_Study2.xlsx")

# Rename and recode variables in raw data
df2 <- df2 %>%
mutate(
  par_gender = case_when(male1 == 1 ~ 1,
                         male1 == 2 ~ 0),
  treat = case_when(hunger1 == 1 ~ 1,
                    hunger1 == 2 ~ 0),
  male1 = NULL,
  hunger1 = NULL
)

# Factor variables

df2$par_gender <- factor(df2$par_gender, levels = c(0, 1),
                          labels = c("Women Participants", "Men Participants"))

df2$treat <- factor(df2$treat, levels = c(0, 1),
                     labels = c("Satiation", "Hunger"))

# Reshape data to long format
df2_long <- df2 %>%
pivot_longer(
  cols = c("Neutral Woman@1":"Muscular Man @48"),
  names_to = "pic",
  values_to = "duration"
) %>%
arrange(id)

# Create dummy variables that identify the category of the pictures
df2_long <- df2_long %>%
mutate(
  pic_int = case_when(str_detect(pic, "Neutral") ~ 0,
                         !str_detect(pic, "Neutral") ~ 1),
  pic_gender = case_when(str_detect(pic, "Woman") ~ 0,
                         str_detect(pic, "Man") ~ 1)
)

# Factor variables

df2_long$pic_int <- factor(df2_long$pic_int, levels = c(0, 1),
                            labels = c("Neutral-Sexual Traits",
                              "Intense-Sexual Traits"))

df2_long$pic_gender <- factor(df2_long$pic_gender, levels = c(0, 1),
                               labels = c("Female Pictures", "Male Pictures"))

## Analyses ====

# Manipulation check on felt hunger
describeBy(df2$hungry, df2$treat)

t.test(hungry ~ treat, df2, var.equal = TRUE)

# Linear mixed model

# Show significant treat * pic_gender interaction
# at par_gender == "Men Participants"
# when pic_int == "Neutral-Sexual Traits" or "Intense-Sexual Traits"
lmx2_1 <- lmer(duration ~ treat * relevel(par_gender,
ref = "Men Participants") * pic_gender * pic_int + (1 | id), df2_long)

summary(lmx2_1)
rownames(summary(lmx2_1)$coefficients)

summary(lmx2_1)$coefficients[c(7, 14), , drop = FALSE]

# Show significant treat * pic_int interaction
# at par_gender == "Women Participants" and pic_gender == "Male Pictures"
lmx2_2 <- lmer(duration ~ treat * par_gender * relevel(pic_gender,
ref = "Male Pictures") * pic_int + (1 | id), df2_long)

summary(lmx2_2)
rownames(summary(lmx2_2)$coefficients)

summary(lmx2_2)$coefficients[9, , drop = FALSE]

# Get mean viewing time in each condition

lmx2 <- lmer(duration ~ treat * par_gender * pic_gender * pic_int
+ (1 | id), df2_long)

df2_mean <- emmip(lmx2, ~ treat | par_gender + pic_gender + pic_int,
CIs = TRUE, pbkrtest.limit = 7872, lmerTest.limit = 7872, plotit = FALSE)

## Figure 2 ====

# Collapse pic_int and pic_gender into one variable
df2_mean <- df2_mean %>%
mutate(
  pic_int_gender = paste(pic_int, pic_gender, sep = " ")
)

fig2 <- ggplot(df2_mean, aes(x = fct_relevel(pic_int_gender,
c("Neutral-Sexual Traits Female Pictures",
"Intense-Sexual Traits Female Pictures",
"Neutral-Sexual Traits Male Pictures")),
y = yvar, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LCL, ymax = UCL),
    width = 0.1, position = position_dodge(0.9)) +
  geom_text(aes(label = round(yvar, 2), y = UCL),
    family = "Helvetica Neue Light", vjust = -1, size = 5,
    position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0, 7.5), breaks = seq(0, 7, by = 1)) +
  scale_x_discrete(labels = ~ str_wrap(., width = 20)) +
  labs(title = "Figure 2. Study 2",
    y = "Mean Viewing Time (Seconds)",
    fill = "Treatment \nCondition",
    caption = "Error bars represent 95% confidence intervals.") +
  lemon::facet_rep_wrap(~ fct_relevel(par_gender, "Men Participants"),
    nrow = 2, repeat.tick.labels = TRUE) +
  theme_light() +
  theme(
    text = element_text(family = "Helvetica Neue Light", size = 18),
    plot.title = element_text(margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0, margin = margin(30, 0, 0, 0)),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "black", size = 16),
    strip.text = element_text(size = 16, color = "black"),
    strip.background = element_rect(fill = "lightgrey"),
    panel.grid.major.y = element_line(linewidth = 1, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) + # horizontal line
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 1 - 0.225, y = 4.16 + 1.5,
        xend = 1 + 0.225, yend = 4.16 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 1 - 0.225, y = 4.16 + 1.5 + 0.01,
        xend = 1 - 0.225, yend = 4.16 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 1 + 0.225, y = 4.16 + 1.5 + 0.01,
        xend = 1 + 0.225, yend = 4.16 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 1, y = 4.16 + 1.5 + 0.25, label = "**"),
    color = "black", size = 7
  ) + # horizontal line
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 2 - 0.225, y = 4.89 + 1.5,
        xend = 2 + 0.225, yend = 4.89 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 2 - 0.225, y = 4.89 + 1.5 + 0.01,
        xend = 2 - 0.225, yend = 4.89 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 2 + 0.225, y = 4.89 + 1.5 + 0.01,
        xend = 2 + 0.225, yend = 4.89 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 2, y = 4.89 + 1.5 + 0.25, label = "***"),
    color = "black", size = 7
  ) + # horizontal line
  geom_segment(
    data = filter(df2_mean, par_gender == "Women Participants"),
    aes(x = 4 - 0.225, y = 4.21 + 1.5,
        xend = 4 + 0.225, yend = 4.21 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df2_mean, par_gender == "Women Participants"),
    aes(x = 4 - 0.225, y = 4.21 + 1.5 + 0.01,
        xend = 4 - 0.225, yend = 4.21 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df2_mean, par_gender == "Women Participants"),
    aes(x = 4 + 0.225, y = 4.21 + 1.5 + 0.01,
        xend = 4 + 0.225, yend = 4.21 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df2_mean, par_gender == "Women Participants"),
    aes(x = 4, y = 4.21 + 1.5 + 0.25, label = "***"),
    color = "black", size = 7
  )

# Study 3 ####

## Data Management ====

# Load data
df3 <- readxl::read_excel("Data_Study3.xlsx")

# Rename and recode variables in raw data
df3 <- df3 %>%
mutate(
  par_gender = case_when(male1 == 1 ~ 1,
                         male1 == 2 ~ 0),
  treat = case_when(hunger1 == 1 ~ 1,
                    hunger1 == 2 ~ 0),
  pic_int = case_when(pic_cat %in% c(1, 3) ~ 0,
                      pic_cat %in% c(2, 4) ~ 1),
  pic_gender = case_when(pic_cat %in% c(1, 2) ~ 0,
                         pic_cat %in% c(3, 4) ~ 1),
  # reward-seeking tendency
  rst = rowMeans(select(., rst1:rst3), na.rm = TRUE),
  male1 = NULL,
  hunger1 = NULL
)

# Factor variables

df3$par_gender <- factor(df3$par_gender, levels = c(0, 1),
                         labels = c("Women Participants", "Men Participants"))

df3$treat <- factor(df3$treat, levels = c(0, 1),
                    labels = c("Satiation", "Hunger"))

df3$pic_int <- factor(df3$pic_int, levels = c(0, 1),
                      labels = c("Neutral-Sexual Traits",
                      "Intense-Sexual Traits"))

df3$pic_gender <- factor(df3$pic_gender, levels = c(0, 1),
                          labels = c("Female Pictures", "Male Pictures"))

# Reshape data to long format
df3_long <- df3 %>%
pivot_longer(
  cols = c(rating_pic1:rating_pic3),
  names_to = "pic",
  values_to = "rating"
) %>%
arrange(id)

## Analyses ====

# Reliability of the rewarding-seeking tendency measures
alpha(df3[, 2:4])

# Linear mixed model

# Show significant treat * pic_gender interaction
# at par_gender == "Men Participants"
# when pic_int == "Neutral-Sexual Traits" or "Intense-Sexual Traits"
lmx3_1 <- lmer(rating ~ treat * relevel(par_gender,
ref = "Men Participants") * pic_gender * pic_int + (1 | id), df3_long)

summary(lmx3_1)
rownames(summary(lmx3_1)$coefficients)

summary(lmx3_1)$coefficients[c(7, 14), , drop = FALSE]

# Show significant treat * pic_int interaction
# at par_gender == "Women Participants" and pic_gender == "Male Pictures"
lmx3_2 <- lmer(rating ~ treat * par_gender * relevel(pic_gender,
ref = "Male Pictures") * pic_int + (1 | id), df3_long)

summary(lmx3_2)
rownames(summary(lmx3_2)$coefficients)

summary(lmx3_2)$coefficients[9, , drop = FALSE]

# Mediation Analyses: 'treat -> rst -> rating'

# For men participants viewing female pictures
me1 <- PROCESS(
  df3_long[df3_long$par_gender == "Men Participants" &
             df3_long$pic_gender == "Female Pictures", ],
  y = "rating",
  x = "treat",
  meds = "rst",
  nsim = 1000,
  seed = 123
)

# For women participants viewing intense-sexual traits male pictures
me2 <- PROCESS(
  df3_long[df3_long$par_gender == "Women Participants" &
             df3_long$pic_gender == "Male Pictures" &
             df3_long$pic_int == "Intense-Sexual Traits", ],
  y = "rating",
  x = "treat",
  meds = "rst",
  nsim = 1000,
  seed = 123
)

# Get mean attractiveness ratings in each condition

lmx3 <- lmer(rating ~ treat * par_gender * pic_gender * pic_int
+ (1 | id), df3_long)

df3_mean <- emmip(lmx3, ~ treat | par_gender + pic_gender + pic_int,
CIs = TRUE, pbkrtest.limit = 7872, lmerTest.limit = 7872, plotit = FALSE)

## Figure 3 ====

# Collapse pic_int and pic_gender into one variable
df3_mean <- df3_mean %>%
mutate(
  pic_int_gender = paste(pic_int, pic_gender, sep = " ")
)

fig3 <- ggplot(df3_mean, aes(x = fct_relevel(pic_int_gender,
c("Neutral-Sexual Traits Female Pictures",
"Intense-Sexual Traits Female Pictures",
"Neutral-Sexual Traits Male Pictures")),
y = yvar, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LCL, ymax = UCL),
    width = 0.1, position = position_dodge(0.9)) +
  geom_text(aes(label = round(yvar, 2), y = UCL),
    family = "Helvetica Neue Light", vjust = -1, size = 5,
    position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  scale_x_discrete(labels = ~ str_wrap(., width = 20)) +
  labs(title = "Figure 3. Study 3",
    y = "Mean Attractiveness Rating",
    fill = "Treatment \nCondition",
    caption = "Error bars represent 95% confidence intervals.") +
  lemon::facet_rep_wrap(~ fct_relevel(par_gender, "Men Participants"),
    nrow = 2, repeat.tick.labels = TRUE) +
  theme_light() +
  theme(
    text = element_text(family = "Helvetica Neue Light", size = 18),
    plot.title = element_text(margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0, margin = margin(30, 0, 0, 0)),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "black", size = 16),
    strip.text = element_text(size = 16, color = "black"),
    strip.background = element_rect(fill = "lightgrey"),
    panel.grid.major.y = element_line(linewidth = 1, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) + # horizontal line
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 1 - 0.225, y = 4.91 + 1.5,
        xend = 1 + 0.225, yend = 4.91 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 1 - 0.225, y = 4.91 + 1.5 + 0.01,
        xend = 1 - 0.225, yend = 4.91 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 1 + 0.225, y = 4.91 + 1.5 + 0.01,
        xend = 1 + 0.225, yend = 4.91 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 1, y = 4.91 + 1.5 + 0.25, label = "**"),
    color = "black", size = 7
  ) + # horizontal line
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 2 - 0.225, y = 5.09 + 1.5,
        xend = 2 + 0.225, yend = 5.09 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 2 - 0.225, y = 5.09 + 1.5 + 0.01,
        xend = 2 - 0.225, yend = 5.09 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 2 + 0.225, y = 5.09 + 1.5 + 0.01,
        xend = 2 + 0.225, yend = 5.09 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 2, y = 5.09 + 1.5 + 0.25, label = "**"),
    color = "black", size = 7
  ) + # horizontal line
  geom_segment(
    data = filter(df3_mean, par_gender == "Women Participants"),
    aes(x = 4 - 0.225, y = 4.28 + 1.5,
        xend = 4 + 0.225, yend = 4.28 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df3_mean, par_gender == "Women Participants"),
    aes(x = 4 - 0.225, y = 4.28 + 1.5 + 0.01,
        xend = 4 - 0.225, yend = 4.28 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df3_mean, par_gender == "Women Participants"),
    aes(x = 4 + 0.225, y = 4.28 + 1.5 + 0.01,
        xend = 4 + 0.225, yend = 4.28 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df3_mean, par_gender == "Women Participants"),
    aes(x = 4, y = 4.28 + 1.5 + 0.25, label = "**"),
    color = "black", size = 7
  )

# Study 4 ####

## Data Management ====

df4 <- readxl::read_excel("Data_Study4.xlsx")

# Rename and recode variables in raw data
df4 <- df4 %>%
mutate(
  par_gender = case_when(male1 == 1 ~ 1,
                         male1 == 2 ~ 0),
  treat = case_when(hunger1 == 1 ~ 1,
                    hunger1 == 2 ~ 0),
  male1 = NULL,
  hunger1 = NULL
)

# Factor variables

df4$par_gender <- factor(df4$par_gender, levels = c(0, 1),
                         labels = c("Women Participants", "Men Participants"))

df4$treat <- factor(df4$treat, levels = c(0, 1),
                    labels = c("Satiation", "Hunger"))

# Reshape data to long format
df4_long <- df4 %>%
pivot_longer(
  cols = c("Neutral Woman (Image 1)":"Male Lion  (Image 3)"),
  names_to = "pic",
  values_to = "rating"
) %>%
arrange(id)

# Create dummy variables that identify the category of the pictures
df4_long <- df4_long %>%
mutate(
  pic_int = case_when(str_detect(pic, "Neutral") ~ 0,
                      str_detect(pic, "Sexy|Muscular") ~ 1,
                      str_detect(pic, "Flower|Lion") ~ 2),
  pic_gender = case_when(str_detect(pic, "Woman|Flower") ~ 0,
                         str_detect(pic, "Man|Lion") ~ 1)
)

# Factor variables

df4_long$pic_int <- factor(df4_long$pic_int, levels = c(0, 1, 2),
                           labels = c("Neutral-Sexual Traits",
                              "Intense-Sexual Traits",
                              "Non-Human"))

df4_long$pic_gender <- factor(df4_long$pic_gender, levels = c(0, 1),
                              labels = c("Female Pictures", "Male Pictures"))

## Analyses ====

# Manipulation check on felt hunger
describeBy(df4$hungry, df4$treat)

t.test(hungry ~ treat, df4, var.equal = TRUE)

# Linear mixed model

# Show significant treat * pic_gender interaction
# at par_gender == "Men Participants"
# when pic_int == "Neutral-Sexual Traits" or "Intense-Sexual Traits"
# or "Non-Human"
lmx4_1 <- lmer(rating ~ treat * relevel(par_gender,
ref = "Men Participants") * pic_gender * pic_int + (1 | id), df4_long)

summary(lmx4_1)
rownames(summary(lmx4_1)$coefficients)

summary(lmx4_1)$coefficients[c(8, 19, 20), , drop = FALSE]

# Show significant treat * pic_gender interaction
# at par_gender = "Women Participants"
# when pic_int == "Intense-Sexual Traits" or "Non-Human"
# but not when pic_int = "Neutral-Sexual Traits"
lmx4_2 <- lmer(rating ~ treat * par_gender * pic_gender * relevel(pic_int,
ref = "Intense-Sexual Traits") + (1 | id), df4_long)

summary(lmx4_2)
rownames(summary(lmx4_2)$coefficients)

summary(lmx4_2)$coefficients[c(8, 19, 20), , drop = FALSE]

# Get mean attractiveness ratings in each condition

lmx4 <- lmer(rating ~ treat * par_gender * pic_gender * pic_int
+ (1 | id), df4_long)

df4_mean <- emmip(lmx4, ~ treat | par_gender + pic_gender + pic_int,
CIs = TRUE, pbkrtest.limit = 3060, lmerTest.limit = 3060, plotit = FALSE)

## Figure 5 ====

# Collapse pic_int and pic_gender into one variable
df4_mean <- df4_mean %>%
mutate(
  pic_int_gender = paste(pic_int, pic_gender, sep = " ")
)

# Relabel the two non-human picture categories
df4_mean$pic_int_gender[df4_mean$pic_int_gender ==
"Non-Human Female Pictures"] <- "Flower Pictures"

df4_mean$pic_int_gender[df4_mean$pic_int_gender ==
"Non-Human Male Pictures"] <- "Male Lion Pictures"

fig5 <- ggplot(df4_mean, aes(x = fct_relevel(pic_int_gender,
c("Neutral-Sexual Traits Female Pictures",
"Intense-Sexual Traits Female Pictures",
"Flower Pictures",
"Neutral-Sexual Traits Male Pictures",
"Intense-Sexual Traits Male Pictures")),
y = yvar, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LCL, ymax = UCL),
    width = 0.1, position = position_dodge(0.9)) +
  geom_text(aes(label = round(yvar, 2), y = UCL),
    family = "Helvetica Neue Light", vjust = -1, size = 5,
    position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  scale_x_discrete(labels = ~ str_wrap(., width = 10)) +
  labs(title = "Figure 5. Study 4",
    y = "Mean Attractiveness Rating",
    fill = "Treatment \nCondition",
    caption = "Error bars represent 95% confidence intervals.") +
  lemon::facet_rep_wrap(~ fct_relevel(par_gender, "Men Participants"),
    nrow = 2, repeat.tick.labels = TRUE) +
  theme_light() +
  theme(
    text = element_text(family = "Helvetica Neue Light", size = 18),
    plot.title = element_text(margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0, margin = margin(30, 0, 0, 0)),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "black", size = 16),
    strip.text = element_text(size = 16, color = "black"),
    strip.background = element_rect(fill = "lightgrey"),
    panel.grid.major.y = element_line(linewidth = 1, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) + # horizontal line
  geom_segment(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 1 - 0.225, y = 4.92 + 1.5,
        xend = 1 + 0.225, yend = 4.92 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 1 - 0.225, y = 4.92 + 1.5 + 0.01,
        xend = 1 - 0.225, yend = 4.92 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 1 + 0.225, y = 4.92 + 1.5 + 0.01,
        xend = 1 + 0.225, yend = 4.92 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 1, y = 4.92 + 1.5 + 0.25, label = "**"),
    color = "black", size = 7
  ) + # horizontal line
  geom_segment(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 2 - 0.225, y = 5.10 + 1.5,
        xend = 2 + 0.225, yend = 5.10 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 2 - 0.225, y = 5.10 + 1.5 + 0.01,
        xend = 2 - 0.225, yend = 5.10 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 2 + 0.225, y = 5.10 + 1.5 + 0.01,
        xend = 2 + 0.225, yend = 5.10 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 2, y = 5.1 + 1.5 + 0.25, label = "***"),
    color = "black", size = 7
  ) + # horizontal line
  geom_segment(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 3 - 0.225, y = 6.55 + 1.5,
        xend = 3 + 0.225, yend = 6.55 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 3 - 0.225, y = 6.55 + 1.5 + 0.01,
        xend = 3 - 0.225, yend = 6.55 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 3 + 0.225, y = 6.55 + 1.5 + 0.01,
        xend = 3 + 0.225, yend = 6.55 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df4_mean, par_gender == "Men Participants"),
    aes(x = 3, y = 6.55 + 1.5 + 0.25, label = "**"),
    color = "black", size = 7
  ) + # horizontal line
  geom_segment(
    data = filter(df4_mean, par_gender == "Women Participants"),
    aes(x = 5 - 0.225, y = 3.96 + 1.5,
        xend = 5 + 0.225, yend = 3.96 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df4_mean, par_gender == "Women Participants"),
    aes(x = 5 - 0.225, y = 3.96 + 1.5 + 0.01,
        xend = 5 - 0.225, yend = 3.96 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df4_mean, par_gender == "Women Participants"),
    aes(x = 5 + 0.225, y = 3.96 + 1.5 + 0.01,
        xend = 5 + 0.225, yend = 3.96 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df4_mean, par_gender == "Women Participants"),
    aes(x = 5, y = 3.96 + 1.5 + 0.25, label = "*"),
    color = "black", size = 7
  ) + # horizontal line
  geom_segment(
    data = filter(df4_mean, par_gender == "Women Participants"),
    aes(x = 6 - 0.225, y = 6.81 + 1.5,
        xend = 6 + 0.225, yend = 6.81 + 1.5),
    color = "black", linewidth = 0.4
  ) + # vertical line, left
  geom_segment(
    data = filter(df4_mean, par_gender == "Women Participants"),
    aes(x = 6 - 0.225, y = 6.81 + 1.5 + 0.01,
        xend = 6 - 0.225, yend = 6.81 + 1.25),
    color = "black", linewidth = 0.4
  ) + # vertical line, right
  geom_segment(
    data = filter(df4_mean, par_gender == "Women Participants"),
    aes(x = 6 + 0.225, y = 6.81 + 1.5 + 0.01,
        xend = 6 + 0.225, yend = 6.81 + 1.25),
    color = "black", linewidth = 0.4
  ) + # stars
  geom_text(
    data = filter(df4_mean, par_gender == "Women Participants"),
    aes(x = 6, y = 6.81 + 1.5 + 0.25, label = "**"),
    color = "black", size = 7
  )

# Export Figures ####

ggsave(filename = "Figures/Fig1.png",
       width = 10, height = 14,
       dpi = 600,
       plot = fig1)

ggsave(filename = "Figures/Fig2.png",
       width = 15, height = 14,
       dpi = 600,
       plot = fig2)

ggsave(filename = "Figures/Fig3.png",
       width = 15, height = 14,
       dpi = 600,
       plot = fig3)

ggsave(filename = "Figures/Fig5.png",
       width = 15, height = 14,
       dpi = 600,
       plot = fig5)