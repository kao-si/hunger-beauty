
y_offset <- 1.5
x_offset <- 0.225


# Figure 1 ####


fig1 <- ggplot(df1a_mean, aes(x = pic_gender, y = yvar, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("Satiation" = "#3288BD", "Hunger" = "#A50026")) + 
  geom_errorbar(aes(ymin = yvar - SE, ymax = yvar + SE), width = 0.1, position = position_dodge(0.9)) + 
  geom_text(aes(label = round(yvar, 2), y = UCL), family = "CMU Serif",
            vjust = -1, position = position_dodge(0.9)) + 
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  labs(y = "Mean Attractiveness Rating", fill = "Treatment \nCondition") +
  facet_rep_wrap(~ fct_relevel(par_gender, "Men Participants"), nrow = 2, repeat.tick.labels = TRUE) +
  ggtitle("Figure 1. Study 1A") +
  theme_linedraw() +
  theme(
    text = element_text(family = "CMU Serif", size = 14),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + # 横线
  geom_segment(
    data = filter(df1a_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 5.31 + 1.5,
        xend = 1 + x_offset, yend = 5.31 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df1a_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 5.31 + 1.5,
        xend = 1 - x_offset, yend = 5.31 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df1a_mean, par_gender == "Men Participants"),
    aes(x = 1 + x_offset, y = 5.31 + 1.5,
        xend = 1 + x_offset, yend = 5.31 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df1a_mean, par_gender == "Men Participants"),
    aes(x = 1, y = 5.31 + 1.5 + 0.25, label = "***"),
    color = "black", size = 8
  )


# Figure 2 ####


fig2 <- ggplot(df1b_mean, aes(x = pic_gender, y = yvar, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("Satiation" = "#3288BD", "Hunger" = "#A50026")) + 
  geom_errorbar(aes(ymin = yvar - SE, ymax = yvar + SE), width = 0.1, position = position_dodge(0.9)) + 
  geom_text(aes(label = round(yvar, 2), y = UCL), family = "CMU Serif",
            vjust = -1, position = position_dodge(0.9)) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  labs(y = "Mean Attractiveness Rating", fill = "Treatment \nCondition") +
  facet_rep_wrap(~ fct_relevel(par_gender, "Men Participants"), nrow = 2, repeat.tick.labels = TRUE) +
  ggtitle("Figure 2. Study 1B") +
  theme_linedraw() +
  theme(
    text = element_text(family = "CMU Serif", size = 14),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + # 横线
  geom_segment(
    data = filter(df1b_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 4.94 + 1.5,
        xend = 1 + x_offset, yend = 4.94 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df1b_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 4.94 + 1.5,
        xend = 1 - x_offset, yend = 4.94 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df1b_mean, par_gender == "Men Participants"),
    aes(x = 1 + x_offset, y = 4.94 + 1.5, 
        xend = 1 + x_offset, yend = 4.94 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df1b_mean, par_gender == "Men Participants"),
    aes(x = 1, y = 4.94 + 1.5 + 0.25, label = "***"),
    color = "black", size = 8
  )


# Figure 3 ####


df2_mean <- df2_mean %>% mutate(
  pic_genderwithcue = paste(pic_cue, pic_gender, sep = " ")
)

fig3 <- ggplot(df2_mean, aes(x = fct_relevel(pic_genderwithcue, c("Sexual Cue-Absent Female Pictures",
                                                          "Sexual Cue-Present Female Pictures",
                                                          "Sexual Cue-Absent Male Pictures")), 
                                     y = yvar, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("Satiation" = "#3288BD", "Hunger" = "#A50026")) + 
  geom_errorbar(aes(ymin = yvar - SE, ymax = yvar + SE), width = 0.1, position = position_dodge(0.9)) + 
  geom_text(aes(label = round(yvar, 2), y = UCL), family = "CMU Serif",
            vjust = -1, position = position_dodge(0.9)) + 
  scale_y_continuous(limits = c(0, 7.5), breaks = seq(0, 7, by = 1)) +
  scale_x_discrete(labels = ~ str_wrap(., width = 20)) +
  labs(y = "Mean Viewing Time (Seconds)", fill = "Treatment \nCondition") +
  facet_rep_wrap(~ fct_relevel(par_gender, "Men Participants"), nrow = 2, repeat.tick.labels = TRUE) +
  ggtitle("Figure 3. Study 2") +
  theme_linedraw() +
  theme(
    text = element_text(family = "CMU Serif", size = 14),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + # 横线
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 4.16 + 1.5,
        xend = 1 + x_offset, yend = 4.16 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 4.16 + 1.5,
        xend = 1 - x_offset, yend = 4.16 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 1 + x_offset, y = 4.16 + 1.5,
        xend = 1 + x_offset, yend = 4.16 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 1, y = 4.16 + 1.5 + 0.25, label = "**"),
    color = "black", size = 8
  ) + # 第二个横线
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 2 - x_offset, y = 4.89 + 1.5,
        xend = 2 + x_offset, yend = 4.89 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 2 - x_offset, y = 4.89 + 1.5,
        xend = 2 - x_offset, yend = 4.89 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 2 + x_offset, y = 4.89 + 1.5,
        xend = 2 + x_offset, yend = 4.89 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df2_mean, par_gender == "Men Participants"),
    aes(x = 2, y = 4.89 + 1.5 + 0.25, label = "***"),
    color = "black", size = 8
  ) + # 第三个横线
  geom_segment(
    data = filter(df2_mean, par_gender == "Women Participants"),
    aes(x = 4 - x_offset, y = 4.21 + 1.5,
        xend = 4 + x_offset, yend = 4.21 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df2_mean, par_gender == "Women Participants"),
    aes(x = 4 - x_offset, y = 4.21 + 1.5,
        xend = 4 - x_offset, yend = 4.21 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df2_mean, par_gender == "Women Participants"),
    aes(x = 4 + x_offset, y = 4.21 + 1.5,
        xend = 4 + x_offset, yend = 4.21 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df2_mean, par_gender == "Women Participants"),
    aes(x = 4, y = 4.21 + 1.5 + 0.25, label = "***"),
    color = "black", size = 8
  )


# Figure 4 ####


df3_mean <- df3_mean %>% mutate(
  pic_genderwithcue = paste(pic_cue, pic_gender, sep = " ")
)

fig4 <- ggplot(df3_mean, aes(x = fct_relevel(pic_genderwithcue, c("Sexual Cue-Absent Female Pictures",
                                                          "Sexual Cue-Present Female Pictures",
                                                          "Sexual Cue-Absent Male Pictures")), 
                     y = yvar, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("Satiation" = "#3288BD", "Hunger" = "#A50026")) + 
  geom_errorbar(aes(ymin = yvar - SE, ymax = yvar + SE), width = 0.1, position = position_dodge(0.9)) + 
  geom_text(aes(label = round(yvar, 2), y = UCL), family = "CMU Serif",
            vjust = -1, position = position_dodge(0.9)) + 
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  scale_x_discrete(labels = ~ str_wrap(., width = 20)) +
  labs(y = "Mean Attractiveness Rating", fill = "Treatment \nCondition") +
  facet_rep_wrap(~ fct_relevel(par_gender, "Men Participants"), nrow = 2, repeat.tick.labels = TRUE) +
  ggtitle("Figure 4. Study 3") +
  theme_linedraw() +
  theme(
    text = element_text(family = "CMU Serif", size = 14),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + # 横线
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 4.91 + 1.5, 
        xend = 1 + x_offset, yend = 4.91 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 4.91 + 1.5,
        xend = 1 - x_offset, yend = 4.91 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 1 + x_offset, y = 4.91 + 1.5,
        xend = 1 + x_offset, yend = 4.91 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 1, y = 4.91 + 1.5 + 0.25, label = "**"),
    color = "black", size = 8
  ) + # 第二个横线
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 2 - x_offset, y = 5.09 + 1.5,
        xend = 2 + x_offset, yend = 5.09 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 2 - x_offset, y = 5.09 + 1.5,
        xend = 2 - x_offset, yend = 5.09 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 2 + x_offset, y = 5.09 + 1.5,
        xend = 2 + x_offset, yend = 5.09 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df3_mean, par_gender == "Men Participants"),
    aes(x = 2, y = 5.09 + 1.5 + 0.25, label = "**"),
    color = "black", size = 8
  ) + # 第三个横线
  geom_segment(
    data = filter(df3_mean, par_gender == "Women Participants"),
    aes(x = 4 - x_offset, y = 4.28 + 1.5,
        xend = 4 + x_offset, yend = 4.28 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df3_mean, par_gender == "Women Participants"),
    aes(x = 4 - x_offset, y = 4.28 + 1.5,
        xend = 4 - x_offset, yend = 4.28 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df3_mean, par_gender == "Women Participants"),
    aes(x = 4 + x_offset, y = 4.28 + 1.5,
        xend = 4 + x_offset, yend = 4.28 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df3_mean, par_gender == "Women Participants"),
    aes(x = 4, y = 4.28 + 1.5 + 0.25, label = "**"),
    color = "black", size = 8
  )


# Figure 6 ####


df5_mean <- df5_mean %>% mutate(
  pic_genderwithcue = paste(pic_cue, pic_gender, sep = " ")
)

df5_mean$pic_genderwithcue[df5_mean$pic_genderwithcue == "Non-Human Cue Female Pictures"] <- "Flower Pictures"

df5_mean$pic_genderwithcue[df5_mean$pic_genderwithcue == "Non-Human Cue Male Pictures"] <- "Male Lion Pictures"

fig6 <- ggplot(df5_mean, aes(x = fct_relevel(pic_genderwithcue, c("Sexual Cue-Absent Female Pictures",
                                                          "Sexual Cue-Present Female Pictures",
                                                          "Flower Pictures",
                                                          "Sexual Cue-Absent Male Pictures",
                                                          "Sexual Cue-Present Male Pictures")), 
                     y = yvar, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("Satiation" = "#3288BD", "Hunger" = "#A50026")) + 
  geom_errorbar(aes(ymin = yvar - SE, ymax = yvar + SE), width = 0.1, position = position_dodge(0.9)) + 
  geom_text(aes(label = round(yvar, 2), y = UCL), family = "CMU Serif",
            vjust = -1, position = position_dodge(0.9), size = 3) + 
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  scale_x_discrete(labels = ~ str_wrap(., width = 10)) +
  labs(y = "Mean Attractiveness Rating", fill = "Treatment \nCondition") +
  facet_rep_wrap(~ fct_relevel(par_gender, "Men Participants"), nrow = 2, repeat.tick.labels = TRUE) +
  ggtitle("Figure 6. Study 5") +
  theme_linedraw() +
  theme(
    text = element_text(family = "CMU Serif", size = 14),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + # 第一个横线
  geom_segment(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 4.92 + 1.5,
        xend = 1 + x_offset, yend = 4.92 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 1 - x_offset, y = 4.92 + 1.5,
        xend = 1 - x_offset, yend = 4.92 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 1 + x_offset, y = 4.92 + 1.5,
        xend = 1 + x_offset, yend = 4.92 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 1, y = 4.92 + 1.5 + 0.25, label = "**"),
    color = "black", size = 8
  ) + # 第二个横线
  geom_segment(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 2 - x_offset, y = 5.10 + 1.5,
        xend = 2 + x_offset, yend = 5.10 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 2 - x_offset, y = 5.10 + 1.5,
        xend = 2 - x_offset, yend = 5.10 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 2 + x_offset, y = 5.10 + 1.5,
        xend = 2 + x_offset, yend = 5.10 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 2, y = 5.1 + 1.5 + 0.25, label = "***"),
    color = "black", size = 8
  ) + # 第三个横线
  geom_segment(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 3 - x_offset, y = 6.55 + 1.5,
        xend = 3 + x_offset, yend = 6.55 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 3 - x_offset, y = 6.55 + 1.5,
        xend = 3 - x_offset, yend = 6.55 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 3 + x_offset, y = 6.55 + 1.5,
        xend = 3 + x_offset, yend = 6.55 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df5_mean, par_gender == "Men Participants"),
    aes(x = 3, y = 6.55 + 1.5 + 0.25, label = "**"),
    color = "black", size = 8
  ) + # 第四个横线
  geom_segment(
    data = filter(df5_mean, par_gender == "Women Participants"),
    aes(x = 5 - x_offset, y = 3.96 + 1.5,
        xend = 5 + x_offset, yend = 3.96 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Women Participants"),
    aes(x = 5 - x_offset, y = 3.96 + 1.5,
        xend = 5 - x_offset, yend = 3.96 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Women Participants"),
    aes(x = 5 + x_offset, y = 3.96 + 1.5,
        xend = 5 + x_offset, yend = 3.96 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df5_mean, par_gender == "Women Participants"),
    aes(x = 5, y = 3.96 + 1.5 + 0.25, label = "*"),
    color = "black", size = 8
  ) + # 第五个横线
  geom_segment(
    data = filter(df5_mean, par_gender == "Women Participants"),
    aes(x = 6 - x_offset, y = 6.81 + 1.5,
        xend = 6 + x_offset, yend = 6.81 + 1.5),
    color = "black", size = 0.5
  ) + # 左竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Women Participants"),
    aes(x = 6 - x_offset, y = 6.81 + 1.5,
        xend = 6 - x_offset, yend = 6.81 + 1.25),
    color = "black", size = 0.5
  ) + # 右竖
  geom_segment(
    data = filter(df5_mean, par_gender == "Women Participants"),
    aes(x = 6 + x_offset, y = 6.81 + 1.5,
        xend = 6 + x_offset, yend = 6.81 + 1.25),
    color = "black", size = 0.5
  ) + # ***
  geom_text(
    data = filter(df5_mean, par_gender == "Women Participants"),
    aes(x = 6, y = 6.81 + 1.5 + 0.25, label = "**"),
    color = "black", size = 8
  )


# Output ####


ggsave(filename = "Figures/Fig1.png",
       width = 7, height = 9,
       dpi = 600,
       plot = fig1)

ggsave(filename = "Figures/Fig2.png",
       width = 7, height = 9,
       dpi = 600,
       plot = fig2)

ggsave(filename = "Figures/Fig3.png",
       width = 9, height = 9,
       dpi = 600,
       plot = fig3)

ggsave(filename = "Figures/Fig4.png",
       width = 9, height = 9,
       dpi = 600,
       plot = fig4)

ggsave(filename = "Figures/Fig6.png",
       width = 11, height = 9,
       dpi = 600,
       plot = fig6)
