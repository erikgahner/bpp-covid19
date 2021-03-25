library("tidyverse")
library("effectsize")
library("stargazer")
library("interplot")
library("conflicted")
library("ggthemes")
library("gridExtra")

# Set theme
theme_set(theme_bw())

gallup <- read_csv("bpp_covid19.csv")

conflict_prefer("select", "dplyr")

summary(gallup$outcome_handwash[gallup$treat_handwash == 1])
summary(gallup$outcome_handwash[gallup$treat_handwash == 0])
summary(gallup$outcome_closecontact[gallup$treat_closecontact == 1])
summary(gallup$outcome_closecontact[gallup$treat_closecontact == 0])

summary(gallup$outcome_handwash_10[gallup$treat_handwash == 1])
summary(gallup$outcome_handwash_10[gallup$treat_handwash == 0])
summary(gallup$outcome_closecontact_10[gallup$treat_closecontact == 1])
summary(gallup$outcome_closecontact_10[gallup$treat_closecontact == 0])

cohens_d(outcome_handwash ~ treat_handwash, data = gallup)
cohens_d(outcome_closecontact ~ treat_closecontact, data = gallup)

summary(lm(male ~ treat_handwash, data = gallup))
summary(lm(age ~ treat_handwash, data = gallup))
summary(lm(gov ~ treat_handwash, data = gallup))

summary(lm(male ~ treat_closecontact, data = gallup))
summary(lm(age ~ treat_closecontact, data = gallup))
summary(lm(gov ~ treat_closecontact, data = gallup))


gallup %>% 
  select(outcome_handwash, outcome_closecontact, treat_handwash, 
         treat_closecontact, male, age, gov) %>% 
  data.frame() %>% 
  stargazer(
      covariate.labels = c(
        "Outcome: Handwash",
        "Outcome: Close contact",
        "Treatment: Handwash",
        "Treatment: Close contact",
        "Male",
        "Age",
        "Government supporter"),
    type = "text",
    out = "tab-descriptivestatistics.htm")

reg_1 <- lm(outcome_handwash ~ treat_handwash, data = gallup)
reg_2 <- lm(outcome_closecontact ~ treat_closecontact, data = gallup)

stargazer(reg_1, reg_2, 
          type = "text",
          digits = 2,
          covariate.labels = c("Treatment: Handwash",
                               "Treatment: Close contact"),
          out = "tab-regression_ate.htm")

reg_3 <- lm(outcome_handwash ~ treat_handwash*male, data = gallup)
reg_4 <- lm(outcome_closecontact ~ treat_closecontact*male, data = gallup)
stargazer(reg_3, reg_4, 
          type = "text",
          digits = 2,
          single.row = TRUE,
          covariate.labels = c("Treatment: Handwash",
                               "Treatment: Close contact",
                               "Male",
                               "Male * Handwash treatment",
                               "Male * Close contact treatment"),
          out = "tab-regression_gender.htm")

haandvask_df <- gallup %>% 
  group_by(treat_handwash) %>% 
  summarise(est = mean(outcome_handwash),
            se = sd(outcome_handwash) / sqrt(n())) %>% 
  rename(treatment = treat_handwash) %>% 
  mutate(treatment = ifelse(treatment == 1, "High (30)", "Low (3)")) %>% 
  mutate(outcome = "Washed hands") 

closecontact_df <- gallup %>% 
  group_by(treat_closecontact) %>% 
  summarise(est = mean(outcome_closecontact),
            se = sd(outcome_closecontact) / sqrt(n())) %>% 
  rename(treatment = treat_closecontact) %>%
  mutate(treatment = ifelse(treatment == 1, "High (15)", "Low (3)")) %>% 
  mutate(outcome = "Close contact")

effect_df <- bind_rows(haandvask_df, closecontact_df)

effect_df %>% 
  ggplot(aes(x = treatment, y = est, ymin = est - se * 1.96, ymax = est + se * 1.96)) +
  geom_point(size = 3)  +
  geom_point(size = 2, shape=21, colour = "white", stroke = .5) +
  geom_errorbar(width = 0) +
  labs(y = "Average treatment effect\n (w/ 95% confidence intervals)",
       x = "Anchor") +
  facet_wrap(~ outcome, scale = "free")

ggsave("fig1.png", width = 7, height = 3)
ggsave("fig1.tiff", width = 7, height = 3)


df_male_handwash <- gallup %>% 
  group_by(treat_handwash, male) %>% 
  summarise(est = mean(outcome_handwash),
            se = sd(outcome_handwash) / sqrt(n())) %>% 
  ungroup() %>% 
  rename(treatment = treat_handwash) %>% 
  mutate(treatment = ifelse(treatment == 1, "High (30)", "Low (3)")) %>% 
  mutate(outcome = "Washed hands",
         male = ifelse(male == 1, "Male", "Female"))

df_male_closecontact <- gallup %>% 
  group_by(treat_closecontact, male) %>% 
  summarise(est = mean(outcome_closecontact),
            se = sd(outcome_closecontact) / sqrt(n())) %>% 
  ungroup() %>% 
  rename(treatment = treat_closecontact) %>% 
  mutate(treatment = ifelse(treatment == 1, "High (15)", "Low (3)")) %>% 
  mutate(outcome = "Close contact",
         male = ifelse(male == 1, "Male", "Female"))

df_male <- bind_rows(df_male_handwash, df_male_closecontact)

df_male %>% 
  ggplot(aes(x = treatment, y = est, ymin = est - se * 1.96, ymax = est + se * 1.96)) +
  geom_point(size = 3)  +
  geom_point(size = 2, shape=21, colour = "white", stroke = .5) +
  geom_errorbar(width = 0) +
  labs(y = "Average treatment effect\n (w/ 95% confidence intervals)",
       x = "Anchor") +
  facet_wrap( outcome ~ male, scale = "free")

ggsave("fig2.png", width = 7, height = 6)
ggsave("fig2.tiff", width = 7, height = 6)


reg_interaction_handwash <- lm(outcome_handwash ~ treat_handwash*age, data = gallup)
reg_interaction_closecontact <- lm(outcome_closecontact ~ treat_closecontact*age, data = gallup)


reg_interaction_handwash_marg <- interplot(m = reg_interaction_handwash, var1 = "treat_handwash", var2 = "age", plot=FALSE) %>% 
  mutate(outcome = "... washing hands")
reg_interaction_closecontact_marg <- interplot(m = reg_interaction_closecontact, var1 = "treat_closecontact", var2 = "age", plot=FALSE) %>% 
  mutate(outcome = "... close contact")

marg_data <- bind_rows(reg_interaction_handwash_marg, reg_interaction_closecontact_marg)

ggplot(marg_data, aes(x = age)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2, linetype = 2) +
  facet_wrap(~ outcome, ncol = 5) +
  theme(legend.position = "none") +
  labs(y = "Marginal effect of anchor on ...",
       x = "Age in years")

ggsave("fig3.png", width = 7, height = 4)
ggsave("fig3.tiff", width = 7, height = 4)

stargazer(reg_interaction_handwash, reg_interaction_closecontact,
          type = "text",
          digits = 2,
          single.row = TRUE,
          covariate.labels = c("Treatment: Handwash",
                               "Treatment: Close contact",
                               "Age",
                               "Age * Handwash treatment",
                               "Age * Close contact treatment"),
          out = "tab-regression_age.htm")


fig_parti_1 <- gallup %>%
  group_by(parti, treat_handwash) %>% 
  summarise(outcome_handwash = mean(outcome_handwash)) %>% 
  drop_na(parti) %>% 
  mutate(treat_handwash = ifelse(treat_handwash == 0, "Low (3)", "High (30)")) %>% 
  mutate(parti = as.factor(parti)) %>% 
  mutate(parti = fct_relevel(parti, levels = c("Nye Borgerlige", "Dansk Folkeparti", "Liberal Alliance", "Konservative", "Venstre", "Socialdemokratiet", "Radikale", "SF", "Alternativet", "Enhedslisten"))) %>% 
  ggplot(aes(parti, outcome_handwash, group = treat_handwash, fill = treat_handwash)) + 
  geom_col(position = "dodge2") + 
  scale_fill_fivethirtyeight() +
  labs(x = NULL,
       y = "Washed hands",
       fill = "Anchor") +
  theme(legend.position = "bottom") +
  coord_flip() 

fig_parti_2 <- gallup %>%
  group_by(parti, treat_closecontact) %>% 
  summarise(outcome_closecontact = mean(outcome_closecontact)) %>% 
  drop_na(parti) %>% 
  mutate(treat_closecontact = ifelse(treat_closecontact == 0, "Low (3)", "High (15)")) %>% 
  mutate(parti = as.factor(parti)) %>% 
  mutate(parti = fct_relevel(parti, levels = c("Nye Borgerlige", "Dansk Folkeparti", "Liberal Alliance", "Konservative", "Venstre", "Socialdemokratiet", "Radikale", "SF", "Alternativet", "Enhedslisten"))) %>% 
  ggplot(aes(parti, outcome_closecontact, group = treat_closecontact, fill = treat_closecontact)) + 
  geom_col(position = "dodge2") + 
  scale_fill_fivethirtyeight() +
  labs(x = NULL,
       y = "Close contact",
       fill = "Anchor") +
  theme(legend.position = "bottom") +
  coord_flip()

pdf("fig-parti.pdf", width = 8, height = 4)
grid.arrange(fig_parti_1, fig_parti_2, ncol = 2)
dev.off()

df_gov_handwash <- gallup %>% 
  group_by(treat_handwash, gov) %>% 
  summarise(est = mean(outcome_handwash),
            se = sd(outcome_handwash) / sqrt(n())) %>% 
  ungroup() %>% 
  rename(treatment = treat_handwash) %>% 
  mutate(treatment = ifelse(treatment == 1, "High (30)", "Low (3)")) %>% 
  mutate(outcome = "Washed hands",
         gov = ifelse(gov == 1, "Government supporter", "Opposition supporter"))

df_gov_closecontact <- gallup %>% 
  group_by(treat_closecontact, gov) %>% 
  summarise(est = mean(outcome_closecontact),
            se = sd(outcome_closecontact) / sqrt(n())) %>% 
  ungroup() %>% 
  rename(treatment = treat_closecontact) %>% 
  mutate(treatment = ifelse(treatment == 1, "High (15)", "Low (3)")) %>% 
  mutate(outcome = "Close contact",
         gov = ifelse(gov == 1, "Government supporter", "Opposition supporter"))

df_gov <- bind_rows(df_gov_handwash, df_gov_closecontact)

df_gov %>% 
  drop_na(gov) %>% 
  ggplot(aes(x = treatment, y = est, ymin = est - se * 1.96, ymax = est + se * 1.96)) +
  geom_point(size = 3)  +
  geom_point(size = 2, shape=21, colour = "white", stroke = .5) +
  geom_errorbar(width = 0) +
  labs(y = "Average treatment effect\n (w/ 95% confidence intervals)",
       x = "Anchor") +
  facet_wrap( outcome ~ gov, scale = "free")

ggsave("fig4.png", width = 7, height = 6)
ggsave("fig4.tiff", width = 7, height = 6)

reg_5 <- lm(outcome_handwash ~ treat_handwash*gov, data = gallup)
reg_6 <- lm(outcome_closecontact ~ treat_closecontact*gov, data = gallup)
stargazer(reg_5, reg_6, 
          type = "text",
          digits = 2,
          single.row = TRUE,
          covariate.labels = c("Treatment: Handwash",
                               "Treatment: Close contact",
                               "Government supporter",
                               "Government supporter * Handwash treatment",
                               "Government supporter * Close contact treatment"),
          out = "tab-regression_gov.htm")
