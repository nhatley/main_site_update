library(tidyverse)
mlb_db_dir = "~/Dropbox/mlb_db/"
wrc <- read_rds(paste0(mlb_db_dir, "output/wrc_plus.rds"))
source(paste0(mlb_db_dir, "code/functions/plot_theme.R"))
color_manual = c(
  "league" = "gray54",
  "team" = "#CE1141"
)


## ---- fig.height=5.5, fig.width=12.5-------------------------------------
wrc %>% 
  mutate(team2 = ifelse(team == "league", "league", "team")) %>% 
  mutate(team_flag = str_extract(team_flag, "^.*\\ +") %>% str_remove("\\+") %>% str_trim) %>% 
  group_by(team_flag) %>%
  fill(mean_wrc_plus) %>%
  ungroup %>%
  mutate(team_flag = paste0(team_flag, "\n", "Team wrc+ = ", round(mean_wrc_plus, 0))) %>% 
  mutate(team_flag = fct_reorder(team_flag, -mean_wrc_plus)) %>% 
  filter(team == "league") %>% 
  select(-team_flag) %>% 
  ggplot(
    aes(
      x = wrc_plus,
      fill = team2
    )
  ) +
    geom_vline(xintercept = 100, linetype = 2) + 

  geom_density(#data = . %>% filter(team != "full"),
    alpha = 0.25
  ) +
  scale_fill_manual(
    values = color_manual
  ) + 
  plot_theme() +
  facet_wrap(
    vars(team),
    scales = "free_x"
  ) + 
  scale_x_continuous(limits = c(0, 200),
                     breaks = seq(0, 200, by = 50),
                     labels = seq(0, 200, by = 50)
  ) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()
  ) +
  labs(caption = "Players with > 100 PA's. League avg for these players is 97",
       x = "wrc+")


## ---- fig.height=5.5, fig.width=12.5-------------------------------------
wrc %>% 
  mutate(team2 = ifelse(team == "league", "league", "team")) %>% 
  mutate(team_flag = str_extract(team_flag, "^.*\\ +") %>% str_remove("\\+") %>% str_trim) %>% 
  group_by(team_flag) %>%
  fill(mean_wrc_plus) %>%
  ungroup %>%
  mutate(team_flag = paste0(team_flag, "\n", "Team wrc+ = ", round(mean_wrc_plus, 0))) %>% 
  mutate(team_flag = fct_reorder(team_flag, -mean_wrc_plus)) %>% 
  filter(str_detect(team_flag, "astros") | str_detect(team_flag, "yankees")) %>% 
  ggplot(
    aes(
      x = wrc_plus,
      fill = team2
    )
  ) +
    geom_vline(xintercept = 100, linetype = 2) + 

  geom_density(#data = . %>% filter(team != "full"),
    alpha = 0.25
  ) +
  scale_fill_manual(
    values = color_manual
  ) + 
  plot_theme() +
  facet_wrap(
    vars(team_flag),
    scales = "free_x"
  ) + 
  scale_x_continuous(limits = c(0, 200),
                     breaks = seq(0, 200, by = 50),
                     labels = seq(0, 200, by = 50)
  ) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()
  ) +
  labs(caption = "Players with > 100 PA's. League avg for these players is 97",
       x = "wrc+")



wrc %>% 
  mutate(team2 = ifelse(team == "league", "league", "team")) %>% 
  mutate(team_flag = str_extract(team_flag, "^.*\\ +") %>% str_remove("\\+") %>% str_trim) %>% 
  group_by(team_flag) %>%
  fill(mean_wrc_plus) %>%
  ungroup %>%
  mutate(team_flag = paste0(team_flag, "\n", "Team wrc+ = ", round(mean_wrc_plus, 0))) %>% 
  mutate(team_flag = fct_reorder(team_flag, -mean_wrc_plus)) %>% 
  filter(str_detect(team_flag, "braves") | 
           str_detect(team_flag, "nationals") |
              str_detect(team_flag, "diamondbacks")) %>%
  ggplot(
    aes(
      x = wrc_plus,
      fill = team2
    )
  ) +
    geom_vline(xintercept = 100, linetype = 2) + 

  geom_density(#data = . %>% filter(team != "full"),
    alpha = 0.25
  ) +
  scale_fill_manual(
    values = color_manual
  ) + 
  plot_theme() +
  facet_wrap(
      vars(team_flag),
    scales = "free_x"
  ) + 
  scale_x_continuous(limits = c(0, 200),
                     breaks = seq(0, 200, by = 50),
                     labels = seq(0, 200, by = 50)
  ) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()
  ) +
  labs(caption = "Players with > 100 PA's. League avg for these players is 97",
       x = "wrc+")


## ---- fig.height=12.5, fig.width=12.5------------------------------------
wrc %>% 
  mutate(team2 = ifelse(team == "league", "league", "team")) %>% 
  mutate(team_flag = str_extract(team_flag, "^.*\\ +") %>% str_remove("\\+") %>% str_trim) %>% 
  group_by(team_flag) %>%
  fill(mean_wrc_plus) %>%
  ungroup %>%
  mutate(team_flag = paste0(team_flag, "\n", "Team wrc+ = ", round(mean_wrc_plus, 0))) %>% 
  mutate(team_flag = fct_reorder(team_flag, -mean_wrc_plus)) %>% 
  ggplot(
    aes(
      x = wrc_plus,
      fill = team2
    )
  ) +
  geom_vline(xintercept = 100, linetype = 2) + 
  geom_density(#data = . %>% filter(team != "full"),
    alpha = 0.25
  ) +
  scale_fill_manual(
    values = color_manual
  ) + 
  plot_theme() +
  facet_wrap(
    vars(team_flag)
    ) + 
  scale_x_continuous(limits = c(0, 200),
                     breaks = seq(0, 200, by = 50),
                     labels = seq(0, 200, by = 50)
                     ) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()
        ) +
  ggtitle("Weighted Runs Created Plus (wrc+) distributions by team",
          subtitle = "Grey density plots represent league as a whole"
          ) +
  labs(caption = "Players with > 100 PA's. League avg for these players is 97",
       x = "wrc+")

