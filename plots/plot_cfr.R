
# plotting CFR of 1918 flu vs. regular flu --------------------------------
# based on figure 3, panel C in Taubenberger & Morens (2006)
# data digitized from screenshot with https://apps.automeris.io/wpd/

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# import data -------------------------------------------------------------

df1918 <- read.csv("CFR1918.csv", header = F, sep = ";", dec = ",", col.names = c("age", "cfr1918"))
df1928 <- read.csv("CFR1928.csv", header = F, sep = ";", dec = ",", col.names = c("age", "cfr1928"))
rki <- read.csv("rki-ind.csv") # German covid-19 data from RKI database
    
# correct negative value to 0              
df1928$cfr1928 <- ifelse(df1928$cfr1928 < 0, 0, df1928$cfr1928)


# data wrangling rki data -------------------------------------------------
rki %>% 
  mutate(time = as.Date(Meldedatum),
         Datenstand = as.Date(Datenstand, format = "%d.%m.%y")) %>% 
  group_by(time, NeuerFall, Datenstand, Altersgruppe) %>% 
  summarize(new_confirmed = sum(AnzahlFall),
            new_deaths = sum(AnzahlTodesfall)) %>%
  mutate(confirmed = cumsum(new_confirmed),
         deaths = cumsum(new_deaths)) -> rki_new_sum

rki_new_sum %>% 
  ungroup() %>% 
  group_by(Altersgruppe) %>% 
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths)) %>% 
  separate(Altersgruppe, c("age_from", "age_to"), "-A") %>% 
  filter(age_from != "unbekannt") -> df2020

df2020$age_from <- df2020$age_from %>% substr(2, 3) %>% as.numeric()
df2020$age_to <- df2020$age_to %>% as.numeric()
df2020$age_to[6] <- 100

df2020 %>% 
  mutate(age_mean = (age_from + age_to)/2) %>% 
  mutate(cfr2020 = deaths/confirmed) %>% 
  rename(age = age_mean) %>% 
  select(age, cfr2020) -> df2020_select


# merge dataframes --------------------------------------------------------

df <- full_join(df1918, df1928) %>% mutate(cfr1918 = cfr1918/100, cfr1928 = cfr1928/100) 
df <- full_join(df, df2020_select)

# plotting ----------------------------------------------------------------

df %>% 
  pivot_longer(cols = cfr1918:cfr2020, names_to = "year", names_prefix = "cfr", values_to = "cfr") %>% 
  drop_na() %>%
  ggplot(aes(age, cfr, color = year, linetype = year, shape = year)) +
  geom_point(size = 2) +
  geom_line() +
  labs(x = "Age", y = "Case Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  scale_color_manual(name = "",
                     labels = c("1918 Flu Pandemic (1918–1919)", "Regular Flu Season (1928–1929)", "Covid-19 (2020)"),
                     values = c("black", "#CA0020", "blue")) +
  scale_linetype_manual(labels = c("1918 Flu Pandemic (1918–1919)", "Regular Flu Season (1928–1929)", "Covid-19 (2020)"),
                        values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(labels = c("1918 Flu Pandemic (1918–1919)", "Regular Flu Season (1928–1929)", "Covid-19 (2020)"),
                     values = c(16, 17, 15)) +
  theme_classic() +
  theme(legend.position = c(0.35, 0.85),
        legend.key.width = unit(3, "line"),
        legend.background = element_rect(fill=scales::alpha(0.1))) +
  guides(linetype = guide_legend(""),
         color = guide_legend(""),
         shape = guide_legend("")) -> p1


# save plot ---------------------------------------------------------------

ggsave("cfr.pdf",
       p1,
       width = 6,
       height = 3,
       units = "in",
       device = "pdf")

