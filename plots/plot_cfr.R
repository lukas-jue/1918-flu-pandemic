
# plotting CFR of 1918 flu vs. regular flu --------------------------------
# based on figure 3, panel C in Taubenberger & Morens (2006)
# data digitized from screenshot with https://apps.automeris.io/wpd/

library(dplyr)
library(ggplot2)
library(tidyr)

# import data -------------------------------------------------------------

df1918 <- read.csv("CFR1918.csv", header = F, sep = ";", dec = ",", col.names = c("age", "cfr1918"))
df1928 <- read.csv("CFR1928.csv", header = F, sep = ";", dec = ",", col.names = c("age", "cfr1928"))
    
# correct negative value to 0              
df1928$cfr1928 <- ifelse(df1928$cfr1928 < 0, 0, df1928$cfr1928)


# merge dataframes --------------------------------------------------------

df <- full_join(df1918, df1928)


# plotting ----------------------------------------------------------------

df %>% 
  pivot_longer(cols = cfr1918:cfr1928, names_to = "year", names_prefix = "cfr", values_to = "cfr") %>% 
  drop_na() %>% 
  mutate(cfr = cfr/100) %>% 
  ggplot(aes(age, cfr, color = year, linetype = year, shape = year)) +
  geom_point(size = 2) +
  geom_line() +
  labs(x = "Age", y = "Case Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  scale_color_manual(name = "",
                     labels = c("1918 Flu Pandemic (1918–1919)", "Regular Flu Season (1928–1929)"),
                     values = c("black", "#CA0020")) +
  scale_linetype_manual(labels = c("1918 Flu Pandemic (1918–1919)", "Regular Flu Season (1928–1929)"),
                        values = c("solid", "dashed")) +
  scale_shape_manual(labels = c("1918 Flu Pandemic (1918–1919)", "Regular Flu Season (1928–1929)"),
                     values = c(16, 17)) +
  theme_classic() +
  theme(legend.position = c(0.5, 0.85),
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

