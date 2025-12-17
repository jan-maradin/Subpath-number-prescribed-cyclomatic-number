library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(knitr)
library(kableExtra)



osnovno <- read.csv("1del_SA_n_9.csv") %>%
  rename(
    `min p_n(G)_SA`= `min.p_n.G.`,
    `max p_n(G)_SA`=`max.p_n.G.`,
    `graph_min_SA` = `graph6_min`,
    `graph_max_SA` = `graph6_max`) %>%
  .[-(1:62), ]

random <- read.csv("1del_SA_n9_random.csv") %>%
rename(
  `min p_n(G)_random`= `min.p_n.G.`,
  `max p_n(G)_random`=`max.p_n.G.`,
  `graph_min_random` = `graph6_min`,
  `graph_max_random` = `graph6_max`)

priblizki8 <- read.csv("1del_SA_n9_priblizki8.csv") %>%
  rename(
    `min p_n(G)_priblizki8`= `min.p_n.G.`,
    `max p_n(G)_priblizki8`=`max.p_n.G.`,
    `graph_min_priblizki8` = `graph6_min`,
    `graph_max_priblizki8` = `graph6_max`) %>%
    .[-(1:22), ]

tocne_vrednosti <- read.csv("1del_mali.csv") %>%
rename(
  `min p_n(G)_pravilno`= `min_pn`,
  `max p_n(G)_pravilno`=`max_pn`,
  `graph_min_pravilno` = `min_g6`,
  `graph_max_pravilno` = `max_g6`) %>%
  .[-(1:62), ]


tabela_vseh_podatkov <- cbind(tocne_vrednosti, osnovno, random, priblizki8) %>%
  select(c(1,2,3,5,9,10,15,16,21,22))





##########################################
#najprej za minimum
graf_1 <- tabela_vseh_podatkov %>%
  select(1, 2, starts_with("min")) %>%
  pivot_longer(-c(1,2), names_to = "parametri", values_to = "vrednosti") %>%
  mutate(
    parametri = factor(parametri)
  )

# pravilne vrednosti (po vrsticah!)
pravilna_1 <- tabela_vseh_podatkov %>%
  pull(`min p_n(G)_pravilno`)

tabela_1 <- tabela_vseh_podatkov %>%
  select(n, mu, `min p_n(G)_pravilno`, starts_with("min p_n(G)_")) %>%
  pivot_longer(
    cols = -c(n, mu, `min p_n(G)_pravilno`),
    names_to = "parametri",
    values_to = "vrednosti"
  ) %>%
  mutate(
    parametri = factor(parametri),
    odmik = vrednosti - `min p_n(G)_pravilno`,
    relativni_odmik = odmik / `min p_n(G)_pravilno`
  )

# povzetek odmikov po metodi/parametru
povzetek_min <- tabela_1 %>%
  group_by(parametri) %>%
  summarise(
    vsota_odmikov   = sum(odmik, na.rm = TRUE),
    vsota_relativnih_odmikov = sum(relativni_odmik, na.rm = TRUE),
    stevilo_odmikov = sum(odmik != 0, na.rm = TRUE),
    .groups = "drop"
  )


#da bom lahko to tabelo uvozila v R
kable(
  povzetek_min,
  format = "latex",
  booktabs = TRUE,
  caption = "Povzetek odmikov za minimalno število podpoti"
) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  save_kable("povzetek_min_n9.tex")



# 1. razširimo v široko tabelo: vsak parameter svoj stolpec
wide <- graf_1 %>%
  select(-n) %>%
  pivot_wider(
    names_from = parametri,
    values_from = vrednosti
  )

# 2. izračunamo odmik od pravilne vrednosti
wide_diff <- wide %>%
  mutate(
    across(
      -`mu`,
      ~ .x - `min p_n(G)_pravilno`
    )
  )

# 3. vrnemo nazaj v dolg format, brez stolpca "pravilno" (ta ima odmik 0)
diff_long <- wide_diff %>%
  select(-`min p_n(G)_pravilno`) %>%
  pivot_longer(
    cols = -`mu`,
    names_to = "parametri",
    values_to = "odmik"
  )

# 4. graf: odmik glede na µ(G)
dodge <- position_dodge(width = 0.7)

ggplot(tabela_1, aes(x = `mu`, y = odmik, color = parametri, group = parametri)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1, position = dodge) +
  geom_point(size = 1, position = dodge) +
  geom_label(
    aes(label = odmik),
    size = 3.5,
    fill = "white",
    label.size = 0,
    label.padding = unit(0.15, "lines")
  ) +
  theme_minimal()

# GRAF KI POKAŽE KOLIKŠNI SO BILI RELATIVNI ODMIKI OD PRAVILNE VREDNOSTI
# 1. široka tabela (vsak parameter svoj stolpec)
wide <- graf_1 %>%
  select(-n) %>%
  pivot_wider(
    names_from = parametri,
    values_from = vrednosti
  )

# 2. RELATIVNI odmik od pravilne vrednosti
#    (če želiš ABSOLUTNI relativni odmik, zavij v abs(...))

wide_rel <- wide %>%
  mutate(
    across(
      -`mu`,
      ~ (.x - `min p_n(G)_pravilno`) / `min p_n(G)_pravilno`
      # za absolutni odmik:
      # ~ abs(.x - `min p_n(G)_pravilno`) / `min p_n(G)_pravilno`
    )
  )

# 3. nazaj v dolg format, brez stolpca "pravilno"
rel_long <- wide_rel %>%
  select(-`min p_n(G)_pravilno`) %>%
  pivot_longer(
    cols = -`mu`,
    names_to = "parametri",
    values_to = "relativni_odmik"
  )

# 4. graf relativnih odmikov
ggplot(rel_long, aes(x = `mu`, y = relativni_odmik, color = parametri, group = parametri)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1, position = position_dodge(width = 0.2)) +
  geom_point(size = 2, position = position_dodge(width = 0.2)) +
  labs(
    x = "mu",
    y = "relativni odmik",
    color = "parametri"
  ) +
  theme_minimal()






#################################
#še za max
graf_2 <- tabela_vseh_podatkov %>%
  select(1, 2, starts_with("max")) %>%
  pivot_longer(-c(1,2), names_to = "parametri", values_to = "vrednosti") %>%
  mutate(parametri = factor(parametri))

# pravilne vrednosti (po vrsticah!)
pravilna_2 <- tabela_vseh_podatkov %>%
  pull(`max p_n(G)_pravilno`)

tabela_2 <- tabela_vseh_podatkov %>%
  select(n, mu, `max p_n(G)_pravilno`, starts_with("max p_n(G)_")) %>%
  pivot_longer(
    cols = -c(n, mu, `max p_n(G)_pravilno`),
    names_to = "parametri",
    values_to = "vrednosti"
  ) %>%
  mutate(
    parametri = factor(parametri),
    odmik = vrednosti - `max p_n(G)_pravilno`,
    relativni_odmik = odmik / `max p_n(G)_pravilno`
  )

# povzetek odmikov po metodi/parametru
povzetek_max <- tabela_2 %>%
  group_by(parametri) %>%
  summarise(
    vsota_odmikov            = sum(odmik, na.rm = TRUE),
    vsota_relativnih_odmikov = sum(relativni_odmik, na.rm = TRUE),
    stevilo_odmikov          = sum(odmik != 0, na.rm = TRUE),
    .groups = "drop"
  )

kable(
  povzetek_max,
  format = "latex",
  booktabs = TRUE,
  caption = "Povzetek odmikov za maksimalno število podpoti"
) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  save_kable("povzetek_max_n9.tex")




# --- GRAF: absolutni odmiki glede na mu ---

wide_max <- graf_2 %>%
  select(-n) %>%
  pivot_wider(
    names_from = parametri,
    values_from = vrednosti
  )

wide_diff_max <- wide_max %>%
  mutate(
    across(
      -mu,
      ~ .x - `max p_n(G)_pravilno`
    )
  )

diff_long_max <- wide_diff_max %>%
  select(-`max p_n(G)_pravilno`) %>%
  pivot_longer(
    cols = -mu,
    names_to = "parametri",
    values_to = "odmik"
  )

dodge <- position_dodge(width = 0.7)

ggplot(diff_long_max, aes(x = mu, y = odmik, color = parametri, group = parametri)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1, position = dodge) +
  geom_point(size = 1, position = dodge) +
  geom_label(
    aes(label = odmik),
    size = 3.5,
    fill = "white",
    label.size = 0,
    label.padding = unit(0.15, "lines")
  ) +
  theme_minimal()

# --- GRAF: relativni odmiki glede na mu ---

wide_rel_max <- wide_max %>%
  mutate(
    across(
      -mu,
      ~ (.x - `max p_n(G)_pravilno`) / `max p_n(G)_pravilno`
      # če želiš absolutni relativni odmik:
      # ~ abs(.x - `max p_n(G)_pravilno`) / `max p_n(G)_pravilno`
    )
  )

rel_long_max <- wide_rel_max %>%
  select(-`max p_n(G)_pravilno`) %>%
  pivot_longer(
    cols = -mu,
    names_to = "parametri",
    values_to = "relativni_odmik"
  )

ggplot(rel_long_max, aes(x = mu, y = relativni_odmik, color = parametri, group = parametri)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1, position = position_dodge(width = 0.2)) +
  geom_point(size = 2, position = position_dodge(width = 0.2)) +
  labs(
    x = "mu",
    y = "relativni odmik",
    color = "parametri"
  ) +
  theme_minimal()

