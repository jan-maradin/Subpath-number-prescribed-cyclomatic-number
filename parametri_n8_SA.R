
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(knitr)
library(kableExtra)

podatki <- read_excel("parametri_n8_SA_1.xlsx")
random <- read.csv("1del_SA_random_n8.csv") %>%
  rename(
     `min p_n(G)_random_1200_6`= `min.p_n.G.`,
     `max p_n(G)_random_1200_6`=`max.p_n.G.`,
     `graph_min_random_1200_6` = `graph6_min`,
     `graph_max_random_1200_6` = `graph6_max`) %>%
  .[ , -c(1,2)]

graf_podatki <- podatki %>%
  .[c(1:62), c(1,2,3,4,7,8,11,12, 15, 16, 19, 20, 23, 25)] %>%
  filter(n == "8")

graf_podatki <- cbind(graf_podatki, random)



#NAJPREJ VSI GRAFI IN VSE TABELE ZA MIN
graf_1 <- graf_podatki %>%
  select(1, 2, starts_with("min")) %>%
  pivot_longer(-c(1,2), names_to = "parametri", values_to = "vrednosti") %>%
  mutate(
    parametri = factor(parametri)
  )


pravilna_1 <- graf_podatki %>%
  pull(`min p_n(G)_pravilno`)

tabela_1 <- graf_podatki %>%
  select(1, 2, starts_with("min")) %>%
  filter(n == 8) %>%
  pivot_longer(
    cols = -c(1, 2),
    names_to = "parametri",
    values_to = "vrednosti"
  ) %>%
  group_by(parametri) %>%
  mutate(
    parametri = factor(parametri),
    odmik = vrednosti - pravilna_1   
  )

#tabela ki zapiše koliko so skupni odmiki za vsake parametre in kolikorat so se pojavili
povzetek_min <- tabela_1 %>%
  group_by(parametri) %>%
  summarise(
    vsota_odmikov  = sum(odmik),
    stevilo_odmikov = sum(odmik != 0),
    .groups = "drop"
  )

kable(
  povzetek_min,
  format = "latex",
  booktabs = TRUE,
  caption = "Povzetek odmikov za minimalno število podpoti"
) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  save_kable("povzetek_min_n8.tex")



# GRAF KI POKAŽE KOLIKŠNI SO BILI ODMIKI OD PRAVILNE VREDNOSTI
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
      -`µ(G)`,
      ~ .x - `min p_n(G)_pravilno`
    )
  )

# 3. vrnemo nazaj v dolg format, brez stolpca "pravilno" (ta ima odmik 0)
diff_long <- wide_diff %>%
  select(-`min p_n(G)_pravilno`) %>%
  pivot_longer(
    cols = -`µ(G)`,
    names_to = "parametri",
    values_to = "odmik"
  )

# 4. graf: odmik glede na µ(G)
dodge <- position_dodge(width = 0.7)

ggplot(tabela_1, aes(x = `µ(G)`, y = odmik, color = parametri, group = parametri)) +
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
      -`µ(G)`,
      ~ (.x - `min p_n(G)_pravilno`) / `min p_n(G)_pravilno`
      # za absolutni odmik:
      # ~ abs(.x - `min p_n(G)_pravilno`) / `min p_n(G)_pravilno`
    )
  )

# 3. nazaj v dolg format, brez stolpca "pravilno"
rel_long <- wide_rel %>%
  select(-`min p_n(G)_pravilno`) %>%
  pivot_longer(
    cols = -`µ(G)`,
    names_to = "parametri",
    values_to = "rel_odmik"
  )

# 4. graf relativnih odmikov
ggplot(rel_long, aes(x = `µ(G)`, y = rel_odmik, color = parametri, group = parametri)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1, position = position_dodge(width = 0.2)) +
  geom_point(size = 2, position = position_dodge(width = 0.2)) +
  labs(
    x = "µ(G)",
    y = "relativni odmik ((SA - pravilno) / pravilno)",
    color = "parametri"
  ) +
  theme_minimal()





###############################################################
#VSE TO ŠE ZA MAX
graf_2 <- graf_podatki %>%
  select(1, 2, starts_with("max")) %>%
  pivot_longer(-c(1,2), names_to = "parametri", values_to = "vrednosti") %>%
  mutate(
    parametri = factor(parametri)
  )


pravilna_2 <- graf_podatki %>%
  pull(`max p_n(G)_pravilno`)

tabela_2 <- graf_podatki %>%
  select(1, 2, starts_with("max")) %>%
  filter(n == 8) %>%
  pivot_longer(
    cols = -c(1, 2),
    names_to = "parametri",
    values_to = "vrednosti"
  ) %>%
  group_by(parametri) %>%
  mutate(
    parametri = factor(parametri),
    odmik = vrednosti - pravilna_2   
  )

#tabela ki zapiše koliko so skupni odmiki za vsake parametre in kolikorat so se pojavili
povzetek_max <- tabela_2 %>%
  group_by(parametri) %>%
  summarise(
    vsota_odmikov  = sum(odmik),
    stevilo_odmikov = sum(odmik != 0),
    .groups = "drop"
  )

kable(
  povzetek_max,
  format = "latex",
  booktabs = TRUE,
  caption = "Povzetek odmikov za maksimalno število podpoti"
) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  save_kable("povzetek_max_n8.tex")

# GRAF KI POKAŽE KOLIKŠNI SO BILI ODMIKI OD PRAVILNE VREDNOSTI
# 1. razširimo v široko tabelo: vsak parameter svoj stolpec
wide <- graf_2 %>%
  select(-n) %>%
  pivot_wider(
    names_from = parametri,
    values_from = vrednosti
  )

# 2. izračunamo odmik od pravilne vrednosti
wide_diff <- wide %>%
  mutate(
    across(
      -`µ(G)`,
      ~ .x - `max p_n(G)_pravilno`
    )
  )

# 3. vrnemo nazaj v dolg format, brez stolpca "pravilno" (ta ima odmik 0)
diff_long <- wide_diff %>%
  select(-`max p_n(G)_pravilno`) %>%
  pivot_longer(
    cols = -`µ(G)`,
    names_to = "parametri",
    values_to = "odmik"
  )

# 4. graf: odmik glede na µ(G)
dodge <- position_dodge(width = 0.7)

ggplot(tabela_2, aes(x = `µ(G)`, y = odmik, color = parametri, group = parametri)) +
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
wide <- graf_2 %>%
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
      -`µ(G)`,
      ~ (.x - `max p_n(G)_pravilno`) / `max p_n(G)_pravilno`
      # za absolutni odmik:
      # ~ abs(.x - `min p_n(G)_pravilno`) / `min p_n(G)_pravilno`
    )
  )

# 3. nazaj v dolg format, brez stolpca "pravilno"
rel_long <- wide_rel %>%
  select(-`max p_n(G)_pravilno`) %>%
  pivot_longer(
    cols = -`µ(G)`,
    names_to = "parametri",
    values_to = "rel_odmik"
  )

# 4. graf relativnih odmikov
ggplot(rel_long, aes(x = `µ(G)`, y = rel_odmik, color = parametri, group = parametri)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1, position = position_dodge(width = 0.2)) +
  geom_point(size = 2, position = position_dodge(width = 0.2)) +
  labs(
    x = "µ(G)",
    y = "relativni odmik od pravilne vrednosti",
    color = "parametri"
  ) +
  theme_minimal()


