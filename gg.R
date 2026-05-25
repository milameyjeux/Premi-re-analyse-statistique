# ========================================
# ANALYSE EXPLORATOIRE : FRANCE / INDE
# Groupe : Noms Etudiants ...
# ========================================

# ----------------
# | Introduction |
# ----------------

# Nous avons choisi de comparer la France et l'Inde sur la culture du Blé.
# Cette comparaison est pertinente car elle oppose un leader mondial de 
# l'exportation (France) à un pays dont la sécurité alimentaire dépend 
# de l'amélioration de ses rendements céréaliers (Inde).
# ...

# ---------------------------
# | Préparation des données |
# ---------------------------

library(tidyverse)

# IMPORTATION DES DONNEES
raw_fao_prod <- read.csv("data_raw/production.csv")

# NETTOYAGE DES DONNEES

fao_prod <- raw_fao_prod |>
  
  # Sélection des colonnes
  select(Zone, Produit, Élément, Année, Unité, Valeur) |>
  
  mutate(
    
    # Nettoyage des espaces sur les colonnes de texte
    Zone = str_trim(Zone),
    Produit = str_trim(Produit),
    Élément = str_trim(Élément),
    Unité = str_trim(Unité),
    
    # Simplification des noms de produits
    Produit = case_when(
      Produit == "Maïs" ~ "Mais",
      Produit == "Blé" ~ "Blé",
      TRUE ~ Produit
    ),
    
    # Simplification des noms d'éléments
    Élément = case_when(
      Élément == "Superficie récoltée" ~ "Surface",
      Élément == "Rendement" ~ "Rendement",
      Élément == "Production" ~ "Production",
      TRUE ~ Élément
    )
    
  )

table(fao_prod$Élément, fao_prod$Unité)

production <- fao_prod |>
  # On ne garde que les colonnes nécessaires 
  select(Zone, Produit, Année, Élément, Valeur) |>
  
  # On transforme les lignes de la colonne Élément en colonnes distinctes
  pivot_wider(
    names_from = Élément, 
    values_from = Valeur
  ) |>
  
  # On renomme les colonnes
  rename(
    `Pays` = Zone,
    `Céréale` = Produit,
    `Année` = Année,
    `Surface` = Surface,
    `Rendement` = Rendement,
    `Production` = Production,
  )
# ---------------------
# | Analyse univariée |
# ---------------------

# On filtre les données pour la France et le Blé, et on s'interesse uniquement au rendement
france_rdt_ble <- production |>
  filter(Pays == "France", Céréale == "Blé") |>
  select(Rendement) 

# On filtre les données pour le Kenya et le Blé, et on s'interesse uniquement au rendement
kenya_ble <- production |>
  filter(Pays == "Inde", Céréale == "Blé") |>
  select(Surface, Rendement, Production) 




library(gt)

# Fonction pour calculer les stats
calc_stats <- function(df) {
  df |> 
    summarise(
      Min = min(Rendement, na.rm = TRUE),
      `1er Qu.` = quantile(Rendement, 0.25, na.rm = TRUE),
      Médiane = median(Rendement, na.rm = TRUE),
      Moyenne = mean(Rendement, na.rm = TRUE),
      `3ème Qu.` = quantile(Rendement, 0.75, na.rm = TRUE),
      Max = max(Rendement, na.rm = TRUE),
      `Écart-type` = sd(Rendement, na.rm = TRUE),
      Variance = var(Rendement, na.rm = TRUE)
    )
}

# Calcul des stats pour les deux pays
stats_fr <- calc_stats(france_rdt_ble)
stats_in <- calc_stats(inde_ble)

# Fusion et mise en forme pour le tableau
stats_compare <- bind_rows(
  stats_fr |> mutate(Pays = "France"),
  stats_in |> mutate(Pays = "Inde")
) |> 
  pivot_longer(
    cols = -Pays, 
    names_to = "Indicateur", 
    values_to = "Valeur"
  ) |> 
  pivot_wider(
    names_from = Pays, 
    values_from = Valeur
  )

# Affichage du tableau avec gt
stats_compare |>
  gt() |>
  tab_header(
    title = "Comparaison Statistique du Rendement",
    subtitle = "Blé : France vs Inde (Données FAOSTAT)"
  ) |>
  fmt_number(
    columns = c(France, Inde),
    decimals = 2,
    sep_mark = " "
  ) |>
  cols_label(
    Indicateur = "Statistique",
    France = "France (kg/ha)",
    Inde = "Inde (kg/ha)"
  ) |>
  tab_options(
    heading.background.color = "#2c3e50",
    column_labels.background.color = "#f2f2f2",
    table.width = pct(100)
  )



# [... ne pas oublier d'interpréter vos résultats ...]

# ---------------------
# | Analyse bivariée |
# ---------------------

# [... ne pas oublier d'interpréter vos résultats ...]

# --------------
# | Conclusion |
# --------------

# [Tenter une conclusion globale de votre analyse]

