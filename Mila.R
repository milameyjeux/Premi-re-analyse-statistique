raw_fao_prod <- read.csv("data_raw/production.csv")

library(tidyverse)

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
      Produit == "Maïs (maïs)" ~ "Maïs",
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
    `Surface` = Surface,
    `Rendement` = Rendement,
    `Production` = Production,
    `Animaux laitiers` = `Animaux laitiers`,
    `Animaux Producteurs/Abattus` = `Animaux Producteurs/Abattus`,
    `Rendement/Poids Carcasse` = `Rendement/Poids Carcasse`
  )


str(production)

str(fao_prod)

plot(production$Surface,production$Rendement)


boxplot(production$Rendement ~ production$Zone,
        col = c("#034BFC","#FF0000"),
        main = paste("Boxplot pour la variable rendement, par zone"),
        ylab = "Quantiles",
        xlab = "zone")



library(ggplot2)

ggplot(data = production, 
       aes(x = Zone, y = Rendement, fill = Zone)) + 
  geom_violin() +
  labs(title = "Rendemaent par zone",
       subtitle = "Visualisation par graphique en violon",
       x = "rendement",
       y = "zone") +
  theme_minimal()


ggplot(data = production, 
       aes(x = Zone, fill = Surface)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
