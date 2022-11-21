library(tidyverse)
library(tmap)
library(scales)
# Data retrieved from dgeq website
# https://dgeq.org/doc/gen2018-10-01/resultats.json
# https://www.donneesquebec.ca/recherche/dataset/2e9cedcb-65d1-4031-b990-ec558a3b4486/resource/4da0f918-beb6-41e2-b4eb-c423b01d7e0a/download/election-quebecoise-de-2007.csv
# https://dgeq.org/resultats.json
# https://dgeq.org/circonscriptions_electorales_2022_shapefile.zip
# data22 <- jsonlite::read_json("~/../Downloads/resultats.json")
# data18 <- jsonlite::read_json("~/../Downloads/resultats2018.json")
# data07 <- read_csv("~/../Downloads/election-quebecoise-de-2007.csv")

shp <- sf::read_sf("~/../Downloads/circonscriptions", layer = "Circonscription_electorale_2022_shapefile") |>
  sf::st_simplify(dTolerance = 300) |>
  select(cirNom = NM_CEP, cirId = CO_CEP)

cir22 <- data22$circonscriptions
cir18 <- data18$circonscriptions
candidats18 <- cir18 |>
  map("candidats") |>
  tibble() |>
  set_names("candidat")

id18 <- map_dfr(cir18, \(x) tibble(
  cirId = x$numeroCirconscription,
  cirNom = x$nomCirconscription,
  voteRejete = x$tauxVoteRejete,
  participation = as.numeric(x$tauxParticipation),
  nbElecteurs = x$nbElecteurInscrit
)) |>
  mutate(cirNom = case_when(
    cirNom == "Bourget" ~ "Camille-Laurin",
    .default = cirNom
  ))
id22 <- map_dfr(cir22, \(x) tibble(
  cirId = x$numeroCirconscription,
  cirNom = x$nomCirconscription,
  voteRejete = x$tauxVoteRejete,
  participation = as.numeric(x$tauxParticipation),
  nbElecteurs = x$nbElecteurInscrit
))


votes18 <- bind_cols(id18, candidats18) |>
  unnest(candidat) |>
  unnest_wider(candidat) |>
  mutate(
    parti = fct_collapse(abreviationPartiPolitique,
                         CAQ = "C.A.Q.-E.F.L.",
                         CAQ = "C.A.Q.-É.F.L.",
                         PLQ = "P.L.Q./Q.L.P.",
                         PQ = "P.Q.",
                         QS = "Q.S.",
                         PCQ = "P.C.Q-E.E.D.",
                         PCQ = "P.C.Q./C.P.Q.",
                         other_level = "Autre"
    )
  )



candidats22 <- cir22 |>
  map("candidats") |>
  tibble() |>
  set_names("candidat")



votes22 <- bind_cols(id22, candidats22) |>
  unnest(candidat) |>
  unnest_wider(candidat) |>
  mutate(
    parti = fct_collapse(abreviationPartiPolitique,
                         CAQ = "C.A.Q.-E.F.L.",
                         CAQ = "C.A.Q.-É.F.L.",
                         PLQ = "P.L.Q./Q.L.P.",
                         PQ = "P.Q.",
                         QS = "Q.S.",
                         PCQ = "P.C.Q-E.E.D.",
                         PCQ = "P.C.Q./C.P.Q.",
                         other_level = "Autre"
    )
  )
votes18
votes22 |>
  print(width = 2 * getOption("width"))


parti22 <- votes22 |>
  group_by(cirNom, parti) |>
  summarise(
    tauxVote = sum(tauxVote, na.rm = TRUE),
    .groups = "drop"
  )

parti22 |>
  pivot_wider(names_from = parti, values_from = tauxVote, values_fill = 0)
parti18 <- votes18 |>
  group_by(cirNom, parti) |>
  summarise(
    tauxVote = sum(tauxVote, na.rm = TRUE),
    .groups = "drop"
  )

parti18 |>
  pivot_wider(names_from = parti, values_from = tauxVote)


comparaison_partis_long <- parti22 |>
  full_join(
    y = parti18,
    by = c("cirNom", "parti"),
    suffix = c("2022", "2018")
  ) |>
  mutate(
    augmentation = (tauxVote2022 - tauxVote2018)
  )
comparaison_partis_long |>
  group_by(parti) |>
  summarise(
    pct_augmentation = mean(augmentation > 0, na.rm = TRUE)
  )
# Le parti conservateur a augmenté son appui dans toutes les circonscriptions
# Malheureusement, la CAQ a augmenté dans 69% des endroits
# Le PLQ a diminué son appui partout au Québec
comparaison_partis <- comparaison_partis_long |>
  pivot_wider(
    id_cols = cirNom,
    names_from = parti,
    values_from = c(tauxVote2022, tauxVote2018, augmentation)
  )


shp_comparaisons_partis <- shp |> 
  left_join(comparaison_partis, by = "cirNom")

saveRDS(shp_comparaisons_partis, here::here("election", "data.RDS"))

# Cartes de comparaison entre 2018 et 2022------

# Ou le taux de participation a-t-il augmenté
# Ou la population augmente le plus
comparaison <- id18 |>
  full_join(
    id22,
    by = c("cirId", "cirNom"),
    suffix = c("2018", "2022")
  ) |>
  mutate(
    increaseElecteurs = (nbElecteurs2022 - nbElecteurs2018) / nbElecteurs2018,
    increaseParticipation = (participation2022 - participation2018) / participation2018
  )
tmap_mode("view")


shp |>
  left_join(comparaison, by = c("cirNom", "cirId")) |>
  tm_shape() +
  tm_polygons(
    col = "increaseParticipation",
    alpha = 0.5,
    border.alpha = 0.2,
    legend.format = list(fun = label_percent(style_positive = "plus")),
    title = "Changement du taux de participation",
    popup.vars = c("participation2018", "participation2022", "increaseElecteurs", "increaseParticipation")
  )

# Il y a une plus grande augmentation de la participation dans les régions ou les gens ont voté
# pour le parti conservateur.
# Intéressant: graphique augmentation participation vs vote conservateur
# Intéressant: taux vraiment plus bas que la moyenne à SMSJ
# Le Caqistan est otherwise apathique

shp |>
  left_join(comparaison, by = c("cirNom", "cirId")) |>
  tm_shape() +
  tm_polygons(
    col = "participation2022",
    alpha = 0.5,
    border.alpha = 0.2,
    palette = "BuPu",
    legend.format = list(fun = label_percent(scale = 1)),
    title = "Taux de participation en 2022 (moy. 66%)",
    popup.vars = c("participation2018", "participation2022", "increaseElecteurs")
  )
# Surprenament, Jean-Lesage est plus bas que la région de Québec,
# Côte-du-Sud est aussi une exception
# À Montréal, les places anglophones ont des taux de votes plusfaibles
# Les taux de participations les plus élevés sont au Centre-du-Québec
# L'Outaouais, l'Ouest-de-l'Ile, la Côte-Nord s'en fout


# Finalement, sur la démographie
shp |>
  left_join(comparaison, by = c("cirNom", "cirId")) |>
  tm_shape() +
  tm_polygons(
    col = "increaseElecteurs",
    alpha = 0.5,
    border.alpha = 0.2,
    # palette = "BuPu",
    legend.format = list(fun = label_percent(style_positive = "plus")),
    title = "Augmentation de la population d'électeurs",
    popup.vars = c("participation2018", "participation2022", "increaseElecteurs", "nbElecteurs2022")
  )
# Regardons quelles circonscriptions sont susceptibles de disparaître

shp |>
  left_join(comparaison, by = c("cirNom", "cirId")) |>
  tm_shape() +
  tm_polygons(
    col = "nbElecteurs2022",
    alpha = 0.5,
    border.alpha = 0.2,
    palette = "BuPu",
    # legend.format = list(fun = label_percent(style_positive = "plus")),
    title = "Nombre d'électeurs",
    popup.vars = c("participation2018", "participation2022", "increaseElecteurs", "nbElecteurs2022")
  )
# On va sans doute enlever une circonscription à Montréal
# Ajouter 1 en Estrie, Lanaudière
# Enlever Gaspésie
# Changer quand même pas mal à Quebec
