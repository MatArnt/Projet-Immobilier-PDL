install.packages("data.table")
install.packages("arrow")
install.packages("fixest")
install.packages("ggplot2")
install.packages("sf")
install.packages("scales")
library(fixest)
library(data.table)
library(arrow)
library(ggplot2)
library(sf)
library(scales)

# Chemin vers le fichier DVF 2010-2013
chemin_fichier <- "/Users/matteoarnoult/Desktop/conduite de projet/dv3f_filtered_2010_2013.parquet"

# Chargement en data.table
dt_2010_2013 <- as.data.table(read_parquet(chemin_fichier))

# Check rapide de la structure
head(dt_2010_2013)

# Liste des départements des Pays de la Loire
dep_pdl <- c("44", "49", "53", "72", "85")

# Filtre spatial sur la région PDL
dt_2010_2013_pdl <- dt_2010_2013[coddep %in% dep_pdl]

# Vérification du nombre de transactions par dpt
comptage_dep <- dt_2010_2013_pdl[, .N, by = coddep][order(coddep)]
print(comptage_dep)

# Chargement de la base post-2014
dt <- fread("/Users/matteoarnoult/Desktop/conduite de projet/Données pays de la loire /1_DONNEES_LIVRAISON/dvf_plus.csv")

head(dt)

# Comptage par année pour vérifier la couverture temporelle
comptage <- dt[, .(nb_transactions = .N), by = anneemut][order(anneemut)]
print(comptage)

# Vérif des colonnes avant fusion
print("Colonnes base 2010-2013 :")
names(dt_2010_2013_pdl)

print("Colonnes base principale :")
names(dt)

colonnes_communes <- intersect(names(dt_2010_2013_pdl), names(dt))
print("Colonnes communes :")
print(colonnes_communes)


# 1. PREP BASE 2010-2013 (AVANT FUSION)

# Nettoyage de number_rooms (on retire le "+" pour pouvoir convertir en numérique)
dt_2010_2013_pdl[, number_rooms := gsub("\\+", "", number_rooms)]
dt_2010_2013_pdl[, number_rooms := as.numeric(number_rooms)]


# 2. PREP BASE POST-2014 (AVANT FUSION)

cols_pieces <- c("nbapt1pp", "nbapt2pp", "nbapt3pp", "nbapt4pp", "nbapt5pp", 
                 "nbmai1pp", "nbmai2pp", "nbmai3pp", "nbmai4pp", "nbmai5pp")

# On remplace les NA par 0 sur le détail des pièces pour éviter les bugs d'addition
for (col in cols_pieces) {
  set(dt, which(is.na(dt[[col]])), col, 0)
}

# Recalcul du nombre de pièces total
dt[, number_rooms := 1*nbapt1pp + 2*nbapt2pp + 3*nbapt3pp + 4*nbapt4pp + 5*nbapt5pp + 
     1*nbmai1pp + 2*nbmai2pp + 3*nbmai3pp + 4*nbmai4pp + 5*nbmai5pp]

# Renommage de sterr pour matcher avec la base 2010
if("sterr" %in% names(dt)) {
  setnames(dt, "sterr", "ffsterr")
}


# 3. SELECTION DES VARIABLES

# On garde l'essentiel : ID, tempo, spatial, filtres, var hédoniques
cols_to_keep <- c(
  "idmutation", "anneemut", "moismut", "coddep", "l_codinsee", 
  "libnatmut", "codtypbien", "libtypbien", 
  "valeurfonc", "sbati", "vefa", "number_rooms", "ffsterr"
)

dt_2010_2013_clean <- dt_2010_2013_pdl[, ..cols_to_keep]
dt_post2014_clean <- dt[, ..cols_to_keep]


# 4. FUSION

# fill = FALSE par sécurité : ça crashera si les colonnes ne matchent pas
dt_finale <- rbindlist(list(dt_2010_2013_clean, dt_post2014_clean), use.names = TRUE, fill = FALSE)

# Tri chrono
setorder(dt_finale, anneemut, moismut)

# On force le type numérique sur les variables quanti 
cols_num <- c("valeurfonc", "sbati", "ffsterr")
dt_finale[, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num]


# 5. VERIFICATIONS POST-FUSION

lignes_2010 <- nrow(dt_2010_2013_clean)
lignes_2014 <- nrow(dt_post2014_clean)
lignes_totales <- nrow(dt_finale)

print(paste("Lignes 2010-2013 :", lignes_2010))
print(paste("Lignes post-2014 :", lignes_2014))
print(paste("Lignes base finale :", lignes_totales))
if(lignes_2010 + lignes_2014 == lignes_totales) print("Fusion parfaite des lignes") else print("Problème de lignes")

print("--- Transactions par année ---")
print(table(dt_finale$anneemut, useNA = "ifany"))

print("--- Résumé du nombre de pièces ---")
print(summary(dt_finale$number_rooms))

print("--- Valeurs manquantes par colonne ---")
print(colSums(is.na(dt_finale)))


# 6. NETTOYAGE ECONOMETRIQUE

# Exclusion des transactions sur plusieurs communes (qui faussent les prix au m2)
dt_finale <- dt_finale[!grepl(",", l_codinsee)]

# Filtre sur les ventes classiques
dt_model <- dt_finale[libnatmut == "Vente" | libnatmut == "Vente en l'état futur d'achèvement"]

# Filtre sur les prix et surfaces (on vire les valeurs nulles ou manquantes)
dt_model <- dt_model[!is.na(valeurfonc) & valeurfonc > 0]
dt_model <- dt_model[!is.na(sbati) & sbati > 0]

# Filtre sur le nombre de pièces (bornes logiques)
dt_model <- dt_model[number_rooms >= 1 & number_rooms <= 15]

# Calcul du prix au m2
dt_model[, price_sqm := valeurfonc / sbati]

# Exclusion des prix au m2 aberrants
dt_model <- dt_model[price_sqm >= 300 & price_sqm <= 15000]

# Variable expliquée (passage au log)
dt_model[, y_i := log(price_sqm)]

print(paste("Nombre de lignes après nettoyage complet :", nrow(dt_model)))


# 7. SEPARATION MAISONS / APPARTS

# Typologie Cerema : 11 = Maisons, 12 = Apparts
dt_maisons <- dt_model[codtypbien %like% "^11" | libtypbien %like% "MAISON"]
dt_apparts <- dt_model[codtypbien %like% "^12" | libtypbien %like% "APPARTEMENT"]

print(paste("Nombre de maisons pour le modèle :", nrow(dt_maisons)))
print(paste("Nombre d'appartements pour le modèle :", nrow(dt_apparts)))


# 8. PREP DES EFFETS FIXES

# Création de la variable temporelle au format YYYY-MM
dt_maisons[, mois_annee := paste(anneemut, sprintf("%02d", moismut), sep="-")]
dt_apparts[, mois_annee := paste(anneemut, sprintf("%02d", moismut), sep="-")]

# Comptage des transactions par commune
dt_maisons[, nb_transac_c := .N, by = l_codinsee]
dt_apparts[, nb_transac_c := .N, by = l_codinsee]

# Traitement des petites communes (seuil fixé à 50) cf slide 11
dt_maisons[, commune_fe := fifelse(nb_transac_c > 50, l_codinsee, "TINY")]
dt_apparts[, commune_fe := fifelse(nb_transac_c > 50, l_codinsee, "TINY")]


# 9. ESTIMATION DES MODELES

# Maisons : prise en compte du terrain (log + 1 pour gérer les zeros)
modele_maisons <- feols(
  y_i ~ number_rooms + vefa + log(ffsterr + 1) | 
    mois_annee + coddep + commune_fe + coddep^anneemut, 
  data = dt_maisons
)

# Apparts : pas de variable terrain
modele_apparts <- feols(
  y_i ~ number_rooms + vefa | 
    mois_annee + coddep + commune_fe + coddep^anneemut, 
  data = dt_apparts
)

print("--- RÉSULTATS MAISONS ---")
print(summary(modele_maisons))

print("--- RÉSULTATS APPARTEMENTS ---")
print(summary(modele_apparts))


# 10. CALCUL DU PRIX NET

# Réduction de la base aux obs réellement utilisées (gère les lignes dropées par feols)
dt_maisons_model <- dt_maisons[obs(modele_maisons)]
dt_apparts_model <- dt_apparts[obs(modele_apparts)]

# Prédiction
dt_maisons_model[, y_pred := predict(modele_maisons)]
dt_apparts_model[, y_pred := predict(modele_apparts)]

# Récupération des betas
beta_rooms_m <- coef(modele_maisons)["number_rooms"]
beta_vefa_m <- coef(modele_maisons)["vefa"]
beta_ter_m <- coef(modele_maisons)["log(ffsterr + 1)"]

beta_rooms_a <- coef(modele_apparts)["number_rooms"]
beta_vefa_a <- coef(modele_apparts)["vefa"]

# Nettoyage de l'effet qualité sur les prix
dt_maisons_model[, log_p_net := y_pred - (beta_rooms_m * number_rooms + beta_vefa_m * vefa + beta_ter_m * log(ffsterr + 1))]
dt_apparts_model[, log_p_net := y_pred - (beta_rooms_a * number_rooms + beta_vefa_a * vefa)]

print("Aperçu des prix nets (Maisons) :")
print(head(dt_maisons_model[, .(valeurfonc, price_sqm, y_pred, log_p_net)]))

print("Aperçu des prix nets (Appartements) :")
print(head(dt_apparts_model[, .(valeurfonc, price_sqm, y_pred, log_p_net)]))


# 11. AGREGATION ET CORRECTION TINY

# Résidus du modèle
dt_maisons_model[, resu := y_i - log_p_net]
dt_apparts_model[, resu := y_i - log_p_net]

# Moyenne des résidus au niveau communal
dt_maisons_model[, mean_resu_c := mean(resu), by = l_codinsee]
dt_apparts_model[, mean_resu_c := mean(resu), by = l_codinsee]

# Application de la correction seulement pour les modalités TINY
dt_maisons_model[commune_fe == "TINY", log_p_net := log_p_net + mean_resu_c]
dt_apparts_model[commune_fe == "TINY", log_p_net := log_p_net + mean_resu_c]

# Agrégation spatio-temporelle à la médiane (reco du prof, slide 14)
dt_ind_m <- dt_maisons_model[, .(log_p_net_med = median(log_p_net)), by = .(l_codinsee, mois_annee)]
dt_ind_a <- dt_apparts_model[, .(log_p_net_med = median(log_p_net)), by = .(l_codinsee, mois_annee)]

print(paste("Nombre de couples (commune, mois) - Maisons :", nrow(dt_ind_m)))
print(paste("Nombre de couples (commune, mois) - Appartements :", nrow(dt_ind_a)))

print("Aperçu de la table agrégée (Maisons) :")
print(head(dt_ind_m[order(l_codinsee, mois_annee)]))


# 12. COMPLETION DU PANEL 

# Pour les maisons
info_c_m <- unique(dt_maisons_model[, .(l_codinsee, coddep, commune_fe, mean_resu_c)])
info_t_m <- unique(dt_maisons_model[, .(mois_annee, anneemut)])

# Création du panel équilibré
grille_m <- CJ(l_codinsee = unique(info_c_m$l_codinsee), 
               mois_annee = unique(info_t_m$mois_annee))

grille_m <- merge(grille_m, info_c_m, by = "l_codinsee", all.x = TRUE)
grille_m <- merge(grille_m, info_t_m, by = "mois_annee", all.x = TRUE)
grille_m <- merge(grille_m, dt_ind_m, by = c("l_codinsee", "mois_annee"), all.x = TRUE)

# Remplissage des caractéristiques par défaut pour les obs manquantes
grille_m[is.na(log_p_net_med), c("number_rooms", "vefa", "ffsterr") := .(0, 0, 0)]

# Prédiction sur les trous
grille_m[is.na(log_p_net_med), pred_net := predict(modele_maisons, newdata = .SD)]
grille_m[is.na(log_p_net_med) & commune_fe == "TINY", pred_net := pred_net + mean_resu_c]

# Mise à jour de la variable finale
grille_m[is.na(log_p_net_med), log_p_net_med := pred_net]

dt_final_maisons <- grille_m[, .(l_codinsee, coddep, mois_annee, anneemut, log_p_net_med)]
setorder(dt_final_maisons, l_codinsee, mois_annee)


# Pour les appartements
info_c_a <- unique(dt_apparts_model[, .(l_codinsee, coddep, commune_fe, mean_resu_c)])
info_t_a <- unique(dt_apparts_model[, .(mois_annee, anneemut)])

grille_a <- CJ(l_codinsee = unique(info_c_a$l_codinsee), 
               mois_annee = unique(info_t_a$mois_annee))

grille_a <- merge(grille_a, info_c_a, by = "l_codinsee", all.x = TRUE)
grille_a <- merge(grille_a, info_t_a, by = "mois_annee", all.x = TRUE)
grille_a <- merge(grille_a, dt_ind_a, by = c("l_codinsee", "mois_annee"), all.x = TRUE)

grille_a[is.na(log_p_net_med), c("number_rooms", "vefa") := .(0, 0)]
grille_a[is.na(log_p_net_med), pred_net := predict(modele_apparts, newdata = .SD)]
grille_a[is.na(log_p_net_med) & commune_fe == "TINY", pred_net := pred_net + mean_resu_c]
grille_a[is.na(log_p_net_med), log_p_net_med := pred_net]

dt_final_apparts <- grille_a[, .(l_codinsee, coddep, mois_annee, anneemut, log_p_net_med)]
setorder(dt_final_apparts, l_codinsee, mois_annee)

print(paste("Lignes grille complète Maisons :", nrow(dt_final_maisons)))
print(paste("Lignes grille complète Appartements :", nrow(dt_final_apparts)))
print("Y a-t-il encore des valeurs manquantes ? (0 = Parfait)")
print(sum(is.na(dt_final_maisons$log_p_net_med)) + sum(is.na(dt_final_apparts$log_p_net_med)))


# 13. INDICE BASE 100

t0 <- "2010-01"

# Pour les maisons
ref_maisons <- dt_final_maisons[mois_annee == t0, .(l_codinsee, log_p_net_ref = log_p_net_med)]
dt_final_maisons <- merge(dt_final_maisons, ref_maisons, by = "l_codinsee", all.x = TRUE)
dt_final_maisons[, indice := 100 * exp(log_p_net_med - log_p_net_ref)]

# Pour les appartements
ref_apparts <- dt_final_apparts[mois_annee == t0, .(l_codinsee, log_p_net_ref = log_p_net_med)]
dt_final_apparts <- merge(dt_final_apparts, ref_apparts, by = "l_codinsee", all.x = TRUE)
dt_final_apparts[, indice := 100 * exp(log_p_net_med - log_p_net_ref)]

print("--- INDICE MAISONS (Aperçu) ---")
print(head(dt_final_maisons[l_codinsee == "44001" & mois_annee %in% c("2010-01", "2010-02", "2010-03", "2015-01", "2020-01")], 5))

print("--- INDICE APPARTEMENTS (Aperçu) ---")
print(head(dt_final_apparts[l_codinsee == "44001" & mois_annee %in% c("2010-01", "2010-02", "2010-03", "2015-01", "2020-01")], 5))


# 14. GRAPHIQUES TEMPORELS PAR DEPARTEMENT (Maisons)

# Repassage en euros/m2 pour l'interprétabilité 
if(!"prix_m2_net" %in% names(dt_final_maisons)) {
  dt_final_maisons[, prix_m2_net := exp(log_p_net_med)]
}

# Fonction pour générer les graphs par dpt
creer_graphique_dep <- function(dep_code, code_ville1, nom_ville1, code_ville2, nom_ville2) {
  
  # Calcul de la moyenne du dpt en référence
  dt_bench <- dt_final_maisons[coddep == dep_code, .(prix_m2_net = mean(prix_m2_net, na.rm=TRUE)), by = mois_annee]
  nom_bench <- paste("Moyenne Dpt", dep_code)
  dt_bench[, type := nom_bench]
  
  # Extraction des 2 villes cibles
  dt_com <- dt_final_maisons[l_codinsee %in% c(code_ville1, code_ville2), .(mois_annee, prix_m2_net, l_codinsee)]
  dt_com[l_codinsee == code_ville1, type := nom_ville1]
  dt_com[l_codinsee == code_ville2, type := nom_ville2]
  dt_com[, l_codinsee := NULL]
  
  # Assemblage
  dt_graph <- rbindlist(list(dt_bench, dt_com), use.names = TRUE)
  dt_graph[, date_graph := as.Date(paste0(mois_annee, "-01"))]
  setorder(dt_graph, type, date_graph)
  
  # Moyenne mobile sur 12 mois pour lisser la courbe
  dt_graph[, prix_lisse := frollmean(prix_m2_net, n = 12, fill = NA, align = "right"), by = type]
  
  # Plot
  couleurs <- setNames(c("#e74c3c", "#3498db", "#2c3e50"), c(nom_ville1, nom_ville2, nom_bench))
  
  g <- ggplot(dt_graph, aes(x = date_graph, y = prix_lisse, color = type)) +
    geom_line(linewidth = 1.2) + 
    scale_color_manual(values = couleurs) +
    labs(title = paste("Niveau des prix immobiliers (Maisons) - Dpt", dep_code),
         subtitle = "À qualité constante - Moyenne mobile sur 12 mois",
         x = "Année",
         y = "Prix net (€ / m²)",
         color = "Légende") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 13),
          axis.text = element_text(size = 10))
  
  return(g)
}

# Génération
g_44 <- creer_graphique_dep("44", "44109", "Nantes", "44036", "Châteaubriant")
g_49 <- creer_graphique_dep("49", "49007", "Angers", "49328", "Saumur")
g_53 <- creer_graphique_dep("53", "53130", "Laval", "53147", "Mayenne")
g_72 <- creer_graphique_dep("72", "72181", "Le Mans", "72154", "La Flèche")
g_85 <- creer_graphique_dep("85", "85194", "Les Sables-d'Olonne", "85191", "La Roche-sur-Yon")

print(g_44)
print(g_49)
print(g_53)
print(g_72)
print(g_85)


# 15. CARTE CHOROPLETHE (Niveau des prix)

# Récupération du geojson PDL (code 52) via l'API gouv
url_geojson <- "https://geo.api.gouv.fr/communes?codeRegion=52&format=geojson&geometry=contour"
carte_pdl <- st_read(url_geojson)

# Coupe transversale sur début 2022
date_carte <- "2022-01"
dt_carte <- dt_final_maisons[mois_annee == date_carte, .(l_codinsee, prix_m2_net)]

# Jointure spatial data + prix
carte_prix <- merge(carte_pdl, dt_carte, by.x = "code", by.y = "l_codinsee", all.x = TRUE)

carte_choro <- ggplot(carte_prix) +
  geom_sf(aes(fill = prix_m2_net), color = "white", size = 0.05) +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       name = "Prix net",
                       labels = label_number(suffix = " €/m²", big.mark = " ")) +
  labs(title = "Carte des prix immobiliers (Maisons) - Pays de la Loire",
       subtitle = paste("Niveau des prix estimés à qualité constante en", date_carte)) +
  theme_void() + # Retire les axes de coordonnées
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
        legend.title = element_text(face = "bold"))

print(carte_choro)


# 16. HEATMAP COMMUNE x TEMPS

dt_heat_total <- copy(dt_final_maisons)

if(!"prix_m2_net" %in% names(dt_heat_total)) {
  dt_heat_total[, prix_m2_net := exp(log_p_net_med)]
}
dt_heat_total[, date_graph := as.Date(paste0(mois_annee, "-01"))]

# Tri des communes par prix moyen pour ordonner l'axe Y
dt_heat_total[, prix_moyen_com := mean(prix_m2_net, na.rm=TRUE), by = l_codinsee]

graph_heatmap_total <- ggplot(dt_heat_total, aes(x = date_graph, y = reorder(l_codinsee, prix_moyen_com), fill = prix_m2_net)) +
  geom_tile() + 
  
  # squish plafonne l'échelle à 5000€ pour ne pas l'écraser par les valeurs extrêmes
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       limits = c(0, 5000), oob = squish, 
                       name = "Prix net (€/m²)") +
  
  # Découpage par dpt
  facet_wrap(~ coddep, ncol = 5, scales = "free_y") +
  
  labs(title = "Heatmap Globale des prix (Maisons) - Pays de la Loire",
       subtitle = "Évolution de toutes les communes (Échelle plafonnée à 5000 €/m² pour lisibilité)",
       x = "Année",
       y = "Densité des communes (Triées de la moins chère en bas à la plus chère en haut)") +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    # Suppression texte axe Y vu le volume de communes
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 14)
  )

print(graph_heatmap_total)

# 17. GRAPHIQUES TEMPORELS PAR DEPARTEMENT (Appartements)

# On repasse en euros/m2 pour la table des appartements
if(!"prix_m2_net" %in% names(dt_final_apparts)) {
  dt_final_apparts[, prix_m2_net := exp(log_p_net_med)]
}

# On recrée une fonction spécifique pour éviter d'écraser l'autre
creer_graphique_dep_apt <- function(dep_code, code_ville1, nom_ville1, code_ville2, nom_ville2) {
  
  # Benchmark dpt
  dt_bench <- dt_final_apparts[coddep == dep_code, .(prix_m2_net = mean(prix_m2_net, na.rm=TRUE)), by = mois_annee]
  nom_bench <- paste("Moyenne Dpt", dep_code)
  dt_bench[, type := nom_bench]
  
  # Data des 2 villes
  dt_com <- dt_final_apparts[l_codinsee %in% c(code_ville1, code_ville2), .(mois_annee, prix_m2_net, l_codinsee)]
  dt_com[l_codinsee == code_ville1, type := nom_ville1]
  dt_com[l_codinsee == code_ville2, type := nom_ville2]
  dt_com[, l_codinsee := NULL]
  
  # Fusion
  dt_graph <- rbindlist(list(dt_bench, dt_com), use.names = TRUE)
  dt_graph[, date_graph := as.Date(paste0(mois_annee, "-01"))]
  setorder(dt_graph, type, date_graph)
  
  # Lissage 12 mois
  dt_graph[, prix_lisse := frollmean(prix_m2_net, n = 12, fill = NA, align = "right"), by = type]
  
  couleurs <- setNames(c("#e74c3c", "#3498db", "#2c3e50"), c(nom_ville1, nom_ville2, nom_bench))
  
  g <- ggplot(dt_graph, aes(x = date_graph, y = prix_lisse, color = type)) +
    geom_line(linewidth = 1.2) + 
    scale_color_manual(values = couleurs) +
    labs(title = paste("Niveau des prix (Appartements) - Dpt", dep_code),
         subtitle = "À qualité constante - Moyenne mobile sur 12 mois",
         x = "Année",
         y = "Prix net (€ / m²)",
         color = "Légende") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 13),
          axis.text = element_text(size = 10))
  
  return(g)
}

# Génération des 5 graphs
g_44_apt <- creer_graphique_dep_apt("44", "44109", "Nantes", "44036", "Châteaubriant")
g_49_apt <- creer_graphique_dep_apt("49", "49007", "Angers", "49328", "Saumur")
g_53_apt <- creer_graphique_dep_apt("53", "53130", "Laval", "53147", "Mayenne")
g_72_apt <- creer_graphique_dep_apt("72", "72181", "Le Mans", "72154", "La Flèche")
g_85_apt <- creer_graphique_dep_apt("85", "85194", "Les Sables-d'Olonne", "85191", "La Roche-sur-Yon")

print(g_44_apt)
print(g_49_apt)
print(g_53_apt)
print(g_72_apt)
print(g_85_apt)


# 18. CARTE CHOROPLETHE (Appartements)

# (On suppose que carte_pdl est déjà chargée via l'étape 15 des maisons)
date_carte <- "2022-01"
dt_carte_apt <- dt_final_apparts[mois_annee == date_carte, .(l_codinsee, prix_m2_net)]

# Jointure
carte_prix_apt <- merge(carte_pdl, dt_carte_apt, by.x = "code", by.y = "l_codinsee", all.x = TRUE)

carte_choro_apt <- ggplot(carte_prix_apt) +
  geom_sf(aes(fill = prix_m2_net), color = "white", size = 0.05) +
  
  # On change la palette pour "viridis" (bleu/vert/jaune) pour différencier des maisons
  scale_fill_viridis_c(option = "viridis", direction = -1, 
                       name = "Prix net",
                       labels = label_number(suffix = " €/m²", big.mark = " ")) +
  
  labs(title = "Carte des prix immobiliers (Appartements) - Pays de la Loire",
       subtitle = paste("Niveau des prix estimés à qualité constante en", date_carte)) +
  theme_void() + 
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
        legend.title = element_text(face = "bold"))

print(carte_choro_apt)


# 19. HEATMAP COMMUNE x TEMPS (Appartements)

dt_heat_total_apt <- copy(dt_final_apparts)

if(!"prix_m2_net" %in% names(dt_heat_total_apt)) {
  dt_heat_total_apt[, prix_m2_net := exp(log_p_net_med)]
}
dt_heat_total_apt[, date_graph := as.Date(paste0(mois_annee, "-01"))]

# Tri Y
dt_heat_total_apt[, prix_moyen_com := mean(prix_m2_net, na.rm=TRUE), by = l_codinsee]

graph_heatmap_total_apt <- ggplot(dt_heat_total_apt, aes(x = date_graph, y = reorder(l_codinsee, prix_moyen_com), fill = prix_m2_net)) +
  geom_tile() + 
  
  # Palette viridis et plafond à 8000€
  scale_fill_viridis_c(option = "viridis", direction = -1, 
                       limits = c(0, 8000), oob = squish, 
                       name = "Prix net (€/m²)") +
  
  facet_wrap(~ coddep, ncol = 5, scales = "free_y") +
  
  labs(title = "Heatmap Globale des prix (Appartements) - Pays de la Loire",
       subtitle = "Évolution de toutes les communes (Échelle plafonnée à 5000 €/m²)",
       x = "Année",
       y = "Densité des communes (Triées par prix)") +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 14)
  )

print(graph_heatmap_total_apt)

# Extraction précise pour la Sarthe en Janvier 2022
check_sarthe <- dt_final_apparts[coddep == "72" & mois_annee == "2022-01", .(l_codinsee, prix_m2_net)]
setorder(check_sarthe, -prix_m2_net) # On trie pour avoir le plus cher en haut
print(check_sarthe)

