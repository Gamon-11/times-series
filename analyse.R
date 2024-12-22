################################################################################
# Auteurs     : DIOP Mandir, GAMONDELE Maxime, SAMAKE Salif
# Projet      : SAE 3.03 - Série Chronologique
# Thématique  : Futures de Matières Premières
# Source      : https://fr.investing.com
################################################################################
library("ggforce") 


################################################################################
# --------------------------  Importation des données  ----------------------- #
################################################################################

# lecture du jeu de donnée
data = readRDS("fichier_de_travail.rds") 

# vérification structure des données 
head(data)
tail(data)
levels(data$Futures)
str(data)

################################################################################
# --------------------------  Analyse de données       ----------------------- #
################################################################################


# fonctions poour les graphique 
# --------------------------  chronique_reg()          ----------------------- #
# Renvoie un chronogramme avec son lissage

chronique_reg <- function(data, titre, y_col, span_, lab_x, lab_y,sub_,cap_) {
  ggplot(data, aes(x = Month, y = !!sym(y_col))) + 
    geom_line(aes(color = "Série brute"), show.legend = FALSE) +  # Exclure "Série brute" de la légende
    geom_smooth(span = span_, aes(color = "Lissage"), size = 0.5, se = FALSE) +  # Lissage en rouge
    labs(
      title = titre,
      x = lab_x,
      y = lab_y,
      subtitle = sub_,
      caption = c(cap_),
      color = ""
    ) + 
    scale_color_manual(values = c("Lissage" = "red")) + 
    theme_bw() + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
      plot.subtitle = element_text(hjust = 0.5, colour = "blue4"),
      plot.caption = element_text(hjust = c(0, 1), face = "bold", colour = "blue4"),
      legend.position = "bottom"
    )
}



# ---------------------------------------------------------------------------- #
#.      1. Boxplots des closed cotation annuels pour chaque matière première   # 
# ---------------------------------------------------------------------------- #

# Affichage des différents raw materials

levels(data$Futures)

# Preparation des données pour le graphique 
data$Year <- year(data$Date)    # Extraire l'année
data$Month <- floor_date(data$Date, "month")  # Extraire le mois
head(data)

# Graphique

ggplot(data, aes(x = factor(Year), y = Closed_Cotation, fill = Futures)) +
  geom_boxplot(outlier.size = 1, outlier.color = "black") +
  facet_wrap(~Futures, scales = "free_y") +
  labs(
    title = "Boxplots annuels des cotations journalières de clôture",
    x = "Année", 
    y = "Cotation de clôture (en $)",
    fill = "Matière première",
    subtitle = "Source : investing.com",
    caption = c("BUT Science des Données - Lisieux","Diop Mandir - Gamondele Maxime - Samake Salif")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 45),
    strip.text = element_text(face = "bold"), 
    legend.position = c(0.85, 0.2),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 24, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, colour = "blue4"),
    plot.caption = element_text(hjust = c(0, 1), face = "bold", colour = "blue4")
  )

# ---------------------------------------------------------------------------- #
#     2. Évolution mensuelle avec une courbe de régression lissée              # 
# ---------------------------------------------------------------------------- #

# Préparation des données pour le graph
# Calcul de la moyenne mensuelle pour chaque matière première
data_mois <- data %>%
  mutate(YearMonth = floor_date(Date, "month")) %>% 
  group_by(YearMonth, Futures) %>% 
  summarise(Moyenne_Cotation = mean(Closed_Cotation, na.rm = TRUE), .groups = "drop")

# Graphique
ggplot(data_mois, aes(x = YearMonth, y = Moyenne_Cotation, )) +
  geom_line(aes(color = Futures), size = 1.2) +  
  geom_smooth(method = "gam", se = FALSE, color = "black", size = 1) + 
  facet_wrap(~Futures, scales = "free_y") +  
  theme_bw() +
  labs(
    title = "Évolution moyenne mensuelle de la cotation journalière de chaque matière première",
    x = "Année",
    y = "Cotation (en $)",
    color = "Matières premières",
    subtitle = "Source : investing.com",
    caption = c("BUT Science des Données - Lisieux","Diop Mandir - Gamondele Maxime - Samake Salif")
  ) +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 45),
    strip.text = element_text(face = "bold"), 
    legend.position = c(0.85, 0.2),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 24, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, colour = "blue4"),
    plot.caption = element_text(hjust = c(0, 1), face = "bold", colour = "blue4")
  )

# ---------------------------------------------------------------------------- #
#.   3. Taux d'évolution de la moyenne mensuelle des cotations journalières    # 
# ---------------------------------------------------------------------------- #

# Groupement et calcul de la moyenne mensuelle
data %>%
  group_by(Futures, Annee_mois = format(Date, "%Y-%m")) %>%
  summarize(moyenne_mensuelle = mean(Closed_Cotation, na.rm = TRUE)) -> data1

# Calcul du taux de variation (1ère méthode avec l'utilisation de la fonction lag())
data1 <- data1 %>%
  group_by(Futures) %>%
  mutate(Taux_variation_mensuel = (moyenne_mensuelle - lag(moyenne_mensuelle)) / lag(moyenne_mensuelle) * 100)

# Calcul du taux de variation sans lag()
data1 <- data1 %>%
  group_by(Futures) %>%
  mutate(Taux_variation_mensuel = c(NA, diff(moyenne_mensuelle) / moyenne_mensuelle[-length(moyenne_mensuelle)] * 100))

# Conversion de Annee_mois en date
data1$Annee_mois <- as.Date(paste0(data1$Annee_mois, "-01"))

# Résultat final
print(data1)


# Conversion de Annee_mois en date
data1$Annee_mois <- as.Date(paste0(data1$Annee_mois, "-01"))

# Tracé
ggplot(data1, aes(x = Annee_mois, y = Taux_variation_mensuel, group = Futures, color = Futures)) +
  geom_line() +
  geom_smooth(method = 'loess') +
  facet_wrap(~ Futures, scales = "free_y") +
  labs(
    title = "Taux d'évolution de la moyenne mensuelle des cotations journalières",
    x = "Date",
    y = "Taux d'évolution (%)",
    color = "Matière première",
    subtitle = "Source : investing.com",
    caption = c("BUT Science des Données - Lisieux","Diop Mandir - Gamondele Maxime - Samake Salif")
    
    
  ) +
  
  theme_bw() +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 45),
    strip.text = element_text(face = "bold"), 
    legend.position = c(0.85, 0.2),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 24, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, colour = "blue4"),
    plot.caption = element_text(hjust = c(0, 1), face = "bold", colour = "blue4")
  )

# ---------------------------------------------------------------------------- #
#.      4.Relation entre les valeurs moyennes mensuelles du café et du cacao  # 
# ---------------------------------------------------------------------------- #

# Étape 1 : Calcul des moyennes mensuelles pour le café
data_cafe <- data %>%
  filter(Futures == "Cotation du café") %>%
  mutate(Annee_mois = format(Date, "%Y-%m")) %>%
  group_by(Annee_mois) %>%
  summarize(Moyenne_Cafe = mean(Closed_Cotation, na.rm = TRUE)) %>%
  ungroup()

# Étape 2 : Calcul des moyennes mensuelles pour le cacao
data_cacao <- data %>%
  filter(Futures == "Cotation du cacao") %>%
  mutate(Annee_mois = format(Date, "%Y-%m")) %>%
  group_by(Annee_mois) %>%
  summarize(Moyenne_Cacao = mean(Closed_Cotation, na.rm = TRUE)) %>%
  ungroup()

# Étape 3 : Fusionner les deux jeux de données sur "Annee_mois"
jointure_mooyenne = merge(data_cafe, data_cacao, by = "Annee_mois", all = TRUE)

##### Modèle de régression linéaire
model <- lm(Moyenne_Cacao ~ Moyenne_Cafe, data = jointure_mooyenne)
summary(model)
cor(jointure_mooyenne$Moyenne_Cacao, jointure_mooyenne$Moyenne_Cafe, method = "pearson")
# Nuage de points avec lissage et régression linéaire
ggplot(jointure_mooyenne, aes(x = Moyenne_Cafe, y = Moyenne_Cacao)) +
  geom_point(color = "blue", 
             size = 2, 
             alpha = 0.7, 
             shape = 21, 
             fill = "blue") +  # Points du nuage
  geom_smooth(aes(color = "Régression linéaire"), 
              method = "lm", 
              se = FALSE, 
              size = 1) +  # Régression linéaire
  geom_smooth(aes(color = "Lissage"), 
              method = "loess", 
              se = TRUE, 
              size = 0.8) +  # Lissage LOESS
  labs(
    title = "Relation entre les valeurs moyennes mensuelles du café et du cacao",
    x = "Moyenne mensuelle - Café",
    y = "Moyenne mensuelle - Cacao",
    subtitle = "Source : investing.com",
    caption = c("BUT Science des Données - Lisieux", "Diop Mandir - Gamondele Maxime - Samake Salif"),
    color = ""
  ) +
  scale_color_manual(values = c("Régression linéaire" = "green4", "Lissage" = "red")) + 
  theme_bw() +
  theme(
    legend.position = "bottom",  
    legend.justification = "center", 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, colour = "blue4"),
    plot.caption = element_text(hjust = c(0, 1), face = "bold", colour = "blue4"),
    legend.title = element_text(face = "bold"),  # Titre de la légende en gras
    legend.text = element_text(size = 10)  # Taille du texte des légendes
  ) +
  annotate(
    "text", 
    x = 125, 
    y = 8000, 
    label = " m(x) = 1186,5 + 10,8x\nR² = 15,56 %\np-val = 0", 
    fontface = "bold",
    color = "green4", 
    size = 3.5
  )


cat(sprintf("P-value : %.5f\n", summary(model)$coefficients[2, 4]))
# ---------------------------------------------------------------------------- #
#          5.1 Etude du brent de 2010 à fin 2024 avec lissage                  #
# ---------------------------------------------------------------------------- #

# Préparation des donnée pour le graphique 5.1

data_brent = filter(data,data$Futures == "Cotation du pétrole Brent") %>% 
  select(Date,Closed_Cotation) %>% 
  mutate(Month = floor_date(Date, "month")) %>%  
  group_by(Month) %>%
  summarise(Monthly_Avg_Closed_Cotation = mean(Closed_Cotation, na.rm = TRUE))

# Représentation du graphique 5.1

# Base du graphique
graph1 = chronique_reg(data_brent,
              "Évolution de la cotation de clôture mensuelle moyenne du pétrole Brent",
              "Monthly_Avg_Closed_Cotation",
              0.2,
              "Janvier 2010 - Octobre 2024",
              "Cotation (en $)",
              "Source : investing.com",
              c("BUT Science des Données - Lisieux","Diop Mandir - Gamondele Maxime - Samake Salif"))

# Ajout d'éléments de précision au graphique
graph1 <- graph1 + 
  geom_vline(xintercept = as.Date("2016-01-01"),
             color = "purple",
             linetype = "dashed") + 
  annotate("text",
           x = as.Date("2016-01-01")+80,
           y = 30, 
           label = "Abondance de l'offre\njanvier 2016", 
           color = "purple", 
           hjust = 0)+ 
  geom_vline(xintercept = as.Date("2020-04-01"),
             color = "blue", 
             linetype = "dashed") + 
  annotate("text", 
           x = as.Date("2020-04-01") +80, 
           y = 30, label = "Crise du Covid-19\navril 2020", 
           color = "blue", 
           hjust = 0)
print(graph1)

# ---------------------------------------------------------------------------- #
#            5.2 Etude de la saisonnalité pour chaque année                    #
# ---------------------------------------------------------------------------- #

# Calcul de la tendance globale yt
trend_brent = loess(Monthly_Avg_Closed_Cotation ~ as.numeric(Month),
                    data = data_brent, span = 0.2)

# Calcul de la saisonnalité st avec st = yt - mt
data_brent = data_brent %>%
  mutate(
    trend = predict(trend_brent), #mt
    saisonnalités = Monthly_Avg_Closed_Cotation - trend,  #st = yt - mt
    year = year(Month), 
    month_day = format(Month, "%m-%d")
  )

# Convertir month_day en Date
data_brent$month_day <- as.Date(paste("2010", data_brent$month_day, "01", sep = "-"))
Sys.setlocale("LC_TIME", "fr_FR.UTF-8") # mon ordi anglophone donc transformation en fr

# Superpositions de la saisonnalité pour chaque année
ggplot(data_brent, aes(x = month_day, y = saisonnalités, color = factor(year), group = year)) +
  geom_line() +
  labs(
    title = "Composante saisonnière de la cotation du Pétrole Brent (2010-2024)",
    x = "Mois",
    y = "Variation (en $)",
    color = "Année",
    subtitle = "Source : investing.com",
    caption = c("BUT Science des Données - Lisieux","Diop Mandir - Gamondele Maxime - Samake Salif")
  ) +
  scale_x_date(labels = scales::date_format("%B"), breaks = "1 month") +  # Mois sous forme littérale
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, colour = "blue4"),
    plot.caption = element_text(hjust = c(0, 1), face = "bold", colour = "blue4")
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# ---------------------------------------------------------------------------- #
#                  5.3 Etude du brent sur la période 2020-2024                 #
# ---------------------------------------------------------------------------- #

# Filtrage des données pour n'avoir que cette période
data_brent_2020 = data_brent %>%
  filter(Month >= as.Date("2020-01-01"))

# base du graphique
lissage1 = chronique_reg(data_brent_2020,
              "Cotation du pétrole Brent",
              "Monthly_Avg_Closed_Cotation",
              0.2,
              "Janvier 2010 - Octobre 2024",
              "Cotation (en $)",
              "Source : investing.com",
              c("BUT Science des Données - Lisieux","Diop Mandir - Gamondele Maxime - Samake Salif"))
# ajout d'élément de précision 
lissage <- lissage1 + 
  geom_vline(xintercept = as.Date("2020-04-01"),
             color = "blue", 
             linetype = "dashed") + 
  annotate("text", 
           x = as.Date("2020-04-01") +80, 
           y = 30, label = "Crise du Covid-19\navril 2020", 
           color = "blue", 
           hjust = 0)+
  geom_vline(xintercept = as.Date("2022-06-01"),
             color = "brown4", 
             linetype = "dashed") + 
  annotate("text", 
           x = as.Date("2022-06-01") +80, 
           y = 110, label = "Sortie de la crise Covid\nguerre en Ukraine\njuin 2022", 
           color = "brown4", 
           hjust = 0)

# affichage du graph
print(lissage)

# ---------------------------------------------------------------------------- #
#                     5.4 Modele linéaire par morceaux                         #
# ---------------------------------------------------------------------------- #

# Définition des points de rupture afin d'obtenir un meilleur modèle (R^2)
t1 <- as.Date("2020-04-01")
t2 <- as.Date("2022-06-01")
t3 <- as.Date("2023-06-01")
t4 <- as.Date("2024-07-01")

# Conversion de Month en format Date
time <- as.Date(data_brent_2020$Month)

# Variables de temps écoulé depuis les points de rupture
time1 <- pmax(0, as.numeric(time - t1)) 
time2 <- pmax(0, as.numeric(time - t2))
time3 <- pmax(0, as.numeric(time - t3))
time4 <- pmax(0, as.numeric(time - t4))

# Créer un modèle de régression linéaire par morceaux avec lm et les différentes périodes
data_brent_2020 <- data_brent_2020 %>%
  mutate(time = time - min(time))


# Refaire le modèle avec la variable centrée
model1<- lm(Monthly_Avg_Closed_Cotation ~ time + time1 + time2 + time3 + time4, data = data_brent_2020)
summary(model1)

# Ajouter les valeurs prédites au dataset
data_brent_2020 <- data_brent_2020 %>%
  mutate(fitted = predict(model1))

# nouveau graphique prend celui de base + regression
lissage_reg <- lissage1 + 
  geom_line(data = data_brent_2020, aes(x = Month, y = fitted, color = "Régression linéaire"), size = 0.7) +  # Ajout de la couleur et du label pour la légende
  labs(title = "Cotation du pétrole Brent") + 
  scale_color_manual(values = c("Régression linéaire" = "green4", "Lissage" = "red")) + 
  annotate(
    "text", 
    x = min(data_brent_2020$Month) + 230,  
    y = max(data_brent_2020$fitted) * 1, 
    label = "R² = 92,67 %", 
    vjust = 1,
    fontface = "bold",
    color = "green4", 
    size = 4
  )+
  geom_segment(
    aes(
      x = as.Date("2022-04-01"), y = 62.5,         # Point de départ
      xend = as.Date("2021-11-15"), yend = 85.5   # Point d'arrivée
    ),
    arrow = arrow(length = unit(0.2, "cm")),     # Taille et style de la flèche
    color = "green4",
    size = 0.5
  )+
  annotate(
    "text", 
    x = as.Date("2022-04-01"), 
    y = 50, 
    label = "β1 = 0.097\n2.96 $/mois\n35.56 $/an", 
    hjust = -0.2,  # Ajustement horizontal
    vjust = -0.5,  # Ajustement vertical
    fontface = "bold",
    color = "green4", 
    size = 3.5
  )

print(lissage_reg)


# ---------------------------------------------------------------------------- #
#                5.5 Prevision de la valeur du brent sur 26 mois               #
# ---------------------------------------------------------------------------- #

pred = function(date,titre_){
# Filtrer les données à partir de 2020
historique_pred = data_brent %>%
  filter(Month >= as.Date(date))

# Conversion de Month en format Date et centrage des dates
data_brent_pred <- historique_pred %>%
  mutate(time_pred = as.numeric(as.Date(Month) - min(as.Date(Month)))) # Convertir en jours depuis le début

# Création du modèle de régression linéaire
model_pred <- lm(Monthly_Avg_Closed_Cotation ~ time_pred, data = data_brent_pred)
summary(model_pred)

# Création de la période à prédire
data_pred <- data.frame(Month = seq(as.Date("2024-11-01"), as.Date("2026-12-01"), by = "months")) %>%
  mutate(time_pred = as.numeric(as.Date(Month) - min(as.Date(historique_pred$Month)))) # Centrage identique

# Ajout des prédictions au dataset de prédiction
data_pred <- data_pred %>%
  mutate(fitted = predict(model_pred, newdata = ., interval = "prediction"))

# Extraire les colonnes de la matrice 'fitted' et les ajouter au DataFrame
data_pred <- cbind(
  data_pred,
  fit = data_pred$fitted[, "fit"],
  lwr = data_pred$fitted[, "lwr"],
  upr = data_pred$fitted[, "upr"]
)

# Supprimer l'ancienne colonne 'fitted' si elle n'est plus nécessaire
data_pred$fitted <- NULL

# Vérifiez la structure pour confirmer
str(data_pred)
str(historique_pred)

data_brent_final <- full_join(historique_pred, data_pred, by = "Month")

# graphique
epsi <- chronique_reg(
  data_brent_final,
  "Cotation du pétrole Brent",
  "Monthly_Avg_Closed_Cotation",
  0.2,
  "Janvier 2010 - Octobre 2024",
  "Cotation (en $)",
  "Source : investing.com",
  c("BUT Science des Données - Lisieux", "Diop Mandir - Gamondele Maxime - Samake Salif")
) + 
  # Ligne des valeurs prédites
  geom_line(data = data_brent_final, aes(x = Month, y = fit), color = "blue", size = 1) +  
  # Zone de prédiction (intervalle de confiance)
  geom_ribbon(
    data = data_brent_final, 
    aes(x = Month, ymin = lwr, ymax = upr), 
    fill = "blue", 
    alpha = 0.2
  ) +  
  # Titres et légendes
  labs(
    title = titre_,
    x = "Mois",
    y = "Cotation (en $)"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),  # Mettre en gras et augmenter la taille du titre
    plot.subtitle = element_text(hjust = 0.5, colour = "blue4"),
    plot.caption = element_text(hjust = c(0, 1), face = "bold", colour = "blue4"),
    legend.position = "bottom",  
    legend.justification = "center"  # Positionner la légende au centre
  )

# Afficher le graphique
epsi
}

library(patchwork)

# Création des graphiques
p1 <- pred("2010-01-01","2010-01-01")  # Graphique pour la date 2010-01-01
p2 <- pred("2020-01-01")  # Graphique pour la date 2020-01-01
p3 <- pred("2023-01-01")  # Graphique pour la date 2023-01-01
p4 <- pred("2024-01-01")  # Graphique pour la date 2024-01-01

# Combiner les graphiques en un seul avec patchwork
combined_graph <- p1 + p2 + p3 + p4  # Vous pouvez ajuster la disposition comme vous le souhaitez

# Afficher le graphique combiné
combined_graph
