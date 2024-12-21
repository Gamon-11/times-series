###############################################################################
# Formation : BUT Sciences des données (Lisieux) - 
# Module : Projet (M2102) - Analyse de séries temporelles
# Auteur : Mandir DIOP - Salif SAMAKE - Maxime Gamondele
# Création : 12 décembre 2024
# Deadline : 08 Janvier 2025
# Sources : https://fr.investing.com
# Année universitaire : 2024 - 2025
###############################################################################

# Lecture du fichier rds

data <- readRDS("fichier_de_travail.rds")
str(data)

# ============================================================================= #
# ---- Analyse de données ----



######  1. Boxplots des closed cotation annuels pour chaque matière première  ## 

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
    title = "Boxplots annuels des cotations journalières (Closed Cotation)",
    x = "Année", 
    y = "Closed Cotation (en $)",
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

######  2. Évolution mensuelle avec une courbe de régression lissée     ######## 

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
    title = "Évolution moyenne mensuelle de la cotation journaliière de chaque matiéres premières (2010 - 2024)",
    x = "Année",
    y ="Closed Cotation (en $)",
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
