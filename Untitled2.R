
# importation des librairies
library(tabulapdf)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

# importation des jeux de données
D1 = extract_tables("../Data/Futures cacao US - Données Historiques.pdf",col_names = FALSE,method = "stream",output = "character")
D2 = extract_tables("../Data/Futures café US C - Données Historiques.pdf",col_names = FALSE,method = "stream",output = "character")
D3 = extract_tables("../Data/Futures jus dorange - Données Historiques.pdf",col_names = FALSE,method = "stream",output = "character")
D4 = extract_tables("../Data/Futures pétrole Brent - Données Historiques.pdf",col_names = FALSE,method = "stream",output = "character")
D5 = extract_tables("../Data/Futures sucre Londres - Données Historiques.pdf",col_names = FALSE,method = "stream",output = "character")



data <- D1[[1]]

# 1. Séparer les lignes
lines <- strsplit(data, "\n")[[1]]

# 2. Supprimer les lignes non désirées (par exemple, ici la première ligne)
cleaned_lines <- lines[-1]