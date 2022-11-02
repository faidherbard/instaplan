#Prerequis
library(shiny)
library(shinydashboard, warn.conflicts = FALSE)
library(shinyWidgets)
library(ggplot2)
library(tibble)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(readr)
library(stringr)

#Initialisation du site
link <- "https://faidherbard.shinyapps.io/instaplan/"

#Initialisation des données
choixFilieres <- c("Nucléaire","Gaz fossile","Houille fossile","Fuel / TAC",
                   "Station de transfert d'énergie par pompage hydraulique","Réservoir hydraulique","Fil de l'eau et éclusé hydraulique")

#Initialisation filtres a partir de la sauvegarde si elle existe
choixGroupes <- NULL
if(!file.exists("local.rda")) {
  save(choixGroupes, file = "local.rda")
}
load("local.rda")

#Initialisation de la selection par défaut : tout sauf exceptions
exceptionGroupes<-c("FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "CORDEMAIS 3", #Arrêt définitif
                    "RINGVAART STEG", "SERAING TV", "SERAING TG1", "SERAING TG2") #Belgique
exceptionFilieres<-c("Station de transfert d'énergie par pompage hydraulique", "Réservoir hydraulique", "Fil de l'eau et éclusé hydraulique")
selectionGroupes <- setdiff(choixGroupes, exceptionGroupes)
selectionFilieres <- setdiff(choixFilieres, exceptionFilieres)

#Initialisation fenetre d'observation par défaut
debut <- Sys.Date()-dmonths(2)
fin <- debut+dmonths(13)

#Initialisation de la legende
legendeFilieres <- tibble(
  etiquette = c("Nucléaire 1450 MW","Nucléaire 1300 MW","Nucléaire 900 MW","Gaz fossile","Houille fossile","TAC","STEP","Réservoir hydraulique","Fil de l'eau"),
  filiere = c("Nucléaire","Nucléaire","Nucléaire","Gaz fossile","Houille fossile","Fuel / TAC","Station de transfert d'énergie par pompage hydraulique","Réservoir hydraulique","Fil de l'eau et éclusé hydraulique"),
  palier = c("Nucléaire1500","Nucléaire1300","Nucléaire900","Gaz fossile","Houille fossile","Fuel / TAC","Station de transfert d'énergie par pompage hydraulique","Réservoir hydraulique","Fil de l'eau et éclusé hydraulique"), 
  couleur = c("olivedrab","darkred","royalblue4","seashell4","khaki","purple","royalblue1","lightsteelblue","lightskyblue"))

#Fonction de traitement des donnees EDF
preparation_csv <- function(tableau, xduree, xdebut, xfin, tri, exceptionGroupes, exceptionFilieres, partiel) {
  tableau <- tableau %>%
    mutate(fin=as.Date(ymd_hms(`Date de fin`, tz="Europe/Paris")),
           debut=as.Date(ymd_hms(`Date de début`, tz="Europe/Paris")),
           duree=fin-debut,
           risque = if_else(str_detect(`Information complémentaire`, "susceptible"), TRUE, NA),
           palier = paste0(`Filière`, if_else(`Filière` == "Nucléaire", as.character(100*round(`Puissance maximale (MW)`/100)), "")),
           code = paste0(substr(gsub('GRAND ', 'G', gsub('ST ', 'SS', Nom)), 1, 3), substr(Nom, nchar(Nom), nchar(Nom)))) %>%
    filter(Status == "Active",
           Type %in% c("Planifiée","Fortuite"),
           duree >= xduree,
           `Date de fin` >= xdebut,
           `Date de début` <= xfin,
           `Puissance disponible (MW)` <= (1-partiel/100)*`Puissance maximale (MW)`,
           ! Nom %in% exceptionGroupes,
           ! `Filière` %in% exceptionFilieres) %>%
    select(-Type,-Cause,-`Information complémentaire`,-`Date de début`,-`Date de fin`,
           -`Date de publication`, -`Puissance maximale (MW)`) %>%
    arrange(switch(tri, palier = palier, paliernom = palier, filiere = `Filière`, filierenom = `Filière`, ""),
            switch(tri, paliernom = "", filierenom = "", nom = "", pmin(xfin, fin)),
            switch(tri, paliernom = "", filierenom = "", nom = "", pmax(xdebut, debut)),
            Nom) %>% # On trie et on numérote
    mutate(ordre = row_number()) %>%
    group_by(Nom) %>% # On regroupe par unité de production pour avoir les indisponibilités d'une unité sur la même ligne
    mutate(ordre = min(ordre)) %>%
    ungroup()
  ordres <- tableau %>% # On supprime les lignes vides en re numérotant
    distinct(Nom, .keep_all = TRUE) %>%
    arrange(ordre) %>%
    mutate(ordre = row_number()) %>%
    select(Nom, ordre)
  left_join(tableau, ordres, by="Nom", suffix = c("_old", "")) %>%
    select(-ordre_old)
}
