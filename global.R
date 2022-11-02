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

#Initialisation des autres variables par défaut
debut <- now()-dmonths(2)
fin <- debut+dmonths(13)
duree <- round((fin-debut)/ddays(1)*25/1000)
partiel <- 33 # Indispo d'au moins 33% de la Pmax
code <- TRUE
publication <- now()

#Initialisation de la legende
legendeFilieres <- tibble(
  etiquette = c("Nucléaire 1450 MW","Nucléaire 1300 MW","Nucléaire 900 MW","Gaz fossile","Houille fossile","TAC","STEP","Réservoir hydraulique","Fil de l'eau"),
  filiere = c("Nucléaire","Nucléaire","Nucléaire","Gaz fossile","Houille fossile","Fuel / TAC","Station de transfert d'énergie par pompage hydraulique","Réservoir hydraulique","Fil de l'eau et éclusé hydraulique"),
  palier = c("Nucléaire1500","Nucléaire1300","Nucléaire900","Gaz fossile","Houille fossile","Fuel / TAC","Station de transfert d'énergie par pompage hydraulique","Réservoir hydraulique","Fil de l'eau et éclusé hydraulique"), 
  couleur = c("olivedrab","darkred","royalblue4","seashell4","khaki","purple","royalblue1","lightsteelblue","lightskyblue"))

#Fonction de traitement des donnees EDF
preparation_csv <- function(tableau, xduree = duree, xdebut = debut, xfin = fin, tri = "",
                            xexceptionGroupes = exceptionGroupes, xexceptionFilieres = exceptionFilieres,
                            xpartiel = partiel, xpublication = publication) {
  tableau <- tableau %>%
    mutate(fin=as.Date(ymd_hms(`Date de fin`, tz="Europe/Paris")),
           debut=as.Date(ymd_hms(`Date de début`, tz="Europe/Paris")),
           publication=as.Date(ymd_hms(`Date de publication`, tz="Europe/Paris")),
           duree=fin-debut,
           risque = if_else(str_detect(`Information complémentaire`, "susceptible"), TRUE, NA),
           palier = paste0(`Filière`, if_else(`Filière` == "Nucléaire", as.character(100*round(`Puissance maximale (MW)`/100)), "")),
           code = paste0(substr(gsub('GRAND ', 'G', gsub('ST ', 'SS', Nom)), 1, 3), substr(Nom, nchar(Nom), nchar(Nom)))) %>%
    filter(publication <= xpublication) %>%
    group_by(Identifiant) %>% #on regroupe par identifiant de version
    mutate(indice_max = max(`Numéro de version`)) %>%
    ungroup() %>%
    filter(`Numéro de version` == `indice_max`, #on ne garde que les dernières version d'une indispo
           Type %in% c("Planifiée","Fortuite"),
           duree >= xduree,
           `Date de fin` >= xdebut,
           `Date de début` <= xfin,
           `Puissance disponible (MW)` <= (1-xpartiel/100)*`Puissance maximale (MW)`,
           ! Nom %in% xexceptionGroupes,
           ! `Filière` %in% xexceptionFilieres) %>%
    select(-Status, -Type,-Cause,-`Information complémentaire`,-`Date de début`,-`Date de fin`,
           -`Date de publication`, -`Puissance maximale (MW)`, -publication, -indice_max) %>%
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

#Fonction de création du graphique
graphique_indispo <- function(t, xduree = duree, xdebut = as_date(debut), xfin = as_date(fin),
                              dateFichier = "", filieres = selectionFilieres, xcode = code) {
  codeT <- rep(code, nrow(t))
  
  # Partie graphe
  ggplot(t, aes(xmin = debut, xmax = fin, ymin = ordre-1, ymax = ordre)) +
    labs(title = "Indisponibilités déclarées par EDF",
         subtitle = paste("Planning des arrêts de plus de", xduree, "jours vu au", dateFichier)) +
    #Ajustement du titre et du sous-titre
    theme(plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    #Adaptation de la fenêtre de dessin par un zoom
    coord_cartesian(xlim = c(xdebut, xfin)) +
    #Ajustements de l'axe des abscisses
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", labels = scales::label_date_short(), expand = c(0.01, 0)) +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(hjust = -1), panel.grid.major.x = element_line(color = "grey", size = 0.3),
          panel.grid.minor.x = element_line(color = "ivory", size = 0.3), axis.text = element_text(size = 13)) +
    #Ajustement de l'axe des ordonnées et inversion du sens
    scale_y_reverse(expand = c(0.01, 0)) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
          axis.ticks.y = element_blank()) +
    #Dessin des rectangles principaux, ceux des dates en cours
    geom_rect(aes(fill = palier)) +
    #Ajout d'un calque qui montre la date actuelle
    annotate("rect", xmin = as_date(0), xmax = Sys.Date(), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.25) +
    geom_vline(xintercept = Sys.Date(), colour = "black", linetype = 2) +
    #Ajout du nom
    annotate("text", x = pmin(xfin-10, pmax(xdebut+10, t$debut+(t$fin-t$debut)/2)), y = t$ordre-0.5, label = if_else(codeT, t$code, t$Nom), size = 12/.pt, fontface = 2, colour = if_else(t$palier == "Nucléaire900","grey","black")) +
    #Ajout des alertes
    geom_point(aes(x = pmax(xdebut+1, pmin(xfin-1, fin+3)), y = ordre-0.4, shape = risque), color = "red", stroke = 2, size = 3) +
    #Coloration des catégories
    scale_fill_manual(name = "", values = deframe(select(legendeFilieres, palier, couleur)), limits = deframe(select(filter(legendeFilieres, filiere %in% filieres), palier)), labels = deframe(select(filter(legendeFilieres, filiere %in% filieres), etiquette))) +
    #Motif de l'alerte
    scale_shape_manual(values = 24, na.translate = FALSE, name = "", labels = c("Arrêt susceptible d'être allongé")) +
    #Ajout de la légende
    theme(legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(size = 13)) +
    guides(fill = guide_legend(ncol = 3))
}

#debug
#tableauFiltre <- read_delim("Export_toutes_versions.csv", skip = 1, delim=";", locale=locale(encoding='latin1', decimal_mark=".")) %>%
#  preparation_csv
#graphique_indispo(tableauFiltre)
