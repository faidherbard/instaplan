#Prerequis
library(shiny)
library(shinydashboard, warn.conflicts = FALSE)
library(shinyWidgets)
library(ggplot2)
library(tibble)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)
library(readr)
library(stringr)

#Initialisation du site
link <- "https://applis.shinyapps.io/instaplan/"
accesDistant <- TRUE

#Initialisation des variables persistantes a travers les sessions
dateAccesDistant <- dmy_hms("01/01/2022", truncated = 3)
if(!file.exists("dateAccesDistant.rda")) {
  save(dateAccesDistant, file = "dateAccesDistant.rda")
}
load("dateAccesDistant.rda")
choixGroupes <- NULL
if(!file.exists("choixGroupes.rda")) {
  save(choixGroupes, file = "choixGroupes.rda")
}
load("choixGroupes.rda") #Pour eviter un appel reactive() en plus coté server.R

#Initialisation des données
choixFilieres <- c("Nucléaire","Gaz fossile","Houille fossile","Fuel / TAC",
                   "Station de transfert d'énergie par pompage hydraulique","Réservoir hydraulique","Fil de l'eau et éclusé hydraulique")

#Initialisation de la selection par défaut : tout sauf exceptions
exceptionGroupes<-c("FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", "CORDEMAIS 3", #Arrêt définitif
                    "RINGVAART STEG", "SERAING TV", "SERAING TG1", "SERAING TG2") #Belgique
exceptionFilieres<-c("Station de transfert d'énergie par pompage hydraulique", "Réservoir hydraulique", "Fil de l'eau et éclusé hydraulique")
selectionGroupes <- setdiff(choixGroupes, exceptionGroupes)
selectionFilieres <- setdiff(choixFilieres, exceptionFilieres)

#Initialisation des autres variables par défaut
fichierDistant <- "https://www.edf.fr/doaat/export/light/csv"
fichierLocal <- "./Export_toutes_versions.csv"
debut <- now()-dmonths(2)
fin <- debut+dmonths(13)
duree <- round((fin-debut)/ddays(1)*25/1000)
partiel <- 33 # Indispo d'au moins 33% de la Pmax
code <- TRUE
publication <- now()
dateRef <- dmy_hms("15/09/2022", truncated = 3)
delta <- FALSE

#Initialisation de la legende
legendeFilieres <- tibble(
  etiquette = c("Nucléaire 1450 MW","Nucléaire 1300 MW","Nucléaire 900 MW","Gaz fossile","Houille fossile","Fuel / TAC","STEP","Réservoir hydraulique","Fil de l'eau et éclusé"),
  filiere = c("Nucléaire","Nucléaire","Nucléaire","Gaz fossile","Houille fossile","Fuel / TAC","Station de transfert d'énergie par pompage hydraulique","Réservoir hydraulique","Fil de l'eau et éclusé hydraulique"),
  palier = c("Nucléaire1500","Nucléaire1300","Nucléaire900","Gaz fossile","Houille fossile","Fuel / TAC","Station de transfert d'énergie par pompage hydraulique","Réservoir hydraulique","Fil de l'eau et éclusé hydraulique"), 
  couleur = c("olivedrab","darkred","royalblue4","seashell4","khaki","purple","royalblue1","lightsteelblue","lightskyblue"))
legendeDelta <- tibble(
  etiquette = c("Favorable","Défavorable"),
  couleur = c("limegreen","red"))
unitesDate <- c("1 year", "1 month", "1 week", "1 day", "4 hour")
decalageEtiquette <- c(days(2), days(1), hours(4), hours(1))

#Fonction de lecture de la date à partir du CSV EDF
dateFichier <- function(fichier = fichierLocal) {
  fichier %>%
    read_lines(n_max=1, locale=locale(encoding='latin1')) %>%
    str_sub(37, 54) %>%
    paste0(":00") %>%
    dmy_hms(tz="Europe/Paris")
}

#Fonction de traitement des donnees EDF
preparation_csv <- function(tableau, xduree = duree, xdebut = debut, xfin = fin, tri = "",
                            xexceptionGroupes = exceptionGroupes, xexceptionFilieres = exceptionFilieres,
                            xpartiel = partiel, xpublication = publication) {
  tableau <- tableau %>%
    mutate(fin=ymd_hms(`Date de fin`, tz="Europe/Paris"),
           debut=ymd_hms(`Date de début`, tz="Europe/Paris"),
           publication=ymd_hms(`Date de publication`, tz="Europe/Paris"),
           duree=(fin-debut)/ddays(1),
           risque = if_else(str_detect(`Information complémentaire`, "susceptible"), TRUE, NA),
           palier = paste0(`Filière`, if_else(`Filière` == "Nucléaire", as.character(100*round(`Puissance maximale (MW)`/100)), "")),
           code = paste0(substr(gsub('GRAND ', 'G', gsub('ST ', 'SS', Nom)), 1, 3), substr(Nom, nchar(Nom), nchar(Nom)))) %>%
    filter(publication <= xpublication) %>%
    group_by(Identifiant) %>% #on regroupe par identifiant de version
    mutate(indice_max = max(`Numéro de version`)) %>%
    ungroup() %>%
    filter(`Numéro de version` == `indice_max`, #on ne garde que les dernières version d'une indispo
           ! Status %in% c("Annulée", "Supprimée"),
           Type %in% c("Planifiée","Fortuite"),
           duree >= xduree,
           `Date de fin` >= xdebut,
           `Date de début` <= xfin,
           `Puissance disponible (MW)` <= (1-xpartiel/100)*`Puissance maximale (MW)`,
           ! Nom %in% xexceptionGroupes,
           ! `Filière` %in% xexceptionFilieres) %>%
    select(-Status, -Type,-Cause,-`Information complémentaire`,-`Date de début`,-`Date de fin`,
           -`Date de publication`, -publication, -indice_max) %>%
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
graphique_indispo <- function(t, xduree = duree, xdebut = debut, xfin = fin,
                              dateFichier = now(), filieres = selectionFilieres, xcode = code, xdelta = delta) {
  codeT <- rep(xcode, nrow(t))
  if(xdelta) {
    legendeDeltaEtiquette <- legendeDelta$etiquette
  } else {
    legendeDeltaEtiquette <- NULL
    t <- mutate(t, debut_ref = debut, fin_ref = fin)
  }
  decalageDate <- ifelse(xfin-xdebut<dweeks(100), ifelse(xfin-xdebut<dweeks(17), ifelse(xfin-xdebut<ddays(25), 4, 3), 2), 1)
  decalage <- decalageEtiquette[decalageDate]
  
  # Partie graphe
  ggplot(t, aes(xmin = debut, xmax = fin, ymin = ordre-1, ymax = ordre)) +
    labs(title = "Indisponibilités déclarées par EDF",
         subtitle = paste("Planning des arrêts de plus de", xduree, "jours vu au", dateFichier)) +
    #Ajustement du titre et du sous-titre
    theme(plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    #Adaptation de la fenêtre de dessin par un zoom
    coord_cartesian(xlim = c(xdebut, xfin)) +
    #Ajustements de l'axe des abscisses
    scale_x_datetime(date_breaks = unitesDate[decalageDate], date_minor_breaks = unitesDate[1+decalageDate], labels = scales::label_date_short(), expand = c(0.01, 0)) +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(hjust = -1), panel.grid.major.x = element_line(color = "grey", linewidth = 0.3),
          panel.grid.minor.x = element_line(color = "ivory", linewidth = 0.3), axis.text = element_text(size = 13)) +
    #Ajustement de l'axe des ordonnées et inversion du sens
    scale_y_reverse(expand = c(0.01, 0)) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
          axis.ticks.y = element_blank()) +
    #Dessin des indispos en avance
    geom_rect(fill=legendeDelta$couleur[1], xmin = t$debut_ref, xmax = t$fin_ref) +
    #Dessin des indispos principales, celles des dates en cours
    geom_rect(aes(fill = palier)) +
    #Dessin des indispos en retard 
    geom_rect(fill=legendeDelta$couleur[2], xmin = pmin(t$debut, t$debut_ref), xmax = t$debut_ref) +
    geom_rect(fill=legendeDelta$couleur[2], xmin = t$fin_ref, xmax = pmax(t$fin_ref, t$fin)) +
    geom_rect(fill=legendeDelta$couleur[2], data = filter(t, is.na(debut_ref))) +
    #Ajout d'un calque qui montre la date actuelle
    annotate("rect", xmin = dmy_hms("01/01/2000", truncated = 3), xmax = now(), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.25) +
    geom_vline(xintercept = now(), colour = "black", linetype = 2) +
    #Ajout du nom
    annotate("text", x = pmin(xfin-10*decalage, pmax(xdebut+10*decalage, t$debut+(t$fin-t$debut)/2)), y = t$ordre-0.5, label = if_else(codeT, t$code, t$Nom), size = 12/.pt, fontface = 2, colour = if_else(t$palier == "Nucléaire900","grey","black")) +
    #Ajout des alertes
    geom_point(data = filter(t, !is.na(debut)), aes(x = pmax(xdebut+decalage, pmin(xfin-decalage, fin+3*decalage)), y = ordre-0.4, shape = risque), color = "red", stroke = 2, size = 3) +
    #Coloration des catégories
    scale_fill_manual(name = "",
                      values = c(deframe(select(legendeFilieres, palier, couleur)),deframe(legendeDelta)),
                      limits = c(deframe(select(filter(legendeFilieres, filiere %in% filieres), palier)),legendeDeltaEtiquette),
                      labels = c(deframe(select(filter(legendeFilieres, filiere %in% filieres), etiquette)),legendeDeltaEtiquette)) +
    #Motif de l'alerte
    scale_shape_manual(values = 24, na.translate = FALSE, name = "", labels = c("Arrêt susceptible d'être allongé")) +
    #Ajout de la légende
    theme(legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(size = 13)) +
    guides(shape = guide_legend(order = 1), fill = guide_legend(ncol = 2, order = 2))
}

resume_filieres_date <- function(tableau, xdate = now()) {
  tableau %>%
    filter(debut <= xdate, fin >= xdate) %>%
    group_by(palier) %>% 
    summarize(indispo = sum(`Puissance maximale (MW)`-`Puissance disponible (MW)`))
}

projection <- function(tableau, xdebut = debut, xfin = fin) {
  t <- tibble(date = sort(unique(c(tableau$debut, tableau$fin)))) %>%
    add_row(date = xdebut) %>%
    add_row(date = xfin) %>%
    filter(date >= xdebut, date <= xfin) %>%
    rowwise() %>%
    mutate(resume = list(resume_filieres_date(tableau, date))) %>%
    unnest(resume) %>%
    group_by(palier) %>%
    mutate(fin = lead(date, order_by = date) - minutes(1))
  bind_rows(select(t, date, palier, indispo),
            rename(select(t, fin, palier, indispo), date = fin))
}

graphique_projete <- function(t, xduree = duree, xdebut = debut, xfin = fin,
                              dateFichier = now(), filieres = selectionFilieres) {
  decalageDate <- ifelse(xfin-xdebut<dweeks(100), ifelse(xfin-xdebut<dweeks(17), ifelse(xfin-xdebut<ddays(25), 4, 3), 2), 1)
  
  ggplot(t, aes(x=date, y=indispo, fill=palier)) +
    labs(title = "Indisponibilités déclarées par EDF",
         subtitle = paste("Planning des arrêts de plus de", xduree, "jours vu au", dateFichier)) +
    #Ajustement du titre et du sous-titre
    theme(plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    #Adaptation de la fenêtre de dessin par un zoom
    coord_cartesian(xlim = c(xdebut, xfin)) +
    #Ajustements de l'axe des abscisses
    scale_x_datetime(date_breaks = unitesDate[decalageDate], date_minor_breaks = unitesDate[1+decalageDate], labels = scales::label_date_short(), expand = c(0.01, 0)) +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(hjust = -1), panel.grid.major.x = element_line(color = "grey", linewidth = 0.3),
          panel.grid.minor.x = element_line(color = "ivory", linewidth = 0.3), axis.text = element_text(size = 13)) +
    #Ajustement de l'axe des ordonnées et inversion du sens
    scale_y_continuous(labels = scales::label_number(scale = 1/1000, suffix = " GW"), expand = c(0.01, 0)) +
    theme(axis.title.y = element_blank(), axis.text.y = element_text(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    #Dessin des indispos
    geom_area(position = position_stack(reverse = TRUE)) +
    #Ajout d'un calque qui montre la date actuelle
    annotate("rect", xmin = dmy_hms("01/01/2000", truncated = 3), xmax = now(), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.25) +
    geom_vline(xintercept = now(), colour = "black", linetype = 2) +
    #Coloration des catégories
    scale_fill_manual(name = "",
                      values = deframe(select(legendeFilieres, palier, couleur)),
                      limits = deframe(select(filter(legendeFilieres, filiere %in% filieres), palier)),
                      labels = deframe(select(filter(legendeFilieres, filiere %in% filieres), etiquette))) +
    #Ajout de la légende
    theme(legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(size = 13)) +
    guides(fill = guide_legend(ncol = 2))
}

#debug
#tableauFiltre <- preparation_csv(read_delim(fichierLocal, skip = 1, delim=";", locale=locale(encoding='latin1', decimal_mark=".")))
#graphique_indispo(tableauFiltre)
#tableauProjete <- projection(tableauFiltre)
#graphique_projete(tableauProjete)

