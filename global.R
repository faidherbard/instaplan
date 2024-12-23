#Prerequis
library(tidyverse, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(shiny)
library(shinydashboard, warn.conflicts = FALSE)
library(shinyWidgets)
library(shinyjs, warn.conflicts = FALSE)
library(shinycssloaders)
library(scales, warn.conflicts = FALSE)
library(maps, warn.conflicts = FALSE)
library(mapproj)
library(ggrepel)
library(httr2)

#Initialisation du site
Sys.setenv(TZ="Europe/Paris")
link <- "https://applis.shinyapps.io/instaplan/"
majAuto <- TRUE

#Initialisation des variables persistantes a travers les sessions
tableauLocal <- coordCarte <- coordSites <- coordGroupes <- choixGroupes <- selectionGroupes <- NULL
dateMaj <- dateLocale <- dmy_hms("01/01/2022", truncated = 3)

if(file.exists("instaplan.dateMaj.rda")) {
  load("instaplan.dateMaj.rda")
}

if(file.exists("instaplan.dateLocale.rda")) {
  load("instaplan.dateLocale.rda")
}

#Initialisation groupes
choixGroupesF <- function(tableau) {
  return(tableau %>% select(Nom) %>% unique() %>% arrange(Nom) %>% deframe())
}

if(file.exists("instaplan.tableauLocal.rda")) {
  load("instaplan.tableauLocal.rda")
  choixGroupes <- choixGroupesF(tableauLocal)
}

if(file.exists("instaplan.coord.rda")) {
  load("instaplan.coord.rda")
}

#Initialisation cartographie
initCarto <- function(tableau) {
  choixGroupes <- choixGroupesF(tableau)
  coordCarte <- map_data("world") %>% filter(region=="France", is.na(subregion))
  coordSites <- read_delim("overpass_edf.csv", delim=";", locale=locale(encoding='latin1', decimal_mark=","))
  coordGroupes <-  tibble(Nom = choixGroupes) %>%
    full_join(coordSites, by = "Nom") %>% # Recherche directe du nom 
    mutate(Nom2 = substr(Nom, 1, nchar(Nom)-2)) %>% # Recherche du nom sans le numéro à 1 chiffre
    left_join(coordSites, by = c("Nom2" = "Nom")) %>%
    mutate(lat = coalesce(lat.x, lat.y), long = coalesce(long.x, long.y)) %>%
    select(Nom, lat, long) %>% 
    mutate(Nom3 = substr(Nom, 1, nchar(Nom)-3)) %>% # Recherche du nom sans le numéro à 2 chiffres
    left_join(coordSites, by = c("Nom3" = "Nom")) %>%
    mutate(lat = coalesce(lat.x, lat.y), long = coalesce(long.x, long.y)) %>%
    select(Nom, lat, long) %>%
    mutate(NomP = substr(Nom, 1, nchar(Nom)-6)) %>% # Recherche directe du nom sans le suffixe ' POMPE'
    left_join(coordSites, by = c("NomP" = "Nom")) %>%
    mutate(lat = coalesce(lat.x, lat.y), long = coalesce(long.x, long.y)) %>%
    select(Nom, lat, long) %>%  
    mutate(Nom2P = substr(Nom, 1, nchar(Nom)-8)) %>% # Recherche du nom sans le numéro à 1 chiffre ni le suffixe ' POMPE'
    left_join(coordSites, by = c("Nom2P" = "Nom")) %>%
    mutate(lat = coalesce(lat.x, lat.y), long = coalesce(long.x, long.y)) %>%
    select(Nom, lat, long) %>%
    filter(Nom %in% choixGroupes, !is.na(lat)) %>%
    arrange(Nom)
  save(coordCarte, coordSites, coordGroupes, file = "instaplan.coord.rda")
}

#Initialisation des données
specColNames <- c("Status","Identifiant","Numéro de version","Nom","Filière","Date de début",
                  "Date de fin","Type","Cause","Information complémentaire", "Puissance maximale (MW)",
                  "Puissance disponible (MW)", "Date de publication")
specColTypes = "ccdccTTcccddT"
choixFilieres <- c("Nucléaire","Nucléaire","Nucléaire",
                   "Station de transfert d'énergie par pompage hydraulique","Station de transfert d'énergie par pompage hydraulique",
                   "Réservoir hydraulique", "Fil de l'eau et éclusé hydraulique","Energie marine",
                   "Eolien offshore",
                   "Gaz fossile","Houille fossile","Fuel / TAC")

#Initialisation de la selection par défaut : tout sauf exceptions
exceptionGroupes<-c("FESSENHEIM 1", "FESSENHEIM 2", "HAVRE 4", #Arrêt définitif et indispo en cours
                    "RINGVAART STEG", "SERAING TV", "SERAING TG1", "SERAING TG2") #Belgique
exceptionFilieres<-c("Station de transfert d'énergie par pompage hydraulique", "Réservoir hydraulique", "Fil de l'eau et éclusé hydraulique","Energie marine",
                     "Eolien offshore")
if (!is.null(choixGroupes)) {
  selectionGroupes <- setdiff(choixGroupes, exceptionGroupes)
}
selectionFilieres <- setdiff(choixFilieres, exceptionFilieres)

#Initialisation des autres variables par défaut
fichierDistant <- "https://www.edf.fr/doaat/export/light/csv"
debut <- now()-dmonths(2)
fin <- debut+dmonths(13)
duree <- round((fin-debut)/ddays(1)*25/1000)
partiel <- 33 # Indispo d'au moins 33% de la Pmax
faible <- 85 # Indispo d'au moins 85 MW (Pmax TAC)
code <- TRUE
publication <- now()
reference <- dmy_hms("15/09/2023", truncated = 3)
delta <- FALSE

#Initialisation de la legende
legendeFilieres <- tibble(
  etiquette = c("Nucléaire 1500 MW","Nucléaire 1300 MW","Nucléaire 900 MW",
                "STEP","STEP Pompe","Réservoir hydraulique", "Fil de l'eau et éclusé","Energie marine",
                "Eolien offshore",
                "Gaz fossile","Houille fossile","Fuel / TAC"),
  palier = c("Nucléaire1500","Nucléaire1200","Nucléaire900",
             "Station de transfert d'énergie par pompage hydraulique","Station de transfert d'énergie par pompage hydraulique Pompe",
             "Réservoir hydraulique","Fil de l'eau et éclusé hydraulique","Energie marine",
             "Eolien offshore",
             "Gaz fossile","Houille fossile","Fuel / TAC"),
  filiere = choixFilieres,
  couleur = c("olivedrab","darkred","royalblue4",
              "royalblue3","royalblue1","lightsteelblue","lightskyblue","navy",
              "turquoise",
              "seashell4","khaki","purple"))
legendeDelta <- tibble(
  etiquette = c("Favorable","Défavorable"),
  couleur = c("limegreen","red"))
unitesDate <- c("1 year", "1 month", "1 week", "1 day", "4 hour")
decalageEtiquette <- c(days(2), days(1), hours(4), hours(1))

#Options ui.R
options(spinner.type = 6, spinner.size = 2, spinner.color = "#3c8dbc")

#Fonction de lecture de la date à partir du CSV EDF
dateFichier <- function(fichier) {
  date <- fichier %>%
    read_lines(n_max=1, locale=locale(encoding='latin1')) %>%
    str_sub(37, 54) %>%
    paste0(":00") %>%
    dmy_hms(tz="Europe/Paris", truncated = 3) %>%
    as.POSIXct()
}

dateTexte <- function(date = dateLocale, xpublication = publication) {
  if (xpublication + ddays(1) < date) { # Pour afficher l'historique
    format(xpublication, "%d/%m/%Y")
  } else {
    format(date, "%d/%m/%Y à %H:%M")
  }
}

#Fonction d'affichage court de la date
dateCourteTexte <- function(date = debut, reference = debut) {
  case_when(year(date) == year(reference) ~ format(date, "%d/%m"),
            TRUE ~ format(date, "%d/%m/%y"))
}

#Fonction de création du code à partir du nom du groupe
codeGroupe <- function(NomP) {
  Nom <- gsub(' POMPE', '', NomP)
  paste0(substr(gsub('GRAND ', 'G', gsub('ST ', 'SS', gsub('MONTE', 'MT', Nom))), 1, 3),
         case_when(substr(Nom, nchar(Nom)-1, nchar(Nom)-1) == ' ' ~ substr(Nom, nchar(Nom), nchar(Nom)),
                   substr(Nom, nchar(Nom)-1, nchar(Nom)-1) == '1' ~ substr(Nom, nchar(Nom)-1, nchar(Nom)),
                   TRUE ~ ""),
         case_when(Nom != NomP  ~ "P",
                   TRUE ~ ""))
}

#Fonction de préparation
preparation <- function(tableau) {
  mutate(tableau,
         fin=ymd_hms(`Date de fin`, tz="Europe/Paris", truncated = 3),
         debut=ymd_hms(`Date de début`, tz="Europe/Paris", truncated = 3),
         publication=ymd_hms(`Date de publication`, tz="Europe/Paris", truncated = 3),
         duree=(fin-debut)/ddays(1),
         risque = case_when(str_detect(`Information complémentaire`, "susceptible") ~ TRUE),
         palier = factor(paste0(`Filière`,
                                case_when(`Filière` == "Nucléaire" ~ as.character(300*round(`Puissance maximale (MW)`/300)),
                                          grepl(" POMPE", Nom) ~ " Pompe",
                                          TRUE ~ "")),
                         levels = deframe(select(legendeFilieres, palier))),
         code = codeGroupe(Nom)) %>%
    group_by(Identifiant) %>% #on regroupe par identifiant de version
    mutate(indice_max = max(`Numéro de version`)) %>%
    ungroup() %>%
    # La "Notice utilisateur des données publiées au titre du règlement REMIT et mises à disposition sur le site edf.fr"
    # indique que le statut "Inactive" est utilisé "s'il ne s’agit pas de la dernière version communiquée au marché".
    # Ce statut est aussi utilisé pour les indispos passées (affichées dans Instaplan avec la fonctionnalité "historique")
    # Les indispos présentes et futures au statut "Inactive" sont donc des erreurs, d'ailleurs absentes du site EDF
    mutate(Status = replace(Status,
                            Status == "Inactive" & `Numéro de version` == `indice_max` &`Date de fin` >= now(),
                            "Bug"))  %>%
    filter(Type %in% c("Planifiée","Fortuite")) %>%
    select(-Type,-Cause,-`Information complémentaire`,-`Date de début`,-`Date de fin`,-`Date de publication`)
}

#Fonction d'historique
historique <- function(tableau, xpublication = publication) {
  filter(tableau, publication <= xpublication) %>% #consultation historique
    group_by(Identifiant) %>%
    mutate(indice_max = max(`Numéro de version`)) %>% #on doit donc renumeroter
    ungroup() %>%
    filter(`Numéro de version` == `indice_max`, #on ne garde que les dernières version d'une indispo
           ! Status %in% c("Annulée", "Supprimée", "Bug")) %>%
    select(-Status,-publication, -indice_max)
}

#Fonction de filtrage
filtrage <- function(tableau, xduree = duree, xdebut = debut, xfin = fin,
                     xexceptionGroupes = exceptionGroupes, xexceptionFilieres = exceptionFilieres,
                     xpartiel = partiel, xfaible = faible) {
  filter(tableau,
         duree >= xduree, fin >= xdebut, debut <= xfin,
         `Puissance disponible (MW)` <= (1-xpartiel/100)*`Puissance maximale (MW)`,
         `Puissance maximale (MW)`-`Puissance disponible (MW)` >= xfaible,
         ! Nom %in% xexceptionGroupes,
         ! `Filière` %in% xexceptionFilieres)
}

#Fonction de tri
tri <- function(tableau, xdebut = debut, xfin = fin, tri = "", xdelta = delta) {
  if(!xdelta) {
    tableau <- mutate(tableau, debut_ref = debut, fin_ref = fin)
  }
  tableau <- tableau %>%
    arrange(switch(tri, palier = palier, paliernom = palier, filiere = `Filière`, filierenom = `Filière`, ""),
            switch(tri, paliernom = "", filierenom = "", nom = "", pmin(xfin, if_else(is.na(fin), fin_ref, fin))),
            switch(tri, paliernom = "", filierenom = "", nom = "", pmax(xdebut, if_else(is.na(debut), debut_ref, debut))),
            Nom) %>% # On trie et on numérote
    mutate(ordre = row_number()) %>%
    group_by(Nom) %>% # On regroupe par unité de production pour avoir les indisponibilités d'une unité sur la même ligne
    mutate(ordre = min(ordre)) %>%
    ungroup()
  if(!xdelta) {
    tableau <- select(tableau, -debut_ref, -fin_ref)
  }
  ordres <- tableau %>% # On supprime les lignes vides en re numérotant
    distinct(Nom, .keep_all = TRUE) %>%
    arrange(ordre) %>%
    mutate(ordre = row_number()) %>%
    select(Nom, ordre)
  left_join(tableau, ordres, by="Nom", suffix = c("_old", "")) %>%
    select(-ordre_old)
}

#Fonction de fusion entre tableau et référence
fusion <- function(t, ref) {
  full_join(t, ref,
            by = c("Identifiant", "Nom", "Filière", "palier", "code", "Puissance maximale (MW)"),
            suffix = c("", "_ref")) %>%
    select(-risque_ref, -duree_ref, -`Numéro de version_ref`, -`Puissance disponible (MW)_ref`)
}

#Fonction de création du graphique
graphique <- function(t, xduree = duree, xdebut = debut, xfin = fin,
                      dateTexte = now(), filieres = selectionFilieres, xcode = code, xdelta = delta) {
  legendeDeltaEtiquette <- legendeDelta$etiquette
  if(!xdelta) {
    legendeDeltaEtiquette <- NULL
    t <- mutate(t, debut_ref = debut, fin_ref = fin)
  } else { # On ajoute de données vides pour éviter show.legend = TRUE nécessaire depuis ggplot 3.5.0
    t <- t %>%
      add_row(palier = "Favorable") %>%
      add_row(palier = "Défavorable")
  }
  decalageDate <- ifelse(xfin-xdebut<dweeks(100), ifelse(xfin-xdebut<dweeks(17), ifelse(xfin-xdebut<ddays(25), 4, 3), 2), 1)
  decalage <- decalageEtiquette[decalageDate]
  eval(xcode) # Contournement de bug : variable "xcode" non réactive dans graphique()
  
  # Partie graphe
  ggplot(t, aes(xmin = debut, xmax = fin, x = pmin(xfin-10*decalage, pmax(xdebut+10*decalage, debut+(fin-debut)/2)),
                ymin = ordre-1, ymax = ordre, y = ordre-0.5,
                label = case_when(xcode ~ code, TRUE ~ Nom), fill = palier)) +
    labs(title = "Indisponibilités déclarées par EDF",
         subtitle = paste("Planning des arrêts de plus de", xduree, "jours vu au", dateTexte)) +
    #Ajustement du titre et du sous-titre
    theme(plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    #Adaptation de la fenêtre de dessin par un zoom
    coord_cartesian(xlim = c(xdebut, xfin)) +
    #Ajustements de l'axe des abscisses
    scale_x_datetime(date_breaks = unitesDate[decalageDate], date_minor_breaks = unitesDate[1+decalageDate], labels = label_date_short(), expand = c(0.01, 0)) +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(hjust = -1), panel.grid.major.x = element_line(color = "grey", linewidth = 0.3),
          panel.grid.minor.x = element_line(color = "ivory", linewidth = 0.3), axis.text = element_text(size = 13)) +
    #Ajustement de l'axe des ordonnées et inversion du sens
    scale_y_reverse(expand = c(0.01, 0)) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
          axis.ticks.y = element_blank()) +
    #Dessin des indispos en avance
    geom_rect(fill=legendeDelta$couleur[1], aes(xmin = debut_ref, xmax = fin_ref)) +
    #Dessin des indispos principales, celles des dates en cours
    geom_rect() +
    #Dessin des indispos en retard 
    geom_rect(fill=legendeDelta$couleur[2], aes(xmin = pmin(debut, debut_ref), xmax = debut_ref)) +
    geom_rect(fill=legendeDelta$couleur[2], aes(xmin = fin_ref, xmax = pmax(fin_ref, fin))) +
    geom_rect(data = filter(t, is.na(debut_ref)), fill=legendeDelta$couleur[2]) +
    #Ajout d'un calque qui montre la date actuelle
    annotate("rect", xmin = dmy_hms("01/01/2000", truncated = 3), xmax = now(), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.25) +
    geom_vline(xintercept = now(), colour = "black", linetype = 2) +
    #Ajout du nom
    geom_text(size = 12/.pt, fontface = 2, aes(colour = (palier %in% c("Nucléaire900", "Energie marine")))) +
    geom_text(data = filter(t, is.na(debut)), size = 12/.pt, fontface = 2,
              aes(x = pmin(xfin-10*decalage, pmax(xdebut+10*decalage, debut_ref+(fin_ref-debut_ref)/2)))) +
    #Ajout des alertes
    geom_point(data = filter(t, !is.na(debut)), color = "red", stroke = 2, size = 3, fill = NA,
               aes(x = pmax(xdebut+decalage, pmin(xfin-decalage, fin+3*decalage)), y = ordre-0.4, shape = risque)) +
    #Coloration des catégories
    scale_fill_manual(name = "",
                      values = c(deframe(select(legendeFilieres, palier, couleur)),deframe(legendeDelta)),
                      limits = c(deframe(select(filter(legendeFilieres, filiere %in% filieres), palier)),legendeDeltaEtiquette),
                      labels = c(deframe(select(filter(legendeFilieres, filiere %in% filieres), etiquette)),legendeDeltaEtiquette),
                      drop = FALSE) +
    scale_colour_manual(values = c("black", "grey")) +
    #Motif de l'alerte
    scale_shape_manual(values = 24, na.translate = FALSE, name = "", labels = c("Arrêt susceptible d'être allongé")) +
    #Ajout de la légende
    theme(legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(size = 13)) +
    guides(shape = guide_legend(order = 1), fill = guide_legend(ncol = 2, order = 2), colour = "none")
}

projectionDate <- function(tableau, xdate = now(), progres = 1) {
  if (isRunning()) {
    incProgress(progres)
  }
  tableau %>%
    filter(debut <= xdate, fin > xdate) %>%
    group_by(Nom) %>%
    mutate(`Puissance disponible (MW)` = min(`Puissance disponible (MW)`)) %>%
    group_by(palier) %>% 
    summarize(indispo = sum(`Puissance maximale (MW)`-`Puissance disponible (MW)`)) %>%
    full_join(unique(select(tableau, palier)), by = "palier") %>%
    mutate_if(is.numeric, coalesce, 0)
}

projection <- function(tableau, xdebut = debut, xfin = fin) {
  t <- tibble(date = unique(c(tableau$debut, tableau$fin))) %>%
    add_row(date = xdebut) %>%
    add_row(date = xfin) %>%
    arrange(date) %>%
    filter(date >= xdebut, date <= xfin) %>%
    rowwise() %>%
    mutate(resume = list(projectionDate(tableau, date, 1/nrow(.)))) %>%
    unnest(resume) %>%
    group_by(palier) %>%
    mutate(fin = lead(date, order_by = date) - minutes(1))
  bind_rows(select(t, date, palier, indispo),
            rename(select(t, fin, palier, indispo), date = fin))
}

empilement <- function(t, xduree = duree, xdebut = debut, xfin = fin,
                       dateTexte = now(), filieres = selectionFilieres) {
  volumes <- arrange(t, date) %>%
    mutate(area_rectangle = (lead(date) - date) * pmin(indispo, lead(indispo)),
           area_triangle = 0.5 * (lead(date) - date) * abs(indispo - lead(indispo))) %>%
    summarise(indispoGWh = round(sum(area_rectangle + area_triangle, na.rm = TRUE)
                                 /(1e6*dhours(1))*10)/10) #arrondi à 0,1 TWh
  
  decalageDate <- ifelse(xfin-xdebut<dweeks(100), ifelse(xfin-xdebut<dweeks(17), ifelse(xfin-xdebut<ddays(25), 4, 3), 2), 1)
  
  ggplot(t, aes(x=date, y=indispo, fill=palier)) +
    labs(title = "Indisponibilités déclarées par EDF",
         subtitle = paste("Planning des arrêts de plus de", xduree, "jours vu au", dateTexte)) +
    #Ajustement du titre et du sous-titre
    theme(plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    #Adaptation de la fenêtre de dessin par un zoom
    coord_cartesian(xlim = c(xdebut, xfin)) +
    #Ajustements de l'axe des abscisses
    scale_x_datetime(date_breaks = unitesDate[decalageDate], date_minor_breaks = unitesDate[1+decalageDate], labels = label_date_short(), expand = c(0.01, 0)) +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(hjust = -1), panel.grid.major.x = element_line(color = "grey", linewidth = 0.3),
          panel.grid.minor.x = element_line(color = "ivory", linewidth = 0.3), axis.text = element_text(size = 13)) +
    #Ajustement de l'axe des ordonnées et inversion du sens
    scale_y_continuous(position = "right", labels = label_number(scale = 1/1000, suffix = " GW", decimal.mark = ","), breaks = breaks_extended(7), expand = c(0.02, 0)) +
    theme(axis.title.y = element_blank(), axis.text.y = element_text(face = "bold")) +
    #Dessin des indispos
    geom_area(position = position_stack(reverse = TRUE)) +
    #Ajout d'un calque qui montre la date actuelle
    annotate("rect", xmin = dmy_hms("01/01/2000", truncated = 3), xmax = now(), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.25) +
    geom_vline(xintercept = now(), colour = "black", linetype = 2) +
    #Coloration des catégories
    scale_fill_manual(name = "",
                      values = deframe(select(legendeFilieres, palier, couleur)),
                      limits = deframe(select(filter(legendeFilieres, filiere %in% filieres), palier)),
                      labels = str_glue_data(select(filter(left_join(legendeFilieres, volumes, by = "palier"),
                                                           filiere %in% filieres),
                                                    etiquette, indispoGWh),
                                             "{etiquette} ({indispoGWh} TWh)")) +
    #Ajout de la légende
    theme(legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(size = 13)) +
    guides(fill = guide_legend(ncol = 2))
}

geolocalisation <- function(t, xdebut = debut, xfin = fin, xcode = code) {
  left_join(t, coordGroupes, by = "Nom") %>%
    filter(!is.na(debut)) %>% # Supprimer les arrets annules
    arrange(Nom) %>%
    mutate(texte = paste0(code, ". ",
                          case_when(debut > xdebut ~ dateCourteTexte(debut, xdebut), TRUE ~ ""),
                          "->", dateCourteTexte(fin, xdebut),
                          case_when(risque ~ " /!\\", TRUE ~ ""))) %>%
    group_by(lat, long) %>%
    summarise(texte = paste(texte, collapse='\n'),
              palier = last(palier),
              .groups = "keep") %>%
    left_join(coordSites, by = c("lat", "long")) %>%
    mutate(code = substr(gsub('GRAND ', 'G', gsub('ST ', 'SS', gsub('MONTE', 'MT', Nom))), 1, 3),
           texte = case_when(xcode ~ texte, TRUE ~ paste0(Nom, '\n', gsub(code, '', texte))))
}

carte <- function(t, xduree = duree, xdebut = debut, xfin = fin,
                  dateTexte = now(), filieres = selectionFilieres) {
  ggplot(t, aes(x = long, y = lat, fill = palier)) +
    labs(title = "Indisponibilités déclarées par EDF",
         subtitle = paste("Planning des arrêts de plus de", xduree, "jours vu au", dateTexte)) +
    theme_void() +
    #Ajustement du titre et du sous-titre
    theme(plot.title = element_text(hjust = 0.5, size = 15), plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    geom_polygon(data = coordCarte, aes(x = long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_point() +
    #Ajout du nom
    geom_label_repel(aes(label = texte, colour = (palier %in% c("Nucléaire900", "Energie marine"))),
                     size = 8/.pt, fontface = 2, hjust = 0,
                     box.padding = 0.2, min.segment.length = 0, label.r = 0, point.size = 2, ylim = c(-Inf, NA),
                     max.overlaps = Inf, max.time = 2, max.iter = 100000) +
    coord_quickmap(clip = "off") +
    #Coloration des catégories
    scale_fill_manual(name = "",
                      values = deframe(select(legendeFilieres, palier, couleur)),
                      limits = deframe(select(filter(legendeFilieres, filiere %in% filieres), palier)),
                      labels = deframe(select(filter(legendeFilieres, filiere %in% filieres), etiquette))) +
    scale_colour_manual(values = c("black", "grey")) +
    #Ajout de la légende
    theme(legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(size = 13)) +
    guides(fill = guide_legend(ncol = 2, override.aes = aes(label = "")), colour = "none")
}

#debug
#tableau <- read_delim(fichier, skip = 2, delim=";", locale=locale(encoding='latin1', decimal_mark=","),
#                       col_names = specColNames, col_types = specColTypes) %>% preparation()
#tableauFiltre <- historique(tableau) %>% filtrage()
#tableauFiltreRef <- historique(tableau, reference) %>% filtrage()
#tableauTrie <- tri(fusion(tableauFiltre, tableauFiltreRef), xdelta = TRUE)
#graphique(tableauTrie, xdelta = TRUE)
#tableauProjete <- projection(tableauFiltre)
#empilement(tableauProjete)
#tableauGeo <- geolocalisation(tableauFiltre)
#carte(tableauGeo)
