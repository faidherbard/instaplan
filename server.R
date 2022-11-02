# Define server logic
server <- function(input, output, session) {
  
  tableau <- reactive({
    fichier <- input$fichier$datapath
    dateFichier <- "01/01/2021 00:00"
    if(!is.null(fichier)) {
      dateFichier <- str_replace(str_sub(read_lines(fichier, n_max=1), 37, 54), "\xe0 ", "") 
    }
    
    fichierBase <- "./Export_toutes_versions.csv"
    dateFichierBase <- "01/01/2022 00:00"
    if(file.exists(fichierBase)) {
      dateFichierBase <- str_replace(str_sub(read_lines(fichierBase, n_max=1), 37, 54), "\xe0 ", "") 
    }
    
    #Import et traitement du fichier EDF
    tableau <- NULL
    if(!is.null(fichier)) {
      tableau <- read_delim(fichier, skip = 1, delim=";", locale=locale(encoding='latin1', decimal_mark="."))
    } else if (file.exists(fichierBase)) {
      tableau <- read_delim(fichierBase, skip = 1, delim=";", locale=locale(encoding='latin1', decimal_mark="."))
    } else {
      return()
    }
    
    # Mettre a jour base si fichier charge plus recent
    if (dmy_hm(dateFichier) > dmy_hm(dateFichierBase)) {
      write_file(read_file(fichier), "./Export_toutes_versions.csv")
      
      # On enregistre le choix pour eviter un rafraichissement reactive()
      choixGroupes <- unique(tableau$Nom)
      save(choixGroupes, file = "local.rda")
    }
    
    return(tableau)
  })
  
  tableauFiltre <- reactive({
    #Dates de filtrage des données et dates du graphe
    xdebut <- input$dateRange[1]
    xfin <- input$dateRange[2]
    duree <- input$duree
    tri <- input$tri
    exceptionGroupes <- setdiff(tableau()$Nom, input$groupes)
    exceptionFilieres <- setdiff(tableau()$`Filière`, input$filieres)
    partiel <- input$partiel
    
    preparation_csv(tableau(), duree, xdebut, xfin, tri, exceptionGroupes, exceptionFilieres, partiel)
  })
  
  graphique <- reactive({
    # Dates de filtrage des données, et dates du graphe
    xdebut <- input$dateRange[1]
    xfin <- input$dateRange[2]
    duree <- input$duree
    fichier <- input$fichier$datapath
    fichierBase <- "./Export_toutes_versions.csv"
    dateFichier <- "<date inconnue>"
    if(!is.null(fichier)) {
      dateFichier <- str_sub(read_lines(fichier, n_max=1, locale=locale(encoding='latin1')), 37, 54)
    } else if(file.exists(fichierBase)) {
      dateFichier <- str_sub(read_lines(fichierBase, n_max=1, locale=locale(encoding='latin1')), 37, 54)
    } else {
      return()
    }
    
    t <- tableauFiltre()
    code <- rep(input$code, nrow(t))
    
    # Partie graphe
    ggplot(t, aes(xmin = debut, xmax = fin, ymin = ordre-1, ymax = ordre)) +
      labs(title = "Indisponibilités déclarées par EDF",
           subtitle = paste("Planning des arrêts de plus de", duree, "jours vu au", dateFichier)) +
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
      annotate("text", x = pmin(xfin-10, pmax(xdebut+10, t$debut+(t$fin-t$debut)/2)), y = t$ordre-0.5, label = if_else(code, t$code, t$Nom), size = 12/.pt, fontface = 2, colour = if_else(t$palier == "Nucléaire900","grey","black")) +
      #Ajout des alertes
      geom_point(aes(x = pmax(xdebut+1, pmin(xfin-1, fin+3)), y = ordre-0.4, shape = risque), color = "red", stroke = 2, size = 3) +
      #Coloration des catégories
      scale_fill_manual(name = "", values = deframe(select(legendeFilieres, palier, couleur)), limits = deframe(select(filter(legendeFilieres, filiere %in% input$filieres), palier)), labels = deframe(select(filter(legendeFilieres, filiere %in% input$filieres), etiquette))) +
      #Motif de l'alerte
      scale_shape_manual(values = 24, na.translate = FALSE, name = "", labels = c("Arrêt susceptible d'être allongé")) +
      #Ajout de la légende
      theme(legend.position = "bottom", legend.box = "horizontal", legend.text = element_text(size = 13)) +
      guides(fill = guide_legend(ncol = 3))
  })
  
  output$distPlot <- renderPlot({
    graphique()
  }, height = 950)
  
  # Telecharger l'image
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(format(Sys.Date(), format = "%Y%m%d"), "indispos EDF.png")
    },
    content = function(file) {
      print(graphique())
      dev.print(file, device = png, width = 1000, height = 950)
    },
    contentType = 'image/png'
  )
  
  observe({
    # Lire les parametres depuis l'URL
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['debut']])) {
      updateDateRangeInput(session, "dateRange", start = as.Date(dmy(query[['debut']])))
    }
    if (!is.null(query[['fin']])) {
      updateDateRangeInput(session, "dateRange", end = as.Date(dmy(query[['fin']])))
    }
    if (!is.null(query[['duree']])) {
      updateSliderInput(session, "duree", value = as.numeric(query[['duree']]))
    }
    if (!is.null(query[['tri']])) {
      updateRadioButtons(session, "tri", selected = query[['tri']])
    }
    if (!is.null(query[['nom']])) {
      updateCheckboxInput(session, "code", value = FALSE)
    }
    if (!is.null(query[['partiel']])) {
      updateSliderInput(session, "partiel", value = as.numeric(query[['partiel']]))
    }
    if (!is.null(query[['groupes']])) {
      choixGroupesT <- tibble(nom = unique(tableau()$Nom), code = paste0(substr(gsub('GRAND ', 'G', gsub('ST ', 'SS', nom)), 1, 3), substr(nom, nchar(nom), nchar(nom))))
      selectionGroupesT <- tibble(code = unlist(strsplit(query[['groupes']], ",")))
      selectionGroupes <- deframe(select(left_join(selectionGroupesT, choixGroupesT, by = "code"), nom))
      updatePickerInput(session, "groupes", selected = selectionGroupes)
    }
    if (!is.null(query[['filieres']])) {
      choixFilieresT <- tibble(nom = unique(tableau()$`Filière`), code = str_to_upper(substr(gsub('é', 'e', nom), 1, 3)))
      selectionFilieresT <- tibble(code = unlist(strsplit(query[['filieres']], ",")))
      selectionFilieres <- deframe(select(left_join(selectionFilieresT, choixFilieresT, by = "code"), nom))
      updatePickerInput(session, "filieres", selected = selectionFilieres)
    }
     
    # Adapter la duree a la fenetre d'observation (uniquement si pas déjà fixée via l'URL)
    if (is.null(query[['duree']])) {
      updateSliderInput(session, "duree", value = round((input$dateRange[2]-input$dateRange[1])/ddays(1)*25/1000))
    }
  })
  
  
  
  
}
