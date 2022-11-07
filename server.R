# Define server logic
server <- function(input, output, session) {
  v <- reactiveValues(datapath = NULL)
  
  observeEvent(input$fichier, {
    v$datapath <- "https://www.edf.fr/doaat/export/light/csv"
  })
  
  tableau <- reactive({
    fichier <- v$datapath
    dateFichier <- "01/01/2021 00:00"
    if(!is.null(fichier)) {
      dateFichier <- str_sub(read_lines(fichier, n_max=1, locale=locale(encoding='latin1')), 37, 54)
    }
    
    fichierBase <- "./Export_toutes_versions.csv"
    dateFichierBase <- "01/01/2022 00:00"
    if(file.exists(fichierBase)) {
      dateFichierBase <- str_sub(read_lines(fichierBase, n_max=1, locale=locale(encoding='latin1')), 37, 54)
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
    exceptionGroupes <- setdiff(tableau()$Nom, input$groupes)
    exceptionFilieres <- setdiff(tableau()$`Filière`, input$filieres)
    
    tableauF <- preparation_csv(tableau(), input$duree, input$dateRange[1], input$dateRange[2], input$tri, exceptionGroupes, exceptionFilieres, input$partiel,
                                input$publication)
    
    if (input$delta) {
      tableauF <- full_join(tableauF, tableauFiltreRef(),
                            by = c("Identifiant", "Nom", "Filière", "palier", "code"),
                            suffix = c("", "_ref")) %>%
        select(-ordre_ref, -risque_ref, -duree_ref, -`Numéro de version_ref`, -`Puissance disponible (MW)_ref`)
    } else {
      tableauF <- tableauF %>% mutate(debut_ref = debut, fin_ref = fin)
    }
  })
  
  tableauFiltreRef <- reactive({
    if (input$delta) {
      exceptionGroupes <- setdiff(tableau()$Nom, input$groupes)
      exceptionFilieres <- setdiff(tableau()$`Filière`, input$filieres)
      
      preparation_csv(tableau(), input$duree, input$dateRange[1], input$dateRange[2], input$tri, exceptionGroupes, exceptionFilieres, input$partiel,
                      input$dateRef)
    }
  })
  
  graphique <- reactive({
    fichier <- v$datapath
    fichierBase <- "./Export_toutes_versions.csv"
    dateFichier <- "<date inconnue>"
    if(!is.null(fichier)) {
      dateFichier <- str_sub(read_lines(fichier, n_max=1, locale=locale(encoding='latin1')), 37, 54)
    } else if(file.exists(fichierBase)) {
      dateFichier <- str_sub(read_lines(fichierBase, n_max=1, locale=locale(encoding='latin1')), 37, 54)
    } else {
      return()
    }
    if (input$publication + ddays(1) < dmy_hm(dateFichier, tz="Europe/Paris")) { # Pour afficher l'historique
      dateFichier <- format(input$publication, "%d/%m/%Y")
    }
    
    graphique_indispo(tableauFiltre(), input$duree, input$dateRange[1], input$dateRange[2], dateFichier, input$filieres, input$code)
  })
  
  output$graphique <- renderPlot({
    graphique()
  }, height = 950)
  
  # Telecharger l'image
  output$downloadImage <- downloadHandler(
    filename = paste(format(now(), format = "%Y%m%d"), "Instaplan.png"),
    contentType = "image/png",
    content = function(file) {
      png(file, width = 1000, height = 950)
      print(graphique())
      dev.off()
    }
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
    if (!is.null(query[['delta']])) {
      updateCheckboxInput(session, "delta", value = TRUE)
    }
    
    # Adapter la duree a la fenetre d'observation (uniquement si pas déjà fixée via l'URL)
    if (is.null(query[['duree']])) {
      updateSliderInput(session, "duree", value = round((input$dateRange[2]-input$dateRange[1])/ddays(1)*25/1000))
    }
  })
  
  observe({
    if (input$dateRef) {
      updateSliderInput(session, "publication", min=input$dateRef, timeFormat = "%d/%m/%y")
    }
  })
  
  observeEvent(input$aide, {
    showModal(modalDialog(
      includeHTML("README.html"),
      footer = modalButton("Fermer", icon = icon("ok", lib = "glyphicon")),
      easyClose = TRUE
    ))
  })
  
}
