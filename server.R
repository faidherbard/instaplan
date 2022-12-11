# Define server logic
server <- function(input, output, session) {

  fichierInput <- reactive({
    fichier <- input$fichier$datapath
    fichierInput <- NULL
    load("dateAccesDistant.rda") #Pour synchroniser cette session avec les autres

    # Priorité au fichier importé
    if(!is.null(fichier)) {
      fichierInput <- fichier
    } # Puis au fichier distant, lu une fois par heure
    else if (accesDistant && (now()-dateAccesDistant > dhours(1))) {
      dateAccesDistant <- now()
      save(dateAccesDistant, file = "dateAccesDistant.rda") #On enregistre l'info pour toutes les sessions
      fichierInput <- fichierDistant
    } # Puis au fichier local
    else if (file.exists(fichierLocal)) {
      fichierInput <- fichierLocal
    }
  })

  tableau <- reactive({
    dateFichierLocal <- dmy_hms("01/01/2022", truncated = 3)
    if(file.exists(fichierLocal)) {
      dateFichierLocal <- dateFichier(fichierLocal)
    }

    #Import et traitement du fichier EDF
    tableau <- read_delim(fichierInput(), skip = 1, delim=";", locale=locale(encoding='latin1', decimal_mark="."))
    
    # Mettre a jour base si fichier charge plus recent
    dateFichier <- dateFichier(fichierInput())
    if (dateFichier > dateFichierLocal) {
      choixGroupes <- unique(tableau$Nom)
      save(choixGroupes, file = "choixGroupes.rda") #On enregistre l'info pour toutes les sessions
      write_file(read_file(fichierInput()), fichierLocal)
    }
    
    return(tableau)
  })
  
  tableauFiltre <- reactive({
    exceptionGroupes <- setdiff(tableau()$Nom, input$groupes)
    exceptionFilieres <- setdiff(tableau()$`Filière`, input$filieres)
    
    tableauF <- preparation_csv(tableau(), input$duree,
                                ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
                                input$tri, exceptionGroupes, exceptionFilieres,
                                input$partiel, ymd_hms(input$publication, truncated = 3))
    
    if (input$delta) {
      tableauF <- full_join(tableauF, tableauFiltreRef(),
                            by = c("Identifiant", "Nom", "Filière", "palier", "code"),
                            suffix = c("", "_ref")) %>%
        select(-ordre_ref, -risque_ref, -duree_ref, -`Numéro de version_ref`, -`Puissance disponible (MW)_ref`)
    }
    return(tableauF)
  })
  
  tableauFiltreRef <- reactive({
    if (input$delta) {
      exceptionGroupes <- setdiff(tableau()$Nom, input$groupes)
      exceptionFilieres <- setdiff(tableau()$`Filière`, input$filieres)
      
      preparation_csv(tableau(), input$duree,
                      ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
                      input$tri, exceptionGroupes, exceptionFilieres,
                      input$partiel, ymd_hms(input$dateRef, truncated = 3))
    }
  })
  
  graphique <- reactive({
    dateFichier <- dateFichier(fichierInput())
    if (input$publication + ddays(1) < dateFichier) { # Pour afficher l'historique
      dateFichier <- format(input$publication, "%d/%m/%Y")
    } else {
      dateFichier <- format(dateFichier, "%d/%m/%Y à %H:%M")
    }
    
    graphique_indispo(tableauFiltre(), input$duree,
                      ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
                      dateFichier, input$filieres, input$code, input$delta)
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
  
  tableauProjete <- reactive({
    withProgress(message = "Calcul en cours",
                 detail = "Le temps de calcul est directement lié au nombre d'indisponibilités traitées.
                           Pour aller plus vite : réduire la période d'observation ou masquer les arrêts courts et partiels.",
                 value = 0, {
      projection(tableauFiltre(),
                 ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3))
    })
  })
  
  graphiqueProjete <- reactive({
    dateFichier <- dateFichier(fichierInput())
    if (input$publication + ddays(1) < dateFichier) { # Pour afficher l'historique
      dateFichier <- format(input$publication, "%d/%m/%Y")
    } else {
      dateFichier <- format(dateFichier, "%d/%m/%Y à %H:%M")
    }
    
    graphique_projete(tableauProjete(), input$duree,
                      ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
                      dateFichier, input$filieres)
  })
  
  output$graphiqueProjete <- renderPlot({
    graphiqueProjete()
  }, height = 950)
  
  observe({
    # Lire les parametres depuis l'URL
    query <- parseQueryString(session$clientData$url_search)
    
    # Paramètres combinés
    combine = (!is.null(query[['hebdo']]) || !is.null(query[['mensu']]) || !is.null(query[['mensuel']]) || !is.null(query[['annuel']]))
    if (combine) {
      reference <- periode <- decalage <- duree <- 0
      if (!is.null(query[['hebdo']])) {
        reference = floor_date(now(), "weeks", week_start = 4) - days(1)
        periode = days(13)
        decalage = str_count(query[['hebdo']], " ") - str_count(query[['hebdo']], "-") # L'URL transforme + en espace
        duree = 1
      }
      if (!is.null(query[['mensu']]) || !is.null(query[['mensuel']])) {
        reference=floor_date(now(), "months")
        periode = months(1)
        decalage = str_count(query[['mensuel']], " ") - str_count(query[['mensuel']], "-")
        duree = 2
      }
      if (!is.null(query[['annuel']])) {
        reference=floor_date(now(), "years")
        periode = years(1)
        decalage = str_count(query[['annuel']], " ") - str_count(query[['annuel']], "-")
        duree = 20
      }
      updateDateRangeInput(session, "dateRange", start = reference+decalage*periode, end = reference+(decalage+1)*periode)
      updateSliderInput(session, "duree", value = duree)
      updateRadioButtons(session, "tri", selected = "paliernom")
      updateCheckboxInput(session, "delta", value = TRUE)
      updateSliderInput(session, "dateRef", value = reference+decalage*periode, timeFormat = "%d/%m/%y")
      updatePickerInput(session, "filieres", selected = choixFilieres)
      updateSliderInput(session, "publication", min = reference+decalage*periode, timeFormat = "%d/%m/%y")
    }    
    
    # Paramètres simples
    if (!is.null(query[['debut']])) {
      updateDateRangeInput(session, "dateRange", start = dmy_hms(query[['debut']], truncated = 5))
    }
    if (!is.null(query[['fin']])) {
      updateDateRangeInput(session, "dateRange", end = dmy_hms(query[['fin']], truncated = 5))
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
      if (0 != nrow(filter(selectionGroupesT, code %in% c("tout")))) {
        selectionGroupesT <- select(choixGroupesT, code)
      }
      selectionGroupes <- deframe(select(left_join(selectionGroupesT, choixGroupesT, by = "code"), nom))
      updatePickerInput(session, "groupes", selected = selectionGroupes)
    }
    if (!is.null(query[['filieres']])) {
      choixFilieresT <- tibble(nom = unique(tableau()$`Filière`), code = str_to_upper(substr(gsub('é', 'e', nom), 1, 3)))
      selectionFilieresT <- tibble(code = unlist(strsplit(query[['filieres']], ",")))
      if (0 != nrow(filter(selectionFilieresT, code %in% c("tout")))) {
        selectionFilieresT <- select(choixFilieresT, code)
      }
      selectionFilieres <- deframe(select(left_join(selectionFilieresT, choixFilieresT, by = "code"), nom))
      updatePickerInput(session, "filieres", selected = selectionFilieres)
    }
    if (!is.null(query[['delta']])) {
      updateCheckboxInput(session, "delta", value = TRUE)
    }

    # Adapter la duree a la fenetre d'observation (uniquement si pas déjà fixées via l'URL)
    if (is.null(query[['duree']]) && !combine) {
      updateSliderInput(session, "duree", value = round((input$dateRange[2]-input$dateRange[1])/ddays(1)*25/1000))
    }

    # Adapter la date min historique a la reference (uniquement si pas déjà fixées via l'URL)
    if (!combine) {
      updateSliderInput(session, "publication", min=input$dateRef, timeFormat = "%d/%m/%y")
    }
  })

  observeEvent(input$plus, {
    periode = days((input$dateRange[2]-input$dateRange[1])/ddays(1))
    updateDateRangeInput(session, "dateRange", start = input$dateRange[1]+periode, end = input$dateRange[2]+periode)
    updateSliderInput(session, "dateRef", value = input$dateRef+periode, timeFormat = "%d/%m/%y")
  })
  
  observeEvent(input$moins, {
    periode = days((input$dateRange[2]-input$dateRange[1])/ddays(1))
    updateDateRangeInput(session, "dateRange", start = input$dateRange[1]-periode, end = input$dateRange[2]-periode)
    updateSliderInput(session, "dateRef", value = input$dateRef-periode, timeFormat = "%d/%m/%y")
  })

  observeEvent(input$aide, {
    showModal(modalDialog(
      includeHTML("README.html"),
      footer = modalButton("Fermer", icon = icon("ok", lib = "glyphicon")),
      easyClose = TRUE
    ))
  })
  
}
