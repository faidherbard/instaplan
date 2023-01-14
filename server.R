# Define server logic
server <- function(input, output, session) {
  
  fichierInput <- reactive({
    fichier <- input$fichier$datapath
    fichierInput <- NULL
    load("instaplan.date.rda") #Pour synchroniser cette session avec les autres
    
    # Priorité au fichier importé
    if(!is.null(fichier)) {
      fichierInput <- fichier
    } # Puis au fichier distant, lu une fois par heure
    else if (accesDistant && (now()-dateAccesDistant > dhours(1))) {
      dateAccesDistant <- now()
      save(dateAccesDistant, file = "instaplan.date.rda") #On enregistre l'info pour toutes les sessions
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
      save(choixGroupes, file = "instaplan.groupes.rda") #On enregistre l'info pour toutes les sessions
      write_file(read_file(fichierInput()), fichierLocal)
    }
    
    return(tableau)
  })
  
  tableauFiltre <- reactive({
    exceptionGroupes <- setdiff(tableau()$Nom, input$groupes)
    exceptionFilieres <- setdiff(tableau()$`Filière`, input$filieres)
    
    filtrage(tableau(), input$duree,
             ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
             exceptionGroupes, exceptionFilieres,
             input$partiel, input$faible, ymd_hms(input$publication, truncated = 3))
  })
  
  tableauTrie <- reactive({
    tableauF <- tableauFiltre()%>%
      tri(ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
          input$tri, FALSE)
    
    if (input$delta) {
      tableauF <- fusion(tableauF, tableauFiltreRef()) %>%
        tri(ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
            input$tri, input$delta)
    }
    return(tableauF)
  })
  
  tableauFiltreRef <- reactive({
    if (input$delta) {
      exceptionGroupes <- setdiff(tableau()$Nom, input$groupes)
      exceptionFilieres <- setdiff(tableau()$`Filière`, input$filieres)
      
      filtrage(tableau(), input$duree,
               ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
               exceptionGroupes, exceptionFilieres,
               input$partiel, input$faible, ymd_hms(input$reference, truncated = 3))
    }
  })
  
  graphiqueR <- reactive({
    graphique(tableauTrie(), input$duree,
              ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
              dateFichierTexte(fichierInput(), input$publication),
              input$filieres, input$code, input$delta)
  })
  
  output$graphique <- renderPlot({
    graphiqueR()
  }, height = 950)
  
  tableauProjete <- reactive({
    withProgress(
      message = "Calcul en cours",
      detail = "Le temps de calcul est directement lié au nombre d'indisponibilités traitées.
                Pour aller plus vite : réduire la période d'observation ou masquer les arrêts mineurs.",
      value = 0, {
        projection(tableauFiltre(),
                   ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3))
      })
  })
  
  empilementR <- reactive({
    empilement(tableauProjete(), input$duree,
               ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
               dateFichierTexte(fichierInput(), input$publication),
               input$filieres)
  })
  
  output$empilement <- renderPlot({
    empilementR()
  }, height = 950)
  
  tableauGeo <- reactive({
    geolocalisation(tableauFiltre(),
                    ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
                    input$code)
  })
  
  carteR <- reactive({
    withProgress(
      message = "Calcul en cours",
      detail = "Le temps d'affichage est directement lié au nombre d'indisponibilités traitées.
                Pour aller plus vite : réduire la période d'observation ou masquer les arrêts mineurs.",
      value = 0, {
        carte(tableauGeo(), input$duree,
              ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
              dateFichierTexte(fichierInput(), input$publication),
              input$filieres)
      })
  })
  
  output$carte <- renderPlot({
    carteR()
  }, height = 930)
  
  # Telecharger l'image
  output$downloadImage <- downloadHandler(
    filename = paste(format(now(), format = "%Y%m%d"), "Instaplan.png"),
    contentType = "image/png",
    content = function(file) {
      png(file, width = 1000, height = 950)
      print(switch(input$tabset,
                   "Détail par groupe" = graphiqueR(),
                   "Empilement en GW" = empilementR(),
                   "Carte" = carteR()
      ))
      dev.off()
    }
  )
  
  observe({
    # Lire les parametres depuis l'URL
    query <- parseQueryString(session$clientData$url_search)
    
    # Paramètres combinés
    combine = (!is.null(query[['hebdo']]) || !is.null(query[['mensu']]) || !is.null(query[['mensuel']]) || !is.null(query[['annuel']]))
    if (combine) {
      base <- periode <- decalage <- duree <- 0
      if (!is.null(query[['hebdo']])) {
        base = floor_date(now(), "weeks", week_start = 4) - days(1)
        periode = days(13)
        decalage = str_count(query[['hebdo']], " ") - str_count(query[['hebdo']], "-") # L'URL transforme + en espace
        duree = 1
      }
      if (!is.null(query[['mensu']]) || !is.null(query[['mensuel']])) {
        base=floor_date(now(), "months")
        periode = months(1)
        decalage = str_count(query[['mensuel']], " ") - str_count(query[['mensuel']], "-")
        duree = 2
      }
      if (!is.null(query[['annuel']])) {
        base=floor_date(now(), "years")
        periode = years(1)
        decalage = str_count(query[['annuel']], " ") - str_count(query[['annuel']], "-")
        duree = 20
      }
      updateDateRangeInput(session, "dateRange", start = base+decalage*periode, end = base+(decalage+1)*periode)
      updateSliderInput(session, "duree", value = duree)
      updateRadioButtons(session, "tri", selected = "paliernom")
      updateCheckboxInput(session, "delta", value = TRUE)
      updateSliderInput(session, "reference", value = base+decalage*periode, timeFormat = "%d/%m/%y")
      updatePickerInput(session, "filieres", selected = choixFilieres)
      updateSliderInput(session, "publication", min = base+decalage*periode, timeFormat = "%d/%m/%y")
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
    if (!is.null(query[['faible']])) {
      updateSliderInput(session, "faible", value = as.numeric(query[['faible']]))
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
      updateSliderInput(session, "publication", min=input$reference, timeFormat = "%d/%m/%y")
    }
  })
  
  observeEvent(input$plus, {
    periode = days((input$dateRange[2]-input$dateRange[1])/ddays(1))
    updateDateRangeInput(session, "dateRange", start = input$dateRange[1]+periode, end = input$dateRange[2]+periode)
    updateSliderInput(session, "reference", value = input$reference+periode, timeFormat = "%d/%m/%y")
  })
  
  observeEvent(input$moins, {
    periode = days((input$dateRange[2]-input$dateRange[1])/ddays(1))
    updateDateRangeInput(session, "dateRange", start = input$dateRange[1]-periode, end = input$dateRange[2]-periode)
    updateSliderInput(session, "reference", value = input$reference-periode, timeFormat = "%d/%m/%y")
  })
  
  observeEvent(input$aide, {
    showModal(modalDialog(
      includeHTML("README.html"),
      footer = modalButton("Fermer", icon = icon("ok", lib = "glyphicon")),
      easyClose = TRUE
    ))
  })
  
}
