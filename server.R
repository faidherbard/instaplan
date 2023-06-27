# Define server logic
server <- function(input, output, session) {
  
  debounced_groupes <- shiny::debounce(reactive({input$groupes}), 1500)
  debounced_filieres <- shiny::debounce(reactive({input$filieres}), 1500)
  
  fichierInput <- reactive({
    fichierInput <- NULL
    if(file.exists("instaplan.date.rda")) {
      load("instaplan.date.rda") #Pour synchroniser cette session avec les autres
    }
    
    # Priorité au fichier importé
    if (!is.null(input$fichier$datapath)) {
      fichierInput <- input$fichier$datapath
    } # Puis au fichier distant, lu une fois par heure
    else if (majAuto && (now()-dateMaj > dhours(1))) {
      dateMaj <- now()
      save(dateMaj, file = "instaplan.date.rda") #On enregistre l'info pour toutes les sessions
      fichierInput <- fichierDistant
    } # Sinon tableauLocal et dateLocale
  })
  
  tableauR <- reactive({
    if(!is.null(fichierInput())) {
      #Import et traitement du fichier EDF
      tableau <- read_delim(fichierInput(), skip = 1, delim=";",
                            locale=locale(encoding='latin1', decimal_mark=".")) %>%
        preparation()
      
      # Mettre a jour base si fichier charge plus recent et enrichir avec historique
      dateFichier <- dateR()
      if(dateFichier > dateLocale) {
        if(file.exists("instaplan.indispo.rda") & !is.null(tableauLocal)) {
          load("instaplan.indispo.rda") #Pour synchroniser cette session avec les autres
          tableau <- union(select(tableau, -Status), select(tableauLocal, -Status)) %>%
            full_join(tableau) %>%
            replace_na(list(Status = "Inactive"))
        }
        else { # Premier lancement
          choixGroupes <- choixGroupesF(tableau)
          selectionGroupes <- setdiff(choixGroupes, exceptionGroupes)
          updatePickerInput(session, "groupes", choices = choixGroupes, selected = selectionGroupes)
          initCarto(tableau)
        }
        tableauLocal <- tableau
        dateLocale <- dateFichier
        save(tableauLocal, dateLocale, file = "instaplan.indispo.rda") #On enregistre l'info pour toutes les sessions
      } 
      return(tableau)
    }
    else {
      return(tableauLocal)
    }
  })
  
  dateR <- reactive({
    if(!is.null(fichierInput())) {
      return(dateFichier(fichierInput()))
    }
    else {
      return(dateLocale)
    }
  })
  
  dateTexteR <- reactive({
    return(dateTexte(dateR(), input$publication))
  })
  
  
  tableauFiltre <- reactive({
    req(tableauR())
    exceptionGroupes <- setdiff(tableauR()$Nom, debounced_groupes())
    exceptionFilieres <- setdiff(tableauR()$`Filière`, debounced_filieres())
    
    historique(tableauR(), ymd_hms(input$publication, truncated = 3)) %>%
      filtrage(input$duree,
               ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
               exceptionGroupes, exceptionFilieres, input$partiel, input$faible)
  })
  
  tableauTrie <- reactive({
    req(tableauFiltre())
    
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
    req(tableauR())
    if (input$delta) {
      exceptionGroupes <- setdiff(tableauR()$Nom, debounced_groupes())
      exceptionFilieres <- setdiff(tableauR()$`Filière`, debounced_filieres())
      
      historique(tableauR(), ymd_hms(input$reference, truncated = 3)) %>%
        filtrage(input$duree,
                 ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
                 exceptionGroupes, exceptionFilieres, input$partiel, input$faible)
    }
  })
  
  graphiqueR <- reactive({
    req(tableauTrie())
    graphique(tableauTrie(), input$duree,
              ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
              dateTexteR(), debounced_filieres(), input$code, input$delta)
  })
  
  output$graphique <- renderPlot({
    graphiqueR()
  }, height = 950)
  
  tableauProjete <- reactive({
    req(tableauFiltre())
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
    req(tableauProjete())
    empilement(tableauProjete(), input$duree,
               ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
               dateTexteR(), debounced_filieres())
  })
  
  output$empilement <- renderPlot({
    empilementR()
  }, height = 950)
  
  tableauGeo <- reactive({
    req(tableauFiltre())
    geolocalisation(tableauFiltre(),
                    ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
                    input$code)
  })
  
  carteR <- reactive({
    req(tableauGeo())
    carte(tableauGeo(), input$duree,
          ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
          dateTexteR(), debounced_filieres())
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
      updateSwitchInput(session, "delta", value = TRUE)
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
      choixGroupesT <- tibble(nom = unique(tableauR()$Nom), code = codeGroupe(nom))
      selectionGroupesT <- tibble(code = unlist(strsplit(query[['groupes']], ",")))
      if (0 != nrow(filter(selectionGroupesT, code %in% c("tout")))) {
        selectionGroupesT <- select(choixGroupesT, code)
      }
      selectionGroupes <- deframe(select(left_join(selectionGroupesT, choixGroupesT, by = "code"), nom))
      updatePickerInput(session, "groupes", selected = selectionGroupes)
    }
    if (!is.null(query[['filieres']])) {
      choixFilieresT <- tibble(nom = unique(tableauR()$`Filière`), code = str_to_upper(substr(gsub('é', 'e', nom), 1, 3)))
      selectionFilieresT <- tibble(code = unlist(strsplit(query[['filieres']], ",")))
      if (0 != nrow(filter(selectionFilieresT, code %in% c("tout")))) {
        selectionFilieresT <- select(choixFilieresT, code)
      }
      selectionFilieres <- deframe(select(left_join(selectionFilieresT, choixFilieresT, by = "code"), nom))
      updatePickerInput(session, "filieres", selected = selectionFilieres)
    }
    if (!is.null(query[['delta']])) {
      updateSwitchInput(session, "delta", value = TRUE)
    }
    if (!is.null(query[['reference']])) {
      updateSliderInput(session, "reference", value = dmy_hms(query[['reference']], truncated = 5), timeFormat = "%d/%m/%y")
      updateSwitchInput(session, "delta", value = TRUE)
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
  
  observeEvent(input$tabset, {
    if(input$tabset == "Détail par groupe") {
      enable("tri")
      enable("reference")
      updateSwitchInput(session, "delta", disabled = FALSE)
      enable("code")
    } else {
      disable("tri")
      disable("reference")
      updateSwitchInput(session, "delta", disabled = TRUE)
      if(input$tabset == "Empilement en GW") {
        disable("code")
      } else {
        enable("code")
      }
    }
  })
  
}
