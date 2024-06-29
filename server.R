# Define server logic
server <- function(input, output, session) {
  
  groupesDebounced_R <- debounce(reactive({input$groupes}), 1500)
  filieresDebounced_R <- debounce(reactive({input$filieres}), 1500)
  
  fichierInput_R <- reactive({
    fichierInput <- NULL
    
    #Pour synchroniser cette session avec les autres
    if(file.exists("instaplan.dateMaj.rda")) {
      load("instaplan.dateMaj.rda")
    }
    
    # Priorité au fichier importé
    if (!is.null(input$fichier$datapath)) {
      fichierInput <- input$fichier$datapath
    } # Puis au fichier distant, lu une fois par heure
    else if (majAuto && (now()-dateMaj > dminutes(30))) {
      dateMaj <- now()
      
      #On enregistre l'info pour toutes les sessions
      save(dateMaj, file = "instaplan.dateMaj.rda")
      fichierInput <- fichierDistant
    } # Sinon tableauLocal et dateLocale
  })
  
  dateMaj_R <- reactive({
    fichierInput_R() #Juste pour forcer un rafraichissement reactif
    #Pour synchroniser cette session avec les autres
    if(file.exists("instaplan.dateMaj.rda")) {
      load("instaplan.dateMaj.rda")
    }
    return(dateMaj)
  })
  
  tableau_R <- reactive({
    #Pour synchroniser cette session avec les autres
    if(file.exists("instaplan.dateLocale.rda")) {
      load("instaplan.dateLocale.rda")
    }
    if(file.exists("instaplan.tableauLocal.rda")) {
      load("instaplan.tableauLocal.rda")
    }
    else { # Premier lancement
      showNotification("Premier lancement d'Instaplan. Initialisation des variables : charger un fichier d'indispo (de préférence le fichier avec l'historique complet https://www.edf.fr/doaat/export/full/csv) puis redémarrez l'appli Shiny.", duration = NULL)
    }
    
    if(!is.null(fichierInput_R())) {
      #Import et traitement du fichier EDF
      tableau <- read_delim(fichierInput_R(), skip = 2, delim=";", locale=locale(encoding='latin1', decimal_mark=","),
                            col_names = specColNames, col_types = specColTypes) %>% preparation()
      
      # Mettre a jour base si fichier charge plus recent et enrichir avec historique
      dateFichier <- date_R()
      if(dateFichier > dateLocale) {
        if(!is.null(tableauLocal)) {
          tableau <- union(select(tableau, -Status), select(tableauLocal, -Status)) %>%
            full_join(tableau) %>%
            replace_na(list(Status = "Inactive"))
        }
        else { # Premiere initialisation
          choixGroupes <- choixGroupesF(tableau)
          selectionGroupes <- setdiff(choixGroupes, exceptionGroupes)
          updatePickerInput(session, "groupes", choices = choixGroupes, selected = selectionGroupes)
          initCarto(tableau)
        }
        tableauLocal <- tableau
        dateLocale <- dateFichier
        
        #On enregistre l'info pour toutes les sessions
        save(tableauLocal, file = "instaplan.tableauLocal.rda")
        save(dateLocale, file = "instaplan.dateLocale.rda")
      } 
      return(tableau)
    }
    else {
      return(tableauLocal)
    }
  })
  
  date_R <- reactive({
    #Pour synchroniser cette session avec les autres
    if(file.exists("instaplan.dateLocale.rda")) {
      load("instaplan.dateLocale.rda")
    }
    
    if(!is.null(fichierInput_R())) {
      return(dateFichier(fichierInput_R()))
    }
    else {
      return(dateLocale)
    }
  })
  
  dateTexte_R <- reactive({
    return(dateTexte(date_R(), input$publication))
  })
  
  
  tableauFiltre_R <- reactive({
    req(tableau_R())
    exceptionGroupes <- setdiff(tableau_R()$Nom, groupesDebounced_R())
    exceptionFilieres <- setdiff(tableau_R()$`Filière`, filieresDebounced_R())
    
    historique(tableau_R(), ymd_hms(input$publication, truncated = 3)+days(1)) %>%
      filtrage(input$duree,
               ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
               exceptionGroupes, exceptionFilieres, input$partiel, input$faible)
  })
  
  tableauTrie_R <- reactive({
    req(tableauFiltre_R())
    
    tableauF <- tableauFiltre_R()%>%
      tri(ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
          input$tri, FALSE)
    
    if (input$delta) {
      tableauF <- fusion(tableauF, tableauFiltreRef_R()) %>%
        tri(ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
            input$tri, input$delta)
    }
    return(tableauF)
  })
  
  tableauFiltreRef_R <- reactive({
    req(tableau_R())
    if (input$delta) {
      exceptionGroupes <- setdiff(tableau_R()$Nom, groupesDebounced_R())
      exceptionFilieres <- setdiff(tableau_R()$`Filière`, filieresDebounced_R())
      
      historique(tableau_R(), ymd_hms(input$reference, truncated = 3)) %>%
        filtrage(input$duree,
                 ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
                 exceptionGroupes, exceptionFilieres, input$partiel, input$faible)
    }
  })
  
  graphique_R <- reactive({
    req(tableauTrie_R())
    graphique(tableauTrie_R(), input$duree,
              ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
              dateTexte_R(), filieresDebounced_R(), input$code, input$delta)
  })
  
  output$graphique <- renderPlot({
    graphique_R()
  }, height = 950)
  
  tableauProjete_R <- reactive({
    req(tableauFiltre_R())
    withProgress(
      message = "Calcul en cours",
      detail = "Le temps de calcul est directement lié au nombre d'indisponibilités traitées.
                Pour aller plus vite : réduire la période d'observation ou masquer les arrêts mineurs.",
      value = 0, {
        projection(tableauFiltre_R(),
                   ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3))
      })
  })
  
  empilement_R <- reactive({
    req(tableauProjete_R())
    empilement(tableauProjete_R(), input$duree,
               ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
               dateTexte_R(), filieresDebounced_R())
  })
  
  output$empilement <- renderPlot({
    empilement_R()
  }, height = 950)
  
  tableauGeo_R <- reactive({
    req(tableauFiltre_R())
    geolocalisation(tableauFiltre_R(),
                    ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
                    input$code)
  })
  
  carte_R <- reactive({
    req(tableauGeo_R())
    carte(tableauGeo_R(), input$duree,
          ymd_hms(input$dateRange[1], truncated = 3), ymd_hms(input$dateRange[2], truncated = 3),
          dateTexte_R(), filieresDebounced_R())
  })
  
  output$carte <- renderPlot({
    carte_R()
  }, height = 930)
  
  output$dateMaj <- renderText({
    paste('Dernière mise à jour automatique : ', dateMaj_R())
  })
  
  # Telecharger l'image
  output$downloadImage <- downloadHandler(
    filename = paste(format(now(), format = "%Y%m%d"), "Instaplan.png"),
    contentType = "image/png",
    content = function(file) {
      png(file, width = 1000, height = 950)
      print(switch(input$tabset,
                   "Détail par groupe" = graphique_R(),
                   "Empilement en GW" = empilement_R(),
                   "Carte" = carte_R()
      ))
      dev.off()
    }
  )
  
  observeEvent(session$clientData$url_search, {
    # Lire les parametres depuis l'URL
    query <- parseQueryString(session$clientData$url_search)
    
    # Paramètres combinés
    combine = (!is.null(query[['hebdo']]) || !is.null(query[['mensuel']]) || !is.null(query[['annuel']]))
    if (combine) {
      base <- periode <- decalage <- duree <- 0
      if (!is.null(query[['hebdo']])) {
        base = floor_date(now(), "weeks", week_start = 4) - days(1)
        periode = days(13)
        decalage = str_count(query[['hebdo']], " ") - str_count(query[['hebdo']], "-") # L'URL transforme + en espace
        duree = 1
      }
      if (!is.null(query[['mensuel']])) {
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
      updateDateInput(session, "reference", value = min(ymd(base+decalage*periode), input$publication))
      updatePickerInput(session, "filieres", selected = choixFilieres)
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
      choixGroupesT <- tibble(nom = unique(tableau_R()$Nom), code = codeGroupe(nom))
      selectionGroupesT <- tibble(code = unlist(strsplit(query[['groupes']], ",")))
      if (0 != nrow(filter(selectionGroupesT, code %in% c("tout")))) {
        selectionGroupesT <- select(choixGroupesT, code)
      }
      selectionGroupes <- deframe(select(left_join(selectionGroupesT, choixGroupesT, by = "code"), nom))
      updatePickerInput(session, "groupes", selected = selectionGroupes)
    }
    if (!is.null(query[['filieres']])) {
      choixFilieresT <- tibble(nom = unique(tableau_R()$`Filière`), code = str_to_upper(substr(gsub('é', 'e', nom), 1, 3)))
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
      updateDateInput(session, "reference", value = dmy_hms(query[['reference']], truncated = 5))
      updateSwitchInput(session, "delta", value = TRUE)
    }
    if (!is.null(query[['historique']])) {
      updateDateInput(session, "publication", value = dmy_hms(query[['historique']], truncated = 5))
    }
  })
  
  observe({
    # Lire les parametres depuis l'URL
    query <- parseQueryString(session$clientData$url_search)
    
    # Paramètres combinés
    combine = (!is.null(query[['hebdo']]) || !is.null(query[['mensuel']]) || !is.null(query[['annuel']]))
    
    # Adapter la duree a la fenetre d'observation (uniquement si pas déjà fixées via l'URL)
    if (is.null(query[['duree']]) && !combine) {
      updateSliderInput(session, "duree", value = round((input$dateRange[2]-input$dateRange[1])/ddays(1)*25/1000))
    }
    
    # Désactivation conditionnelle du choix de la date de publication
    if(input$delta && input$tabset == "Détail par groupe") {
      enable("reference")
    } else {
      disable("reference")
    }
  })
  
  observeEvent(input$publication, {
    # Adapter la date max de publication a la date de publication
    updateDateInput(session, "reference", max=input$publication)
  })
  
  observeEvent(input$plus, {
    periode = days((input$dateRange[2]-input$dateRange[1])/ddays(1))
    updateDateRangeInput(session, "dateRange", start = input$dateRange[1]+periode, end = input$dateRange[2]+periode)
    updateDateInput(session, "reference", value = min(input$reference+periode, input$publication))
  })
  
  observeEvent(input$moins, {
    periode = days((input$dateRange[2]-input$dateRange[1])/ddays(1))
    updateDateRangeInput(session, "dateRange", start = input$dateRange[1]-periode, end = input$dateRange[2]-periode)
    updateDateInput(session, "reference", value = input$reference-periode)
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
      updateSwitchInput(session, "delta", disabled = FALSE)
    } else {
      disable("tri")
      updateSwitchInput(session, "delta", disabled = TRUE)
    }
    if(input$tabset == "Empilement en GW") {
      disable("code")
    } else {
      enable("code")
    }
  })
  
}
