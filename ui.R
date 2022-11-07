#Page
ui <- dashboardPage(
  title = "Instaplan",
  dashboardHeader(
    title = tags$a("Instaplan", href=link, style="color:white;"),
    dropdownMenu(type = "messages",
                 icon = icon("flash", lib = "glyphicon"),
                 badgeStatus = NULL,
                 headerText = strong("Astuce"), 
                 messageItem(
                   from = tags$div("Vous pouvez précharger les", tags$br(),
                                   "paramètres directement depuis", tags$br(),
                                   "l'URL. Exemple ci-dessous :",
                                   style = "display: inline-block; vertical-align: middle;"),
                   message = tags$a(link, tags$br(),
                                    "?debut=03092022&fin=30032023&duree=7", tags$br(),
                                    "&partiel=100&tri=filiere&filieres=NUC,FUE", tags$br(),
                                    "&groupes=SSL1,SSL2,VAI1,VAI2&nom&delta", tags$br(),
                                    href=paste0(link, "?debut=03092022&fin=30032023&duree=7&partiel=100&tri=filiere&filieres=NUC,FUE&groupes=SSL1,SSL2,VAI1,VAI2&nom&delta"),
                                    style = "color: #3c8dbc; text-decoration: underline; padding: 0 10px; display: inline-block; font-size: 0.9em; "),
                   icon = icon("flash", lib = "glyphicon")
                 )
    ),
    tags$li(
      actionLink("aide", "", icon("question-sign", lib = "glyphicon")),
      class = "dropdown"
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        width = 4,
        title = "Période d'observation",
        collapsible = TRUE,
        collapsed = TRUE,
        dateRangeInput("dateRange","",
                       start = debut,
                       end = fin,
                       weekstart = 1, language = "fr",
                       format = "dd/mm/yyyy",
                       separator = " à ")
      ),
      box(
        width = 4,
        title = "Charger les indisponibilités",
        collapsible = TRUE,
        collapsed = TRUE,
        actionButton("fichier", icon = icon("import", lib = "glyphicon"), "Charger")
      ),
      box(
        width = 4,
        title = "Télécharger le graphique",
        collapsible = TRUE,
        collapsed = TRUE,
        downloadButton("downloadImage", "Télécharger", icon = icon("export", lib = "glyphicon"))
      )
    ),
    fluidRow(
      box(
        status = "primary",
        width = 12, height = 1000,
        plotOutput("graphique")
      )
    ),
    fluidRow(
      box(
        width = 4,
        title = "Afficher les variations",
        collapsible = TRUE,
        collapsed = TRUE,
        switchInput("delta","Différences", value = delta,
                    size = "mini", onStatus = "success", onLabel = "Oui", offLabel = "Non"),
        sliderInput("dateRef","Date de référence",
                    min = as_date(now()-dmonths(12)), max = publication, value = dateRef,
                    timeFormat = "%d/%m/%y")
      ),
      box(
        width = 4,
        title = "Masquer courts et partiels",
        collapsible = TRUE,
        collapsed = TRUE,
        sliderInput("duree","Durée minimale d'indisponibilité",
                    min = 0, max = 100, value = duree,
                    post = " jours"),
        sliderInput("partiel","Pourcentage indisponible (par rapport à la puissance maximale)",
                    min = 0, max = 100, value = partiel,
                    post = " %")
      ),
      box(
        width = 4,
        title = "Tri des indisponibilités",
        collapsible = TRUE,
        collapsed = TRUE,
        radioButtons("tri","",
                     c("Filière, palier puis date" = "palier",
                       "Filière puis date" = "filiere",
                       "Date" = "date",
                       "Filière, palier puis nom" = "paliernom",
                       "Filière puis nom" = "filierenom",
                       "Nom" = "nom"))
      )
    ),
    fluidRow(
      box(
        width = 4,
        title = "Consulter l'historique",
        collapsible = TRUE,
        collapsed = TRUE,
        sliderInput("publication","",
                    min = dateRef, max = publication, value = publication,
                    timeFormat = "%d/%m/%y",step = ddays(5),
                    animate = animationOptions(interval = 1800)),
        helpText("Cliquez sur le boutton", icon("play"), "ci-dessus pour animer l'historique")
      ),
      box(
        width = 4,
        title = "Sélection des groupes",
        collapsible = TRUE,
        collapsed = TRUE,
        pickerInput("groupes","",
                    multiple = TRUE, options = list(`actions-box` = TRUE),
                    choices = sort(choixGroupes), selected = selectionGroupes),
        checkboxInput("code","Libellé court",
                      value = code)
      ),
      box(
        width = 4,
        title = "Sélection des filières",
        collapsible = TRUE,
        collapsed = TRUE,
        pickerInput("filieres","",
                    multiple = TRUE, options = list(`actions-box` = TRUE),
                    choices = sort(choixFilieres), selected = selectionFilieres)
      )
    )
  )
)
  