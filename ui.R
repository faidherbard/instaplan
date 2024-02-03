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
                                    "?debut=03082023&fin=30032024&duree=7", tags$br(),
                                    "&partiel=100&faible=50&tri=filiere&filieres=tout", tags$br(),
                                    "&groupes=SSL1,SSL2,VAI3,GMA11&nom", tags$br(),
                                    "&delta&reference=01052023&historique=01012024", tags$br(),
                                    href=paste0(link, "?debut=03082023&fin=30032024&duree=7&partiel=100&faible=50&tri=filiere&filieres=tout&groupes=SSL1,SSL2,VAI3,GMA11&nom&delta&reference=01052023&historique=01012024"),
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
    useShinyjs(),
    tags$head(tags$style(HTML('
      .recalculating {opacity: 0.8;}
    '))),
    fluidRow(
      box(
        width = 4,
        title = "Période d'observation",
        collapsible = TRUE,
        collapsed = TRUE,
        dateRangeInput("dateRange","",
                       start = as_date(debut), min = as_date(dmy_hms("01/01/2014", truncated = 3)),
                       end = as_date(fin),
                       weekstart = 1, language = "fr",
                       format = "dd/mm/yyyy",
                       separator = " à "),
        actionButton("moins", icon("minus"), class = "btn-xs", width = "49%"),
        actionButton("plus", icon("plus"), class = "btn-xs", width = "49%")
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
      tabBox(
        id = "tabset",
        width = 12, height = 1000,
        tabPanel("Détail par groupe", plotOutput("graphique") %>% withSpinner(hide.ui = FALSE)),
        tabPanel("Empilement en GW", plotOutput("empilement") %>% withSpinner(hide.ui = FALSE)),
        tabPanel("Carte", plotOutput("carte") %>% withSpinner(hide.ui = FALSE))
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
        sliderInput("reference","Date de référence",
                    min = as_date(dmy_hms("01/01/2014", truncated = 3)), max = as_date(publication), value = as_date(reference),
                    timeFormat = "%d/%m/%y")
      ),
      box(
        width = 4,
        title = "Consulter l'historique",
        collapsible = TRUE,
        collapsed = TRUE,
        sliderInput("publication","",
                    min = as_date(dmy_hms("01/01/2014", truncated = 3)), max = as_date(publication), value = as_date(publication),
                    timeFormat = "%d/%m/%y",
                    animate = animationOptions(interval = 2000)),
        helpText("Cliquez sur le boutton", icon("play"), "ci-dessus pour animer l'historique")
      ),
      box(
        width = 4,
        title = "Charger les indisponibilités",
        collapsible = TRUE,
        collapsed = TRUE,
        a(href=fichierDistant, "Cliquez ici pour télécharger le fichier depuis le site EDF"),
        fileInput("fichier", "", accept = c(".csv", ".zip"), buttonLabel = list(icon("import", lib = "glyphicon"), "Charger"))
      )
    ),
    fluidRow(
      box(
        width = 4,
        title = "Sélection des filières",
        collapsible = TRUE,
        collapsed = TRUE,
        pickerInput("filieres","",
                    multiple = TRUE, options = list(`actions-box` = TRUE),
                    choices = sort(choixFilieres), selected = selectionFilieres)
      ),
      box(
        width = 4,
        title = "Sélection des groupes",
        collapsible = TRUE,
        collapsed = TRUE,
        pickerInput("groupes","",
                    multiple = TRUE,
		    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE),
                    choices = sort(choixGroupes), selected = selectionGroupes),
        checkboxInput("code","Libellé court",
                      value = code)
      ),
      box(
        width = 4,
        title = "Masquer arrêts mineurs",
        collapsible = TRUE,
        collapsed = TRUE,
        sliderInput("duree", "Durée minimale d'indisponibilité",
                    min = 0, max = 100, value = duree,
                    post = " jours"),
        sliderInput("partiel", "Pourcentage minimal d'indisponibilité",
                    min = 0, max = 100, value = partiel,
                    post = " % Puissance maximale"),
        sliderInput("faible", "Volume minimal d'indisponibilité",
                    min = 0, max = 1500, value = faible,
                    post = " MW")
      )
    )
  )
)
