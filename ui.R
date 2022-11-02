#Page
ui <- dashboardPage(title = "Instaplan",
  dashboardHeader(
    title = tags$a("Instaplan", href=link, style="color:white;"),
    dropdownMenu(type = "messages",
                 messageItem(
                   from = tags$b("Préchargez vos paramètres", tags$br(),
                                 "depuis l'URL. Exemple :"),
                   message = tags$a(link, tags$br(),
                                    "?debut=03092022&fin=30032023", tags$br(),
                                    "&duree=7&partiel=100", tags$br(),
				    "&tri=filiere&filieres=NUC,FUE", tags$br(),
				    "&groupes=SSL1,SSL2,VAI1,VAI2&nom", tags$br(),
                                    href=paste0(link, "?debut=03092022&fin=30032023&duree=7&partiel=100&tri=filiere&filieres=NUC,FUE&groupes=SSL1,SSL2,VAI1,VAI2&nom")),
                   icon = icon("download")
                 )
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        title = "Charger le fichier des indisponibilités",
        collapsible = TRUE,
        collapsed = TRUE,
        a(href="https://www.edf.fr/doaat/export/light/csv",
          "Cliquer ici pour télécharger le fichier depuis le site EDF"),
        fileInput("fichier","",
                  accept = ".csv",
                  buttonLabel = "Charger...")
      ),
      box(
        title = "Télécharger le graphique",
        collapsible = TRUE,
        collapsed = TRUE,
        br(),
        "Clic droit sur le graphique puis ",strong("Enregistrer l'image sous"),
        #downloadButton("downloadData", "Télécharger"),
        br(), br(),
      )
    ),
    fluidRow(
      box(
        status = "primary",
        width = 12, height = 1000,
        plotOutput("distPlot")
      )
    ),
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
        title = "Masquer les indisponibilités courtes et partielles",
        collapsible = TRUE,
        collapsed = TRUE,
        sliderInput("duree","Durée minimale d'indisponibilité",
                    min = 0, max = 100, value = round((fin-debut)/ddays(1)*25/1000),
                    post = " jours"),
        sliderInput("partiel","Pourcentage indisponible rapporté à la puissance maximale",
                    min = 0, max = 100, value = 33,
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
        width = 6,
        title = "Sélection des groupes",
        collapsible = TRUE,
        collapsed = TRUE,
        pickerInput("groupes","",
                    multiple = TRUE, options = list(`actions-box` = TRUE),
                    choices = sort(choixGroupes), selected = selectionGroupes),
        checkboxInput("code","Libellé court",
                      value = TRUE)
      ),
      box(
        width = 6,
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
