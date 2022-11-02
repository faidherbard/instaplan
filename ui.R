#Page
ui <- dashboardPage(title = "Instaplan",
                    dashboardHeader(
                      title = tags$a("Instaplan", href=link, style="color:white;"),
                      dropdownMenu(type = "messages",
                                   messageItem(
                                     from = tags$div("Vous pouvez précharger les", tags$br(),
                                                     "paramètres directement depuis", tags$br(),
                                                     "l'URL. Exemple ci-dessous :",
                                                     style = "display: inline-block; vertical-align: middle;"),
                                     message = tags$a(link, tags$br(),
                                                      "?debut=03092022&fin=30032023", tags$br(),
                                                      "&duree=7&partiel=100", tags$br(),
                                                      "&tri=filiere&filieres=NUC,FUE", tags$br(),
                                                      "&groupes=SSL1,SSL2,VAI1,VAI2&nom", tags$br(),
                                                      href=paste0(link, "?debut=03092022&fin=30032023&duree=7&partiel=100&tri=filiere&filieres=NUC,FUE&groupes=SSL1,SSL2,VAI1,VAI2&nom"),
                                                      style = "color: #3c8dbc; text-decoration: underline; padding: 0 10px; display: inline-block;"),
                                     icon = icon("flash", lib = "glyphicon")
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
                          downloadButton("downloadImage", "Télécharger"),
                          br(), br(),
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
                                      min = 0, max = 100, value = duree,
                                      post = " jours"),
                          sliderInput("partiel","Pourcentage indisponible rapporté à la puissance maximale",
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
                                      min = now()-dmonths(12), max = now(), value = publication,
                                      timeFormat = "%d/%m/%y", step = ddays(15),
                                      animate = animationOptions(interval = 1500)),
                          em("Cliquez sur le boutton"), icon("play"), em("ci-dessus pour animer l'historique")
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
