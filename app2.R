if (interactive()) {
library(shiny)
#library(readr)
library(dplyr)


# Dados das espécies ameaçadas --------------------------------------------

ameacadas <- readr::read_rds("flora.rds")
fauna <- readr::read_rds("fauna.rds")

# Modulo com as funções do SERVER  --------------------------------------------

csvFileInput <- function(id, label = "Planilha (.xlsx ou .csv) com as espécie do inventário") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        fileInput(ns("file"), label,
                  multiple = FALSE,
                  accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                      ".csv",
                      ".xlsx"
                  ),
                  buttonLabel = "Procurar...",
                  placeholder = "Selecione um arquivo")
    )
}

csvFile <- function(input, output, session, stringsAsFactors) {
    
    # The selected file, if any
    userFile <- reactive({
        
        # If no file is selected, don't do anything
        validate(need(input$file, message = "\n\tNenhum arquivo está selecionado"))
        infile <- input$file
        
    if (tolower(tools::file_ext(infile$datapath)) == "xlsx") { 
        
        # output$extensao <- renderText("XLSX")
        # outputOptions(output, "extensao", suspendWhenHidden = FALSE)
        
        inventario <- readxl::read_xlsx(infile$datapath,
                                        col_names = T, col_types = "text")
    } else {
        
        # output$extensao <- renderText("CSV")
        # outputOptions(output, "extensao", suspendWhenHidden = FALSE)
        
        inventario <- read.csv(infile$datapath,
                               #sep = input$sep,
                               #quote = input$quote,
                               stringsAsFactors = stringsAsFactors)

    }
    
    # Return the reactive that yields the data frame
    return(inventario)
})
}

# Seção UI ----------------------------------------------------------------

ui <- fluidPage(
    
    # Arquivo CSS -------------------------------------------------------------
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # Título da página --------------------------------------------------------
    
    titlePanel(title = tags$div("Busca por Espécie Ameaçada de Extinção"), windowTitle = "Busca por Espécie Ameaçada de Extinção"),
    sidebarPanel(
        
        # Painel Inventário ----------------------------------------------------
        
        conditionalPanel(
            'input.tabela === "Inventário"',
            # Arquivo CSV
            csvFileInput("datafile", "Planilha (.xlsx ou .csv) com as espécie do inventário"),
            
            tags$div(
                tags$b("ATENÇÃO:"),
                "A coluna com os nomes das espécies", tags$b("deve"), "ser nomeada como", tags$b("Especie"), "(sem acento).",
                tags$span(class="dropt", "Exemplo de planilha",
                          tags$span(tags$img(src = "exemplo-planilha.png"), tags$br())
                )
            ),
        
            # Horizontal line ----
            tags$hr(),
            
           
            # # Input: Checkbox if file has header ----
            # #checkboxInput("header", "Cabeçalho", TRUE),
            # 
            # Input: Select separator ----
            radioButtons("sep", "Separador de colunas",
                         choices = c("Vírgula" = ",",
                                     "Ponto e vírgula" = ";",
                                     Tab = "\t"),
                         selected = ","),

            # Input: Select quotes ----
            radioButtons("quote", "Delimitador de Texto",
                         choices = c(Nenhum = "",
                                     "Aspas duplas" = '"',
                                     "Aspas simples" = "'"),
                         selected = '"')
            
            #Horizontal line ----
            # tags$hr(),
            # 
            # #Input: Select number of rows to display ----
            # radioButtons("disp", "Visualizar",
            #              choices = c("Linhas inicias" = "head",
            #                          Tudo = "all"),
            #              selected = "all")
            #}
            
            
        ),
        
        
        
        # Painel FLORA ---------------------------------------------------
        
        conditionalPanel(
            'input.tabela === "FLORA"',
            selectInput("fam",
                        "Família:",
                        c(TODAS = "All",
                          unique(as.character(ameacadas$Familia)))),
            
            selectInput("cat",
                        "Categoria:",
                        c(TODAS = "All",
                          unique(as.character(ameacadas$Categoria)))),
            
            selectInput("in608",
                        "Presente na IN 6/2008:",
                        c("Sim/Não" = "All",
                          unique(as.character(ameacadas$IN608))))
        ),
        
        # Painel Comparativo ------------------------------------------------------
        
        conditionalPanel(
            'input.tabela === "Comparativo"',
            
            # Baixar tabela
            downloadButton("downloadData", "Clique aqui para para baixar a tabela comparativa"),
            
            tags$div(
                tags$br(),
                tags$b("*Nomes aceitos seguem: "), 
                tags$a(href="http://floradobrasil.jbrj.gov.br", "Flora do Brasil 2020,", target="_blank"), 
                "acesso em: 24/01/2020"
            )
        ),
        
        # Painel FAUNA ---------------------------------------------------
        
        conditionalPanel(
            'input.tabela === "FAUNA"',
            
            selectInput("port",
                        "Portaria:",
                        c(TODAS = "All",
                          unique(as.character(fauna$Portaria)))),
            
            selectInput("grp",
                        "Grupo Taxonômico:",
                        c(TODOS = "All",
                          unique(as.character(fauna$Grupo)))),
            
            selectInput("fami",
                        "Família:",
                        c("TODAS" = "All",
                          unique(as.character(fauna$Familia)))),
            
            selectInput("cate",
                        "Categoria:",
                        c("TODAS" = "All",
                          unique(as.character(fauna$Categoria))))
        ),
        
        
        # Painel de referẽncias ---------------------------------------------------
        
        conditionalPanel(
            'input.tabela === "Referências"',
            tags$div(
                "Dúvidas, sugestões ou ... -->",
                tags$a(href = "mailto:cadu@e.email?subject=APP%20sobre%20Espécies%20Ameaçadas%20de%20Extinção", "cadu@e.email", target = "_blank")
            )
        )
    ),
    
    
    # Definição dos painéis ---------------------------------------------------
    
    mainPanel(
        tabsetPanel(
            id = 'tabela',
            tabPanel("FLORA", DT::dataTableOutput("contents")),
            tabPanel("Inventário", DT::dataTableOutput("inventario")),
            tabPanel("Comparativo", DT::dataTableOutput("compara")),
            tabPanel("FAUNA", DT::dataTableOutput("fauna")),
            tabPanel("Referências", uiOutput("ref"))
            
        )
    )
    
)


# Seção SERVER ------------------------------------------------------------

server <- function(input, output) {
    
    # Carrega o Módulo com a planilha enviada 
    
    datafile <- callModule(csvFile, "datafile",
                           stringsAsFactors = FALSE)
    
    
    # Tabela inventário ----------------------------------------------------------
    
    output$inventario <- DT::renderDataTable(DT::datatable({
        
        datafile()
    },
    options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.20/i18n/Portuguese-Brasil.json'))
    ))
    
    
    # Tabela comparativa ------------------------------------------------------
    
    output$compara <- DT::renderDataTable(DT::datatable({
        
        df <- datafile() %>% 
            mutate(Especie = stringr::str_c(stringr::word(Especie, start = 1), stringr::word(Especie, start = 2), sep = " "))
        
        nomes <- flora::get.taxa(df$Especie) %>% 
            select(original.search, Especie = search.str, Obs. = notes)
        
        comp <-inner_join(ameacadas, nomes, by = "Especie") %>% 
            select(Familia, Especie, Categoria, original.search, Obs.) %>% 
            mutate(Obs. = if_else(Obs. == "was misspelled", "grafia incorreta", 
                                  if_else(Obs. == "replaced synonym", "sinônimo", 
                                          if_else(Obs. ==  "not found", "não encontrado", 
                                                  if_else(Obs. == "was misspelled|replaced synonym", "grafia incorreta, sinônimo", Obs.))))) %>% 
            distinct(Especie, .keep_all = T) %>% 
            select(Familia, "Nome aceito*" = Especie, Categoria, "Nome procurado" = original.search, Obs.)
        
        return(comp)
        
    },
    options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.20/i18n/Portuguese-Brasil.json')
    )
    ))
    
    
    # Botão download ---------------------------------------------------------
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("compara_Ameacadas", ".csv", sep = "")
        },
        content = function(file) {
            df <- datafile() %>% 
                mutate(Especie = stringr::str_c(stringr::word(Especie, start = 1), stringr::word(Especie, start = 2), sep = " "))
            
            nomes <- flora::get.taxa(df$Especie) %>% 
                select(original.search, Especie = search.str, Obs. = notes)
            
            comp <-inner_join(ameacadas, nomes, by = "Especie") %>% 
                select(Familia, Especie, Categoria, original.search, Obs.) %>% 
                mutate(Obs. = if_else(Obs. == "was misspelled", "grafia incorreta", 
                                      if_else(Obs. == "replaced synonym", "sinônimo", 
                                              if_else(Obs. ==  "not found", "não encontrado", 
                                                      if_else(Obs. == "was misspelled|replaced synonym", "grafia incorreta, sinônimo", Obs.))))) %>% 
                distinct(Especie, .keep_all = T) %>% 
                select(Familia, "Nome aceito" = Especie, Categoria, "Nome procurado" = original.search, Obs.)
            
            write.csv(comp, file, row.names = FALSE)
        }
    )
    
    
    # Tabela FLORA Ameaçada ---------------------------------------------------
    
    output$contents <- DT::renderDataTable(DT::datatable({
        
        data <- ameacadas
        if (input$fam != "All") {
            data <- data[data$Familia == input$fam,]
        }
        if (input$cat != "All") {
            data <- data[data$Categoria == input$cat,]
        }
        if (input$in608 != "All") {
            data <- data[data$IN608 == input$in608,]
        }
        data
    },
    options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.20/i18n/Portuguese-Brasil.json')
    )
    ))
    
    # Tabela FAUNA Ameaçada ---------------------------------------------------
    
    output$fauna <- DT::renderDataTable(DT::datatable({
        
        dfFauna <- fauna
        if (input$port != "All") {
            dfFauna <- dfFauna[dfFauna$Portaria == input$port,]
        }
        if (input$grp != "All") {
            dfFauna <- dfFauna[dfFauna$Grupo == input$grp,]
        }
        if (input$fami != "All") {
            dfFauna <- dfFauna[dfFauna$Familia == input$fami,]
        }
        if (input$cate != "All") {
            dfFauna <- dfFauna[dfFauna$Categoria == input$cate,]
        }
        dfFauna
    },
    options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.20/i18n/Portuguese-Brasil.json')
    )
    ))
    
    # Referências -------------------------------------------------------------
    
    output$ref <- renderUI({
        
        tags$div(class = "conteudo", style = "",
                 
                 tags$div("Este aplicativo foi elaborado com o ambiente de programação", tags$cite("R"),tags$sup("1"),",",
                          "incluindo os seguintes pacotes:",
                          tags$cite("shiny"),tags$sup("2"),",",
                          tags$cite("readr"),tags$sup("3"),",",
                          tags$cite("dplyr"),tags$sup("4"),",",
                          tags$cite("stringr"),tags$sup("5"),"e",
                          tags$cite("flora"),tags$sup("6"),".",
                          tags$p("As fontes dos dados vieram das Portarias MMA","443",tags$sup("7"),",","444",tags$sup("8"),"e","445",tags$sup("8"),"."),
                          tags$p("A validação dos nomes científicos das espécies da vegetação segue",tags$cite("Flora do Brasil 2020"),tags$sup("9"),"."),
                          style = "margin-top: 2em; font-size: medium"
                 ),
                 tags$br(),
                 tags$h4("Referências"),
                 tags$p(tags$b("1"),"R Core Team (2019). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.",
                        tags$a(href ="https://www.R-project.org/", "https://www.R-project.org/", target = "_blank")
                 ),
                 
                 tags$p(tags$b("2"),"Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie e Jonathan McPherson (2019). shiny: Web Application Framework for R. R package version 1.4.0.",
                        tags$a(href = "https://CRAN.R-project.org/package=shiny", "https://CRAN.R-project.org/package=shiny", target = "_blank")
                 ),
                 
                 tags$p(tags$b("3"),"Hadley Wickham, Jim Hester e Romain Francois (2018). readr: Read Rectangular Text Data. R package version 1.3.1.",
                        tags$a(href ="https://CRAN.R-project.org/package=readr", "https://CRAN.R-project.org/package=readr", target = "_blank")
                 ),
                 
                 tags$p(tags$b("4"),"Hadley Wickham, Romain François, Lionel Henry e Kirill Müller (2020). dplyr: A Grammar of Data Manipulation. R package version 0.8.4.",
                        tags$a(href = "https://CRAN.R-project.org/package=dplyr", "https://CRAN.R-project.org/package=dplyr", target = "_blank")
                 ),
                 
                 tags$p(tags$b("5"),"Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for Common String Operations. R package version 1.4.0.",
                        tags$a(href = "https://CRAN.R-project.org/package=stringr", "https://CRAN.R-project.org/package=stringr", target = "_blank")
                 ),
                 
                 tags$p(tags$b("6"),"Gustavo Carvalho (2017). flora: Tools for Interacting with the Brazilian Flora 2020. R package version 0.3.0.",
                        tags$a(href = "https://CRAN.R-project.org/package=flora", "https://CRAN.R-project.org/package=flora", target = "_blank")
                 ),
                 
                 tags$p(tags$b("7"),"Ministério do Meio Ambiente.", 
                        tags$a(href="http://dados.gov.br/dataset/portaria_443", "Portaria MMA n. 443 de 2014.", target = "_blank"), 
                        "Acesso em: 24/01/2020"
                 ),
                 
                 tags$p(tags$b("8"),"Ministério de Meio Ambiente.",
                        tags$a(href="https://www.mma.gov.br/biodiversidade/conservacao-de-especies/fauna-ameacada/fauna.html", "Portarias MMA ns. 444 e 445 de 2014.", target="_blank"),
                        "Acesso em: 24/01/2020"
                 ),
                 
                 tags$p(tags$b("9"),"Flora do Brasil 2020 em construção. Jardim Botânico do Rio de Janeiro. Disponível em:",
                        tags$a(href="http://floradobrasil.jbrj.gov.br", "http://floradobrasil.jbrj.gov.br.", target="_blank"), 
                        "Acesso em: 24/01/2020"
                 )
                 
                 
        )
    })
}

# Create Shiny app ----
shinyApp(ui, server)
}