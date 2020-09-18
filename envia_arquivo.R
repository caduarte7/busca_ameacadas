ArqEntrada_UI <- function(id, label) {
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
              placeholder = "Selecione um arquivo"),
    
    tags$div(
      tags$b("ATENÇÃO:"),
      "A coluna com os nomes das espécies", tags$b("deve"), "ser nomeada como", tags$b("Especie"), "(sem acento).",
      tags$span(class="dropt", "Exemplo de planilha",
                tags$span(tags$img(src = "exemplo-planilha.png"), tags$br())
      )
    ),
    
    conditionalPanel(
      'toReturn.extensao === "CSV"',
      tags$hr(),
      
      #checkboxInput("header", "Cabeçalho", TRUE),
      
      radioButtons(ns("sep"), "Separador de colunas",
                   choices = c("Vírgula" = ",",
                               "Ponto e vírgula" = ";",
                               Tab = "\t"),
                   selected = ","),
      
      radioButtons(ns("quote"), "Delimitador de Texto",
                   choices = c(Nenhum = "",
                               "Aspas duplas" = '"',
                               "Aspas simples" = "'"),
                   selected = '"'),
    ),
    
    conditionalPanel(
      'toReturn.extensao === "XLSX"',
      tags$hr()
    )
    
  )
}



ArqEntrada <- function(input, output, session, stringsAsFactors) {
  
  toReturn    <-  reactiveValues(
    variables = NULL,
    extensao = NULL,
    trigger = 0
  )
  
 
  observeEvent(input$file, {
    
    toReturn$variable <- if (tolower(tools::file_ext(input$file$datapath)) == "xlsx") { 
      
      readxl::read_xlsx(input$file$datapath,
                        ol_names = T, col_types = "text")
    } else {
      
     read.csv(input$file$datapath,
              #sep = input$file$sep,
              #quote = input$file$quote,
              stringsAsFactors = stringsAsFactors)
      
    } 
    
    extensao <- if (tolower(tools::file_ext(input$file$datapath)) == "xlsx") { 
      
      renderText({"XLSX"})
      #outputOptions(output, "extensao", suspendWhenHidden = FALSE)
      
    } else {
      
      renderText({"CSV"})
      #outputOptions(output, "extensao", suspendWhenHidden = FALSE)
    }
      
    toReturn$trigger <- toReturn$trigger + 1

  })
  return(toReturn)

}