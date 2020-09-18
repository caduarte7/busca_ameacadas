TabCompara_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    DT::renderDataTable(DT::datatable({
      ns("comparado")
      }, 
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.20/i18n/Portuguese-Brasil.json')
        )
      ))
    )
}

TabCompara <- function(input, output, session, stringsAsFactors) {
  
  ns <- session$ns
  
  comparar <- reactive({
    
    validate(need(input$file, message = "\n\tSelecione arquivo na aba inventário"))
    ArqRecebido <- callModule(module = ArqEntrada, id = "mod_inventario2", stringsAsFactors = FALSE)
    
    ArqRecebido() %>% 
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
    
  })
  output$comparado <- DT::renderDataTable(DT::datatable({
    
    comparar()
  },
  options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.20/i18n/Portuguese-Brasil.json')
  )
  ))
}
