
server <- function(input, output, session) {
  
  ## I. Główny dashboard -----------------
  i_prog <- 1
  tot_step <- 25
  
  # Pasek postępu
  withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
    # Zwiększ pasek postępu i zaaktualizuj tekst
    incProgress( i_prog/tot_step, detail = NULL)
  })
  
  i_prog <- i_prog + 1
  
  ## Główne statystyki ----------------
  ## Wskaźnik rotacji
  output$attrRate <- renderText({
    paste0(attritionRate, "%")
  })
  
  ## Średni przychód osoby rotującej
  output$averageIncome <- renderText({
    paste0("$",avgIncome)
  })
  
  ## Średni wiek osoby rotującej
  output$averageAge <- renderText({
    paste0(avgAgeAttrition)
  })
  
  ## Pozycja osoby rotującej (czyli na jakiej pozycji rotuje najwięcej osób)
  output$rotationPosition <- renderText({
    paste0(jobRole$Stanowisko[1])
  })
  
  # Pasek postępu 
  withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
    # Zwiększ pasek postępu i zaaktualizuj tekst
    incProgress( i_prog/tot_step, detail = NULL)
  })
  
  i_prog <- i_prog + 1
  
  ## W Y K R E S Y ----------------------
  
  ## Wykres wskaźnika rotacji
  output$attrRatePlot <- renderHighchart({
    shinyjs::runjs("$('#loading-spinner').hide();") # Ukryj spinnera po załadowaniu wykresu
    
    req(input$filter_var)  # Sprawdzanie czy zmienna jest dostępna
    
    if (input$gender_split) { # Warunek podziału na płeć
      
      # Jeśli zaznaczony jest podział na płeć
      attrRateData <- data %>%
        group_by(across(all_of(input$filter_var)), Płeć) %>%
        summarise(AttritionRate = round((sum(Rotacja == "Tak") / n()) * 100, 2)) %>%
        ungroup()
      
      ## W Y K R E S -------
      hchart(attrRateData, "column", hcaes(x = !!sym(input$filter_var), y = AttritionRate,
                                           group = Płeć)) %>%
        hc_title(text = "Wskaźnik rotacji") %>%
        hc_yAxis(title = list(text = "Wskaźnik rotacji (%)"), labels = list(format = "{value}%")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}%'))) %>%
        hc_colors(c("#128870", "#333333")) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Wskaźnik rotacji</b>: {point.y}% ') %>%
        hc_legend(title = list(text = "Płeć", align = "center")) %>%  
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator",
                                                                       "downloadPNG", "downloadJPEG",
                                                                       "downloadPDF", "downloadSVG"))))
      
    } else {
      
      # Jeśli nie zaznaczony jest podział na płeć
      attrRateData <- data %>%
        group_by(across(all_of(input$filter_var))) %>%
        summarise(AttritionRate = round((sum(Rotacja == "Tak") / n()) * 100, 2)) %>%
        ungroup()
      
      ## W Y K R E S ------
      hchart(attrRateData, "column", hcaes(x = !!sym(input$filter_var), y = AttritionRate)) %>%
        hc_title(text = "Wskaźnik rotacji") %>%
        hc_yAxis(title = list(text = "Wskaźnik rotacji (%)"), labels = list(format = "{value}%")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}%'))) %>%
        hc_colors(c("#128870")) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Wskaźnik rotacji</b>: {point.y}%') %>%
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator",
                                                                       "downloadPNG", "downloadJPEG", 
                                                                       "downloadPDF", "downloadSVG"))))
    }
  })
  
  ## --------------
  ## Wykres ilości rotujących osób 
  output$attritionCountPlot <- renderHighchart({
    req(input$filter_var)  
    
    # Jeśli podział na płeć jest zaznaczony
    if (input$gender_split) {
      attritionCount <- data %>%
        filter(Rotacja == "Tak") %>%
        group_by(!!sym(input$filter_var), Płeć) %>%
        summarise(Count = n(), .groups = 'drop')
      
      ## W Y K R E S --------
      hchart(attritionCount, "column", hcaes(x = !!sym(input$filter_var), y = Count, group = Płeć)) %>%
        hc_title(text = "Liczba rotujących pracowników") %>%
        hc_yAxis(title = list(text = "Liczba Pracowników")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}'))) %>%
        hc_colors(c("#128870", "#333333")) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Liczba Pracowników</b>: {point.y}') %>% 
        hc_legend(title = list(text = "Płeć", align = "center")) %>%  
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator",
                                                                       "downloadPNG", "downloadJPEG", 
                                                                       "downloadPDF", "downloadSVG"))))
      
    } else {
      
      attritionCount <- data %>%
        filter(Rotacja == "Tak") %>%
        group_by(!!sym(input$filter_var)) %>%
        summarise(Count = n(), .groups = 'drop')
      
      ## W Y K R E S -------
      hchart(attritionCount, "column", hcaes(x = !!sym(input$filter_var), y = Count)) %>%
        hc_title(text = "Liczba rotujących pracowników") %>%
        hc_yAxis(title = list(text = "Liczba Pracowników")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}'))) %>%
        hc_colors(c("#333333")) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Liczba Pracowników</b>: {point.y}') %>% 
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                       "downloadPNG", "downloadJPEG",
                                                                       "downloadPDF", "downloadSVG"))))
    }
  })
  

  ## ------------------
  ## Wykres przedstawia rozrzut miesięcznego przychodu dla wybranego czynnika, 
  ## ponieważ jednym z czynników jest "Grupa dochodowa" zmienna nie może się wtedy zmieniać dla tego wykresu
  
  
  # Reaktywna zmienna do zapamiętania poprzedniego wyboru
  previous_var <- reactiveVal(NULL)
  
  observe({
    # Sprawdzamy, czy bieżąca zmienna to nie "Grupa dochodowa"
    if (input$filter_var != "GrupaDochodowa") {
      previous_var(input$filter_var)  # Aktualizujemy poprzednią zmienną, gdy wybrana jest inna niż "IncomeGroup"
    }
  })

  output$monthlyIncomeYesPlot <- renderHighchart({
    
    # Sprawdzamy, czy wybrana zmienna jest grupą dochodową, jeśli tak - zmienna pozostaje bez zmian
    selected_var <- if (input$filter_var == "GrupaDochodowa") previous_var() else input$filter_var 
    
    plot_data <- data %>%
      filter(Rotacja == "Tak") %>%
      select(MiesięcznyPrzychód, !!sym(selected_var), Płeć) %>% 
      group_by(!!sym(selected_var), Płeć)
    
    if (input$gender_split) {
      # Jeśli zaznaczony jest podział na płeć
      
      ## W Y K R E S ----------
      hchart(plot_data, "scatter", 
             hcaes(x = !!sym(selected_var), y = MiesięcznyPrzychód, 
                   group = Płeć)) %>%  
        hc_title(text = "Rozrzut miesięcznych dochodów dla rotujących pracowników") %>% 
        hc_yAxis(title = list(text = "Miesięczny przychód")) %>% 
        hc_xAxis(title = list(text = selected_var)) %>% 
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Miesięczny przychodów: {point.y}') %>% 
        hc_legend(title = list(text = "Płeć"), align = "center") %>% 
        hc_colors(c("#128870", "#333333")) %>% 
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                       "downloadPNG", "downloadJPEG", 
                                                                       "downloadPDF", "downloadSVG"))))
    } else {
      hchart(plot_data, "scatter", hcaes(x = !!sym(selected_var), y = MiesięcznyPrzychód, 
                                         group = !!sym(selected_var))) %>%
        hc_title(text = "Rozrzut miesięcznych przychodów dla rotujących pracowników") %>%
        hc_yAxis(title = list(text = "Miesięczny przychód")) %>%
        hc_xAxis(title = list(text = selected_var)) %>%
        hc_legend(enabled = FALSE) %>% 
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Miesięczny przychód:</b> {point.y}') %>% 
        hc_colors(c("#128870")) %>%  
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                       "downloadPNG", "downloadJPEG",
                                                                       "downloadPDF", "downloadSVG"))))
    }
  })

  
  ## ------------------------
  # Wykres dla pracowników nierotujących
  output$monthlyIncomeNoPlot <- renderHighchart({
    selected_var <- if (input$filter_var == "GrupaDochodowa") previous_var() else input$filter_var
    
    plot_data <- data %>%
      filter(Rotacja == "Nie") %>%
      select(MiesięcznyPrzychód, !!sym(selected_var), Płeć) %>% 
      group_by(!!sym(selected_var), Płeć)
    
    if (input$gender_split) {
      # Jeśli zaznaczony jest podział na płeć
      
      ## W Y K R E S ---------
      hchart(plot_data, "scatter", 
             hcaes(x = !!sym(selected_var), y = MiesięcznyPrzychód, 
                   group = Płeć)) %>%  
        hc_title(text = "Rozrzut miesięcznych dochodów dla nierotujących pracowników") %>% 
        hc_yAxis(title = list(text = "Miesięczny przychód")) %>% 
        hc_xAxis(title = list(text = selected_var)) %>% 
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Miesięczny przychodów: {point.y}') %>% 
        hc_legend(title = list(text = "Płeć"), align = "center") %>% 
        hc_colors(c("#128870", "#333333")) %>% 
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                       "downloadPNG", "downloadJPEG", 
                                                                       "downloadPDF", "downloadSVG"))))
    } else {

      hchart(plot_data, "scatter", hcaes(x = !!sym(selected_var), y = MiesięcznyPrzychód,
                                         group = !!sym(selected_var))) %>%
        hc_title(text = "Rozrzut miesięcznych przychodów dla nierotujących pracowników") %>%
        hc_yAxis(title = list(text = "Miesięczny przychód")) %>%
        hc_xAxis(title = list(text = selected_var)) %>%
        hc_legend(enabled = FALSE) %>% 
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Miesięczny przychód: {point.y}') %>%  
        hc_colors(c("#333333")) %>%  
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                       "downloadPNG", "downloadJPEG", 
                                                                       "downloadPDF", "downloadSVG"))))
    }
  })
  
  ## ---------------
  ## Wykres przedstawiający % pracowników, którzy pracowali na nadgodziny i odeszli z pracy

  ## Funkcja filtrująca dane, pozwalająca na określenie dla jakiej płci będą dostarczane
  generatePlot <- function(płeć) {
    filteredData <- data %>%
      filter(Rotacja == "Tak", Płeć == płeć) %>%
      group_by(Nadgodziny, !!sym(input$filter_var)) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      ungroup() %>%
      group_by(!!sym(input$filter_var)) %>%
      mutate(Percentage = round(Count / sum(Count) * 100))
    
    return(filteredData)
  }
  
  observeEvent(input$gender_split, {
    
    # Wykresy są aktualizowane w zależności od stanu checkboxa
    if (input$gender_split) {
      
      output$femalePlot <- renderHighchart({
        femaleData <- generatePlot("Kobieta")
        
        ## W Y K R E S --------
        hchart(femaleData, "column", hcaes(x = !!sym(input$filter_var), y = Percentage, group = Nadgodziny)) %>%
          hc_title(text = "Kobiety: % rotujących, pracujących ponad godziny pracy") %>%
          hc_yAxis(title = list(text = "% pracowników"), min = 0) %>%
          hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}%'))) %>%
          hc_colors(c("#128870", "#333333")) %>% 
          hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>% kobiet</b>: {point.y}%') %>% 
          hc_legend(title = list(text = "Nadgodziny", align = "center")) %>%  
          hc_exporting(enabled = TRUE,
                       buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                         "downloadPNG", "downloadJPEG", 
                                                                         "downloadPDF", "downloadSVG"))))
      })
      
      output$malePlot <- renderHighchart({
        maleData <- generatePlot("Mężczyzna")
        
        ## W Y K R E S --------
        hchart(maleData, "column", hcaes(x = !!sym(input$filter_var), y = Percentage, group = Nadgodziny)) %>%
          hc_title(text = "Mężczyźni: % rotujących pracujących ponad godziny pracy") %>%
          hc_yAxis(title = list(text = "% pracowników"), min = 0) %>%
          hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}%'))) %>%
          hc_colors(c("#128870", "#333333")) %>% 
          hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>% mężczyzn</b>: {point.y}%') %>% 
          hc_legend(title = list(text = "Nadgodziny", align = "center")) %>%  
          hc_exporting(enabled = TRUE, 
                       buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                         "downloadPNG", "downloadJPEG", 
                                                                         "downloadPDF", "downloadSVG"))))
      })
      
    } else {
      # Wykres bez podziału na płeć
      output$overTimePlot <- renderHighchart({
        overTimeData <- data %>%
          filter(Rotacja == "Tak") %>%
          group_by(Nadgodziny, !!sym(input$filter_var)) %>%
          summarise(Count = n(), .groups = 'drop') %>%
          ungroup() %>%
          mutate(Percentage = round(Count / sum(Count) * 100))
        
        ## W Y K R E S --------
        hchart(overTimeData, "column", hcaes(x = !!sym(input$filter_var), y = Percentage, group = Nadgodziny)) %>%
          hc_title(text = "% pracowników rotujących pracujących ponad godziny pracy") %>%
          hc_yAxis(title = list(text = "% pracowników"), min = 0) %>%
          hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}%'))) %>%
          hc_colors(c("#128870", "#333333")) %>%
          hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>% pracowników</b>: {point.y}%') %>% 
          hc_legend(title = list(text = "Nadgodziny", align = "center")) %>%  
          hc_exporting(enabled = TRUE, 
                       buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator",
                                                                         "downloadPNG", "downloadJPEG", 
                                                                         "downloadPDF", "downloadSVG"))))
      })
    }
  })
  
  ## ----------------
  ## Wykres zadowolenia z pracy dla rotujących pracowników
  output$satisfactionRotationPlot <- renderHighchart({
    req(input$filter_var) 

    if (input$gender_split) {
      
      # Jeśli zaznaczony jest podział na płeć
      avgWorkRate <- data %>%
        filter(Rotacja == "Tak") %>%
        group_by(!!sym(input$filter_var), Płeć) %>%
        summarise(AvgWorkRate = round(mean(ZadowolenieZPracy), 2), .groups = 'drop')
      
      ## W Y K R E S -----------
      hchart(avgWorkRate, "column", hcaes(x = !!sym(input$filter_var), y = AvgWorkRate, group = Płeć)) %>%
        hc_title(text = "Średnia ocena pracy dla rotujących pracowników") %>%
        hc_yAxis(title = list(text = "Średnia Ocena Pracy")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}'))) %>%
        hc_colors(c("#128870", "#333333")) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Średnia Ocena Pracy</b>: {point.y}') %>%
        hc_legend(title = list(text = "Płeć", align = "center"))  %>% 
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator",
                                                                       "downloadPNG", "downloadJPEG", 
                                                                       "downloadPDF", "downloadSVG"))))
      
    } else {

      avgWorkRate <- data %>%
        filter(Rotacja == "Tak") %>%
        group_by(!!sym(input$filter_var)) %>%
        summarise(AvgWorkRate = round(mean(ZadowolenieZPracy), 2), .groups = 'drop')
      
      ## W Y K R E S --------
      hchart(avgWorkRate, "column", hcaes(x = !!sym(input$filter_var), y = AvgWorkRate)) %>%
        hc_title(text = "Średnia ocena pracy dla rotujących pracowników") %>%
        hc_yAxis(title = list(text = "Średnia Ocena Pracy")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}'))) %>%
        hc_colors(c("#333333")) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Średnia Ocena Pracy</b>: {point.y}') %>%
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                       "downloadPNG", "downloadJPEG", 
                                                                       "downloadPDF", "downloadSVG"))))
    }
  })
  
  ## ----------------------
  ## Wykres zadowolenia z pracy dla nierotujących pracowników
  output$satisfactionNoRotationPlot <- renderHighchart({
    req(input$filter_var)  
    
    if (input$gender_split) {
      
      # Jeśli zaznaczony jest podział na płeć
      avgWorkRate <- data %>%
        filter(Rotacja == "Nie") %>%
        group_by(!!sym(input$filter_var), Płeć) %>%
        summarise(AvgWorkRate = round(mean(ZadowolenieZPracy), 2), .groups = 'drop')
      
      ## W Y K R E S --------
      hchart(avgWorkRate, "column", hcaes(x = !!sym(input$filter_var), y = AvgWorkRate, group = Płeć)) %>%
        hc_title(text = "Średnia ocena pracy dla nierotujących pracowników") %>%
        hc_yAxis(title = list(text = "Średnia Ocena Pracy")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}'))) %>%
        hc_colors(c("#128870", "#333333")) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Średnia Ocena Pracy</b>: {point.y}') %>%
        hc_legend(title = list(text = "Płeć", align = "center")) %>%  
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator",
                                                                       "downloadPNG", "downloadJPEG", 
                                                                       "downloadPDF", "downloadSVG"))))
      
    } else {

      avgWorkRate <- data %>%
        filter(Rotacja == "Nie") %>% 
        group_by(!!sym(input$filter_var)) %>%
        summarise(AvgWorkRate = round(mean(ZadowolenieZPracy),2), .groups = 'drop')
      
      ## W Y K R E S -------
      hchart(avgWorkRate, "column", hcaes(x = !!sym(input$filter_var), y = AvgWorkRate)) %>%
        hc_title(text = "Średnia ocena pracy dla nierotujących pracowników") %>%
        hc_yAxis(title = list(text = "Średnia Ocena Pracy")) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}'))) %>%
        hc_colors(c("#128870")) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Średnia Ocena Pracy</b>: {point.y}') %>%
        hc_exporting(enabled = TRUE,
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                       "downloadPNG", "downloadJPEG",
                                                                       "downloadPDF", "downloadSVG"))))
    }
  })
  
  
  ## Wykres kołowy rotacji wg płci
  output$genderAttritionPlot <- renderHighchart({
    
    genderAttrition <- data %>%
      filter(Rotacja == "Tak") %>%
      group_by(Płeć) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100)  
    
    ## W Y K R E S -------
    hchart(genderAttrition, "pie", hcaes(y = Percentage, name = Płeć)) %>%
      hc_title(text = "Procentowy udział w rotacji według płci") %>%
      hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format = '{point.y:.1f}%', 
          style = list(color = "white", textOutline = '0px'),connectorWidth = 0, distance = -30),
        startAngle = 0, endAngle = 360, innerSize = '0%',  size = '70%')) %>%
      hc_colors(c("#128870", "#333333")) %>%  
      hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Procentowy udział</b>: {point.y:.1f}%') %>% 
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                     "downloadPNG", "downloadJPEG", 
                                                                     "downloadPDF", "downloadSVG"))))
  })
  
  # Mapa cieplna satysfakcji z pracy względem płci i działu
  output$worklifeBalanceHeatmapPlot <- renderHighchart({

    worklifeBalance_data <- data %>%
      filter(Rotacja == "Tak") %>% 
      group_by(Płeć, !!sym(input$filter_var)) %>%
      summarise(AvgWorkLifeBalance = round(mean(WorkLifeBalance), 2)) %>%
      ungroup()
    
    ## W Y K R E S ---------------
    hchart(worklifeBalance_data, type = "heatmap", 
           hcaes(x = Płeć, y = !!sym(input$filter_var), value = AvgWorkLifeBalance)) %>%
      hc_title(text = "Mapa cieplna Work-Life Balance dla rotujących pracowników") %>%
      hc_colorAxis(stops = color_stops(10, viridisLite::viridis(10))) %>%
      hc_xAxis(title = list(text = "Płeć")) %>%
      hc_tooltip(pointFormat = 'Średni Work-Life Balance: <b>{point.value}</b>') %>%
      hc_plotOptions(heatmap = list(dataLabels = list(enabled = TRUE, format = '{point.value}'))) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = c("viewFullscreen", 
                                                                     "separator", 
                                                                     "downloadPNG", 
                                                                     "downloadJPEG", 
                                                                     "downloadPDF", 
                                                                     "downloadSVG"))))
  })
  
  # 2. V A L U E   B O X Y ----------
  ## Pasek postępu
  withProgress(message = "Loading...", value = (i_prog-1)/tot_step,{
    # Zwiększ pasek postępu i zaaktualizuj tekst
    incProgress(i_prog/tot_step, detail = NULL)
  })
  
  i_prog <- i_prog + 1
  

  
  ## 1. Wskaźnik rotacji
  output$attritionRate <- renderValueBox({
    valueBox(
      paste0(attritionRate, "%"), "Wskaźnik rotacji pracowników", icon = icon("rotate"), 
      color = "olive")
  })
  
  ## 2. Liczba rotujących pracowników 
  output$numberOfAttrPp <- renderValueBox({
    valueBox(
      paste0(numberOfAttrPp$Count), "Ilość rotujących osób", icon = icon("venus-mars"), color = "teal")
  })
  
  ## 3. Liczba rotujących kobiet
  output$femaleAttrition <- renderValueBox({
    valueBox(
      paste0(genderAttrition[1,2], " z ", numberOfAttrPp$Count), "Ilość rotujących kobiet", 
      icon = icon("person-dress") , color = "maroon")
  })
  
  ## 4. Liczba rotujących mężczyzn
  output$maleAttrition <- renderValueBox({
   valueBox(
     paste0(genderAttrition[2,2], " z ", numberOfAttrPp$Count), "Ilość rotujących mężczyzn",
     icon = icon("person"), color = "purple"
   ) 
  })
  
  ## 5. Średni wiek rotującej osoby
  output$avgAge <- renderValueBox({
    valueBox(
      paste0(avgAgeAttrition), "Średnia wieku rotujących pracowników", icon = icon("cake-candles"),
      color="yellow"
    )
  })
  
  ## 6. Średni miesięczny przychód rotującej osoby
  output$avgIncome <- renderValueBox({
    valueBox(
      paste0("$",avgIncome), "Średni miesięczny przychód rotujących pracowników",
      icon = icon("hand-holding-dollar"), color = "green"
    )
  })
  
  ## 7. Średnia ilość lat przepracowanych w firmie przez osobę rotjącą
  output$avgYearsAtCompany <- renderValueBox({
    valueBox(
      paste0(avgYearsAtCompanyAttrition), "Średnia ilość lat w firmie",
      icon = icon("briefcase"), color = "black"
    )
  })
  
  ## 8. Średni dystans od domu do biura dla osoby rotującej
  output$avgDistanceFromHome <- renderValueBox({
    valueBox(
      paste0(avgDistanceFromHome, " km"), "Średnia odległość z biura do domu",
      icon = icon("route"), color = "orange"
    )
  })
  
  ## 9. Średnia ocena pracy przez rotującego pracownika
  output$avgJobRate <- renderValueBox({
    valueBox(
      paste0(workRateAvgAtrittion), 
      "Ocena pracy",
      icon = icon("star"), color = "fuchsia"
    )
  })
  
  ## 10. Średni Work Life Balance dla rotującego pracownika
  output$avgWorkLifeBalance <- renderValueBox({
    valueBox(
      paste0(workLifeBalanceAvgAttrition), "Work Life Balance rotujących pracowników",
      icon = icon("heart-circle-check"), color = "maroon"
    )
  })
  
  ## 11. % osób, które rotowały i pracowały w godzinach nadliczbowych
  output$leftAndOverWorked <- renderValueBox({
    valueBox(
      paste0(overtimeAttrition, "%"), 
      "Osoby, które opuściły firmę i pracowały w godzinach nadliczbowych",
      icon = icon("clock"), color = "purple"
    )
  })
  
  ## 12. Pozycja, na której rotuje najwięcej osób
  output$jobRole <- renderValueBox({
    valueBox(
      paste0(jobRole$Stanowisko[1]), 
      "Stanowisko, na którym rotuje najwięcej osób",
      icon = icon("vial-virus"), color = "yellow"
    )
  }) 

}

