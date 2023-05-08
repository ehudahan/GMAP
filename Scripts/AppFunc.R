# Ehud Dahan

if (1) {
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  load("Data/App.Rdata")
  LAST_UPDATE <- as.character(file.info("Scripts/DeployApp.R")$mtime)
}

library(shiny)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(scales)
library(RColorBrewer)
library(tibble)
library(dplyr)
library(reshape2)
library(ggpubr)
library(arrangements) #for combinations

ShinyAppFunction <- function(metadata, 
                             mat, 
                             tab.n, 
                             samples,
                             colorGMAPDataFunction,
                             LAST_UPDATE){
  
  ############################## constants ################################################################
  SITE_TITLE <- HTML("<h1>EasyMAP</h1>",
                     "<b>Interactive tool to analaysed MaAsLin2 result from GMAP project</b>",
                     "<div>by Ehud Dahan from Moran Yassour lab</div>", 
                     "<div>Last update: ", LAST_UPDATE, "<br><br><br>")
  ALPHA = 0.3
  JITTER_WIDTH = 0.4
  
  # GUI ####
  ui <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      SITE_TITLE,
      box(
        uiOutput('heatmap_subset_filter'),
        uiOutput('heatmap_feature_filter'),
        uiOutput('heatmap_measured_variable_filter'),
        uiOutput('heatmap_qval_filter'),
        hr(),
        actionButton(
          'update_heatmap',
          icon = icon(name = "refresh"),
          label = "",
          width = "100%"
        ),
        hr(),
        actionButton(
          'add_heatmap_to_pdf',
          label = HTML("Add To PDF"),
          width = "100%"
        ),
        width = 2,
        solidHeader = T,
        collapsible = T,
        collapsed = F,
        title = "Heatmap filters",
        status = "primary"
      ),
      box(
        plotOutput(
          'heatmap',
          click = "plotClick",
          hover = "heatmapHover",
          width = "100%",
          height = "700px"
        ),
        textOutput('heatmapTextOutput'),
        uiOutput('qval_slider', align = "right"),
        width = 10,
        solidHeader = T,
        collapsible = T,
        title = "Heatmap",
        status = "primary"
      ),
      box(
        pickerInput(
          inputId = "colored_by",
          label = "Colored by:",
          choices = names(metadata)[4:length(names(metadata))], 
          options = list(`actions-box` = TRUE),
          selected = names(metadata)[length(names(metadata))]),
        pickerInput(
          inputId = "facet_by",
          label = "Facet by:",
          choices = c("Unfaceted", names(metadata)[4:length(names(metadata))]), 
          selected = "Unfaceted",
          options = list(`actions-box` = TRUE)),
        HTML("<b>Drop NAs</b>"),
        switchInput(
          inputId = "drop_na",
          value = FALSE,
          width = "100%"
        ),
        actionButton(
          'update_plot',
          icon = icon(name = "refresh"),
          label = "",
          width = "100%"
        ),
        
        width = 2,
        solidHeader = T,
        collapsible = T,
        title = "Box Plot Filters",
        status = "primary"
      ),
      # Box Plot part
      box(
        plotOutput('interactive_box_plot', width = "100%"),
        width = 10,
        solidHeader = T,
        collapsible = T,
        title = "Interactive Box Plot",
        status = "primary"
      ),
      
          # End of Box Plot part

          box(
            plotOutput('export1'),
            actionButton(inputId = "add1", label = "Add plot here"),
            plotOutput('export2'),
            actionButton(inputId = "add2", label = "Add plot here"),
            plotOutput('export3'),
            actionButton(inputId = "add3", label = "Add plot here"),
            downloadButton(outputId = "download", label = "Download PDF"),
            solidHeader = T,
            width = 12,
            collapsible = T,
            collapsed = F,
            title = "Export plots",
            status = "primary"
          )
        )
    )
  
  
  
  
  # SERVER ####
  server <- shinyServer(function(input, output) {

    reactive_mat     <- reactive({ 
      mat2 <- mat %>% 
        filter(Subset %in% input$Subsets) %>%
        as.data.frame()
      return(mat2)
    })
    
    output$heatmap_qval_filter <- renderUI({
      numericInput(inputId = "qval",label = "Q value",value = 0.25, min = 0,step = 0.01,width = "80%") # CHECK WHY 1.4
    })
    
    filtered_mat     <- reactive({
      reactive_mat() %>%
        filter(Feature %in% input$Feature) %>%
        filter(Measured.Variable %in% input$Measured.Variable) %>%
        filter(qval<=input$qval)
    })
    
    
    output$heatmap_subset_filter     <- renderUI({pickerInput(inputId = "Subsets", choices = sort(unique(mat$Subset)), selected = sort(unique(mat$Subset))[1], options = list(`actions-box` = TRUE), label = "Subset of Samples")})
    output$heatmap_feature_filter <- renderUI({pickerInput(inputId = "Feature", choices = sort(unique(reactive_mat()[,"Feature"])), selected = unique(reactive_mat()[,"Feature"]), options = list(`actions-box` = TRUE), label = "Feature", multiple = T)})
    output$heatmap_measured_variable_filter <- renderUI({pickerInput(inputId = "Measured.Variable", choices = sort(unique(reactive_mat()[,"Measured.Variable"])), selected = unique(reactive_mat()[,"Measured.Variable"]), options = list(`actions-box` = TRUE), label = "Variables", multiple = T)})
    output$heatmap_model_name_filter <- renderUI({pickerInput(inputId = "Model.Name", choices = sort(unique(reactive_mat()[,"Model.Name"])), selected = unique(reactive_mat()[,"Model.Name"]), options = list(`actions-box` = TRUE), label = "Model Name", multiple = T)})
    reactive_heatmap <- reactive({
      ggplot(data = filtered_mat(), aes(y = Feature, x = Measured.Variable, fill = qval)) +
        geom_tile() +
        facet_grid(~Model.Name,drop = T,
                   scales = "free",space = "free",
                   labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
        scale_fill_gradientn(colours = c("#377EB8", "white"), # "#E41A1C"
                             values = scales::rescale(c(input$qval_slider[1],input$qval_slider[2])),
                             limits=input$qval_slider, oob = scales::squish)+
        theme_linedraw()+
        labs(title = input$Subset)+
        theme(panel.grid.major = element_blank(),
              strip.placement = "outside",
              legend.position = "bottom",legend.justification = "right",
              axis.text.x = element_text(angle = 90, hjust = 1))
    })

    y_variable       <- reactive({
      filtered_mat() %>% 
        .$Feature %>% 
        as.character() %>% 
        factor() %>% 
        levels() %>%
        .[round(input$plotClick$y)] %>% 
        as.character()
    })
    x_variable       <- reactive({
      filtered_mat() %>% 
        filter(Model.Name == as.character(input$plotClick$panelvar1)) %>% 
        .$Measured.Variable %>% 
        as.character() %>% 
        factor() %>% 
        levels() %>%
        .[round(input$plotClick$x)] %>% 
        as.character()
    })
    
    stat_table <- reactive({
      
      results <- mat %>% 
        filter(Subset %in% input$Subsets) %>%
        filter(Model.Name == as.character(input$plotClick$panelvar1)) %>% 
        filter(Feature == y_variable()) %>% 
        filter(Measured.Variable == x_variable())
      
      stat_table <- tibble(.rows = nrow(results))      
      stat_table$group1 <- results$Test.Variable
      stat_table$group2 <- results$Reference.Variable
      stat_table$y.position <- max(data()[,"value"]) + 0.1
      stat_table$q.value <- paste0("q=", results$qval)
      
      return(stat_table)
    })

    data             <- reactive({
      subset_samples <- samples[[input$Subsets]]
      tab.n.filtered <- tab.n %>% rownames_to_column("short.names")
      tab.n.filtered <- tab.n.filtered[,c("short.names", subset_samples)]
      
      data <- tab.n.filtered %>% 
        filter(short.names == y_variable()) %>%
        melt(variable = "sampleID") %>% 
        left_join(metadata,by = "sampleID") 
      
      if (input$drop_na == TRUE){
        return(na.omit(data))
      }
      return(data)
    })
    
    reactive_plot    <- reactive({
      title = paste0("Subset: ", input$Subsets)
      subtitle <- paste0(input$plotClick$mapping$panelvar1, ": ", input$plotClick$panelvar1, "\n",
                         input$plotClick$mapping$panelvar2, ": ", input$plotClick$panelvar2)
      x_axis <- x_variable()
      bac <- y_variable()
      
      x_axis_vars <- data() %>% select(!!!x_axis) %>% unique() %>% .[,1] # the groups to color
      Combs <- combinations(k =  2, v = x_axis_vars, replace = F,layout = "list")
      
      
      g <- ggplot(data = data(), aes_string(x = x_axis, y = "value"))
      if(x_axis=="visit_age_mo") { # VISIT_AGE_MO is numeric
        g2 <- g +
          geom_smooth(aes_string(color = input$colored_by),method = "glm", se = F, alpha = ALPHA)+
          geom_point(aes_string(color = input$colored_by), alpha = ALPHA, size = 4)+
          geom_text(size = 2, aes_string(label = "record_id"))
      } else{
        g1 <- g + 
          geom_point(aes(color = get(input$colored_by)),alpha = ALPHA, size = 4, position = position_jitterdodge(jitter.width = JITTER_WIDTH, seed = 1))+
          geom_text(aes_string(fill = input$colored_by,
                               label = "record_id"), size = 2,inherit.aes = T,position = position_jitterdodge(jitter.width = JITTER_WIDTH, seed = 1))+
          geom_boxplot(inherit.aes = T,aes(fill = get(input$colored_by)),alpha = ALPHA, outlier.size = 0) 
          
        if(input$facet_by!="Unfaceted"){
        g2 <- g1 + facet_grid(~get(input$facet_by)) +
          labs(subtitle = "P-Values calculated by Students T Test") +
          stat_compare_means(comparisons = Combs, method = "t.test")
        }else{
          
        g2 <- g1 +
            labs(subtitle = "Q-Values calculated by MaAsLin2")+
            stat_pvalue_manual(stat_table(), label = "q.value",step.increase = 0.1)
        }
      }
      g3 <- g2 +
        scale_fill_manual(breaks = input$colored_by, values = colorGMAPDataFunction(input$colored_by))+
        scale_color_manual(input$colored_by, values = colorGMAPDataFunction(input$colored_by))+
        labs(title = title,
             subtitle = subtitle,
             y = paste0(bac," (AST)"), 
             x = x_axis) + 
        theme_classic() + 
        theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),  axis.title.y = element_text(size = 10))
      
      
      return(g3)
    })
    
    
    observeEvent(input$update_heatmap, {
      output$heatmapTextOutput <- renderText({
        if(length(input$heatmapHover)){
          
          y_hover       <- reactive({
            filtered_mat() %>% 
              .$Feature %>% 
              as.character() %>% 
              factor() %>% 
              levels() %>% 
              .[round(input$heatmapHover$y)] %>% 
              as.character()
          })
          x_hover       <- reactive({
            filtered_mat() %>% 
              filter(Model.Name == as.character(input$heatmapHover$panelvar1)) %>% 
              .$Measured.Variable %>% 
              as.character() %>% 
              factor() %>% 
              levels() %>% 
              .[round(input$heatmapHover$x)] %>% 
              as.character()
          })
          
          
          text <- HTML(x_hover(), " | ",
                       y_hover())
          return(text)
        }})
      
    })
    
    output$qval_slider                   <- renderUI({
      min <- 0.0001
      max <- 0.25
      
      sliderInput(inputId = "qval_slider",label = "Q values scale",
                  min = min, max = max, 
                  round = T,ticks = F,width = "20%",
                  step = 0.01, 
                  value = c(min,max))
    })
    

    observeEvent(input$update_heatmap, {
      output$heatmap <- renderPlot(isolate(reactive_heatmap()))
    })
    observeEvent(input$add_heatmap_to_pdf, {
      plots$heatmap <- isolate(reactive_heatmap())
    })

    observeEvent(input$plotClick, {
      output$interactive_box_plot <- renderPlot(isolate(reactive_plot()))
    })
    
    plots <- reactiveValues()

    observeEvent(input$add1, {
      isolate_react_plot <- isolate(ggarrange(reactive_plot()))
      output$export1 <- renderPlot(isolate_react_plot)
      plots$export1 <- isolate_react_plot
    })
    observeEvent(input$add2, {
      isolate_react_plot <- isolate(ggarrange(reactive_plot()))
      output$export2 <- renderPlot(isolate_react_plot)
      plots$export2 <- isolate_react_plot
    })
    observeEvent(input$add3, {
      isolate_react_plot <- isolate(ggarrange(reactive_plot()))
      output$export3 <- renderPlot(isolate_react_plot)
      plots$export3 <- isolate_react_plot
    })
    
    output$download <- downloadHandler(filename = "output.pdf",content = function(file){
      ggsave(file,device = "pdf",
             plot = ggarrange(plotlist = list(plots$heatmap,
                                              plots$export1, 
                                              plots$export2, 
                                              plots$export3),
                              nrow = 4),
             width = 21.5,
             height = 29.7)
    })
    
    
    output$download_comp <- downloadHandler(filename = "output.pdf", content = function(file){
      ggsave(file, device = "pdf", plot = plots$comp)
    })
  })
  
  
  # Run the application 
  return(shinyApp(ui = ui, server = server))
}

if(1){
ShinyAppFunction(metadata, 
                 maaslin_results, 
                 tab.ast, 
                 samples, 
                 colorGMAPDataFunction,
                  LAST_UPDATE)
}
  