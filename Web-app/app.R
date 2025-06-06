library(shiny)
library(shinyWidgets)
library(readr)
library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(scales)
library(stats)
library(ggpubr)
library(ggrepel)


options(scipen = 80)  

file_pre <- "./"     

heart_sf <- sf::st_read(dsn = paste(file_pre, "heart.geojson", sep = "/"), 
                    stringsAsFactors = FALSE, quiet = TRUE)      

heart_sf1 <- sf::st_read(dsn = paste(file_pre,  "heart1.geojson", sep = "/"),   
                        stringsAsFactors = FALSE,  quiet = TRUE)

demo_df <- read_csv(file = paste(file_pre, "heart_longdata.csv", sep = "/"),
                    col_names = TRUE)


Meta_v <- sort(unique(demo_df$Metabolite))

Mann_stat  <- read.csv("Heart.MannWhitney.BH.csv")

all_regions <- c("LV", "IVS", "RV", "LA", "IAS", "RA", "AV", "MV", "PV", "TV", "LCA", "RCA", "AA", "PA", "SVC", "IVC", "PVn")

Region_df <- data.frame(x = 1:length(all_regions), stringsAsFactors = FALSE,
                        Region = all_regions)

region_order <- function(odf, allregions){
  df_Region <- odf$Region
  index_order <- c()
  for (x in seq_along(allregions)){
    index <- which(df_Region %in% allregions[x])
    index_order <- c(index_order, index)
  }
  return(odf[index_order,])
}


ui <- fluidPage(
    
    title="The Metabolome Atlas of the Nonfailing and Failing Human Heart",

    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        
    ),


    fluidRow(
      
      column(
        width = 8, align = "left",   
        HTML(paste("", h1("The Metabolome Atlas of the Nonfailing and Failing Human Heart", style = "font-family:arial; font-size: 28pt; color:#FFCD00"), sep = '<br/>'))
      ),      
      
      column(
        width = 4, align = "right",
        tags$a(
          img(src = "logo.png", style = "width: 100%; height: auto;"), target = "_blank")
      ),
        
        id = "titleBar"
    ),

    sidebarLayout(
        sidebarPanel(width = 3, 
            h2("Parameter Setting", align = "center",
               style = "font-family:arial; font-size:25pt; color:#000000"),
            hr(),
            
            pickerInput(
                inputId = "meta",
                label = "Metabolite",
                choices = Meta_v,
                multiple = FALSE,
                options = list(
                    `actions-box` = TRUE,
                    size = 17),
                selected = c("Adenosine")
            ),
            br(),
            hr(),

            p(img(src = "heart_figure.png", height = "auto", width = "100%"), align = "left"),
            br(),
            awesomeCheckboxGroup(
                inputId = "region",
                label = "Heart Region (Multiple options)",

                choices = c("1.Left ventricle (LV)" = "LV",
                            "2.Interventricular septum (IVS)" = "IVS",
                            "3.Right ventricle (RV)" = "RV",
                            "4.Left atrium (LA)" = "LA",
                            "5.Interatrial septum (IAS)" = "IAS",
                            "6.Right atrium (RA)" = "RA",
                            "7.Aortic valve (AV)" = "AV",
                            "8.Mitral valve (MV)" = "MV",
                            "9.Pulmonary valve (PV)" = "PV",
                            "10.Tricuspid valve (TV)" = "TV",
                            "11.Left coronary artery (LCA)" = "LCA",
                            "12.Right coronary artery (RCA)" = "RCA",
                            "13.Aorta (AA)" = "AA",
                            "14.Pulmonary artery (PA)" = "PA",
                            "15.Superior vena cava (SVC)" = "SVC",
                            "16.Inferior vena cava (IVC)" = "IVC",
                            "17.Pulmonary vein (PVn)" = "PVn"),
                
                selected = Region_df$Region
                
            ),
            br(),
            hr(),

            awesomeCheckboxGroup(
              inputId = "class",
              label = "Class (Multiple options)",
              choices = c("Nonfailing heart", "Failing heart"),
              selected = c("Nonfailing heart", "Failing heart"),
              inline = TRUE
            ),
            
            hr()
            
            
        ),

        mainPanel(
            
            fluidRow(
              
                column(width = 10, align = "center",
                  h1(textOutput("text1"), style = "font-family:arial; font-size:30pt; color:#000000")
                ), 
                hr(),
                
                column(width = 5, height = 300, align = "center",
                       plotOutput("plot_w3",  width = "auto", height = 300)),

                column(width = 5, height = 300, align = "center",
                       plotOutput("plot_w92", width = "auto", height = 300)),
                
                column(
                  width = 2, align = "right",    
                  tags$a(img(src = "legend.png", style = "width: auto; height: 300;"))
                ),
                
            ),
            hr(),
            
  
            fluidRow(
               column(width = 5, height = 50,
                      p("Nonfailing heart",
                        style = "font-family:arial;font-size:18pt; color:#F39B7E", align = "center")),
 
               column(width = 5, height = 50,
                      p("Failing heart",
                        style = "font-family:arial;font-size:18pt; color:#7F6149", align = "center")),
            ), 
            hr(),
            
            fluidRow(
               column(width = 12, height = 550,
                      plotOutput("plot_bar", width = "auto", height = 450),
                      br(), 
                      hr(), 

                      p("Note: Mann–Whitney U test was used for significance analysis. 
                        We use the following convention for symbols indicating statistical significance, ns: p > 0.05; *: p <= 0.05; **: p <= 0.01; ***: p <= 0.001; ****: p <= 0.0001", 
                        style = "font-family:arial;font-size:12pt; color:#000000")
                )
               
            )
        )
    )
)


fun1 <- function(df){

  mycolors <- c("#4B95C8", "#FCF5E7", "#DA4F51")
  mypal <- colorRampPalette(mycolors)(101)

  suppressMessages(
  df1 <- df %>% select(Region, Class, Intensity) %>%
    filter( !(Region == "PVn" & Class == "Failing heart") )  %>% 
    group_by(Region, Class) %>% 
    summarise(medianInten = median(Intensity, na.rm = TRUE)) %>% ungroup() ) 

    df1 <- region_order(df1, all_regions)  

    minInten <- min(df1$medianInten)
    maxInten <- max(df1$medianInten)
    
  df_A <- df1 %>% mutate(normInten = round((medianInten - minInten)/(maxInten - minInten)*100)) %>%
    mutate(color = mypal[normInten + 1]) %>% 

    select(Region, Class, color) %>% 
    replace_na(list(color = "#91D1C1"))
  
  df_A <- region_order(df_A, all_regions) 
  
  df_B <- df %>% select(Region, Class, Intensity) %>%
    right_join(Region_df, by = "Region") %>%
    mutate(A2 = factor(x = Class, levels = c("Nonfailing heart", "Failing heart"))) %>%
    select(x, Region, Intensity, Class = A2)  
  
  suppressMessages(
  y_p <- df %>% select(Region, Intensity) %>%   
    group_by(Region) %>%
    summarise(y_max = max(Intensity, na.rm = TRUE),  
              y_diff = sd(Intensity, na.rm = TRUE)/sqrt(sum(!is.na(Intensity))))   %>%
    ungroup() %>% 
    transmute(y_up = y_max) %>% pull(y_up) %>% 
    max() %>% `*`(1.1) )  
  
  return(list(df_A, df_B, y_p))
    
}

server <- function(input, output) {
  
    reaction1 <- reactive({

      req(input$meta, input$region, input$class) 
      options(scipen = 80)
      
      demo_df %>%  filter(Region %in% input$region, 
                          Metabolite == input$meta, 
                          Class %in% input$class)  %>% 
            fun1()     
    })
    
    
    output$text1 <- renderText({ 
        input$meta
    })
    
    
    output$plot_w3 <- renderPlot({
      
      sf_w3 <- heart_sf %>% left_join(filter(reaction1()[[1]], Class == "Nonfailing heart"), by = "Region") %>%
        select(-Class)
      
      sf_w3 <- region_order(sf_w3, all_regions) 
      sf_w3$Region <- factor(sf_w3$Region, levels=all_regions)
      
      ggplot(sf_w3) +
        geom_sf(aes(fill = Region), color = "black", show.legend = FALSE) +
        coord_sf() +
        scale_fill_manual(values = sf_w3$color,
                          labels = sf_w3$Region) +
        theme_void()
    })


    output$plot_w92 <- renderPlot({
      
      sf_w92 <- heart_sf1 %>% left_join(filter(reaction1()[[1]], Class == "Failing heart"), by = "Region") %>%
        select(-Class)
      
      sf_w92 <- region_order(sf_w92, all_regions) 
      sf_w92$Region <- factor(sf_w92$Region, levels=all_regions)
      
      ggplot(sf_w92) + 
        geom_sf(aes(fill = Region), color = "black", show.legend = FALSE) + 
        coord_sf() + 
        scale_fill_manual(values = sf_w92$color, 
                          labels = sf_w92$Region) + 
        theme_void()
    })
    
      
    output$plot_bar <- renderPlot({    
      
      
      out_data <- reaction1()[[2]]
      out_data$Region <- factor(out_data$Region, levels= all_regions )
      
      suppressMessages(
      summ_ifo <- out_data %>% group_by(x, Region, Class)     %>%
        summarise(max_val = max(Intensity, na.rm = TRUE), min_val= min(Intensity, na.rm = TRUE),
                  mean_val = mean(Intensity, na.rm = TRUE), median_val = median(Intensity, na.rm = TRUE),
                  std = sd(Intensity, na.rm = TRUE), ste = sd(Intensity, na.rm = TRUE)/sqrt(sum(!is.na(Intensity))) ) )
      
      summ_ifo <- summ_ifo[order(summ_ifo$x, decreasing = F),]
      
      
      df_Wilcox <- Mann_stat[Mann_stat$Metabolite==input$meta, ]  
      df_Wilcox_fdr <- Region_df %>% left_join(df_Wilcox, by = "Region")
      
    
      ggplot(data = out_data) +
        
        stat_boxplot(aes(x = Region, y = Intensity, fill = Class), geom = "errorbar", width = 0.3, position = position_dodge(.8))  +
        
        geom_boxplot(aes(x = Region, y = Intensity, fill = Class), width = 0.8, outlier.shape = NA)  +   
        
        geom_point(aes(x = Region, y = Intensity, fill = Class), pch=21, size = 1, position = position_jitterdodge(.6))   + 
        
        scale_y_continuous(labels = scientific)  +
        
        scale_fill_manual(breaks = c("Nonfailing heart", "Failing heart"), 
                          values = c("#F39B7E", "#7F6149")) +

        labs(x = "Heart Regions", y = "Peak Intensity") +
        
        annotate("text", x=17.2, y= as.numeric(summ_ifo[(summ_ifo$Region=="PVn" & summ_ifo$Class=="Nonfailing heart"), "max_val"]), label="NA", family="Arial", colour="black", size=5)   +  

        annotate("text", x=df_Wilcox_fdr$x, y= as.numeric(max(summ_ifo$max_val) * 1.1), label=df_Wilcox_fdr$p.adj.reg.star, family="Arial", colour="black", size=6)   +
         
        theme(
          legend.position = 'none',
          axis.text.y  = element_text(size = 18, color = "black"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, color = "black", family="Arial"),
          axis.ticks.length = unit(2, "mm"),
          axis.ticks = element_line(size = 0.5),
          axis.line = element_line(size = 0.5),
          axis.title = element_text(size = 20,  family="Arial", color = "black"),
          panel.grid = element_blank(),
          panel.background = element_blank())
      
    })
 
    
}

shinyApp(ui = ui, server = server)
