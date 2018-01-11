
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(
  function(input, output, clientData, session) {  
    
    rs <- reactiveValues(architect = NULL,
                         genotypes = NULL,
                         archi = NULL)
    
    
  # Load the dataset
  # observeEvent(input$load_data, {
    
    observe({
      rs$names <- read_csv("www/names.csv")
    })
    
    ## Load the data -------
    observeEvent(input$folder_path_button, {
      
      path <- input$folder_path
      # path <- "/Users/g.lobet/Desktop/test"
      
      archi <- rsmlToTable(path, fitter=T)
      architect <- architect(inputrsml = archi, fitter = T)
      
      # genotypes <- unlist(
        
      dats <- strsplit(as.character(architect$FileName), "_")
      factors <- NULL#data.frame(id=c(i:nrow(architect)))
      for(i in c(1:(length(dats[[1]])-1))){
        temp <- unlist(lapply(dats, `[[`, i))[]
        factors <- cbind(factors, unlist(lapply(dats, `[[`, i))[]) 
      #  colnames(factors)[colnames(factors) == "temp"] <- paste0("col-",i)
      }
      rs$factors <- as.data.frame(factors)
      
      dats <- strsplit(as.character(archi$file), "_")
      factors <- NULL#data.frame(id=c(i:nrow(architect)))
      for(i in c(1:length(dats[[1]]))){
        temp <- unlist(lapply(dats, `[[`, i))[]
        factors <- cbind(factors, unlist(lapply(dats, `[[`, i))[]) 
        #  colnames(factors)[colnames(factors) == "temp"] <- paste0("col-",i)
      }
      rs$factors_2 <- as.data.frame(factors)
      
      rs$architect_origin <- architect
      rs$archi_origin <- archi
      #rs$cols1 <- colnames(archi)[-1]
      #rs$cols2 <- colnames(architect)[-1]
      
      
    })
    
    
    ## Update the genotype -----
    observeEvent(input$update_data, {
      
      archi <- rs$archi_origin
      architect <- rs$architect_origin
      factors <- rs$factors
      factors_2 <- rs$factors_2
      
      archi$genotype <- "-"
      archi$treatment1 <- "-"
      archi$treatment2 <- "-"
      archi$date <- 1
      archi$rep <- 1
      
      architect$genotype <- "-"
      architect$treatment1 <- "-"
      architect$treatment2 <- "-"
      architect$date <- 1
      architect$rep <- 1
      
      
      if(input$gens) archi$genotype <- factors_2[[input$genotypes]]
      if(input$tr1) archi$treatment1 <- factors_2[[input$treatment1]]
      if(input$tr2) archi$treatment2 <- factors_2[[input$treatment2]]
      if(input$ti) archi$date <- factors_2[[input$timestamp]]
      if(input$rep) archi$rep <- as.numeric(gsub("[[:alpha:]]", "",factors_2[[input$repetition]]))
      
      if(input$gens) architect$genotype <- factors[[input$genotypes]]
      if(input$tr1) architect$treatment1 <- factors[[input$treatment1]]
      if(input$tr2) architect$treatment2 <- factors[[input$treatment2]]
      if(input$ti) architect$date <- factors[[input$timestamp]]
      if(input$rep) architect$rep <- as.numeric(gsub("[[:alpha:]]", "",factors[[input$repetition]]))

      
      # cols <- match(rs$cols2,colnames(architect))
      architect <-  architect %>%
        select(FileName, genotype, treatment1, treatment2, date, rep, everything())
      
      # cols <- match(rs$cols,colnames(archi))
      archi <-  archi %>%
        select(file, genotype, treatment1, treatment2, date, rep, everything())
      
      rs$architect <- architect
      rs$archi <- archi
      rs$genotypes <- unique(archi$genotype)
      
      archi <- archi %>%
        mutate(plant_id = paste0(file,"-",plant))
      
      
      
      convexhull <- NULL
      for(id in unique(archi$plant_id)){
        temp <- archi %>% filter(plant_id == id)
        temp <- temp %>% 
          mutate(x1 = x1-(min(x1) + (max(x1)-min(x1))/2)) %>%
          mutate(y1 = y1-(min(y1) + (max(y1)-min(y1))/2))
        hpts <- chull(temp$x1, temp$y1)
        hpts <- c(hpts, hpts[1])
        temp <- temp[hpts,]
        temp2 <- temp[,c("x1","y1")] %>%
          mutate(x_end = x1) %>%
          mutate(y_end = y1) %>%
          select(x_end, y_end)
        temp2 <- rbind(temp2[-1,], temp2[1,])
        temp <- cbind(temp,temp2) %>%
          select(x1,y1,x_end,y_end) %>%
          mutate(plant_id = id)
        convexhull <- rbind(convexhull, temp)
      }
      
      convexhull <- merge(convexhull, archi[,c("plant_id", "genotype","treatment1","treatment2","rep","date")], by="plant_id")
      
      ggplot(convexhull) +
        geom_segment(aes(x1,y1,xend=x_end,yend=y_end, group=plant_id)) +
        facet_wrap(~genotype)
        theme_classic()
      
      rs$roots <- ddply(archi, .(file, genotype, treatment1, treatment2, date, rep, plant, root), summarise, n=length(file))
          
    })    
    
    
    
    ## Process the data ---- 
    observeEvent(input$process_data, {
      
      archi <- rs$archi
      architect <- rs$architect
      
      
      withProgress(message = 'Computing the histograms', {
  
        # Compute the angle data to make the histgram
        archi$rangle <- round(archi$orientation)
        hist1 <- ddply(archi, .(time, genotype, treatment1, treatment2, date, rep, file, root, rangle), summarize, n=ceiling(sum(length/10)))
        angle <- expandRows(hist1, "n")

        # Compute the diameter data to make the histgram
        archi$rdiam <- round(archi$diameter2)
        hist1 <- ddply(archi, .(time, genotype, treatment1, treatment2, date, rep, file, root, rdiam), summarize, n=ceiling(sum(length/10)))
        diameter <- expandRows(hist1, "n")
        
        # Compute the growth data to make the histgram
        archi$rgrowth <- round(archi$growth)
        hist1 <- ddply(archi, .(time, genotype, treatment1, treatment2, date, rep, file, root, rgrowth), plyr::summarize, n=ceiling(sum(length/10)))
        growth <- expandRows(hist1, "n")
        
        # Compute the depth data to make the histgram
        archi$rdepth <- round(archi$y2)
        hist1 <- ddply(archi, .(time, genotype, treatment1, treatment2, date, rep, file, root, rdepth), plyr::summarize, n=ceiling(sum(length/10)))
        depth <- expandRows(hist1, "n")
        
        # Compute the geodesic data to make the histgram
        archi$rgeo <- round(archi$geodesic)
        hist1 <- ddply(archi, .(time, genotype, treatment1, treatment2, date, rep, file, root, rgeo), summarize, n=ceiling(sum(length/10)))
        geodesic <- expandRows(hist1, "n")
        
        # Compute the magnitude data to make the histgram
        hist1 <- ddply(archi, .(time, genotype, treatment1, treatment2, date, rep, file, root, magnitude), summarize, n=ceiling(sum(length/10)))
        magnitude <- expandRows(hist1, "n")
        
        # Compute the pathlength data to make the histgram
        hist1 <- ddply(archi, .(time, genotype, treatment1, treatment2, date, rep, file, root, pathlength), summarize, n=ceiling(sum(length/10)))
        pathlength <- expandRows(hist1, "n")
        
        # Compute the length data to make the histgram
        length <- ddply(archi, .(file, plant, root, genotype, treatment1, treatment2, date, rep), summarise, value=sum(length))
        
        histogram = list("angle"=angle,
                         "diameter"=diameter,
                         "growth"=growth,
                         "length"=length,
                         "depth"=depth,
                         "geodesic"=geodesic,
                         "magnitude"=magnitude,
                         "pathlength"=pathlength)
      })
      
      withProgress(message = 'Computing the persistance homology', {
        perhomology <- perhomology(archi)
        
        distance <- bottleneckdist(perhomology)

        distance <- as.data.frame(distance)
        distance$genotypes <- architect$genotype
        distance$treatment1 <- architect$treatment1
        distance$treatment2 <- architect$treatment2
        distance$date <- architect$date
        distance$temp <- architect$rep
        
        perh <- data.frame(matrix(unlist(perhomology), nrow=nrow(rs$roots), byrow=T))
        perh <- cbind(perh,   rs$roots[,c(1:8)])
        perh <- perh %>%
          mutate(dimension = X1) %>%
          mutate(birth = X2) %>%
          mutate(death = X3) %>%
          mutate(y = root)
        perh$type <- "geodesic"
  
  
        perh_summary <- ddply(perh, .(file, genotype, treatment1, treatment2, date, rep, type), summarise, sumdeath=sum(death),
                              sumbirth = sum(birth),
                              maxdeath = max(death),
                              maxbirth = max(birth),
                              life = mean(birth-death))
      })
      
      withProgress(message = 'Computing the summary data', {
        
      archi_summary <- ddply(archi, .(file, genotype, treatment1, treatment2, date, rep), summarise,
                             n_root=length(x1),
                             depth = max(y1),
                             tot_length = sum(length),
                             max_magnitude = max(magnitude),
                             max_path_length = max(pathlength),
                             mean_magnitude = mean(magnitude),
                             mean_path_length = mean(pathlength))

      })
      
      
      rs$histogram <- histogram
      rs$perh <- perh
      rs$distances <- distance
      rs$perh_summary <- perh_summary
      rs$archi_summary <- archi_summary
      rs$perhomology <- perhomology
    })
    
  
    
    observeEvent(input$load_code, {
      text <- 'library(devtools)
install_github("archiDART/archiDART")

library(archidart)

path <- "YOUR_PATH_TO_FOLDER"

archi <- rsmlToTable(path, fitter=T, rsml.connect = T, rsml.date="age")
genotypes <- unlist(lapply(strsplit(as.character(archi$file), "-"), `[[`, 1))[]
rep <- unlist(lapply(strsplit(as.character(archi$file), "-"), `[[`, 3))[]
archi$genotype <- genotypes
archi$rep <- rep
archi$age <- as.numeric(archi$time)
      
      
architect <- architect(inputrsml = archi, fitter=T)
genotypes <- unlist(lapply(strsplit(as.character(architect$FileName), "-"), `[[`, 1))[]
architect$genotype <- genotypes
      '
      showModal(modalDialog(
        pre(text),
        easyClose = TRUE      
        ))
    })    

    

  # UI COMMANDS  ################################

    observe({
      if(is.null(rs$factors)){return()}
      vars <- colnames(rs$factors)
      ct_options <- list()
      sel <- input$genotypes
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      updateSelectInput(session, "genotypes", choices = ct_options, selected=sel) 
    })
    observe({
      if(is.null(rs$factors)){return()}
      vars <- colnames(rs$factors)
      ct_options <- list()
      sel <- input$treatment1
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      updateSelectInput(session, "treatment1", choices = ct_options, selected=sel) 
    })
    observe({
      if(is.null(rs$factors)){return()}
      vars <- colnames(rs$factors)
      ct_options <- list()
      sel <- input$treatment2
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      updateSelectInput(session, "treatment2", choices = ct_options, selected=sel) 
    })
    observe({
      if(is.null(rs$factors)){return()}
      vars <- colnames(rs$factors)
      ct_options <- list()
      sel <- input$timestamp
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      updateSelectInput(session, "timestamp", choices = ct_options, selected=sel) 
    })
    
    observe({
      if(is.null(rs$factors)){return()}
      vars <- colnames(rs$factors)
      ct_options <- list()
      sel <- input$repetition
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      updateSelectInput(session, "repetition", choices = ct_options, selected=sel) 
    })    
    
    
  observe({
    if(is.null(rs$architect)){return()}
    vars <- rs$names$value#colnames(rs$architect)[-c(1,2,ncol(rs$architect))]
    ct_options <- list()
    sel <- input$to_plot
    for(ct in vars) ct_options[[ct]] <- ct
    if(length(sel) == 0 | sel == "") sel = ct_options[1]
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "to_plot", choices = ct_options, selected=sel) 
  })
  
  observe({
    if(is.null(rs$perh_summary)){return()}
    vars <- colnames(rs$perh_summary)[-c(1:4)]
    ct_options <- list()
    sel <- input$to_plot_4
    for(ct in vars) ct_options[[ct]] <- ct
    if(length(sel) == 0 | sel == "") sel = ct_options[1]
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "to_plot_4", choices = ct_options, selected=sel) 
  })
  
  observe({
    if(is.null(rs$archi_summary)){return()}
    vars <- colnames(rs$archi_summary)[-c(1:6)]
    ct_options <- list()
    sel <- input$to_plot_2_bis
    for(ct in vars) ct_options[[ct]] <- ct
    if(length(sel) == 0 | sel == "") sel = ct_options[1]
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "to_plot_2_bis", choices = ct_options, selected=sel) 
  })  
  
  observe({
    if(is.null(rs$architect)){return()}
    vars <- unique(rs$architect$Time)
    sel <- input$time_to_plot
    if(length(sel) == 0 | sel == "") sel = vars[length(vars)]
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSliderInput(session, "time_to_plot", min=min(vars), max=max(vars), step=1, value=sel) 
  })
  
  observe({
    if(is.null(rs$genotypes)){return()}
    vars <- unique(rs$genotypes)
    ct_options <- list()
    sel <- input$genotypes_to_plot
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "genotypes_to_plot", choices = ct_options, selected=sel) 
  }) 
  
  observe({
    if(is.null(rs$genotypes)){return()}
    vars <- unique(rs$genotypes)
    ct_options <- list()
    sel <- input$genotypes_to_plot_1
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "genotypes_to_plot_1", choices = ct_options, selected=sel) 
  }) 
  
  observe({
    if(is.null(rs$genotypes)){return()}
    vars <- unique(rs$genotypes)
    ct_options <- list()
    sel <- input$genotypes_to_plot_2
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "genotypes_to_plot_2", choices = ct_options, selected=sel) 
  })   
  
  observe({
    if(is.null(rs$genotypes)){return()}
    vars <- unique(rs$genotypes)
    ct_options <- list()
    sel <- input$genotypes_to_plot_3
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "genotypes_to_plot_3", choices = ct_options, selected=sel) 
  })    
  
  observe({
    if(is.null(rs$genotypes)){return()}
    vars <- unique(rs$genotypes)
    ct_options <- list()
    sel <- input$genotypes_to_plot_6
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "genotypes_to_plot_6", choices = ct_options, selected=sel) 
  }) 
  
  observe({
    if(is.null(rs$archi)){return()}
    vars <- unique(rs$archi$order)
    ct_options <- list()
    sel <- input$orders_to_plot_6
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "orders_to_plot_6", choices = ct_options, selected=sel) 
  }) 
  
  
  observe({
    if(is.null(rs$architect)){return()}
    vars <- rs$names$value#colnames(rs$architect)[-c(1,2,ncol(rs$architect))]
    ct_options <- list()
    sel <- input$variable_to_pca
    for(ct in vars) ct_options[[ct]] <- ct
    if(is.null(sel)) sel = ct_options
    if(length(sel) == 0 | sel == "") sel = ct_options
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "variable_to_pca", choices = ct_options, selected=sel) 
  }) 
  

  
  # PLOTS  ################################

  ## Angle plot  ################################
  
  output$angle_plot <- renderPlot({
    if(is.null(rs$archi)){return()}
    
    temp <- rs$archi[rs$archi$genotype %in% input$genotypes_to_plot_6,]
    temp <- temp[temp$order %in% input$orders_to_plot_6,]

    temp1 <- ddply(temp, .(plant, root), summarise, median=median(orientation), mean=mean(orientation))
    temp2 <- merge(temp, temp1, by = c("plant", "root"))
    
    print(temp2)
    
    
    pl <- ggplot(temp2, aes(orientation, fill=factor(order))) + 
      # geom_vline(aes(xintercept = median), colour="green") + 
      # geom_vline(aes(xintercept = mean), colour="blue") + 
      coord_polar(start = pi, direction=1) + 
      scale_x_continuous(breaks=seq(0, 360, by=30), expand=c(0,0), lim=c(0, 360)) +
      facet_wrap(~genotype, ncol=input$ncol2) + 
      theme_bw()
    
    if(input$plot_angle_abs) pl <- pl + geom_histogram(alpha=0.5)
    else pl <- pl + stat_density(alpha=0.5) 
    
    pl
  })  
    
  ## Time plot  ################################

  output$time_plot <- renderPlot({
    if(is.null(rs$architect)){return()}
    val <- rs$names$name[rs$names$value == input$to_plot]
    temp <- rs$architect[rs$architect$genotype %in% input$genotypes_to_plot,]
    temp$value <- temp[[val]]
    
    print(unique(temp$date))
    
    pl <- ggplot() +  
      xlab("Time [days]") + 
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), text=element_text(size=15)) +
      ylab(paste0(input$to_plot, " [pixels]")) + 
      ggtitle(input$to_plot) + 
      geom_vline(xintercept = input$time_to_boxplot, lty=2)
    
    if(input$plot_mean){
      pl <- pl + 
        stat_smooth(data = temp, aes(date, value, colour=genotype))
    } else{
      pl <- pl + 
        geom_line(data = temp, aes(date, value, colour=genotype, group=FileName))
    }
    
    pl
    
  }) 
  
  ## Metric boxplot  ################################
  
  output$metric_boxplot <- renderPlot({
    
    val <- rs$names$name[rs$names$value == input$to_plot]
    temp <- rs$architect[rs$architect$genotype %in% input$genotypes_to_plot,]
    temp$value <- temp[[val]]
    temp <- filter(temp, Time == input$time_to_boxplot)
    
    pl <- ggplot(temp, aes(genotype, value, fill=genotype)) + 
      geom_boxplot() + 
      theme_classic() +
      theme(legend.position = "none",
            text=element_text(size=15),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      ylab(paste0(input$to_plot, " [pixels]")) + 
      xlab("Genotype") + 
      ggtitle(paste0("Time = ",max(temp$Time)))
    
    pl
  })   
  
  
  observeEvent(input$metric_code, {
    val <- rs$names$name[rs$names$value == input$to_plot]
    text <- paste0('library(tidyverse)

temp <- filter(architect, Time == ',input$time_to_boxplot,')

ggplot(data = temp, aes(genotype, ',val,', fill="genotype)) +  
  geom_boxplot() + 
  xlab("genotype") +
  ylab("',input$to_plot,'") + 
  theme_classic()')    
    
    showModal(modalDialog(
      pre(text),
      easyClose = TRUE      
    ))
  })   
  
  
  observeEvent(input$time_code, {
    val <- rs$names$name[rs$names$value == input$to_plot]
    text <- paste0('library(ggplot2)
ggplot(data = architect) +  
  xlab("Time [days]") + 
  ylab("',input$to_plot,'") + 
  ggtitle("',input$to_plot,'") + 
  theme_classic() +
  ')    
    
    if(input$plot_mean){
      text <- paste0(text, 'stat_smooth(aes(Time,',input$to_plot,', colour = genotype))')
    }else{
      text <- paste0(text, 'geom_line(aes(Time,',input$to_plot,', colour = genotype, group = FileName))')
    }
    
    showModal(modalDialog(
      pre(text),
      easyClose = TRUE      
    ))
  })   
  
  
  
  ## Archi plot  ################################
  
  output$archi_plot <- renderPlot({
    if(is.null(rs$archi)){return()}
    
    temp <- rs$archi[rs$archi$genotype %in% input$genotypes_to_plot_1,]
    # temp <- temp[as.numeric(temp$rep) <= input$reps_to_plot,]
    temp$value <- temp[[input$to_plot_2]]
    
    
    if(!input$plot_mean_archi){
      pl <- ggplot(temp) + 
          geom_segment(aes(x = x1, y = -y1, xend = x2, yend = -y2, colour=value), size=input$linesize) + 
          coord_fixed() + 
          theme_bw() + 
          scale_colour_gradientn(colours=cscale3,
                               limits = input$psirange) + 
          facet_wrap(~file, ncol=input$ncol)  +
        theme(legend.position="bottom")
         
    }else{
      pl <- ggplot(temp) + 
        geom_segment(aes(x = x1, y = -y1, xend = x2, yend = -y2, colour=value), size=input$linesize, alpha=0.8) + 
        coord_fixed() + 
        theme_bw() + 
        scale_colour_gradientn(colours=cscale3,
                               limits = input$psirange)+
        #scale_colour_gradientn(colours = terrain.colors(10)) + 
        facet_grid(treatment1~genotype) +
        theme(legend.position="top")
    }
    
    pl
  })
  
  observeEvent(input$archi_code, {
    if(!input$plot_mean_archi){
    
      text <- paste0('library(ggplot2)
ggplot(data = archi) +  
  geom_segment(aes(x = x1, y = -y1, xend = x2, yend = -y2, colour = ',input$to_plot_2,')) + 
  coord_fixed() + 
  theme_bw() + 
  facet_wrap(~plant, ncol=',input$ncol,') +
  theme_classic()')    
  }else{
    text <- paste0('library(ggplot2)
ggplot(archi) + 
  geom_segment(aes(x = x1, y = -y1, xend = x2, yend = -y2, colour = ',input$to_plot_2,'), size = 0.5, alpha = 0.5) + 
  coord_fixed() + 
  theme_bw() + 
  facet_wrap(~genotype, ncol=',input$ncol,')')
  }
    showModal(modalDialog(
      pre(text),
      easyClose = TRUE
    ))
  })
  
  
  ## Distri plot  ################################
  
  output$distri_plot <- renderPlot({
    if(is.null(rs$archi)){return()}
    
    temp <- rs$histogram[[input$to_plot_distri]] %>%
      filter(genotype %in% input$genotypes_to_plot_1) %>%
      filter(as.numeric(rep) <= input$reps_to_plot)

    if(input$plot_mean_archi){
      pl <- ggplot(temp, aes(x=value, colour=genotype, group=file)) + 
        geom_density() +
        theme_classic() + 
        theme(legend.position = "null") + 
        facet_wrap(~genotype, ncol=input$ncol)      
    }else{
      pl <- ggplot(temp, aes(x=value, colour=genotype)) + 
        geom_density() +
        theme_classic()
    }
    
    pl
  })
  
  observeEvent(input$distri_code, {
    var <- input$to_plot_distri
    if(var == "depth") var <- "y2"
    text <- paste0('library(ggplot2)
library(plyr)
# Compute the magnitude data to make the histgram
histogram <- archi %>% 
  mutate(value = round(',var,')) %>%
  ddply(.(time, genotype, file, rep, root, rdepth), plyr::summarize, n=ceiling(sum(length/10))) %>%
  ddply(.(time, genotype, file, rep, root), plyr::summarize, value = rep(rdepth,n)) %>%
  filter(genotype %in% ',input$genotypes_to_plot_1,') %>%
  filter(as.numeric(rep) <= ',input$reps_to_plot,')')

    if(input$plot_mean_archi){
      text <- paste0(text, '
pl <- ggplot(histogram, aes(x=value, colour=genotype, group=file)) + 
        geom_density() +
        theme_classic() + 
        theme(legend.position = "null") + 
        facet_wrap(~genotype, ncol=',input$ncol,')')      
    }else{
      text <- paste0(text, "
pl <- ggplot(histogram, aes(x=value, colour=genotype)) + 
        geom_density() +
        theme_classic()")
    }
    showModal(modalDialog(
      pre(text),
      easyClose = TRUE      
    ))
  })  
  
  
  ## Archi boxplot  ################################
  
  output$archi_boxplot <- renderPlot({
    if(is.null(rs$perh_summary)){return()}
    
    temp <- rs$archi_summary %>%
      filter(genotype %in% input$genotypes_to_plot_1)
    
    temp$value <- temp[[input$to_plot_2_bis]]
    
    pl <- ggplot(temp, aes(genotype, value, fill=genotype)) + 
      geom_boxplot() + 
      theme_classic() +
      theme(legend.position = "none",
            text=element_text(size=15),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      ylab(paste0(input$to_plot_2_bis, " [pixels]")) + 
      xlab("Genotype")
    
    
    pl
  })  
  
  observeEvent(input$boxplot_archi_code, {
    text <- paste0('library(plyr)
library(tidyverse)
                   
archi_summary <- ddply(archi, .(file, genotype, rep, type), summarise, 
        n_root=length(x1),
        depth = max(y1),
        tot_length = sum(length),
        max_magnitude = max(magnitude),
        max_path_length = max(pathlength),
        mean_magnitude = mean(magnitude),
        mean_path_length = mean(pathlength))
                   
pl <- ggplot(archi_summary, aes(genotype, ',input$to_plot_2_bis,', fill=genotype)) + 
          geom_boxplot() + 
          theme_classic() +
          theme(legend.position = "none",
          text=element_text(size=15),
          axis.text.x = element_text(angle = 45, hjust = 1))+
          ylab(',input$to_plot_2_bis,') + 
          xlab("Genotype")
                   '
    )
    showModal(modalDialog(
      pre(text),
      easyClose = TRUE      
    ))
  })  
  
  
  


  ## Barcode plot  ################################
  
  output$barcode_plot <- renderPlot({
    if(is.null(rs$perh)){return()}
    
    temp <- rs$perh[rs$perh$genotype %in% input$genotypes_to_plot_2,]
    temp <- temp[temp$type == "geodesic",]
    #temp <- temp[as.numeric(temp$rep) <= input$reps_to_plot_2,]

    print(str(rs$perh))
    
    alph <- 0.5#1/input$reps_to_plot_2
    
    pl <- ggplot(temp) + 
      geom_segment(aes(x = birth, y=y, xend=death, yend=y, alpha=alph, colour=rep)) + 
      facet_wrap(~genotype) + 
      theme_classic() +
      theme(legend.position = "none")+
      ylab("H0") + 
      xlab("Geodesic distance [pixels]")
    
    
    pl
  })
  
  
  observeEvent(input$barcode_code, {
    
    text <- paste0('
library(tidyverse)

# Compile the data in a table
 perhomology <- perhomology(archi, FUN="',input$to_plot_3,'")
 names  <- names(perhomology)
 perh <- NULL
 for(i in c(1:length(perhomology))){
   temp <- data.frame(perhomology[[i]])
   temp$y <- c(1:nrow(temp))
   temp$file <- names[i]
   perh <- rbind(perh, temp)
 }
 genotypes <- unlist(lapply(strsplit(as.character(perh$file), "-"), `[[`, 1))[]
 rep <- unlist(lapply(strsplit(as.character(perh$file), "-"), `[[`, 3))[]
 perh$genotype <- genotypes
 perh$rep <- rep
 
 ggplot(perh) + 
   geom_segment(aes(x = birth, y=y, xend=death, yend=y, alpha=0.1)) + 
   facet_wrap(~genotype) + 
   theme_classic() +
   theme(legend.position = "none")+
   ylab("H0") + 
   xlab("',input$to_plot_3,' distance (cm)")')
    
  showModal(modalDialog(
    pre(text),
    easyClose = TRUE      
  ))
  })  
  
  
  ## Barcode boxplot  ################################
  
  output$barcode_boxplot <- renderPlot({
    if(is.null(rs$perh_summary)){return()}
    
    temp <- rs$perh_summary[rs$perh_summary$genotype %in% input$genotypes_to_plot_2,]
    temp <- temp[temp$type == "geodesic",]
    
    temp$value <- temp[[input$to_plot_4]]

    pl <- ggplot(temp, aes(genotype, value, fill=genotype)) + 
      geom_boxplot() + 
      theme_classic() +
      theme(legend.position = "none",
            text=element_text(size=15),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      ylab(paste0(input$to_plot_4," [-]")) + 
      xlab("Genotype")
    
    
    pl
  })  
  
  observeEvent(input$boxcode_code, {
    text <- paste0('    
library(plyr)
library(tidyverse)

perh_summary <- ddply(perh, .(file, genotype, rep, type), summarise, sumdeath=sum(death),sumbirth = sum(birth),maxdeath = max(death),maxbirth = max(birth),ndeath = length(death),nbirth = length(birth),life = mean(birth-death))

temp <- temp[temp$type == "',input$to_plot_3,'",]
           
temp$value <- temp[["',input$to_plot_4,'"]]
           
pl <- ggplot(temp, aes(genotype, value, fill=genotype)) + 
           geom_boxplot() + 
           theme_classic() +
           theme(legend.position = "none",
           text=element_text(size=15),
           axis.text.x = element_text(angle = 45, hjust = 1))+
           ylab(',input$to_plot_4,') + 
           xlab("Genotype")'
  )
  showModal(modalDialog(
    pre(text),
    easyClose = TRUE      
  ))
  })  
  
  
  
  
  ## Barcode PCA  ################################
  
  output$barcode_PCA <- renderPlot({
    if(is.null(rs$distances)){return()}
    
    distance<-as.data.frame(rs$distances[rs$distances$genotype %in% input$genotypes_to_plot_2,])
    genotype <- distance$genotype
    
    mds <- metaMDS(as.dist(distance[,-(length(distance))]), trymax=200, autotransform=FALSE, wascores = FALSE, noshare=FALSE, expand=FALSE)
    data <- data.frame(mds$points, genotype=genotype)    

    pl <- ggplot(data) +
      geom_point(aes(MDS1, MDS2, colour=genotype)) +
      stat_ellipse(aes(MDS1, MDS2, colour=genotype), level = 0.9, size=1) +
      theme_bw() +
      xlab("NMDS-1") +
      ylab("NMDS-2")
    
    pl
  })
  
  
  
  observeEvent(input$barcode_PCA_code, {
    text <- paste0('library(tidyverse)
library(vegan)

perhomology <- perhomology(archi)

# distance <- as.data.frame(bottleneckdist(perhomology))  ### UNCOMMENT THIS LINE

mds <- metaMDS(as.dist(distance[,-(length(distance))]), trymax=200, autotransform=FALSE, wascores = FALSE, noshare=FALSE, expand=FALSE)
genotypes <- c(rep("dense", 10),rep("fast", 10),rep("mock", 10),rep("shallow", 10),rep("slow", 10),rep("sparse", 10),rep("steep", 10))
data <- data.frame(mds$points, genotype=genotypes)    
                   

ggplot(data) +
      geom_point(aes(MDS1, MDS2, colour=genotype)) +
      stat_ellipse(aes(MDS1, MDS2, colour=genotype), level = 0.9, size=1) +
      theme_bw() +
      xlab("NMDS-1") +
      ylab("NMDS-2")
')
    showModal(modalDialog(
      pre(text),
      easyClose = TRUE      
    ))
  }) 

  
  ## PCA plot  ################################
  output$pca_plot <- renderPlot({
    if(is.null(rs$architect)){return()}
    
    temp <- rs$architect[rs$architect$genotype %in% input$genotypes_to_plot_3,]
    temp <- temp[temp$Time == input$time_to_plot,]
    plants <- temp$FileName
    genotypes <- temp$genotype
    vars <- rs$names$name[rs$names$value %in% input$variable_to_pca]
    temp <- temp[,vars]
    remove <- NULL
    for(i in c(1:ncol(temp))){
      if(sd(temp[,i]) == 0) remove <- c(remove,i)
    }
    
    if(!is.null(remove)) temp <- temp[,-remove]
    pca <- prcomp(temp, retx = T, scale=T)  # Make the PCA
    pca.results <- cbind(plant=plants, genotype=genotypes, data.frame(pca$x)[,])
    
    vars <- apply(pca$x, 2, var)  
    props <- round((vars / sum(vars) * 100), 1)
    xl <- paste0("\nPrincipal Component 1 (",props[1],"%)")
    yl <-paste0("Principal Component 2 (",props[2],"%)\n")
    
    pl1 <- ggplot(data = pca.results) + 
      geom_point(aes(PC1, PC2, colour=genotype)) +
      stat_ellipse(aes(PC1, PC2, colour=genotype), level = 0.9, size=1) + 
      theme_bw() + 
      xlab(xl) + 
      ylab(yl)
    
    z2 <- data.frame(var_names = rownames(pca$rotation), pca$rotation[, 1:2])
    z2$var_names <- gsub("_", " ", z2$var_names)
    
    pl2 <- ggplot(data=z2, aes(0, 0, xend=PC1, yend=PC2)) + 
      geom_segment(col="grey", size=1.2, arrow = arrow(length = unit(0.5,"cm")), alpha=0.9) +
      geom_text_repel(data=z2, aes(PC1, PC2, label=var_names), col="black", size=9) +
      geom_point(aes(x=0, y=0), colour="grey") +
      #scale_y_continuous(limits = c(-1, 0.3)) +
      theme_classic() +
      xlab(xl) + ylab(yl)

    pl <- grid.arrange(pl1, pl2, ncol=1)
      
  })
  
  
  observeEvent(input$pca_code, {
    vars <- paste(input$variable_to_pca,collapse=",")
    text <- paste0(
'library(gridExtra)
library(ggrepel)
library(ggplot2)

# Do the PCA analysis
plants <- rs$architect$FileName
genotypes <- rs$architect$genotype
temp <- rs$architect[,c(',vars,')]
pca <- prcomp(temp, retx = T, scale=T)  # Make the PCA
pca.results <- cbind(plant=plants, genotype=genotypes, data.frame(pca$x)[,])
    
vars <- apply(pca$x, 2, var)  
props <- round((vars / sum(vars) * 100), 1)
xl <- paste0("\nPrincipal Component 1 (",props[1],"%)")
yl <-paste0("Principal Component 2 (",props[2],"%)\n")

pl1 <- ggplot(data = pca.results) + 
  geom_point(aes(PC1, PC2, colour=genotype)) +
  stat_ellipse(aes(PC1, PC2, colour=genotype), level = 0.9, size=1) + 
  theme_bw() + 
  xlab(xl) + 
  ylab(yl)

z2 <- data.frame(var_names = rownames(pca$rotation), pca$rotation[, 1:2])
z2$var_names <- gsub("_", " ", z2$var_names)

pl2 <- ggplot(data=z2, aes(0, 0, xend=PC1, yend=PC2)) + 
  geom_segment(col="grey", size=1.2, arrow = arrow(length = unit(0.5,"cm")), alpha=0.9) +
  geom_text_repel(data=z2, aes(PC1, PC2, label=var_names), col="black", size=9) +
  geom_point(aes(x=0, y=0), colour="grey") +
  #scale_y_continuous(limits = c(-1, 0.3)) +
  theme_classic() +
  xlab(xl) + ylab(yl)

pl <- grid.arrange(pl1, pl2, ncol=1)' 
)
    showModal(modalDialog(
      pre(text),
      easyClose = TRUE      
    ))
  }) 
  
  
  
  
  # TABLES ################################

  output$distribution_data <- DT::renderDataTable({
    if(is.null(rs$architect)){return()}
    temp <- rs$architect %>% mutate_each(funs(round(.,2)), -c(FileName, Time, genotype)) 
    
    DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 10))
  })

  
  ## Factor table ################################
  
  output$factor_data <- DT::renderDataTable({
    if(is.null(rs$factors)){return()}
    temp <- rs$factors
    
    DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 5))
  })  
  
  ## Updated data ################################
  output$updated_data <- DT::renderDataTable({
    if(is.null(rs$architect)){return()}
    if(is.null(rs$architect$genotype)){
      temp <- rs$architect %>% mutate_each(funs(round(.,2)), which(sapply(., is.numeric))) 
    }else{
      temp <- rs$architect %>% mutate_each(funs(round(.,2)), which(sapply(., is.numeric))) 
    }
    DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 5))
  })  
  
  
})
