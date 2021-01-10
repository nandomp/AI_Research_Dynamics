# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("rPref")
# install.packages("lubridate")

library(ggplot2)
library(dplyr)
library(rPref)
library(lubridate)
library(stringr)
library(tidyverse)
library(pals)
library(ggthemes)

openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}



withdata <- grep("csvs", list.dirs())
listDirs <- list.dirs()[withdata]

allNames <- str_split(listDirs, "/")
names <- c()
for (i in 1:length(allNames)){
  names <- c(names, paste0(allNames[[i]][4]," - ",allNames[[i]][5]))
}
names


getTypeInstitution <- function(data){
  # all.aff <- read.csv("AffiliationsInfo.csv")
  all.aff <- read.csv("AffiliationsInfo.v2.csv")
  
  
  data$type <- NA
  
  for(j in 1:nrow(data)){
    # print(paste0("..... ", data$communities_name[j]))
    if(data$communities_name[j] != "?"){
      type <- sapply(all.aff$Institution, function(x) grepl(paste0("\\b",x,"\\b"), data$communities_name[j]))
      type <- as.character(all.aff$Type[type])
      
      
      uni <- max(type %in% c("University", "Research Institute", "Agency", "Academic"))
      com <- max(type %in% c("Company"))
      
      # uni <- grepl("University | Research Institute | Agency", type)
      # com <- grepl("Company", type)
      
      if (uni + com == 2){
        data$type[j] <- "Hybrid"
      }else{
        if(uni){
          data$type[j] <- "Academic"
        }else{
          if(com){
            data$type[j] <- "Company"
          }else{
            data$type[j] <- "Check"
          }
        }
      }
    }else{
      data$type[j] <- "Unknown"
    }
    
    
  }
  return(data)
}

plotPareto <- function(i, MAX = TRUE, cols = 5, legend = "bottom", cut = 60, INV = 0, h.leg = 0.9, s.leg = 10, ptsize = 6){
  
  print(paste("------- Benchmark: ", names[i]))
  
  data <- read.csv(paste0(listDirs[i],"/paper-communities.csv"))
  # data.pareto <- read.csv(paste0(listDirs[i],"/global-pareto.csv"))
  data.comm <- read.csv(paste0(listDirs[i],"/legend-communities.csv"))
  
  
  data$paper_date <- ymd(data$paper_date)
  data$metric <- as.numeric(gsub("%", "",data$metric))
  if (INV != 0) {
      data$metric <- INV - data$metric 
  }
  
  data$communities <- as.character(data$communities)
  data$communities_name <- as.character(data$communities_name)
  Encoding(data$communities_name) <- "UTF-8"
  
  data$communities_name[which(data$communities == "")] <- "NA"
  data$communities_name[which(data$communities_name == "set()")] <- "?"
  data$known <- ifelse(data$communities_name == "?", FALSE, TRUE)
  
  data$communities_name <- gsub("\\{", "",data$communities_name)
  data$communities_name <- gsub("\\}", "",data$communities_name)
  data$communities_name <- gsub("'", "",data$communities_name)
  data$communities_name <- gsub("\"", "",data$communities_name)
  
  data$communities_name <- ifelse(data$communities_name != "?", paste0(data$communities, ": ",data$communities_name), data$communities_name)
  
  data$communities_name <- as.character(data$communities_name)
  data[grepl("+", data$communities, fixed = T), "communities_name"] <- data[grepl("+", data$communities, fixed = T), "communities"]
  
  data$communities <- as.factor(data$communities)
  data$communities_name <- as.factor(data$communities_name)
  
  data$communities_name <- fct_reorder(data$communities_name, as.numeric(as.character(data$communities)))
  data$communities <- fct_reorder(data$communities, as.numeric(as.character(data$communities)))
  
  data <- getTypeInstitution(data)
  
  #Shorten names affils
  
  data$communities_name <- str_replace_all(data$communities_name, "University", "Univ.")
  data$communities_name <- str_replace_all(data$communities_name, "Technology", "Tech.")
  data$communities_name <- str_replace_all(data$communities_name, "Research", "Res.")
  data$communities_name <- str_replace_all(data$communities_name, "Institute", "Ins.")
  data$communities_name <- str_replace_all(data$communities_name, "of ", "")
  data$communities_name <- str_replace_all(data$communities_name, "National", "Nat.")
  data$communities_name <- str_replace_all(data$communities_name, "Science", "Sci.")
  data$communities_name <- str_replace_all(data$communities_name, "Artificial Intelligence", "AI")
  
  if (MAX) {
    skyG <- psel(data, low(as.numeric(paper_date)) * high(metric))
  }else{
    skyG <- psel(data, low(as.numeric(paper_date)) * low(metric))
  }
  summarise(skyG, skyline_size = n())
  
  remove <- which(is.na(data$metric))
  if(length(remove) > 0){
    data <- data[-remove,]
  }
  
 
  data.known <- filter(data, known == TRUE)
  data.unknown <- filter(data, known == FALSE)
  
  grouped_pareto.known <- group_by(data.known, communities, communities_name)
  if (MAX) {
    skyg.k <- psel(grouped_pareto.known, low(as.numeric(paper_date)) * high(metric))
  }else{
    skyg.k <- psel(grouped_pareto.known, low(as.numeric(paper_date)) * low(metric))
  }
  # summarise(skyg.k, skyline_size = n())
  
  grouped_pareto.unknown <- group_by(data.unknown, communities, communities_name)
  if (MAX) {
    skyg.u <- psel(grouped_pareto.unknown, low(as.numeric(paper_date)) * high(metric))
  }else{
    skyg.u <- psel(grouped_pareto.unknown, low(as.numeric(paper_date)) * low(metric))
  }
  
  # summarise(skyg.u, skyline_size = n())
  
  skyg.k <- as.data.frame(skyg.k)
  skyg.u <- as.data.frame(skyg.u)
  
  
  
  plot <- ggplot(data.known, aes(paper_date, metric)) + 
    
    geom_step(data = filter(skyG, !is.na(metric)), direction = "hv", size = 5, alpha = 0.3) +
    geom_step(data = filter(skyG, !is.na(metric)), direction = "hv", linetype = "dashed")
  
  if(nrow(skyg.u)>0){
    plot <- plot +
      # unknown affiliations
      geom_path(data = skyg.u, aes(paper_date, metric, group = communities), size = 2, alpha = 0.25) +
      geom_path(data = skyg.u, aes(paper_date, metric, group = communities), linetype = "dashed") +
      geom_point(data = data.unknown, aes(paper_date, metric, shape = "Unknown affiliation"), size = ptsize, alpha = 0.4) +
      # geom_point(aes(color = factor(str_wrap(communities_name,60))), skyg.k, size = 6) + #empty circles for those points from the same community (vertical)
      geom_text(data = data.unknown, aes(paper_date, metric, label = communities), colour = "black", size = (ptsize*0.65)) 
  }
  
  if(nrow(skyg.k)> 0){
    
    skyg.k$communities_name <- fct_reorder(skyg.k$communities_name, as.numeric(as.character(skyg.k$communities)))
    skyg.k$communities <- fct_reorder(skyg.k$communities, as.numeric(as.character(skyg.k$communities)))
    
    plot <- plot +
      # known affiliations
      geom_path(data = skyg.k, aes(paper_date, metric, group = communities), linetype = "dashed", show.legend = FALSE, alpha = 0.5) +
      geom_path(data = skyg.k, aes(paper_date, metric, group = communities, colour = reorder(str_wrap(communities_name, cut),communities)), size = 2, alpha = 0.3, show.legend = FALSE) +
      geom_point(aes(color = reorder(str_wrap(communities_name,cut),communities), 
                     fill = reorder(str_wrap(communities_name,cut),communities), 
                     shape = type), size = ptsize +1, alpha = 0.7) +
      # geom_point(aes(color = factor(str_wrap(communities_name,60))), skyg.k, size = 6) + #empty circles for those points from the same community (vertical)
      geom_text(aes(label = communities), colour = "black", size = ptsize*0.65) 
  }
  
  if(length(unique(data.known$communities)) <=26){
    plot <- plot +
      scale_color_manual(labels=str_wrap(unique(skyg.k$communities_name),cut),
                         breaks=str_wrap(unique(skyg.k$communities_name),cut),
                         values=as.vector(alphabet(length(unique(data.known$communities))))) + 
      scale_fill_manual(labels=str_wrap(unique(skyg.k$communities_name),cut),
                        breaks=str_wrap(unique(skyg.k$communities_name),cut),
                        values=as.vector(alphabet(length(unique(data.known$communities)))))
  }else{
    
    moreColours <- function(n){
      clr <- c(alphabet(26),alphabet2(26),kelly(22))
      return(clr[1:n])
    }
    
    plot <- plot +
      scale_color_manual(labels=str_wrap(unique(skyg.k$communities_name),cut),
                       breaks=str_wrap(unique(skyg.k$communities_name),cut),
                       values=as.vector(moreColours(length(unique(data.known$communities))))) + 
      scale_fill_manual(labels=str_wrap(unique(skyg.k$communities_name),cut),
                        breaks=str_wrap(unique(skyg.k$communities_name),cut),
                        values=as.vector(moreColours(length(unique(data.known$communities)))))
  
  }
     
  smtcol = "#8ecae6"
  plot <-
    plot +
    geom_smooth(data = data, aes(paper_date, metric), alpha=0.3, linetype=0, span=0.5, fill = smtcol)+
    geom_line(data = data, aes(paper_date, metric),
              stat="smooth",
              size = 1.5,
              linetype ="dotted",
              alpha = 0.7, 
              colour = smtcol) +
  
  
    # scale_color_discrete(breaks = levels(data.known$communities_name)) +
    #scale_shape_manual(values = c("U" = 12, "C" = 25, "UC" = 13, "Unknown affilitation" = 16), labels = c("University","Company","Hybrid","Unknown") ) +
    # scale_colour_manual(values=as.vector(alphabet(length(unique(data.known$communities)))))+
    # scale_fill_manual(values=as.vector(alphabet(length(unique(data.known$communities)))))+
    
    scale_shape_manual(values = c("Academic" = 24, "Company" = 25, "Hybrid" = 23, "Unknown affiliation" = 16)) +
    xlab("") + ylab("Metric") + ggtitle(names[i]) +
    guides(colour = guide_legend(ncol = cols), fill=FALSE, shape = guide_legend(ncol = cols)) +
    theme_minimal() + 
    theme(legend.position=legend,
          legend.key.height=unit(h.leg, "cm"),
          legend.text=element_text(size = s.leg),
          legend.title = element_blank(),
          legend.margin=unit(0.5,"cm"),
          plot.title = element_text(color = "black", face = "bold"),
          plot.subtitle = element_text(color = "#008b9f"),
          plot.caption = element_text(color = "black", face = "italic", size = 6))
  
  # hide_legend(ggplotly(plot))
  
  # data.pareto.comm <- filter(data, communities_name != "?")
  # data.max.comm <- group_by(data.pareto.comm , communities, paper_date) %>% filter(metric == max(metric))
  
  
  return(plot)
  
}

plotParetoAffiliation <- function(i, MAX = TRUE, cols = 5, legend = "bottom", cut = 60, INV = 0, h.leg = 0.9, s.leg = 10, oneAffil = T){
  
  print(paste("------- Benchmark: ", names[i])) #i = 5
  
  data <- read.csv(paste0(listDirs[i],"/paper-communities.csv"))
  # data.pareto <- read.csv(paste0(listDirs[i],"/global-pareto.csv"))
  member <- read.csv(paste0(listDirs[i],"/membership-authors.csv"))
  
  
  if(oneAffil){
    for (j in 1:nrow(data)){
      data$communities_name[j] <-
        gsub("\\{|'|\\}", "",
                    str_split(data$communities_name, ", ")[[j]][1])
    }
  }else{
    data2 <- data.frame(title = NA, metric = NA, paper_date = NA, communities_name = NA)
    for (j in 1:nrow(data)){
      data$communities_name[j] <- gsub("\\{|'|\\}", "", data$communities_name[j])
      temp <- str_split(data$communities_name[j], ", ")[[1]]                                 
      for (z in 1:length(temp)){
        data2 <- rbind(data2, c(data[j,"title"], data[j, "metric"], data[j, "paper_date"], temp[z]))
      }
      
    }
    data <- data2[-1,]
  }
  
 
  # data.comm <- read.csv(paste0(listDirs[i],"/legend-communities.csv"))
  
  
  data$paper_date <- ymd(data$paper_date)
  data$metric <- as.numeric(gsub("%", "",data$metric))
  if (INV != 0) {
    data$metric <- INV - data$metric 
  }
  
  # data$communities <- as.character(data$communities)
  data$communities_name <- as.character(data$communities_name)
  
  # data$communities_name[which(data$communities == "")] <- "?"
  data$communities_name[which(data$communities_name == "set()")] <- "?"
  data$known <- ifelse(data$communities_name == "?", FALSE, TRUE)
  
  # data$communities_name <- gsub("\\{", "",data$communities_name)
  # data$communities_name <- gsub("\\}", "",data$communities_name)
  # data$communities_name <- gsub("'", "",data$communities_name)
  # data$communities_name <- gsub("\"", "",data$communities_name)
  
  # data$communities_name <- ifelse(data$communities_name != "?", paste0(data$communities, ": ",data$communities_name), data$communities_name)
  
  data$communities_name <- as.character(data$communities_name)
  # data[grepl("+", data$communities, fixed = T), "communities_name"] <- data[grepl("+", data$communities, fixed = T), "communities"]
  
  # data$communities <- as.factor(data$communities)
  data$communities_name <- as.factor(data$communities_name)
  
  data$communities_name <- fct_reorder(data$communities_name, as.character(data$communities_name))
  # data$communities <- fct_reorder(data$communities, as.numeric(as.character(data$communities)))
  
  
  
  data <- getTypeInstitution(data)
  
  
  if (MAX) {
    skyG <- psel(data, low(as.numeric(paper_date)) * high(metric))
  }else{
    skyG <- psel(data, low(as.numeric(paper_date)) * low(metric))
  }
  summarise(skyG, skyline_size = n())
  
  remove <- which(is.na(data$metric))
  if(length(remove) > 0){
    data <- data[-remove,]
  }
  
  
  data.known <- filter(data, known == TRUE)
  data.unknown <- filter(data, known == FALSE)
  
  grouped_pareto.known <- group_by(data.known, communities_name)
  if (MAX) {
    skyg.k <- psel(grouped_pareto.known, low(as.numeric(paper_date)) * high(metric))
  }else{
    skyg.k <- psel(grouped_pareto.known, low(as.numeric(paper_date)) * low(metric))
  }
  # summarise(skyg.k, skyline_size = n())
  
  grouped_pareto.unknown <- group_by(data.unknown, communities_name)
  if (MAX) {
    skyg.u <- psel(grouped_pareto.unknown, low(as.numeric(paper_date)) * high(metric))
  }else{
    skyg.u <- psel(grouped_pareto.unknown, low(as.numeric(paper_date)) * low(metric))
  }
  
  # summarise(skyg.u, skyline_size = n())
  
  skyg.k <- as.data.frame(skyg.k)
  skyg.u <- as.data.frame(skyg.u)
  
  plot <- ggplot(data.known, aes(paper_date, metric)) + 
    
    geom_step(data = filter(skyG, !is.na(metric)), direction = "hv", size = 5, alpha = 0.3) +
    geom_step(data = filter(skyG, !is.na(metric)), direction = "hv", linetype = "dashed")
  
  if(nrow(skyg.u)>0){
    plot <- plot +
      # unknown affiliations
      # geom_path(data = skyg.u, aes(paper_date, metric, group = communities_name), size = 2, alpha = 0.25) +
      # geom_path(data = skyg.u, aes(paper_date, metric, group = communities_name), linetype = "dashed") +
      geom_point(data = data.unknown, aes(paper_date, metric, shape = "Unknown affiliation"), size = 6, alpha = 0.4) 
      # geom_point(aes(color = factor(str_wrap(communities_name,60))), skyg.k, size = 6) + #empty circles for those points from the same community (vertical)
      # geom_text(data = data.unknown, aes(paper_date, metric, label = communities_name), colour = "black", size = 3) 
  }
  
  if(nrow(skyg.k)> 0){
    
    # skyg.k$communities_name <- fct_reorder(skyg.k$communities_name, as.numeric(as.character(skyg.k$communities)))
    # skyg.k$communities <- fct_reorder(skyg.k$communities, as.numeric(as.character(skyg.k$communities)))
    h = (range(skyg.k$metric)[2] - range(skyg.k$metric)[1])*0.02
    w = (range(skyg.k$paper_date)[2] - range(skyg.k$paper_date)[1])*0.02
    w = as.numeric(w)
    
    plot <- plot +
      # known affiliations
      geom_path(data = skyg.k, aes(paper_date, metric, group = communities_name), linetype = "dashed", show.legend = FALSE, alpha = 0.5) +
      geom_path(data = skyg.k, aes(paper_date, metric, group = communities_name, colour = str_wrap(communities_name, cut)), size = 2, alpha = 0.3, show.legend = FALSE) +
      geom_jitter(aes(color = str_wrap(communities_name,cut), 
                     fill = str_wrap(communities_name,cut), 
                     shape = type), size = 7, alpha = 0.7, width = w, height = h) 
      # geom_point(aes(color = factor(str_wrap(communities_name,60))), skyg.k, size = 6) + #empty circles for those points from the same community (vertical)
      # geom_text(aes(label = communities_name), colour = "black", size = 4) + 
      # scale_color_manual(labels=str_wrap(unique(skyg.k$communities_name),cut),
      #                      breaks=str_wrap(unique(skyg.k$communities_name),cut),
      #                      values=as.vector(alphabet(length(unique(data.known$communities_name))))) +
      # scale_fill_manual(labels=str_wrap(unique(skyg.k$communities_name),cut),
      #                     breaks=str_wrap(unique(skyg.k$communities_name),cut),
      #                     values=as.vector(alphabet(length(unique(data.known$communities_name)))))
  }
  # smtcol = "#8ecae6"
  plot <-
     plot +
  #   geom_smooth(data = data, aes(paper_date, metric), alpha=0.3, linetype=0, span=0.5, fill = smtcol)+
  #   geom_line(data = data, aes(paper_date, metric),
  #             stat="smooth",
  #             size = 1.5,
  #             linetype ="dotted",
  #             alpha = 0.7, 
  #             colour = smtcol) +
    
    
    # scale_color_discrete(breaks = levels(data.known$communities_name)) +
    #scale_shape_manual(values = c("U" = 12, "C" = 25, "UC" = 13, "Unknown affilitation" = 16), labels = c("University","Company","Hybrid","Unknown") ) +
    # scale_colour_manual(values=as.vector(alphabet(length(unique(data.known$communities)))))+
    # scale_fill_manual(values=as.vector(alphabet(length(unique(data.known$communities)))))+
    
    scale_shape_manual(values = c("Academic" = 24, "Company" = 25, "Hybrid" = 23, "Unknown affiliation" = 16)) +
    xlab("") + ylab("Metric") + ggtitle(paste0(names[i]," - Baseline: Affiliation")) +
    guides(colour = guide_legend(ncol = cols), fill=FALSE, shape = guide_legend(ncol = cols)) +
    theme_minimal() + 
    theme(legend.position=legend,
          legend.key.height=unit(h.leg, "cm"),
          legend.text=element_text(size = s.leg),
          legend.title = element_blank(),
          legend.margin=unit(0.5,"cm"),
          plot.title = element_text(color = "black", face = "bold"),
          plot.subtitle = element_text(color = "#008b9f"),
          plot.caption = element_text(color = "black", face = "italic", size = 6))
  
  plot
  # hide_legend(ggplotly(plot))
  
  # data.pareto.comm <- filter(data, communities_name != "?")
  # data.max.comm <- group_by(data.pareto.comm , communities, paper_date) %>% filter(metric == max(metric))
  
  
  return(plot)
  
}


plotParetoAuthor <- function(i, MAX = TRUE, cols = 5, legend = "bottom", cut = 60, INV = 0, h.leg = 0.9, s.leg = 10, prolific = F, show.leg = F){
  
  print(paste("------- Benchmark: ", names[i])) #i = 5
  
  member <- read.csv(paste0(listDirs[i],"/membership-authors.csv"))
  data <- read.csv(paste0(listDirs[i],"/paper-authors.csv"))
  
 
  data$paper_date <- ymd(data$paper_date)
  data$metric <- as.numeric(gsub("%", "",data$metric))
  if (INV != 0) {
    data$metric <- INV - data$metric 
  }
  
  data$communities_name <- data$affiliations
  data$communities_name <- as.character(data$communities_name)
  data$communities_name[which(data$communities_name == "")] <- "?"
  data$known <- ifelse(data$communities_name == "?", FALSE, TRUE)
  data$communities_name <- as.character(data$communities_name)
  data$communities_name <- as.factor(data$communities_name)
  data$communities_name <- fct_reorder(data$communities_name, as.character(data$communities_name))

  data <- getTypeInstitution(data)
  data$authors <- as.factor(data$authors)
  
  
  
  if (MAX) {
    skyG <- psel(data, low(as.numeric(paper_date)) * high(metric))
  }else{
    skyG <- psel(data, low(as.numeric(paper_date)) * low(metric))
  }
  summarise(skyG, skyline_size = n())
  
  remove <- which(is.na(data$metric))
  if(length(remove) > 0){
    data <- data[-remove,]
  }
  remove.author <- which(is.na(data$authors) | (data$authors == ""))
  if(length(remove.author) > 0){
    data <- data[-remove.author,]
  }

  data.auth <- data
  if(prolific){
    data2 <- data %>% group_by(authors) %>% summarise(num.authors = n()) %>% arrange(desc(num.authors))
    which.auth <- data2$authors[1:10]
    data.auth <- filter(data, authors %in% which.auth)
  }
  
  grouped_pareto <- group_by(data.auth, authors)
  if (MAX) {
    skyg.k <- psel(grouped_pareto, low(as.numeric(paper_date)) * high(metric))
  }else{
    skyg.k <- psel(grouped_pareto, low(as.numeric(paper_date)) * low(metric))
  }


  skyg.k <- as.data.frame(skyg.k)
  

  plot <- ggplot(data, aes(paper_date, metric)) + 
    
    geom_step(data = filter(skyG, !is.na(metric)), direction = "hv", size = 5, alpha = 0.3) +
    geom_step(data = filter(skyG, !is.na(metric)), direction = "hv", linetype = "dashed")
  
  
  if(nrow(skyg.k)> 0){
    
    # skyg.k$communities_name <- fct_reorder(skyg.k$communities_name, as.numeric(as.character(skyg.k$communities)))
    # skyg.k$communities <- fct_reorder(skyg.k$communities, as.numeric(as.character(skyg.k$communities)))
    h = (range(skyg.k$metric)[2] - range(skyg.k$metric)[1])*0.02
    w = (range(skyg.k$paper_date)[2] - range(skyg.k$paper_date)[1])*0.02
    w = as.numeric(w)
    plot <- plot +
      # known affiliations
      geom_path(data = skyg.k, aes(paper_date, metric, group = authors), linetype = "dashed", show.legend = FALSE, alpha = 0.5) +
      geom_path(data = skyg.k, aes(paper_date, metric, group = authors, colour = authors), size = 2, alpha = 0.3, show.legend = FALSE) 
    
    if(show.leg){
      plot <- plot + geom_jitter(aes(paper_date, metric, group = authors,
                                     colour = authors,
                                     fill = authors, 
                                     shape = type), size = 7, alpha = 0.7, show.legend = TRUE, width = w, height = h) 
    }else{
      plot <- plot + geom_jitter(aes(paper_date, metric, group = authors,
                      colour = authors,
                      fill = authors, 
                      shape = type), size = 7, alpha = 0.7, show.legend = FALSE, width = w, height = h) 
    }
      
    # geom_point(aes(color = factor(str_wrap(communities_name,60))), skyg.k, size = 6) + #empty circles for those points from the same community (vertical)
    # geom_text(aes(label = communities_name), colour = "black", size = 4) + 
    # scale_color_manual(labels=str_wrap(unique(skyg.k$communities_name),cut),
    #                      breaks=str_wrap(unique(skyg.k$communities_name),cut),
    #                      values=as.vector(alphabet(length(unique(data.known$communities_name))))) +
    # scale_fill_manual(labels=str_wrap(unique(skyg.k$communities_name),cut),
    #                     breaks=str_wrap(unique(skyg.k$communities_name),cut),
    #                     values=as.vector(alphabet(length(unique(data.known$communities_name)))))
  }
  # smtcol = "#8ecae6"
  
  if(prolific){
    title = paste0(names[i]," - Baseline: Prolific Authors")
  }else{
    title = paste0(names[i]," - Baseline: Authors")
  }
  plot <-
    plot + 
    scale_shape_manual(values = c("Academic" = 24, "Company" = 25, "Hybrid" = 23, "Unknown affiliation" = 16)) +
    xlab("") + ylab("Metric") + ggtitle(title) +
    guides(colour = guide_legend(ncol = cols), fill=FALSE, shape = guide_legend(ncol = cols)) +
    theme_minimal() + 
    theme(legend.position=legend,
          legend.key.height=unit(h.leg, "cm"),
          legend.text=element_text(size = s.leg),
          legend.title = element_blank(),
          legend.margin=unit(0.5,"cm"),
          plot.title = element_text(color = "black", face = "bold"),
          plot.subtitle = element_text(color = "#008b9f"),
          plot.caption = element_text(color = "black", face = "italic", size = 6))
  
  # hide_legend(ggplotly(plot))
  
  # data.pareto.comm <- filter(data, communities_name != "?")
  # data.max.comm <- group_by(data.pareto.comm , communities, paper_date) %>% filter(metric == max(metric))
  
  
  return(plot)
  
}

