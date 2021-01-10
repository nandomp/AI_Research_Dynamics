#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("rPref")
#install.packages("lubridate")

library(ggplot2)
library(dplyr)
library(rPref)
library(lubridate)
library(stringr)
library(xtable)



Indicator_with_Ties <- function(a,b) {
  if (is.na(a) || (is.na(b)))
    return(NA)
  if (a > b) {
    return(1)
  } else if (a < b) {
    return(0)
  } else {
    return(0.5)
  }
}

openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  pdf(paste(file, ".pdf", sep=""), width, height)
}



# NEW CHANGES
# - Dejo sólo una entrada cuando hay varios resultados del mismo paper (dejo la mejor). Eso me hacía la puñeta con los "attempts", en realidad que en el mismo paper hayan probado dos cosas se puede considerar como un "attempt".
# - Elimino algún paper del 2021 (aunque esto no tiene ningún efecto)
# - Para las hipótesis 2 y 4, si no hay más de 7 puntos en la SOTA considero que hay pocos puntos para decir si la hipótesis se cumple y pongo NA. Para la hipótesis 3, considero que si no hay más de 4 puntos multi-affiliation en la SOTA considero que hay pocos puntos multi-affiliationa para decir si la hipótesis se cumple. En realidad estos criterios nos favorecen, pero son razonables.



MIN_MULTIPLE_SOTA <- 4 #-1 # -1  # -1 4 # IF THE NUMBER OF MULTIPLE-AFFILIATION COMMUNITIES ON THE SOTA IS LOWER OR EQUAL THAN THIS NUMBER HYPOTHESIS 3 IS NA
MIN_SOTA <- 7 # -1 # -1  7  # IF THE NUMBER OF COMMUNITIES ON THE SOTA IS LOWER OR EQUAL THAN THIS NUMBER HYPOTHESIS 3 AND 4 IS NA

### DATA ####
# List-of-benchs.csv -> Tasks, benchs ands directories

# AffiliationsInfo.v2.csv -> Afiliaciones and type
# AffiliationsInfo.v3.csv -> Afiliaciones and type

# In each directory (task) and subdirectory (bench)
#  - members-communities-year.csv -> members by community and year

affiliations <- read.csv("AffiliationsInfo.v3.csv")
table(affiliations$Type)
#affiliations$Institution[275] 
# "University of California, Los Angeles"
#affiliations$Institution[305] 
# "University of Illinois at Urbana-Champaign"  # long hyphen

# We remove ' from institutions
affiliations$Institution[365] <- "Xian Jiaotong-Liverpool University"  
# It was: "Xi'an Jiaotong-Liverpool University"
affiliations$Institution[366] <- "Xian Jiaotong University"
# It was: "Xi'an Jiaotong University"
affiliations$Institution[367] <- "Xian Jiaotong University"
# It was: "Xi'an Jiaotong University"
affiliations$Institution[28] <- "Brigham and Womens Hospital"
# It was: "Brigham and Women's Hospital"
affiliations$Institution[200] <-  "Queens University"
# It was "Queen's University"
affiliations$Institution[125] <-  "Kings College London"
# It was "King's College London"

#affiliations$Institution <- iconv(affiliations$Institution,to="ASCII//TRANSLIT") # Remove accents
#affiliations$Institution[305]
#affiliations$Institution[305] <- "University of Illinois at Urbana-Champaign" # instead of "University of Illinois at Urbana-Champaign"

directories <- read.csv("List-of-benchs.csv")
#directories <- directories[-12,] # Penn Treebank (word level)
#directories <- directories[-1,]   # Human3.6M
#reverseMetrics <- c(8,10,11,30,31) # With old list of 31 datasets (removing -1 and -12)
#reverseMetrics <- c(9,11,12,31,32) # With new list of 32 datasets (removing -12)
#reverseMetrics <- c(9,11,12,32,33) # With new list of 33 datasets

#benchmarksList <- 11 # WikiText-103
#benchmarksList <- 17 # NLI
#benchmarksList <- 23 # Human Pose


BENCHMARK_LIST <- "Final25"

# To remove (OLD)
#directories <- directories[-grep("WikiText-103", directories$path),]    # 1, 0,  0,  0,  1,   NA, 0,  NA   # Was in submission
#directories <- directories[-grep("SNLI", directories$path),]            # 1, 0,  0,  0,  1,   NA, 0,  1
#directories <- directories[-grep("MPII Human Pose", directories$path),] # 1, 0,  0,  0,  0.5, NA, NA, 1    # Was in submission
# To keep (NEW)
#directories <- directories[-grep("Penn Treebank", directories$path),]   #.5, 1*, NA, NA, 1.0, NA, NA, NA
#directories <- directories[-grep("Human3.6M", directories$path),]       # 1, 0*, 0*, 0*, NA,  NA, NA, NA

# To remove (Nando)
if (BENCHMARK_LIST == "26NoCriteria") {
  directories <- directories[-grep("Human3.6M", directories$path),]       # 1, 0*, 0*, 0*, NA,  NA, NA, NA
  directories <- directories[-grep("SNLI", directories$path),]            # 1, 0,  0,  0,  1,   NA, 0,  1
  directories <- directories[-grep("enwik8", directories$path),]          # 1	NA	NA	NA	0.5	 NA  NA	 NA
  directories <- directories[-grep("Citeseer", directories$path),]        # 0	NA	NA	NA	NA	NA	NA	NA
  directories <- directories[-grep("Pubmed", directories$path),]          # 0	NA	NA	NA	NA	NA	NA	NA
  directories <- directories[-grep("AG News", directories$path),]         # 1	NA	NA	NA	0.0	NA	NA	NA
  directories <- directories[-grep("PASCAL VOC 2007", directories$path),] # 1	NA	NA	NA	0.5	NA	NA	NA    # Was in submission
}

if (BENCHMARK_LIST == "25NoCriteria") {
  directories <- directories[-grep("WikiText-103", directories$path),]    # 1, 0,  0,  0,  1,   NA, 0,  NA   # Was in submission
  directories <- directories[-grep("Human3.6M", directories$path),]       # 1, 0*, 0*, 0*, NA,  NA, NA, NA
  directories <- directories[-grep("enwik8", directories$path),]          # 1	NA	NA	NA	0.5	 NA  NA	 NA
  directories <- directories[-grep("Citeseer", directories$path),]        # 0	NA	NA	NA	NA	NA	NA	NA
  directories <- directories[-grep("Pubmed", directories$path),]          # 0	NA	NA	NA	NA	NA	NA	NA
  directories <- directories[-grep("AG News", directories$path),]         # 1	NA	NA	NA	0.0	NA	NA	NA
  directories <- directories[-grep("PASCAL VOC 2007", directories$path),] # 1	NA	NA	NA	0.5	NA	NA	NA    # Was in submission
  directories <- directories[-grep("STL-10", directories$path),]          # 1	0  	1  	1.0	1.0	NA	NA	NA #     
}

if (BENCHMARK_LIST == "Final25") {
  directories <- directories[-grep("WikiText-103", directories$path),]    # 1, 0,  0,  0,  1,   NA, 0,  NA   # Was in submission
  directories <- directories[-grep("SNLI", directories$path),]            # 1, 0,  0,  0,  1,   NA, 0,  1
  directories <- directories[-grep("Human3.6M", directories$path),]       # 1, 0*, 0*, 0*, NA,  NA, NA, NA
  directories <- directories[-grep("Citeseer", directories$path),]        # 0	NA	NA	NA	NA	NA	NA	NA
  directories <- directories[-grep("Pubmed", directories$path),]          # 0	NA	NA	NA	NA	NA	NA	NA
  directories <- directories[-grep("AG News", directories$path),]         # 1	NA	NA	NA	0.0	NA	NA	NA
  directories <- directories[-grep("PASCAL VOC 2007", directories$path),] # 1	NA	NA	NA	0.5	NA	NA	NA    # Was in submission
  directories <- directories[-grep("STL-10", directories$path),]          # 1	0  	1  	1.0	1.0	NA	NA	NA #     
}

reverseMetrics <- NULL
reverseMetrics <- c(reverseMetrics, grep("CIFAR-10/", directories$path))
reverseMetrics <- c(reverseMetrics, grep("enwik8/", directories$path))
reverseMetrics <- c(reverseMetrics, grep("WikiText-103/", directories$path))
reverseMetrics <- c(reverseMetrics, grep("LibriSpeech test-clean/", directories$path))
reverseMetrics <- c(reverseMetrics, grep("AG News/", directories$path))

directories$path[reverseMetrics]  # The ones that have to be reversed
# [1] "./exports/results/Image Generation/CIFAR-10/csvs"                
# [2] "./exports/results/Language Modelling/enwik8/csvs"                
# [3] "./exports/results/Language Modelling/WikiText-103/csvs"          
# [4] "./exports/results/Speech Recognition/LibriSpeech test-clean/csvs"
# [5] "./exports/results/Text Classification/AG News/csvs"    



Years <- 2000:2021
col.types = c(rep("double", length(Years)))
col.names = c(as.character(Years))

CommunitySize <- read.table(text = "",
                            colClasses = col.types,
                            col.names = col.names)

names(CommunitySize) <- col.names

CommunitySizeSOTA <- CommunitySize

hypotheses <- c("H_Collaboration","H_Persistence","H_Hybrid","H_Company","H_Increase","H_Consecutive","H_CommRise","H_CommWane")
col.types = c("character", "character", rep("double", length(hypotheses)))
col.names = c("benchmarkCategory", "benchmarkName", hypotheses)

Results <- read.table(text = "",
                 colClasses = col.types,
                 col.names = col.names)

benchmarksList <- 1:nrow(directories)
#benchmarksList <- 5 # CIFAR-100
#benchmarksList <- 3 # Moctezuma
#benchmarksList <- 19 # Node Classification  # First three hypothesies are false
#benchmarksList <- 11 # WikiText-102
#benchmarksList <- 17 # NLI
#benchmarksList <- 23 # Human Pose
#benchmarksList <- 10 # enwik8

#benchmarksList <- benchmarksList[-17]

#benchmarksList <- benchmarksList[-1]   # Human3.6M
#benchmarksList <- benchmarksList[-12]   # Penn Treebank (word level)

numBenchmarks <- length(benchmarksList)

for (benchIndex in benchmarksList) {
# benchIndex <- 13 # 6 (imagenet) is better to write and test the code as it has 1+2 points. # 10 # 6 # 14  # 10 Collaboration false

  if (benchIndex %in% reverseMetrics) {
    reverseMetric <- -1 # 1 doesn't reverse metric, and -1 reverses it
  } else {
    reverseMetric <- 1
  } 
  benchmarkName <- directories[benchIndex,]$benchs  # Name
  benchmarkCategory <- directories[benchIndex,]$tasks # Task category
  path <- substring(directories[benchIndex,]$path, 19) # Remove first part of the path
  
  cat(paste0("\n", benchmarkName, "\n"))
  
  #data <- read.csv("Image Classification/ImageNet/csvs/paper-communities.csv")
  data <- read.csv(paste0(path, "/paper-communities.csv"))
  #data.comm <- read.csv(paste0(path, "/legend-communities.csv"))
  
  data$paper_date <- ymd(data$paper_date)
  
  ELIMINATE_2020 <- FALSE
  if (ELIMINATE_2020) {
    data <- data[data$paper_date < "2020-01-01",]
  }
  
  ELIMINATE_2021 <- TRUE
  if (ELIMINATE_2021) {
    data <- data[data$paper_date < "2021-01-01",]  # There are some papers after 2021, which is impossible
  }
  
  RATIO_LAPLACE_SMOOTHING <- FALSE
  
  
  data$metric <- as.numeric(gsub("%", "",data$metric))
  
  data <- data[!is.na(data$metric),]        # Eliminate na metrics
  
  data$metric <- data$metric * reverseMetric
  
  
  ONLY_ONE_PAPER <- TRUE  # Removes several entries of the same paper (keeping the one with highest metric)
  if (ONLY_ONE_PAPER) {
    titlesOK <- gsub("@.*", "", data$title)  # Remove ending part (the @@@)
    data$titlesOK <- titlesOK
    
    
    # First. Sort in the order putting the less desired items last within the groups
    aa <- data[order(data$titlesOK, data$metric, decreasing = TRUE), ] #sort by titlesOK and decreasing in metric
    # Then: Remove items after the first within I'd groups
    
    aa <- aa[!duplicated(aa$titlesOK), ]       
    data <- aa
    rm(aa)
  }  
  
  data$communities <- as.character(data$communities)
  data$communities_name <- as.character(data$communities_name)
  
  data$communities_name[which(data$communities == "")] <- "NA"
  data$communities_name[which(data$communities_name == "set()")] <- "?"
  data$communities_name <- paste0(data$communities, ": ",data$communities_name)
  
  
  a <- data$communities_name
  Encoding(a)
  #Encoding(a) <- "latin-1"
  Encoding(a) <- "UTF-8"
  a
  data$communities_name <- a
  
  skyG <- psel(data, low(as.numeric(paper_date)) * high(metric))
  
  PLOT <- FALSE
  if (PLOT) {
    data.max.comm <- group_by(data, communities, paper_date) %>% filter(metric == max(metric))
    ggplot(data, aes(paper_date, metric, label = communities)) + 
      geom_step(data = skyG, direction = "hv", size = 3, alpha = 0.3) +
      geom_point(aes(colour = str_wrap(communities_name,60)), size = 5) +
      geom_line(data = data.max.comm, aes(paper_date, metric, colour = str_wrap(communities_name,60)), size = 2, alpha = 0.4) +
      # geom_line(aes(colour = communities), linetype = "dashed") +
      geom_text(colour = "black", size = 3) +
      guides(colour = guide_legend(ncol=8)) +
      theme_minimal() + 
      theme(legend.position="bottom",
            legend.key.height=unit(1, "cm"),
            legend.text=element_text(size=6.5),
            legend.title = element_blank())
  }
    
  
  
  warnings() 
  # I remove warnings to start fresh and see if I get warnings from this point
  assign("last.warning", NULL, envir = baseenv())
  # View(data)
  
  
  
  ############################
  # HYPOTHESIS H_Collaboration
  # The majority of points on the SOTA front belong to multi-institution communities. 
  # This hypothesis is satisfied if 50% or more points in the SOTA belong to multi-institution communities
  # (i.e., formed by multiple university and/or company institutions).
  ############################
  
  
  nrow(skyG)
  sota <- skyG[!is.na(skyG$paper_date),]  # Eliminate na dates
  sota <- sota[!is.na(sota$metric),]      # Eliminate na metrics
  sota <- sota[order(sota$paper_date),]
  
  ELIMINATE_FIRST_POINT_FROM_SOTA <- FALSE
  if (ELIMINATE_FIRST_POINT_FROM_SOTA) {
    sota <- sota[-1,]  # Eliminate first paper from SOTA to calculate the hypotheses (it has no merit for the dynamics)
  }  
  #sota$communities
  #sota$communities_name
  
  x <- sota$communities_name
  #x <- x[-grep("\\?", x)]  # Remove communities with no affiliations ("N: ?")
  sota$community_size <- lengths(regmatches(x, gregexpr(",", x))) +1
  remove <- grep("\\?", x) # Communities with no affiliations ("N: ?")
  
  sota$community_size[remove] <- 0 # Set them to 0 (we do not include them for the hypothesis)
  
  sota.single <- sum(sota$community_size == 1)
  print(sota.single)
  sota.multiple <- sum(sota$community_size > 1)
  print(sota.multiple)
  
  H_Collaboration <- (sota.multiple >= sota.single) * 1.0  # This is how it was calculated in the submission. Ties in favour.
  #H_Collaboration <- Indicator_with_Ties(sota.multiple, sota.single)
  
  
  
  ##########################
  # HYPOTHESIS H_Persistence
  # A higher proportion of points on the SOTA front belong to multi-attempt communities than overall. 
  # This hypothesis is satisfied if the ratio of points on the SOTA belonging to multi-attempt communities
  # over points from one single-attempt community is higher than the same ratio for non-SOTA points.
  # Note that, for all hypotheses there are comparisons ('majority', 'higher', 'more likely', 'decrease'), 
  # so the hypothesis may be true, false or neither (a tie) for one benchmark, corresponding to interpretations 
  # such as ">", "<" or "=", respectively. 
  ##########################
  
  all <- data
  all <- all[!is.na(all$communities),]        # Eliminate na communites
  all <- all[all$communities != "",]        # Eliminate empty communites
   all <- all[!is.na(all$paper_date),]  # Eliminate na dates
  all <- all[!is.na(all$metric),]        # Eliminate na metrics
  nrow(all)
   
  ONLY_ONE_PAPER <- FALSE   # ALREADY APPLIED ABOVE
  if (ONLY_ONE_PAPER) {
    titlesOK <- gsub("@.*", "", all$title)  # Remove ending part (the @@@)
    all$titlesOK <- titlesOK
    
    
    # First. Sort in the order putting the less desired items last within the groups
    all2 <- data[order(all$titlesOK, all$metric, decreasing= TRUE), ] #sort by title and and decreasing in metric
    # Then: Remove items after the first within I'd groups
    
    all2 <- all2[!duplicated(all2$titlesOK), ]       
  } else {
    all2 <- all
  } 
  
  
  comms <- all$communities
  # unlist(strsplit("1+2+3", "\\+"))
  comms_numbers <- as.integer(unlist(strsplit(comms, "\\+"))) # We extract all communities as numbers
  communities <- sort(unique(comms_numbers))  # We remove duplicates and order them
  # sum(communities[4] == comms_numbers) # How many times community 4 appears in the data?
  communities.attempts <- NULL
  for (i in 1:length(communities)) {
    communities.attempts[i] <- sum(communities[i] == comms_numbers)
  }  
  # In attempts we have how many points each community has.
  
  attemptRatio <- function(points) {
    if (nrow(points) == 0)
      return(NA)
    points$multiattempt_community <- rep(FALSE, nrow(points))
    for (i in 1:nrow(points)) {
      points_communities <- points$communities[i]  
      comm_numbers <- as.integer(unlist(strsplit(points_communities, "\\+"))) # We extract all communities as numbers
      numComms <-  length(comm_numbers) # How many communities the point has
      multiattempts_comms <- 0
      for (j in comm_numbers) {
        # print(length(comm_numbers))
        comm_index <- which(communities == j) # We extract the index in the communities vector with that community number
        att <- communities.attempts[comm_index]
        if (att > 1) {
          multiattempts_comms <- multiattempts_comms + 1
        }  else if (att < 0) {
          ERROR("Community without attempt?")
        }
      }
      # points$multiattempt_community[i] <- (multiattempts_comms >= 1)  # If a point has two or more communities, and one is multiattemp, the point is multiattempt
      # points$multiattempt_community[i] <- (multiattempts_comms >= numComms/2) # If a point has two or more communities, the point is multiattempt if at least half of them are multiattempt
      points$multiattempt_community[i] <- (multiattempts_comms == numComms) # If a point has two or more communities, the point is multiattempt if all of them are multiattempt
    
      if (FALSE) { # (numComms > 1) {
        # points$multiattempt_community[i] <- NA  # If a point has two or more communities, we exclude the point
        # points$multiattempt_community[i] <- TRUE  # If a point has two or more communities, we make it TRUE
        points$multiattempt_community[i] <- FALSE  # If a point has two or more communities, we make it FALSE
      }
    }
    # print(points)
    num <- sum(points$multiattempt_community == TRUE, na.rm=TRUE) 
    den <- sum(points$multiattempt_community == FALSE, na.rm=TRUE)
    if (RATIO_LAPLACE_SMOOTHING) {
      (num+1)/(den+1)
    } else {
      num/den
    }
  }
  
  
  
  
  all_ratio1 <- attemptRatio(all)
  print(all_ratio1)
  # 6.851852 for Imagenet
  
  sota_ratio1 <- attemptRatio(sota)
  print(sota_ratio1)
  # 17 for Imagenet
  
  #H_Persistence <- (sota_ratio >= all_ratio) * 1.0 
  H_Persistence <- Indicator_with_Ties(sota_ratio1, all_ratio1)
  if (sota.single + sota.multiple <= MIN_SOTA) {
    H_Persistence <- NA
  }
  
  
  
  
  ##########################
  # HYPOTHESIS H_Hybrid
  # A higher proportion of points in the SOTA front belong to hybrid communities than overall.
  # This hypothesis is satisfied if the ratio of points belonging to hybrid communities 
  # over pure university or pure company communities, is higher in the SOTA than outside the SOTA
  ##########################
  
  # Needs company / university classification
  
  hybridRatio <- function(points) { # Also uses affiliations
    if (nrow(points) == 0)
      return(NA)
    
    points$hybrid_community <- rep(FALSE, nrow(points))
    for (i in 1:nrow(points)) {
      # i <- 37 # 42  
      # all$communities_name[i]
      points_communities_names <- points$communities_name[i]  
      points_communities_names <- gsub("}.*", "", points_communities_names)  # Remove ending part (the })
      points_communities_names <- gsub(".*\\{", "", points_communities_names)  # Remove first part
      points_communities_names <- gsub(".*: ", "", points_communities_names)  # Remove first part when there are no affiliations (example: "11: ?")
      # points_communities_names <- gsub("â???", "-", points_communities_names) # Treats bizarre encodings: "Urbana-Champaign"
      # points_communities_names <- iconv(points_communities_names,to="ASCII//TRANSLIT") # Remove accents
      points_communities_names <- gsub("a\\?", "-", points_communities_names) # Treats bizarre encodings: "Urbana-Champaign"
      points_communities_names <- gsub("\',", "#", points_communities_names) # Replaces ', by separator #
      points_communities_names <- gsub("\",", "#", points_communities_names) # Replaces ", by separator #
      points_communities_names <- gsub("# ", "#", points_communities_names) # Removes spaces after commas
      points_communities_names <- gsub('[\'\"\\\\]', '', points_communities_names) # Remove ' " and \ 
      if (points_communities_names == "?") { # The community does not have affiliations
        # points$hybrid_community[i] <- TRUE  # We consider communities without affiliations hybrid
        # points$hybrid_community[i] <- FALSE  # We consider communities without affiliations pure
        points$hybrid_community[i] <- NA    # We exclude communities without affiliations (neither hybrid nor pure)
      } else  { # The community does have affiliations
        aff_names <- unlist(strsplit(points_communities_names, "\\#")) # We extract the affiliation names using the separator #
        Encoding(aff_names) <- "UTF-8"
        academic <- 0
        company <- 0
        for (j in aff_names) {
          # print(j)
          aff_index <- which(affiliations$Institution == j) # We extract the index in the affiliation
          # print(aff_index)
          if (length(aff_index) == 0) {
            ERROR("Affiliation not found")
            break
          }
          if (length(aff_index) > 1) {
            if (aff_index[1] != 366) { # If it is not Xian
              cat("More than one affiliation found: ")
              #ERROR("More than one affiliation found")
              cat(aff_index)
              cat("| ")
            }  
            aff_index <- aff_index[1]
            break
          }
          if (affiliations[aff_index,]$Type == "Academic") {
            academic <- academic + 1
          } else if (affiliations[aff_index,]$Type == "Company") {
            company <- company + 1
          } else {
            ERROR("Type of affiliation not found or unknown")
          }  
        }  
        hybrid <-  ((academic > 0) && (company > 0))   # communities without affiliations are treated above
        # if (length(aff_names) < 2) { # Communities with one affiliation are not considered (they could not be hybrid)
        #  hybrid <- FALSE
        # }  
        
        points$hybrid_community[i] <- hybrid
      }
    }
    
    num_hybrid_communities <- sum(points$hybrid_community == TRUE, na.rm = TRUE)
    num_pure_communities <- sum(points$hybrid_community == FALSE, na.rm = TRUE)
    num_hybrid_communities + num_pure_communities
    # must be equal or lower than
    nrow(points)
    
     
    num <- num_hybrid_communities
    den <- num_pure_communities
    if (RATIO_LAPLACE_SMOOTHING) {
      (num+1)/(den+1)
    } else {
      num/den
    }
    
  }
  
  all_ratio2 <- hybridRatio(all)
  # 2.473684 for imagenet
  
  sota_ratio2 <- hybridRatio(sota)
  # 8
  
  #H_Hybrid <- (sota_ratio >= all_ratio) * 1.0
  H_Hybrid <- Indicator_with_Ties(sota_ratio2, all_ratio2)
  if (sota.multiple <= MIN_MULTIPLE_SOTA) {
    H_Hybrid <- NA
  }
  
  
  ##########################
  # HYPOTHESIS H_Company
  # Points ont he SOTA are more likely to have companies than outside the SOTA front. This hypothesis is satisfied if the ratio of points that contain at least one company over points without one is higher on the SOTA that outside the SOTA.
  ##########################
  
  # Needs company / university classification
  
  withcompanyRatio <- function(points) { # Also uses affiliations
    if (nrow(points) == 0)
      return(NA)
    
    points$withcompany_community <- rep(FALSE, nrow(points))
    for (i in 1:nrow(points)) {
      # i <- 37 # 42  
      # all$communities_name[i]
      points_communities_names <- points$communities_name[i]  
      points_communities_names <- gsub("}.*", "", points_communities_names)  # Remove ending part (the })
      points_communities_names <- gsub(".*\\{", "", points_communities_names)  # Remove first part
      points_communities_names <- gsub(".*: ", "", points_communities_names)  # Remove first part when there are no affiliations (example: "11: ?")
      # points_communities_names <- gsub("â???", "-", points_communities_names) # Treats bizarre encodings: "Urbana-Champaign"
      # points_communities_names <- iconv(points_communities_names,to="ASCII//TRANSLIT") # Remove accents
      points_communities_names <- gsub("a\\?", "-", points_communities_names) # Treats bizarre encodings: "Urbana-Champaign"
      points_communities_names <- gsub("\',", "#", points_communities_names) # Replaces ', by separator #
      points_communities_names <- gsub("\",", "#", points_communities_names) # Replaces ", by separator #
      points_communities_names <- gsub("# ", "#", points_communities_names) # Removes spaces after commas
      points_communities_names <- gsub('[\'\"\\\\]', '', points_communities_names) # Remove ' " and \ 
      if (points_communities_names == "?") { # The community does not have affiliations
        # points$withcompany_community[i] <- TRUE  # We consider communities without affiliations with company
        # points$withcompany_community[i] <- FALSE  # We consider communities without affiliations without company
        points$withcompany_community[i] <- NA    # We exclude communities without affiliations (neither with nor without company)
      } else  { # The community does have affiliations
        aff_names <- unlist(strsplit(points_communities_names, "\\#")) # We extract the affiliation names using the separator #
        Encoding(aff_names) <- "UTF-8"
        academic <- 0
        company <- 0
        for (j in aff_names) {
          # print(j)
          aff_index <- which(affiliations$Institution == j) # We extract the index in the affiliation
          # print(aff_index)
          if (length(aff_index) == 0) {
            ERROR("Affiliation not found")
            break
          }
          if (length(aff_index) > 1) {
            if (aff_index[1] != 366) { # If it is not Xian
              cat("More than one affiliation found: ")
              #ERROR("More than one affiliation found")
              cat(aff_index)
              cat("| ")
            }  
            aff_index <- aff_index[1]
            break
          }
          if (affiliations[aff_index,]$Type == "Academic") {
            academic <- academic + 1
          } else if (affiliations[aff_index,]$Type == "Company") {
            company <- company + 1
          } else {
            ERROR("Type of affiliation not found or unknown")
          }  
        }  
        
        points$withcompany_community[i] <- (company > 0)
        
      }
    }
    
    num_withcompany_communities <- sum(points$withcompany_community == TRUE, na.rm = TRUE)
    num_withoutcompany_communities <- sum(points$withcompany_community == FALSE, na.rm = TRUE)
    num_withcompany_communities + num_withoutcompany_communities
    # must be equal or lower than
    nrow(points)
    
    
    num <- num_withcompany_communities
    den <- num_withoutcompany_communities
    if (RATIO_LAPLACE_SMOOTHING) {
      (num+1)/(den+1)
    } else {
      num/den
    }
    
  }
  
  all_ratio3 <- withcompanyRatio(all)
  # 2.473684 for imagenet
  
  sota_ratio3 <- withcompanyRatio(sota)
  # 8
  
  #H_company <- (sota_ratio >= all_ratio) * 1.0
  H_Company <- Indicator_with_Ties(sota_ratio3, all_ratio3)
  #if (sota.multiple < MIN_MULTIPLE_SOTA) {
  #  H_Company <- NA
  #}
  if (sota.single + sota.multiple <= MIN_SOTA) {
    H_Company <- NA
  }
  
  ##########################
  # HYPOTHESIS H_Increase
  # Jumps with a SOTA increase higher than the total increase of the SOTA in the previous year 
  # are associated with higher-than-usual activity growth ratio in the following year. 
  # Higher-than-usual activity growth is defined as an activity growth ratio higher than 
  # the average activity variation ratio
  
  # Option 3a
  # The SOTA metric per year is positively correlated with the activity in the following year
  # Expected, as both the activity and the sota grow.
  
  # Option 3b
  # The SOTA metric per year is more positively correlated with the activity in the following year than in the same year
  
  # Option 4a
  # The SOTA metric 2-year difference is positively correlated with the all points 2-year activity difference in the following year
  
  # FINALLY IMPLEMENTED
  # Option 4b
  # Jumps with a SOTA increase higher than the total increase of the SOTA in the previous year 
  # are associated with higher-than-average activity growth in the following year.
  # Higher-than-average activity growth is defined as an activity two-year difference higher than 
  # the average activity two-year difference
  ##########################
  
  if (nrow(sota) == 0) {
    H_Increase <- NA
  } else {
    allYears <- format(all$paper_date, "%Y")
    allActivityPerYear <- tapply(all$metric, allYears, length)  # Years with 0 are ignored (so we also avoid divisions by 0)
    
    # average activity variation ratio
    #nYears <- length(allActivityPerYear)
    ##activityVariationRatio <- 0
    #activityVariationRatio <- 1
    #for (i in 2:nYears) {
    #  #activityVariationRatio <- activityVariationRatio + allActivityPerYear[i] / allActivityPerYear[i-1]
    #  activityVariationRatio <- activityVariationRatio * allActivityPerYear[i] / allActivityPerYear[i-1]
    #}
    ##activityVariationRatio <- activityVariationRatio / (nYears-1)
    #activityVariationRatio <- activityVariationRatio ^ -(nYears-1)
    
    sotaYears <- format(sota$paper_date, "%Y")
    sotaMetricPerYear <- tapply(sota$metric, sotaYears, max)
    
    minYear <- as.integer(min(allYears))
    maxYear <- as.integer(max(allYears))
    nYears <- maxYear - minYear + 1
    allActivityPerYearFull <- minYear:maxYear
    names(allActivityPerYearFull) <- minYear:maxYear
    sotaMetricPerYearFull <- minYear:maxYear
    names(sotaMetricPerYearFull) <- minYear:maxYear
    for (i in minYear:maxYear) {
      v <- allActivityPerYear[as.character(i)]
      if (is.na(v)) {
        v <- 0
      } 
      allActivityPerYearFull[as.character(i)] <- v
      
      v <- sotaMetricPerYear[as.character(i)]
      if (is.na(v)) {
        v <- sotaMetricPerYearFull[as.character(i-1)]
      } 
      sotaMetricPerYearFull[as.character(i)] <- v
    }
    
    EXCLUDE_LAST_YEAR <- TRUE
    if (EXCLUDE_LAST_YEAR) {
      nYears <- nYears - 1
    }  
    
    DIFF_CALCULATION <- TRUE
    
    if (!DIFF_CALCULATION) { # Option 3
      a <- sotaMetricPerYearFull[1:(nYears-1)]  # ok
      #c <- sotaMetricPerYearFull[2:nYears]
      b <- allActivityPerYearFull[2:nYears]   # ok
      b0 <- allActivityPerYearFull[1:(nYears-1)]
      
      # Option 3a
      c <- cor(a,b)
      if (FALSE) {
        # Option 3b
        c0 <- cor(a,b0)
        c <- c - c0
      }  
    } else {  # Option 4
      a <- sotaMetricPerYearFull[2:nYears] - sotaMetricPerYearFull[1:(nYears-1)]
      b <- allActivityPerYearFull[2:nYears] - allActivityPerYearFull[1:(nYears-1)]
      
      usual <- mean(b) # average activity growth 
      option4b <- NA
      if (nYears-2 >= 2) {
        for (j in 2:(nYears-2)) {
          if (a[j] > a[j-1]) { # Jump with a SOTA increase higher than the total increase of the SOTA in the previous year
            k <- Indicator_with_Ties(b[j+1], usual) - 0.5
            if (is.na(option4b)) {
              option4b <- k
            } else {
              option4b <- option4b + k
            }
          }
        }
      }
      
      c <- option4b
      
      if (FALSE) {
        # Option 4a
        a <- a[1:(nYears-2)]  # ok
        # a <- a[1:(nYears-1)]  
        b <- b[2:(nYears-1)]  # ok
        # b <- b[1:(nYears-1)]  
        c <- cor(a,b)
      }
      
    }
    

    print(c)
    
    H_Increase <- Indicator_with_Ties(c, 0)
    
    # if (is.na(H_Increase)) STOP("Stop")
    
    #if (cor.test(a,b)$p.value > 0.8)
    #  H_Increase <- NA
  } 

  
  
  
  ##########################
  # HYPOTHESIS H_consecutive
  # Consecutive jumps (on the SOTA front) by the same community are followed by 
  # a lower-than-usual activity growth ratio in the following year.
  # Lower-than-usual activity growth is defined as an activity growth ratio lower than the average activity variation ratio.
  
  # FINALLY IMPLEMENTED
  # Consecutive jumps (on the SOTA front) by the same community are followed by
  # a lower-than-usual activity growth in the following year. 
  # # Higher-than-average activity growth is defined as an activity two-year difference higher than 
  # the average activity two-year difference
  ##########################
  
  
  
  
  #sotaYears <- format(sota$paper_date, "%Y")
  comms <- sota[order(sota$paper_date),]$communities
  print(comms)
  hyp <- NA
  nPoints <- length(comms)
  if (nPoints >= 2) {
    for (i in 2:nPoints) {
      # unlist(strsplit("3+12+5", "\\+"))
      coA <- unlist(strsplit(comms[i], "\\+"))
      coB <- unlist(strsplit(comms[i-1], "\\+"))
      common <- length(intersect(coA,coB)) >= 1 
      #if (comms[i] == comms[i-1]) {   # Consecutive jumps (on the SOTA front) by the same community
      if (common) {   # Consecutive jumps (on the SOTA front) by the same community 
        thisYear <- format(sota[i,]$paper_date, "%Y")
        index <- which(names(b) == thisYear)
        if ((length(index) > 0) && (index < length(b))) {  # if Year found in b and lower than the size (not the last one)
          k <- Indicator_with_Ties(usual, b[index+1]) - 0.5  # Lower than usual (that's why we put usual before b[index-1])
          if (is.na(hyp)) {
            hyp <- k
          } else {
            hyp <- hyp + k
          }  
        }
      }
    }
  }
  

  #print(hyp)
  
  
  # if (!is.na(hyp)) STOP("Stop")
  
  H_Consecutive <- Indicator_with_Ties(hyp, 0)
  
  print(H_Consecutive)
  
  
  #if (benchmarkName == "ImageNet") STOP("Stop")
  
  
  
  
  # Hypotheses for communities:
  
  # Needs info about the no. of members of each community per year (or the original sources of papers, communities and authors)
  
  #if (benchmarkName == "ImageNet") ERROR()
  
  CommunityInfo <- read.csv(paste0(path, "/members-communities-year.csv"))
  # We have to remove 2021
  CommunityInfo <- CommunityInfo[CommunityInfo$year <= 2020,]
  numCommunities <- length(unique(CommunityInfo$communities))
  
  
  
  
  ##########################
  # HYPOTHESIS H_CommRise
  
  # Original JoinSucc
  # Communities attract more members before success years than in other years. 
  # More precisely, we calculate the average number of active members for any years immediately before a success
  # (the community achieving a new jump on the SOTA) and compare it with the average number of members for the other years.
  # If the former average is higher than the latter average then the hypothesis is true.
  # Further active members contribute to SOTA jumps
  
  # FINALLY IMPLEMENTED H_CommRise
  # Communities have more active members before a SOTA success than in other previous years.
  # More precisely, for each new SOTA jump a community makes, 
  # we calculate (1) the number of active members (size) of the community for the *year immediately before* the SOTA jump.
  # and (2) the number of active members (size) of the community for all the previous years. 
  # If (1) is higher than (2) on average then the hypothesis is true. 
  # Further active members contribute to SOTA jumps
  # Nando's explanation: Before a SOTA, communities attract new members, grow alliances and become bigger... and then success!  
  # Jose's reading: Here we focus on absolute size.
  ##########################
  
  # Needs info about the no. of members of each community per year (or the original sources of papers, communities and authors)
  
  RELATIVE_SIZE <- FALSE
  
  trendsCommRise <- function(points, itemsPerCommunity) {
    hyp <- NA
    if (nPoints >= 1) {
      for (i in 1:nPoints) {  # Looking at all the points
        thisCommunity <- comms[i]
        thisYear <- format(points[i,]$paper_date, "%Y")
        thisCommunityRows <- (CommunityInfo$communities == thisCommunity)
        
        allYears <- unique(CommunityInfo[thisCommunityRows,]$year)
        
        #selectedYears <- allYears # All years
        #selectedYears <- allYears[allYears == thisYear] # This year
        #selectedYears <- allYears[allYears <= thisYear] # This and previous years
        selectedYears <- allYears[allYears < thisYear] # Previous years
        #selectedYears <- allYears[allYears != thisYear] # Any year but this year

        #thisCommunityThisYearRows <- (thisCommunityRows & (CommunityInfo$year == thisYear)) # This community and this year
        #thisCommunityThisYearRows <- thisCommunityRows # This community any year
        thisYearRows <- (CommunityInfo$year %in% selectedYears) # The selected years
        thisCommunityThisYearRows <- (thisCommunityRows & thisYearRows) # This community and the selected years
        if (sum(thisCommunityThisYearRows) > 0) { 
          # mThis <- mean(CommunityInfo[thisCommunityThisYearRows,][,itemsPerCommunity])  # Average number of authors
          mThisAnyCommunity <- (sum(CommunityInfo[thisYearRows,][,itemsPerCommunity], na.rm=TRUE) / length(selectedYears)) / numCommunities
          mThis <- sum(CommunityInfo[thisCommunityThisYearRows,][,itemsPerCommunity], na.rm=TRUE) / length(selectedYears)  # Total number of authors / year
          if (RELATIVE_SIZE) {
            mThis <- mThis / mThisAnyCommunity # Relative size of the community rather than absolute size
          }
          
          prevYear <- as.character(as.integer(thisYear) - 1) # Immediately previous year
          #nextYear <- allYears[allYears < thisYear][1]       # First year that precedes
          #prevYear <- allYears[allYears < thisYear]           # Any year that precedes
          
          prevYearRows <- (CommunityInfo$year %in% prevYear)
          thisCommunityPrevYearRows <- (thisCommunityRows & prevYearRows)
          if (sum(thisCommunityPrevYearRows) > 0) {  # The community doesn't have anything in the previous year.
            # mPrev <- mean(CommunityInfo[thisCommunityNextYearRows,][,itemsPerCommunity]) # Average number of authors in the year previous to the success
            mPrevAnyCommunity <- (sum(CommunityInfo[prevYearRows,][,itemsPerCommunity], na.rm=TRUE)  / length(prevYear)) / numCommunities
            mPrev <- sum(CommunityInfo[thisCommunityPrevYearRows,][,itemsPerCommunity], na.rm=TRUE)  / length(prevYear) # Total number of authors in the year previous to the success
            if (RELATIVE_SIZE) {
              mPrev <- mPrev / mPrevAnyCommunity # Relative size of the community rather than absolute size
            }
              
            if (!is.nan(mPrev)) {
              k <- Indicator_with_Ties(mPrev, mThis) - 0.5  # 
              if (is.na(hyp)) {
                hyp <- k
              } else {
                hyp <- hyp + k
              }  
            } 
          }  
        }
      }
      hyp
    }
  }  
  
  print(hyp)
  
  
  hypSota <- trendsCommRise(sota, "authors")  # We could try "papers" too
  hypAll <- trendsCommRise(all, "authors")
  
  print(hypSota)
  print(hypAll)
  

  H_CommRise <- Indicator_with_Ties(hypSota, 0)
  #H_CommRise <- Indicator_with_Ties(hypSota, hypAll)
  
  
  
  
  
  ##########################
  # HYPOTHESIS H_CommWane
  
  # Original SuccJoin
  # Communities attract more members after success years than in other years.
  # More precisely, we calculate the average number of active members for any year immediately after a success
  # (the community achieving a new jump on the SOTA) and compare it with the average number of members for the other years. 
  # If the former average (year after) is higher than the latter average (other years) then the hypothesis is true. 
  # SOTA jumps contribute to further active members
  
  # VARIANT
  # Communities attract more members after success years than in previous years.
  # More precisely, we calculate the  number of active members for any *year after immediately after* a success
  # (the community achieving a new jump on the SOTA) and compare it with the number of members for the previous years. 
  # If the former average (year after) is higher than the latter average (other years) then the hypothesis is true. 
  # SOTA jumps contribute to further active members
  
  
  # NEGATIVE VERSION: H_CommWane  # CommPull
  # Communities become relatively larger after SOTA success. (IF THE HYPOTHESES IS FALSE: communities become relatively smaller after SOTA success)
  # More precisely, for each new jump on the SOTA a community makes, 
  # we calculate (1) the relative size (number of active authors) of the community for the *year immediately after* the SOTA jump.
  # and compare it with (2) the relative size (number of active authors) of the community for all the previous years. 
  # If (1) is higher than (2) on average then the hypothesis is true. 
  # SOTA jumps contribute to further attract active members to the community (IF THE HYPOTHESES IS FALSE: SOTA jumps are associated with fewer active members in the next year )
  
  
  # FINALLY IMPLEMENTED: H_CommWane
  # Communities become relatively smaller after SOTA success.
  # More precisely, for each new jump on the SOTA a community makes, 
  # we calculate (1) the relative size (number of active authors) of the community for the *year immediately after* the SOTA jump.
  # and compare it with (2) the relative size (number of active authors) of the community for all the previous years. 
  # If (1) is higher than (2) on average then the hypothesis is true. 
  # SOTA jumps are associated with fewer active members in the next year 
  # Nando's explanation: the community has met its goal (SOTA) and members do not have the same interest in the benchmark, leaving, possibly moving to other benchmarks, etc.
  # Jose: here we focus on relative size, meaning other communities get larger in proportion.
  
  
  ##########################
  
  
  RELATIVE_SIZE <- TRUE
  
  trendsCommWane <- function(points, itemsPerCommunity) {
    comms <- points[order(points$paper_date),]$communities
    # print(comms)
    nPoints <- length(comms)
    
    hyp <- NA
    if (nPoints >= 1) {
      for (i in 1:nPoints) {  # Looking at all the points
        thisCommunity <- comms[i]
        thisYear <- format(points[i,]$paper_date, "%Y")
        thisCommunityRows <- (CommunityInfo$communities == thisCommunity)
        
        allYears <- unique(CommunityInfo[thisCommunityRows,]$year)
        
        #selectedYears <- allYears # All years
        #selectedYears <- allYears[allYears == thisYear] # This year
        #selectedYears <- allYears[allYears <= thisYear] # This and previous years
        selectedYears <- allYears[allYears < thisYear] # Previous years
        #selectedYears <- allYears[allYears != thisYear] # Any year but this year
        
        #thisCommunityThisYearRows <- (thisCommunityRows & (CommunityInfo$year == thisYear)) # This community and this year
        #thisCommunityThisYearRows <- thisCommunityRows # This community any year
        thisYearRows <- (CommunityInfo$year %in% selectedYears) # The selected years
        thisCommunityThisYearRows <- (thisCommunityRows & thisYearRows) # This community and the selected years
        if (sum(thisCommunityThisYearRows) > 0) { 
          # mThis <- mean(CommunityInfo[thisCommunityThisYearRows,][,itemsPerCommunity])  # Average number of authors
          mThisAnyCommunity <- (sum(CommunityInfo[thisYearRows,][,itemsPerCommunity], na.rm=TRUE) / length(selectedYears)) / numCommunities
          mThis <- sum(CommunityInfo[thisCommunityThisYearRows,][,itemsPerCommunity], na.rm=TRUE) / length(selectedYears)  # Total number of authors / year
          if (RELATIVE_SIZE) {
            mThis <- mThis / mThisAnyCommunity # Relative size of the community rather than absolute size
          }
          
          #nextYear <- as.character(as.integer(thisYear) + 1)  # Immediately following year
          #nextYear <- allYears[allYears > thisYear][1]           # First year that follows
          nextYear <- allYears[allYears > thisYear]           # Any year that follows
          
          nextYearRows <- (CommunityInfo$year %in% nextYear)
          thisCommunityNextYearRows <- (thisCommunityRows & nextYearRows)
          if (sum(thisCommunityNextYearRows) > 0) {  # The community doesn't have anything in the following year.
            # mNext <- mean(CommunityInfo[thisCommunityNextYearRows,][,itemsPerCommunity]) # Average number of authors in the year following the success
            mNextAnyCommunity <- (sum(CommunityInfo[nextYearRows,][,itemsPerCommunity], na.rm=TRUE)  / length(nextYear)) / numCommunities
            mNext <- sum(CommunityInfo[thisCommunityNextYearRows,][,itemsPerCommunity], na.rm=TRUE)  / length(nextYear) # Total number of authors in the year following the success
            if (RELATIVE_SIZE) {
              mNext <- mNext / mNextAnyCommunity # Relative size of the community rather than absolute size
            }
            
            if (!is.nan(mNext)) {
              k <- Indicator_with_Ties(mNext, mThis) - 0.5  # 
              if (is.na(hyp)) {
                hyp <- k
              } else {
                hyp <- hyp + k
              }  
            }  
          }  
        }
      }
    }
    hyp
  }
  
  hypSota <- trendsCommWane(sota, "authors")  # We could try "papers" too
  hypAll <- trendsCommWane(all, "authors")
  #hypSota <- hypAll
  
  print(hypSota)
  print(hypAll)
  
  #H_CommWane <- Indicator_with_Ties(hypSota, 0)
  H_CommWane <- Indicator_with_Ties(0, hypSota)
  #H_CommWane <- Indicator_with_Ties(hypSota, hypAll)
  
  
  
  
  
  
  ##########################
  # COMMUNITY SIZE (ACTIVE MEMBERS) EVOLUTION
  ###########################
 
  myComms <- CommunityInfo # All communities
  
  s <- aggregate(myComms$authors, by=list(year=myComms$year), FUN=mean)
  for (i in s$year) {
    ind <- which(i == Years)
    CommunitySize[benchIndex, ind] <- s[s$year == i,]$x
  }
  
  myComms2 <- CommunityInfo[CommunityInfo$communities %in% sota$communities,]
  
  s <- aggregate(myComms2$authors, by=list(year=myComms2$year), FUN=mean)
  for (i in s$year) {
    ind <- which(i == Years)
    CommunitySizeSOTA[benchIndex, ind] <- s[s$year == i,]$x
  }
  
  ##########################
  # OUTPUTING THE RESULTS OF THE HYPOTHESES
  ###########################
  
  
  
  Results[benchIndex,]$benchmarkCategory <- benchmarkCategory
  Results[benchIndex,]$benchmarkName <- benchmarkName
  
  Results[benchIndex,]$H_Collaboration <- H_Collaboration
  Results[benchIndex,]$H_Persistence <- H_Persistence
  Results[benchIndex,]$H_Hybrid <- H_Hybrid
  Results[benchIndex,]$H_Company <- H_Company
  Results[benchIndex,]$H_Increase <- H_Increase
  Results[benchIndex,]$H_Consecutive <- H_Consecutive
  Results[benchIndex,]$H_CommRise <- H_CommRise
  Results[benchIndex,]$H_CommWane <- H_CommWane
  
  Results[benchIndex,]
}

rownames(Results)[1] <- 1

#Results[,1:5]



########################
######## Community Size Plot
########################

ActiveCommMembers <- colMeans(CommunitySize, na.rm=T)
ActiveCommMembers <- ActiveCommMembers[(names(ActiveCommMembers) >= 2015 & names(ActiveCommMembers) <= 2020)]
dfActiveCommMembers <- data.frame(names(ActiveCommMembers),ActiveCommMembers)
names(dfActiveCommMembers) <- c("Year", "Active Members")
m1 <- mean(ActiveCommMembers)

ActiveCommMembersSOTA <- colMeans(CommunitySizeSOTA, na.rm=T)
ActiveCommMembersSOTA <- ActiveCommMembersSOTA[(names(ActiveCommMembersSOTA) >= 2015 & names(ActiveCommMembersSOTA) <= 2020)]
dfActiveCommMembersSOTA <- data.frame(names(ActiveCommMembersSOTA),ActiveCommMembersSOTA)
names(dfActiveCommMembersSOTA) <- c("Year", "Active Members")
m2 <- mean(ActiveCommMembersSOTA)

#ggplot(dfActiveCommMembers)
# ggplot() + 
#  geom_line(data = dfActiveCommMembers, aes(x = Year, y = `Active Members`, group = 1 ), color = "red") +
#  geom_line(data = dfActiveCommMembersSOTA, aes(x = Year, y = `Active Members`, group = 1 ), color = "blue") +
#  ylab('Community Active Members (Average)') +
#  labs(color="hello")

NANDO_PLOT <- TRUE

if (!NANDO_PLOT) {
  ggplot(dfActiveCommMembers,aes(x = Year, y = `Active Members`, group= 1))+geom_line(aes(color=  paste0("ALL (mean=", format(m1, digits=3), ")")))+
        geom_line(data=dfActiveCommMembersSOTA,aes(color= paste0("SOTA (mean=", format(m2, digits=3), ")")))+
        ylab('Active Members per Community (Average)') +
        theme(legend.position = c(0.2, 0.8)) +
        labs(color="Communities")

  ggsave("ActiveMembers.pdf", width = 5.5, height = 3.3)
} else {
  all_text <- paste0("All communities (mean=", format(m1, digits=3), ")")
  sota_text <-  paste0("SOTA communities (mean=", format(m2, digits=3), ")")
  
  AM <- ggplot(dfActiveCommMembers,aes(x = Year, y = `Active Members`, group= 1))+
          geom_line(aes(color=all_text))+
          geom_point(aes(color=all_text))+
          geom_line(data=dfActiveCommMembersSOTA,aes(color=sota_text))+
          geom_point(data=dfActiveCommMembersSOTA,aes(color=sota_text))+
          ylab('Active Members per Community (Average)') +
          scale_colour_manual("Communities", values = c("#0b3954", "#c81d25")) +
          theme_minimal() +
          theme(legend.position=c(0.2,0.85)) +
          labs(color="")

  openPDFEPS(paste0("ActiveMembersN"), height= 4, width= 8)
  print(AM)
  dev.off()
}  


########################
######## PROCESS RESULTS
########################

options(scipen=999)  # No scientific notation
#options(digits=5)  # 3 digits

NUM_HYP <- 8
COMP_HYP <- 8 # How many hypotheses we have calculated so far
#colMeans(Results[,3:10])
colMeans(Results[,3:(3+COMP_HYP-1)], na.rm=TRUE)

if (numBenchmarks > 1) {
  Stats <- Results[1, 2:(2+NUM_HYP)]  # We inherit the structure
  names(Stats)[1] <- ""
  Stats[1, 2:(2+NUM_HYP-1)] <- colMeans(Results[,3:(3+NUM_HYP-1)], na.rm=TRUE)
  Stats[1,1] <- "probability"
  
  
  Stats[2,1] <- "p-value"
  Stats[3,1] <- "significant (95%)"
  Stats[4,1] <- "significant (95%) with Bonferroni"
  Stats[5,1] <- "non-NA cases"
  Stats[6,1] <- "total cases"

  for (i in 1:COMP_HYP) {
    #i <- 1
    r <- Results[, i+2]
    pos <- sum(r == 1.0, na.rm=TRUE)
    ties <- sum(r == 0.5, na.rm=TRUE)
    neg <- sum(r == 0, na.rm=TRUE)
    nonNA <- sum(!is.na(r))
    if (pos > neg) {
      halfties <- ceiling(ties/2)
    } else {
      halfties <- floor(ties/2)
    }
      
    #binom.test(15+ ceiling(5/2), 21, p = 0.5, alternative = c("two.sided"), conf.level = 0.95)
    # binom.test(21 + ceiling(0), 31, p = 0.5, alternative = c("two.sided"), conf.level = 0.95)  # p-value = 0.07
    # binom.test(22 + ceiling(0), 31, p = 0.5, alternative = c("two.sided"), conf.level = 0.95)  # p-value = 0.029
    mytest <- binom.test(pos+ halfties, nonNA, p = 0.5, alternative = c("two.sided"), conf.level = 0.95)
    mytest
    Stats[2,i+1] <- mytest$p.value
    
    # No Bonferroni: p-value < 0.05             YES
    Stats[3, i+1] <- (mytest$p.value <= 0.05)
    
    # Bonferroni:    p-value < 0.05/6 = 0.0083  YES
    Stats[4, i+1] <- (mytest$p.value <= 0.05/NUM_HYP)
    
    Stats[5, i+1] <- nonNA
      
    Stats[6, i+1] <- numBenchmarks
  }

  Stats[,1:(1+COMP_HYP)]
}



########################
######## EXPORT TO LATEX
########################

Results4LaTeX <- Results
#rownames(Results4LaTeX) <- c()  

names(Results4LaTeX)[1:2] <- c("Category", "Benchmark")

Results4LaTeX[Results4LaTeX == 1] <- "\\cmark"
Results4LaTeX[Results4LaTeX == 0]  <- "\\xmark"
Results4LaTeX[Results4LaTeX == 0.5] <- "$-$"

REMOVE_CATEGORY <- TRUE
if (REMOVE_CATEGORY) {
  Results4LaTeX <- Results4LaTeX[,-1]
}  

# xtable(Results4LaTeX, include.rownames=FALSE)
print.xtable(xtable(Results4LaTeX), file = "results.tex", include.rownames=FALSE, sanitize.text.function=identity)

dig <- c(0,0,rep(3,COMP_HYP))
print.xtable(xtable(Stats, digits=dig), file = "stats.tex", include.rownames=FALSE) #, sanitize.text.function=identity)

