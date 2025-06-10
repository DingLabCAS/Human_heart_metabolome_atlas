suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(Rtsne))


Data_Normalization <- function(data, nor_method){
  
  if(nor_method == "Centering"){
    df <- apply(data, 2, function(x) x - mean(x, na.rm=TRUE)) 
  }
  else if(nor_method == "Auto"){
    df <- apply(data, 2, function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
  }
  else if(nor_method == "Range"){
    df <- apply(data, 2, function(x) (x - mean(x, na.rm=TRUE))/(max(x) - min(x)))
  }
  else if(nor_method == "Min-Max"){
    df <- apply(data, 2, function(x) (x - min(x))/(max(x) - min(x)))
  }
  else if(nor_method == "Pareto"){
    df <- apply(data, 2, function(x) (x - mean(x, na.rm=TRUE))/sqrt(sd(x, na.rm=TRUE)))
  }
  else if(nor_method == "Level"){
    df <- apply(data, 2, function(x) (x - mean(x, na.rm=TRUE))/mean(x, na.rm=TRUE))
  }
  
  return(df)
  
}


# main
wkdir <- "."  
wkdir <- normalizePath(wkdir, winslash = "/")
setwd(wkdir)

data <- read.xlsx("../Heart_atlas_data.xlsx", colNames = F, rowNames = F, check.names = F) 

data2 <- data[-c(2:6), -c(1, 3:5)] 
data2[1,1]  <- "Metabolite"
data_head <- data2[1, ]

all_types <- c("D") # , "O"
class_order <- c("LV","RV","IVS","LA","RA","AVS","AV","PV","TV","MV","AA","PA","SVC","IVC","PVN","LCA","RCA")
class_patt <- paste("D", class_order, collapse="_*|", sep="_")
class_patt <- paste0(class_patt, "_*")

keep_index <- grep(class_patt, data_head)
data3 <- data2[, c(1, keep_index)]
colnames(data3) <- data3[1,]
data3 <- data3[-1, ]
data3 <- data3[, -1]
data3_colnames <- colnames(data3)
rownames(data3) <- seq(1, dim(data3)[1])

groups <- as.character(data3_colnames)
groups <- gsub("_\\d+.*$", "", groups)
groups <- gsub("^.*_", "", groups)
groups <- factor(groups, levels= class_order, ordered=TRUE)

type <- as.character(data3_colnames)
type <- gsub("_.*", "", type)
type <- factor(type, levels= all_types, ordered=TRUE)

data3 <- as.data.frame(t(data3))
data4 <- as.data.frame(matrix(0, dim(data3)[1], dim(data3)[2]))  # 734 2220
colnames(data4) <- colnames(data3)

for(i in 1:dim(data3)[1]){
  for(j in 1:dim(data3)[2]){
    x <- data3[i, j]
    if(is.na(x) | is.null(x)){
      x <- 0
    }
    x <- as.numeric(x)
    if(is.na(x)==F & x>0){
      x <- log(x,10)
    }else{
      x <- 0
    }
    
    data4[i, j] <- as.numeric(x)
  }
}

ori_data4 <- data4 
nor_data4 <- Data_Normalization(data4, "Auto")  
data4 <- nor_data4   


all_colors <- c("#abcae9", "#e2a89e", "#dcbc57", "#c2cc85", "#dcb6cb",
                "#c37b82", "#9cc28b", "#dec9a9",  "#b9d9dc",   "#a392bd",
                "#d69086", "#2885c7", "#f6db9b", "#a4b5a2",  "#808080",
                "#aca9d7", "#dba0cc")

# od_colors <- c("#d55b60", "#5d95b6")


tsne_out2 <- Rtsne(data4, 
                  dims = 3, 
                  perplexity = 30,  
                  exaggeration_factor = 12,
                  eta = 300,
                  theta = 0.8
)  

res2 <- as.data.frame(tsne_out2$Y)
res2$Group = groups
# res2$Type = factor(type, levels=(c("D"))) 

write.table(res2, "Fig.2a-tSNE.txt",sep = "\t", row.names = F, quote = F)


# P012 <- ggplot(res2, aes(x = V1, y = V2, fill = Group)) +   
#   geom_point(size = 8, alpha= 0.8, shape = 21) + 
#   labs(title = "t-SNE",x = "TSNE1", y = "TSNE2") + 
#   theme(plot.title = element_text(hjust = 0.5)) + 
#   scale_fill_manual(name = NULL, 
#                     values = all_colors,
#                     #labels = c("Nonfailing heart", "Failing heart")
#                     )  + 
#   scale_shape_manual(name = "Tpye",
#                      values = c(16:18))  + 
#   theme_bw() 
# 
# 
# pdf("Fig.2a-tSNE.pdf", width = 12, height = 7.5)
# P012
# dev.off()


# save.image("Fig.2a-tSNE.RData")


