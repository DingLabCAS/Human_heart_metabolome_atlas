library(openxlsx)
library(ggplot2)
library(plyr)
library(gridExtra)
library(grid)
library(Cairo)
library(svglite)
library(ComplexUpset)
library(ggupset)
library(dplyr)
library(ggbreak)

setwd("./")

sratio <- 0.5
minsize <- 8

data <- read.xlsx('Upset_data.xlsx', check.names = F, colNames = T) 
colnames(data)[1:14] <- data[5, 1:14]
data <- data[-c(1:5), -c(2:4, 6:14)]
cmpd_ids <- data[, c(1:2)]
colnames(cmpd_ids) <- c("ID" , "Annotation")
rownames(data) <- data[,1]
data <- data[, -c(1:2)]

tissue_order <- c("LV", "IVS", "RV", "LA", "AVS", "RA", "AV", "MV", "PV", "TV", "RCA", "LCA", "AA", "PA", "SVC", "IVC", "PVN") 
tissue_patt <- paste("*", tissue_order, collapse="_*|", sep="_")
tissue_patt <- paste0(tissue_patt, "_*")

keep_index <- grep(tissue_patt, colnames(data))
data <- data[, keep_index]
samples <- colnames(data)[grepl("D_|O_", colnames(data))]
tissues <- gsub("[DO]*_([a-zA-Z]*).*", "\\1", colnames(data))
all_tissues <- unique(tissues)
gnum <- length(all_tissues)

data_stat <- data[, which(!duplicated(tissues))]
colnames(data_stat) <- gsub("[DO]*_([a-zA-Z]*).*", "\\1", colnames(data_stat))

for(g in 1:gnum){
  
  cat(paste0(g, " / ", gnum, "\r"))
  gname <- all_tissues[g]
  gdata <- data[, tissues %in% gname]
  
  cnum <- dim(gdata)[1] 
  snum <- dim(gdata)[2] 
  
  for(c in 1:cnum){
    cdata <- gdata[c, ] 
    bsum <- sum(colSums(cdata != 0))
    bvalue <- ifelse(bsum > snum*sratio, 1, 0)
    data_stat[c, gname] <- as.numeric(bvalue)
  }
  
}

data_stat2 <- data_stat
data_stat2[, "ID"] <- rownames(data)
data_stat3 <-  as.data.frame(matrix(0, dim(data_stat)[1], dim(data_stat)[2]))

for(x in 1:dim(data_stat)[1]){
  for(y in 1:dim(data_stat)[2]){
    data_stat3[x, y] <- as.numeric(data_stat[x,y])
  }
}

colnames(data_stat3)  <- colnames(data_stat) 
rownames(data_stat3)  <- rownames(data_stat)
data_stat3 <- data_stat3[which(rowSums(data_stat3) > 0), ]

for(r in 1:nrow(data_stat)){
  data_stat[r, ] <- as.numeric(data_stat[r, ])
  rsum <- sum(as.numeric(data_stat[r, all_tissues]))
  data_stat2[r, "Num"] <- as.numeric(rsum)
}

data_stat2 <- left_join(cmpd_ids, data_stat2, by = "ID")

orders <- c("PVN", "IVC", "SVC", "PA", "AA", "LCA", "RCA", "TV", "PV", "MV", "AV", "RA", "AVS", "LA", "RV", "IVS", "LV")
all_colors <- c("#e78d8f", "#cb312c", "#d85c60", "#94bfd2", "#366a94", "#5d95b6", "#aaaa53", "#a5c58a", "#38833c", "#64a096", "#c99997", "#d08666", "#edae74", "#866fa7", "#b9a4c5", "#b97ab1", "#dac37f") 
all_colors <- rev(all_colors)

p3 <- ComplexUpset::upset(data_stat3, 
                          orders,
                          min_size = minsize, 
                          name = NULL,
                          height_ratio = 2.5,  
                          width_ratio = 0.5,
                          queries=list(

                            upset_query(set=orders[1], fill=all_colors[1]),
                            upset_query(set=orders[2], fill=all_colors[2]),
                            upset_query(set=orders[3], fill=all_colors[3]),
                            upset_query(set=orders[4], fill=all_colors[4]),
                            upset_query(set=orders[5], fill=all_colors[5]),
                            upset_query(set=orders[6], fill=all_colors[6]),
                            upset_query(set=orders[7], fill=all_colors[7]),
                            upset_query(set=orders[8], fill=all_colors[8]),
                            upset_query(set=orders[9], fill=all_colors[9]),
                            upset_query(set=orders[10], fill=all_colors[10]),
                            upset_query(set=orders[11], fill=all_colors[11]),
                            upset_query(set=orders[12], fill=all_colors[12]),
                            upset_query(set=orders[13], fill=all_colors[13]),
                            upset_query(set=orders[14], fill=all_colors[14]),
                            upset_query(set=orders[15], fill=all_colors[15]),
                            upset_query(set=orders[16], fill=all_colors[16]),
                            upset_query(set=orders[17], fill=all_colors[17])
                            
                          ),
                          
                          base_annotations=list(
                            'Intersection size'=(
                              intersection_size(
                                bar_number_threshold=5,
                                width=0.6, 
                                counts=T,
                              )
                              
                              + scale_y_continuous(expand=expansion(mult=c(0, 0.1))) 
                              + theme(
                                panel.grid.major=element_blank(),
                                panel.grid.minor=element_blank(),
                                axis.line=element_line(colour='black'),
                                axis.ticks.y=element_line(colour = "black", linewidth= 0.5),
                                axis.text=element_text(size=10) 
                                ,axis.line.y.right = element_blank()
                                ,axis.ticks.y.right = element_blank()
                                ,axis.text.y.right = element_blank()
                              )
                              
                            )
                          ),
                          
                          stripes=upset_stripes(
                            geom=geom_segment(size=7),  
                            colors=c('grey95', 'white')
                          ),
                          
                          matrix=intersection_matrix(
                            geom=geom_point(
                              shape='circle filled',
                              size=5, 
                              stroke= NA,
                            )
                          ),
                          
                          set_sizes=(
                            upset_set_size(geom=geom_bar(width=0.6)) 
                            + theme(
                              axis.line.x=element_line(colour='black'),
                              axis.ticks.x=element_line(),
                              axis.text=element_text(size=10, colour="black") 
                            )
                            + geom_text(aes(label=..count..), hjust=1.1, stat='count', size=5)  
                            + expand_limits(y=2350)   
                            
                          ),
                          
                          sort_sets=FALSE, 
                          sort_intersections='descending'
)

ggsave(filename = "Fig.1d.svg",
       width = 9,
       height = 6,
       units = "in",
       dpi = 600
)
p3
dev.off()




=======
library(openxlsx)
library(ggplot2)
library(plyr)
library(gridExtra)
library(grid)
library(Cairo)
library(svglite)
library(ComplexUpset)
library(ggupset)
library(dplyr)
library(ggbreak)

setwd("./")

sratio <- 0.5
minsize <- 8

data <- read.xlsx('Upset_data.xlsx', check.names = F, colNames = T) 
colnames(data)[1:14] <- data[5, 1:14]
data <- data[-c(1:5), -c(2:4, 6:14)]
cmpd_ids <- data[, c(1:2)]
colnames(cmpd_ids) <- c("ID" , "Annotation")
rownames(data) <- data[,1]
data <- data[, -c(1:2)]

tissue_order <- c("LV", "IVS", "RV", "LA", "AVS", "RA", "AV", "MV", "PV", "TV", "RCA", "LCA", "AA", "PA", "SVC", "IVC", "PVN") 
tissue_patt <- paste("*", tissue_order, collapse="_*|", sep="_")
tissue_patt <- paste0(tissue_patt, "_*")

keep_index <- grep(tissue_patt, colnames(data))
data <- data[, keep_index]
samples <- colnames(data)[grepl("D_|O_", colnames(data))]
tissues <- gsub("[DO]*_([a-zA-Z]*).*", "\\1", colnames(data))
all_tissues <- unique(tissues)
gnum <- length(all_tissues)

data_stat <- data[, which(!duplicated(tissues))]
colnames(data_stat) <- gsub("[DO]*_([a-zA-Z]*).*", "\\1", colnames(data_stat))

for(g in 1:gnum){
  
  cat(paste0(g, " / ", gnum, "\r"))
  gname <- all_tissues[g]
  gdata <- data[, tissues %in% gname]
  
  cnum <- dim(gdata)[1] 
  snum <- dim(gdata)[2] 
  
  for(c in 1:cnum){
    cdata <- gdata[c, ] 
    bsum <- sum(colSums(cdata != 0))
    bvalue <- ifelse(bsum > snum*sratio, 1, 0)
    data_stat[c, gname] <- as.numeric(bvalue)
  }
  
}

data_stat2 <- data_stat
data_stat2[, "ID"] <- rownames(data)
data_stat3 <-  as.data.frame(matrix(0, dim(data_stat)[1], dim(data_stat)[2]))

for(x in 1:dim(data_stat)[1]){
  for(y in 1:dim(data_stat)[2]){
    data_stat3[x, y] <- as.numeric(data_stat[x,y])
  }
}

colnames(data_stat3)  <- colnames(data_stat) 
rownames(data_stat3)  <- rownames(data_stat)
data_stat3 <- data_stat3[which(rowSums(data_stat3) > 0), ]

for(r in 1:nrow(data_stat)){
  data_stat[r, ] <- as.numeric(data_stat[r, ])
  rsum <- sum(as.numeric(data_stat[r, all_tissues]))
  data_stat2[r, "Num"] <- as.numeric(rsum)
}

data_stat2 <- left_join(cmpd_ids, data_stat2, by = "ID")

orders <- c("PVN", "IVC", "SVC", "PA", "AA", "LCA", "RCA", "TV", "PV", "MV", "AV", "RA", "AVS", "LA", "RV", "IVS", "LV")
all_colors <- c("#e78d8f", "#cb312c", "#d85c60", "#94bfd2", "#366a94", "#5d95b6", "#aaaa53", "#a5c58a", "#38833c", "#64a096", "#c99997", "#d08666", "#edae74", "#866fa7", "#b9a4c5", "#b97ab1", "#dac37f") 
all_colors <- rev(all_colors)

p3 <- ComplexUpset::upset(data_stat3, 
                          orders,
                          min_size = minsize, 
                          name = NULL,
                          height_ratio = 2.5,  
                          width_ratio = 0.5,
                          queries=list(

                            upset_query(set=orders[1], fill=all_colors[1]),
                            upset_query(set=orders[2], fill=all_colors[2]),
                            upset_query(set=orders[3], fill=all_colors[3]),
                            upset_query(set=orders[4], fill=all_colors[4]),
                            upset_query(set=orders[5], fill=all_colors[5]),
                            upset_query(set=orders[6], fill=all_colors[6]),
                            upset_query(set=orders[7], fill=all_colors[7]),
                            upset_query(set=orders[8], fill=all_colors[8]),
                            upset_query(set=orders[9], fill=all_colors[9]),
                            upset_query(set=orders[10], fill=all_colors[10]),
                            upset_query(set=orders[11], fill=all_colors[11]),
                            upset_query(set=orders[12], fill=all_colors[12]),
                            upset_query(set=orders[13], fill=all_colors[13]),
                            upset_query(set=orders[14], fill=all_colors[14]),
                            upset_query(set=orders[15], fill=all_colors[15]),
                            upset_query(set=orders[16], fill=all_colors[16]),
                            upset_query(set=orders[17], fill=all_colors[17])
                            
                          ),
                          
                          base_annotations=list(
                            'Intersection size'=(
                              intersection_size(
                                bar_number_threshold=5,
                                width=0.6, 
                                counts=T,
                              )
                              
                              + scale_y_continuous(expand=expansion(mult=c(0, 0.1))) 
                              + theme(
                                panel.grid.major=element_blank(),
                                panel.grid.minor=element_blank(),
                                axis.line=element_line(colour='black'),
                                axis.ticks.y=element_line(colour = "black", linewidth= 0.5),
                                axis.text=element_text(size=10) 
                                ,axis.line.y.right = element_blank()
                                ,axis.ticks.y.right = element_blank()
                                ,axis.text.y.right = element_blank()
                              )
                              
                            )
                          ),
                          
                          stripes=upset_stripes(
                            geom=geom_segment(size=7),  
                            colors=c('grey95', 'white')
                          ),
                          
                          matrix=intersection_matrix(
                            geom=geom_point(
                              shape='circle filled',
                              size=5, 
                              stroke= NA,
                            )
                          ),
                          
                          set_sizes=(
                            upset_set_size(geom=geom_bar(width=0.6)) 
                            + theme(
                              axis.line.x=element_line(colour='black'),
                              axis.ticks.x=element_line(),
                              axis.text=element_text(size=10, colour="black") 
                            )
                            + geom_text(aes(label=..count..), hjust=1.1, stat='count', size=5)  
                            + expand_limits(y=2350)   
                            
                          ),
                          
                          sort_sets=FALSE, 
                          sort_intersections='descending'
)

ggsave(filename = "Fig.1d.svg",
       width = 9,
       height = 6,
       units = "in",
       dpi = 600
)
p3
dev.off()
