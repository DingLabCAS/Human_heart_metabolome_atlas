suppressMessages(library(circlize))
suppressMessages(library(reshape2))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))

rm(list=ls())
setwd("./")

trans1 <- "00"
trans2 <- 0.5 
track_height <- 0.05

tissues <- c("LV", "IVS", "RV", "LA", "AVS", "RA", "AV", "MV", "PV", "TV", "LCA", "RCA", "AA", "PA")
tissue_colors <- c("#e78d8f", "#cb302c", "#d85c60", "#94bfd2", "#366a94", "#5d95b6", "#c2c25f", "#a2bd90", "#3d9041", "#6baca1", "#d08666", "#c99997", "#edaf74", "#866fa7")
meta_class <-  c("Benzenoids", "Lipids and lipid-like molecules", "Nucleosides, nucleotides, and analogues", "Organic acids and derivatives", "Organic nitrogen compounds", "Organic oxygen compounds", "Organoheterocyclic compounds", "Others")
all_paths <- data.frame(class = meta_class)
meta_colors  <-  c("#d08666", "#de7477", "#dac37f", "#5d95b6", "#c99997", "#97b37f", "#866fa7", "#2f7233")
meta_cc <- data.frame(class = meta_class, color = meta_colors)
meta_cc <- dplyr::full_join(all_paths, meta_cc, by = "class")

colnames(meta_cc) <- c("SUPERPATHWAY", "Class_color")
meta_cc1 <- meta_cc
colnames(meta_cc1) <- c("SUPERPATHWAY_1", "Class_color_1")
meta_cc2 <- meta_cc
colnames(meta_cc2) <- c("SUPERPATHWAY_2", "Class_color_2")

bed_all <-  read.table("Failing.heart-all.bed.txt", header = T)
pos_bed_uniq <- read.table("Failing.heart-all.pos.txt", header = T, colClasses=c("character", "numeric", "numeric", "character", "character","character", "numeric"))


circos.clear()  
out_file <- "Failing.heart.circ.pdf"
pdf(out_file, width = 12, height = 9)   
circos.par(
  cell.padding =  c(0, 0.02, 0, 0.02),    
  start.degree = 90,      
  track.height = track_height,
  gap.degree = 0,          
  points.overflow.warning = FALSE,  
  gap.after = pos_bed_uniq$gap)          
circos.initialize(pos_bed_uniq$chr, xlim = pos_bed_uniq[, 2:3])
circos.track(track.index = 1, ylim = c(0, 1), bg.border = NA, panel.fun = function(x, y) {
  circos.text(x, y, unique(pos_bed_uniq$group), cex = 2)
})
if(length(unique(pos_bed_uniq$group)) == length(tissues)){
  tissues <- tissues
}else{
  tissues <- tissues[tissues %in% unique(pos_bed_uniq$group)]
}
for(t in 1:length(tissues)){
  highlight.sector(pos_bed_uniq[pos_bed_uniq$group==tissues[t], "chr"], track.index = 1, col = tissue_colors[t],
                   text = tissues[t], cex = 2, text.col = "black",  font = par("font"),  border = NA, padding = c(0, 0, 0, 0), 
                   facing = "bending.inside", niceFacing = TRUE, text.vjust = "8mm")
}
for(i in seq_len(nrow(bed_all))) {
  circos.link(bed_all[i,1], c(bed_all[i,2], bed_all[i,3]), rou1 = 0.908,  
              bed_all[i,4], c(bed_all[i,5], bed_all[i,6]), rou2 = 0.908,
              col = bed_all[i,7], lwd = 1e-4)
}
set_track_gap(mm_h(2))
circos.genomicTrackPlotRegion(
  pos_bed_uniq[, c(1:3)], 
  track.index = 2,
  ylim=c(0, 1), 
  bg.col = pos_bed_uniq[, "color"], 
  track.height = track_height,
  bg.border = NA)
legend("topright", title="Metabolite Class", legend = meta_cc$SUPERPATHWAY, pch = 15, inset=c(0, -0.007),  
       col = meta_cc$Class_color, cex = 0.8, border=NULL, ncol = 1, text.font = 2, box.lty = 0, box.col=NULL) 
title("Failing heart")
dev.off()
circos.clear() 
