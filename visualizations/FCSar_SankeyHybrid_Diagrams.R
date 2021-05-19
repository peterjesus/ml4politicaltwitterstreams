
library(circlize)
library(RColorBrewer)
library(stringr)


extended_brewer.pal=colorRampPalette(c("khaki1","sienna1","salmon","lightpink","hotpink","purple","lightslateblue","skyblue","blue","royalblue4"))

#for (i in seq(487,4870,487)){
for (i in seq(5600,252000,5600)){
  if (i == 252000){i=250147}
  path_file=paste("./Spain_FuzzyCSAR/spain_rules",as.character(i),"_Matrix.csv", sep="") 
  #path_file=paste("./USA_FuzzyCSAR/usa_rules",as.character(i),"_Matrix.csv", sep="") 
  mat <- read.csv(file = path_file,header=T, row.names=1)
  mat <- as.matrix(mat)
  #grid.col = c(negative = "red", positive = "green", neither = "gold",setNames(extended_brewer.pal(n=length(unlist(rownames(mat)))), rownames(mat)))
  grid.col = c(negative = "red", positive = "green", neutral = "gold",setNames(extended_brewer.pal(n=length(unlist(rownames(mat)))), rownames(mat)))
  circos.clear()
  circos.par(start.degree=-100)
  #circos.par(gap.degree=c(rep(2, nrow(mat)-1), 30, rep(2, ncol(mat)-1), 30))
  #png_path=paste(sprintf("./USA_FuzzyCSar/SankeyHybrid/sankey%04d", i),".png", sep="") 
  png_path=paste(sprintf("./Spain_FuzzyCSAR/SankeyHybrid/sankey%06d", i),".png", sep="") 
  #png(png_path, units="in", width=12, height=12, res=200)
  png(png_path, units="in", width=17, height=17, res=200)
  chordDiagram(mat,annotationTrack="grid",preAllocateTracks=list(track.height=0.2),grid.col = grid.col)
  circos.trackPlotRegion(track.index = 1, panel.fun =function(x, y) {
    xlim =get.cell.meta.data("xlim")
    ylim =get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    print(sector.name)
    #if (sector.name %in% c("neither","positive","negative") ){
    if (sector.name %in% c("neutral","positive","negative") ){
      sector.name = toupper(sector.name)
      }
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj =c(0, 0),cex = 0.8)
  }, bg.border = NA)
  title(substitute(paste(italic("FuzzyCSar "), "SPAIN: ",italic("Sankey-Hybrid Diagram. "), nn, " datos analizados", sep=" "),list(nn=as.character(i))),line=0,cex.main=1.5)
  #title(substitute(paste(italic("FuzzyCSar "), "USA: ",italic("Sankey-Hybrid Diagram. "), nn, " datos analizados", sep=" "),list(nn=as.character(i))),line=-0.0,cex.main=1.5)
  dev.off()
}

