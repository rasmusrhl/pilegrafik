








#### pilefunktion med 4 lande ( altså fx uden Island )


pilefunktion <- function(df, sygdom, sprog_kode ) {
  
# legend_english      <-  c("2000-2004 to 2005-2009"               , "2005-2009 to 2010-2014") 
# legend_danish       <-  c("2000-2004 til 2005-2009"              , "2005-2009 til 2010-2014") 
# legend_finnish      <-  c("Vuosista 2000-2004 vuosiin 2005-2009" , "Vuosista 2005-2009 vuosiin 2010-2014") 
# legend_icelandic    <-  c("2000-2004 til 2005-2009"              , "2005-2009 til 2010-2014")
# legend_norwegian    <-  c("2000-2004 til 2005-2009"              , "2005-2009 til 2010-2014") 
# legend_swedish      <-  c("2000-2004 till 2005-2009"             , "2005-2009 till 2010-2014")



if ( sprog_kode == 1) {
  
legend_switch <- legend_english   

# levels( df$variable)[ levels(df$variable) == "Danmark"]  <-  "Denmark"
# levels( df$variable)[ levels(df$variable) == "Norge"]  <-  "Norway"
# levels( df$variable)[ levels(df$variable) == "Sverige"]  <-  "Sweden"

df$variable[ df$variable == "Danmark"]  <-  "Denmark"
df$variable[ df$variable == "Norge"]    <-  "Norway"
df$variable[ df$variable == "Sverige"]  <-  "Sweden"
df$variable[ df$variable == "Finland"]  <-  "Finland"

  } else if(sprog_kode == 2 ){
    
legend_switch <- legend_danish
df$variable[ df$variable == "Danmark"]  <- "Danmark" 
df$variable[ df$variable == "Norge"]    <- "Norge"
df$variable[ df$variable == "Sverige"]  <- "Sverige"
df$variable[ df$variable == "Finland"]  <- "Finland"


} else if(sprog_kode == 3 ){

legend_switch <- legend_finnish
df$variable[ df$variable == "Danmark"]  <- "Tanska"
df$variable[ df$variable == "Norge"]    <- "Norja" 
df$variable[ df$variable == "Sverige"]  <- "Ruotsi" 
df$variable[ df$variable == "Finland"]  <- "Suomi" 


} else if(sprog_kode == 4 ){
  
legend_switch <- legend_icelandic
df$variable[ df$variable == "Danmark"]  <- "Danmörk"  
df$variable[ df$variable == "Norge"]    <- "Noregur"
df$variable[ df$variable == "Sverige"]  <- "Svíþjóð"
df$variable[ df$variable == "Finland"]  <- "Finnland"



} else if(sprog_kode == 5 ){

legend_switch <- legend_norwegian
df$variable[ df$variable == "Danmark"]  <- "Danmark" 
df$variable[ df$variable == "Norge"]    <- "Norge" 
df$variable[ df$variable == "Sverige"]  <- "Sverige" 
df$variable[ df$variable == "Finland"]  <- "Finland" 
  
  
} else if(sprog_kode == 6 ){
  
legend_switch <- legend_swedish
df$variable[ df$variable == "Danmark"] 
df$variable[ df$variable == "Norge"]   
df$variable[ df$variable == "Sverige"] 
df$variable[ df$variable == "Finland"] 

}
  
  
 
  
if ( (dim(df)[1]==0  )|  any( is.na( df$value )) ) {
  
ggplot( iris, aes(  x = Sepal.Width, y = Sepal.Length, color = Species) ) + geom_point(size = 30, alpha = 0.1) +
theme_minimal() + 
  annotate("text", x = 2, y = 7, label = paste0( "Plot for:\n", sygdom, "\n-does not exist because data does not exist."), size = 4, hjust = 0)  + 

  theme( legend.position = "none", axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.background = element_blank() )
  
  
} else {
  

  
   
  lc <- cbind(df$value, df$stigning)
maxlc <- max(lc, na.rm = TRUE)
minlc <- min(lc, na.rm = TRUE)
rownames(lc) <- df$variable
# Kun hver anden linje så hvert land kun én gang
lc <- lc[seq(from= 1, to=8, by=2),]

plot(lc[,1], 1:dim(lc)[1], type= "n", xaxt= "n", yaxt="n", ylab="",
     ylim=c(0,dim(lc)[1]+.2),
     xlim=c(minlc-4.,maxlc +4), xlab="", bty="n")
mtext(rownames(lc), side=2, at= 1:dim(lc)[1],  las= 2, line = 3.5, adj = 0 ) 
abline(h=1:dim(lc)[1], lwd=20, col="lightgrey")
#segments(1:dim(lc)[1], lwd=20, col="lightgrey")
abline(v= unique(ceiling( seq(min(lc, na.rm = TRUE)-5, max(lc, na.rm = TRUE)+5)/5 ) * 5), col="white")
mtext(paste( unique(ceiling( seq(min(lc, na.rm = TRUE)-5, max(lc, na.rm = TRUE)+5)/5 ) * 5),"%", sep=""), side=3, at=unique(ceiling( seq(min(lc, na.rm = TRUE)-5, max(lc, na.rm = TRUE)+5)/5 ) * 5))

# Pile mod højre
pil <- lc[,2] - lc[,1] > 0
Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
       arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
       segment=!FALSE, col="skyblue")
# Der skal kun tegnes et linjestykke hvis ændringen er større end pilen
  pil <- lc[,2] - lc[,1] > 0.3
  segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*rightarrow1, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
           ljoin=1, col="skyblue")
  
  # Pile mod venstre
  pil <- lc[,2] - lc[,1] <  0
  Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
         arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
         col="skyblue")
  pil <- lc[,2 ] - lc[,1] < -0.3
  segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*leftarrow1, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
           ljoin=1, col="skyblue")
  
  # Der er ikke sket nogen ændring, derfor ingen pil, men kun tyndt linjestykke
  pil <- lc[,2] - lc[,1] == 0 
  #Arrows(lc[!pil,1], (1:dim(lc)[1])[!pil],  lc[!pil,2], (1:dim(lc)[1])[!pil], lwd=1, lend=2, ljoin=1, 
  #	arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
  #	col="skyblue")
  segments(lc[pil,1]-.05, (1:dim(lc)[1])[pil],  lc[pil,2]+.05, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
           ljoin=1, col="skyblue")
  
  
  
  # De andre farvede pile
  lc <- cbind(df$value, df$stigning)
  rownames(lc) <- df$variable
  # Kun hver anden linje så hvert land kun én gang
  lc <- lc[seq(from=2, to=8, by=2),]
  
  # Pile mod højre
  pil <- lc[,2] - lc[,1] > 0
  pil[  is.na( pil )]    <- FALSE   # De som er missing bliver til FALSE
  Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
         arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
         segment=!FALSE, col="blue")
  # Der skal kun tegnes et linjestykke hvis ændringen er større end pilen
  pil <- lc[,2] - lc[,1] > 0.3
  pil[  is.na( pil )]    <- FALSE   # De som er missing bliver til FALSE
  
  segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*rightarrow2, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
           ljoin=1, col="blue")
  
  # Pile mod venstre
  pil <- lc[,2] - lc[,1] <  0
  pil[  is.na( pil )]    <- FALSE   # De som er missing bliver til FALSE
  Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
         arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
         col="blue")
  pil <- lc[,2] - lc[,1] < -0.3
  pil[  is.na( pil )]    <- FALSE   # De som er missing bliver til FALSE
  segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*leftarrow2, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
           ljoin=1, col="blue")
  
  # Der er ikke sket nogen ændring, derfor ingen pil, men kun tyndt linjestykke
  pil <- lc[,2] - lc[,1] == 0 
  pil[  is.na( pil )]    <- FALSE   # De som er missing bliver til FALSE
  #Arrows(lc[!pil,1], (1:dim(lc)[1])[!pil],  lc[!pil,2], (1:dim(lc)[1])[!pil], lwd=1, lend=2, ljoin=1, 
  #	arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
  #	col="blue")
  segments(lc[pil,1]-.05, (1:dim(lc)[1])[pil],  lc[pil,2]+.05, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
           ljoin=1, col="blue")
  
 
  
  
  title( main =  sygdom , adj = 0,  cex.main=0.7 )
  legend("bottomleft", horiz = TRUE, legend = legend_switch , fill = c("skyblue", "blue"),
         cex = 0.7, bty = "n")
  

    
}


}  # sidste parentes kommer fra en else som stopper plot-funktionen hvis dim(dataframe)[1]==0




#### pilefunktion med 5 lande :
# 
# pilefunktion <- function(df, sygdom, gender, overskrift ) {
#   
#   
#   
#   
#   
#   lc <- cbind(df$value, df$stigning)
# maxlc <- max(lc, na.rm = TRUE)
# minlc <- min(lc, na.rm = TRUE)
# rownames(lc) <- df$variable
# # Kun hver anden linje så hvert land kun én gang
# lc <- lc[seq(from= 1, to=10, by=2),]
# 
# plot(lc[,1], 1:dim(lc)[1], type= "n", xaxt= "n", yaxt="n", ylab="",
#      ylim=c(0,dim(lc)[1]+.2),
#      xlim=c(minlc-4.,maxlc +4), xlab="", bty="n")
# mtext(rownames(lc), side=2, at= 1:dim(lc)[1], adj= 1.2, las= 2)
# abline(h=1:dim(lc)[1], lwd=20, col="lightgrey")
# #segments(1:dim(lc)[1], lwd=20, col="lightgrey")
# abline(v=seq(0, to=max(lc, na.rm = TRUE)+4, by=5), col="white")
# mtext(paste(seq(0, max(lc, na.rm = TRUE)+4, by=5),"%", sep=""), side=3, at=seq(0, max(lc, na.rm = TRUE)+4, by=5))
# 
# # Pile mod højre
# pil <- lc[,2] - lc[,1] > 0
# Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
#        arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#        segment=!FALSE, col="skyblue")
# # Der skal kun tegnes et linjestykke hvis ændringen er større end pilen
#   pil <- lc[,2] - lc[,1] > 0.3
#   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*rightarrow1, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="skyblue")
#   
#   # Pile mod venstre
#   pil <- lc[,2] - lc[,1] <  0
#   Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
#          arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#          col="skyblue")
#   pil <- lc[,2] - lc[,1] < -0.3
#   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*leftarrow1, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="skyblue")
#   
#   # Der er ikke sket nogen ændring, derfor ingen pil, men kun tyndt linjestykke
#   pil <- lc[,2] - lc[,1] == 0 
#   #Arrows(lc[!pil,1], (1:dim(lc)[1])[!pil],  lc[!pil,2], (1:dim(lc)[1])[!pil], lwd=1, lend=2, ljoin=1, 
#   #	arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#   #	col="skyblue")
#   segments(lc[pil,1]-.05, (1:dim(lc)[1])[pil],  lc[pil,2]+.05, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="skyblue")
#   
#   
#   
#   # De andre farvede pile
#   lc <- cbind(df$value, df$stigning)
#   rownames(lc) <- df$variable
#   # Kun hver anden linje så hvert land kun én gang
#   lc <- lc[seq(from=2, to=10, by=2),]
#   
#   # Pile mod højre
#   pil <- lc[,2] - lc[,1] > 0
#   Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
#          arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#          segment=!FALSE, col="blue")
#   # Der skal kun tegnes et linjestykke hvis ændringen er større end pilen
#   pil <- lc[,2] - lc[,1] > 0.3
#   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*rightarrow2, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="blue")
#   
#   # Pile mod venstre
#   pil <- lc[,2] - lc[,1] <  0
#   Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
#          arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#          col="blue")
#   pil <- lc[,2] - lc[,1] < -0.3
#   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*leftarrow2, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="blue")
#   
#   # Der er ikke sket nogen ændring, derfor ingen pil, men kun tyndt linjestykke
#   pil <- lc[,2] - lc[,1] == 0 
#   #Arrows(lc[!pil,1], (1:dim(lc)[1])[!pil],  lc[!pil,2], (1:dim(lc)[1])[!pil], lwd=1, lend=2, ljoin=1, 
#   #	arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#   #	col="blue")
#   segments(lc[pil,1]-.05, (1:dim(lc)[1])[pil],  lc[pil,2]+.05, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="blue")
#   
#  
#   
#   
#   title( main = paste(   overskrift, sygdom, gender, sep = " "), adj = 0 )
#   legend("bottomleft", horiz = TRUE, legend = c("1999-2003 til 2004-2008",
#                                                 "2004-2008 til 2009-2013"), fill = c("skyblue", "blue"),
#          cex = 0.7, bty = "n")
#   
#   
#   
# }














#### backup af gammel kode


# 
# 
# #### pilefunktion med 4 lande ( altså fx uden Island )
# 
# 
# pilefunktion <- function(df, sygdom, english = FALSE) {
#   
#  
# 
# legend_dansk   <-  c("1999-2003 til 2004-2008", "2004-2008 til 2009-2013") 
# legend_english <-  c("1999-2003 to 2004-2008", "2004-2008 to 2009-2013") 
# 
# 
# if (english) {
#   
# legend_switch <- legend_english   
# 
# # levels( df$variable)[ levels(df$variable) == "Danmark"]  <-  "Denmark"
# # levels( df$variable)[ levels(df$variable) == "Norge"]  <-  "Norway"
# # levels( df$variable)[ levels(df$variable) == "Sverige"]  <-  "Sweden"
# 
# df$variable[ df$variable == "Danmark"]  <-  "Denmark"
# df$variable[ df$variable == "Norge"]  <-  "Norway"
# df$variable[ df$variable == "Sverige"]  <-  "Sweden"
# 
#   } else if( !english){
#     
# legend_switch <- legend_dansk
# 
# }
# 
# if ( (dim(df)[1]==0  )|  any( is.na( df$value )) ) {
#   
# ggplot( iris, aes(  x = Sepal.Width, y = Sepal.Length, color = Species) ) + geom_point(size = 30, alpha = 0.1) +
# theme_minimal() + 
#   annotate("text", x = 2, y = 7, label = paste0( "Plot for:\n", sygdom, "\n-findes ikke fordi data ikke findes."), size = 4, hjust = 0)  + 
# 
#   theme( legend.position = "none", axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.background = element_blank() )
#   
#   
# } else {
#   
# 
#   
#    
#   lc <- cbind(df$value, df$stigning)
# maxlc <- max(lc, na.rm = TRUE)
# minlc <- min(lc, na.rm = TRUE)
# rownames(lc) <- df$variable
# # Kun hver anden linje så hvert land kun én gang
# lc <- lc[seq(from= 1, to=8, by=2),]
# 
# plot(lc[,1], 1:dim(lc)[1], type= "n", xaxt= "n", yaxt="n", ylab="",
#      ylim=c(0,dim(lc)[1]+.2),
#      xlim=c(minlc-4.,maxlc +4), xlab="", bty="n")
# mtext(rownames(lc), side=2, at= 1:dim(lc)[1],  las= 2, line = 3.5, adj = 0 ) 
# abline(h=1:dim(lc)[1], lwd=20, col="lightgrey")
# #segments(1:dim(lc)[1], lwd=20, col="lightgrey")
# abline(v= unique(ceiling( seq(min(lc, na.rm = TRUE)-5, max(lc, na.rm = TRUE)+5)/5 ) * 5), col="white")
# mtext(paste( unique(ceiling( seq(min(lc, na.rm = TRUE)-5, max(lc, na.rm = TRUE)+5)/5 ) * 5),"%", sep=""), side=3, at=unique(ceiling( seq(min(lc, na.rm = TRUE)-5, max(lc, na.rm = TRUE)+5)/5 ) * 5))
# 
# # Pile mod højre
# pil <- lc[,2] - lc[,1] > 0
# Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
#        arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#        segment=!FALSE, col="skyblue")
# # Der skal kun tegnes et linjestykke hvis ændringen er større end pilen
#   pil <- lc[,2] - lc[,1] > 0.3
#   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*rightarrow1, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="skyblue")
#   
#   # Pile mod venstre
#   pil <- lc[,2] - lc[,1] <  0
#   Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
#          arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#          col="skyblue")
#   pil <- lc[,2 ] - lc[,1] < -0.3
#   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*leftarrow1, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="skyblue")
#   
#   # Der er ikke sket nogen ændring, derfor ingen pil, men kun tyndt linjestykke
#   pil <- lc[,2] - lc[,1] == 0 
#   #Arrows(lc[!pil,1], (1:dim(lc)[1])[!pil],  lc[!pil,2], (1:dim(lc)[1])[!pil], lwd=1, lend=2, ljoin=1, 
#   #	arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#   #	col="skyblue")
#   segments(lc[pil,1]-.05, (1:dim(lc)[1])[pil],  lc[pil,2]+.05, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="skyblue")
#   
#   
#   
#   # De andre farvede pile
#   lc <- cbind(df$value, df$stigning)
#   rownames(lc) <- df$variable
#   # Kun hver anden linje så hvert land kun én gang
#   lc <- lc[seq(from=2, to=8, by=2),]
#   
#   # Pile mod højre
#   pil <- lc[,2] - lc[,1] > 0
#   Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
#          arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#          segment=!FALSE, col="blue")
#   # Der skal kun tegnes et linjestykke hvis ændringen er større end pilen
#   pil <- lc[,2] - lc[,1] > 0.3
#   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*rightarrow2, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="blue")
#   
#   # Pile mod venstre
#   pil <- lc[,2] - lc[,1] <  0
#   Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
#          arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#          col="blue")
#   pil <- lc[,2] - lc[,1] < -0.3
#   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*leftarrow2, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="blue")
#   
#   # Der er ikke sket nogen ændring, derfor ingen pil, men kun tyndt linjestykke
#   pil <- lc[,2] - lc[,1] == 0 
#   #Arrows(lc[!pil,1], (1:dim(lc)[1])[!pil],  lc[!pil,2], (1:dim(lc)[1])[!pil], lwd=1, lend=2, ljoin=1, 
#   #	arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
#   #	col="blue")
#   segments(lc[pil,1]-.05, (1:dim(lc)[1])[pil],  lc[pil,2]+.05, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
#            ljoin=1, col="blue")
#   
#  
#   
#   
#   title( main =  sygdom , adj = 0,  cex.main=0.9 )
#   legend("bottomleft", horiz = TRUE, legend = legend_switch , fill = c("skyblue", "blue"),
#          cex = 0.7, bty = "n")
#   
# 
#     
# }
# 
# 
# }  # sidste parenthes kommer fra en else som stopper plot-funktionen hvis dim(dataframe)[1]==0
# 
# 
# 
# 
# #### pilefunktion med 5 lande :
# # 
# # pilefunktion <- function(df, sygdom, gender, overskrift ) {
# #   
# #   
# #   
# #   
# #   
# #   lc <- cbind(df$value, df$stigning)
# # maxlc <- max(lc, na.rm = TRUE)
# # minlc <- min(lc, na.rm = TRUE)
# # rownames(lc) <- df$variable
# # # Kun hver anden linje så hvert land kun én gang
# # lc <- lc[seq(from= 1, to=10, by=2),]
# # 
# # plot(lc[,1], 1:dim(lc)[1], type= "n", xaxt= "n", yaxt="n", ylab="",
# #      ylim=c(0,dim(lc)[1]+.2),
# #      xlim=c(minlc-4.,maxlc +4), xlab="", bty="n")
# # mtext(rownames(lc), side=2, at= 1:dim(lc)[1], adj= 1.2, las= 2)
# # abline(h=1:dim(lc)[1], lwd=20, col="lightgrey")
# # #segments(1:dim(lc)[1], lwd=20, col="lightgrey")
# # abline(v=seq(0, to=max(lc, na.rm = TRUE)+4, by=5), col="white")
# # mtext(paste(seq(0, max(lc, na.rm = TRUE)+4, by=5),"%", sep=""), side=3, at=seq(0, max(lc, na.rm = TRUE)+4, by=5))
# # 
# # # Pile mod højre
# # pil <- lc[,2] - lc[,1] > 0
# # Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
# #        arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
# #        segment=!FALSE, col="skyblue")
# # # Der skal kun tegnes et linjestykke hvis ændringen er større end pilen
# #   pil <- lc[,2] - lc[,1] > 0.3
# #   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*rightarrow1, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
# #            ljoin=1, col="skyblue")
# #   
# #   # Pile mod venstre
# #   pil <- lc[,2] - lc[,1] <  0
# #   Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
# #          arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
# #          col="skyblue")
# #   pil <- lc[,2] - lc[,1] < -0.3
# #   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*leftarrow1, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
# #            ljoin=1, col="skyblue")
# #   
# #   # Der er ikke sket nogen ændring, derfor ingen pil, men kun tyndt linjestykke
# #   pil <- lc[,2] - lc[,1] == 0 
# #   #Arrows(lc[!pil,1], (1:dim(lc)[1])[!pil],  lc[!pil,2], (1:dim(lc)[1])[!pil], lwd=1, lend=2, ljoin=1, 
# #   #	arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
# #   #	col="skyblue")
# #   segments(lc[pil,1]-.05, (1:dim(lc)[1])[pil],  lc[pil,2]+.05, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
# #            ljoin=1, col="skyblue")
# #   
# #   
# #   
# #   # De andre farvede pile
# #   lc <- cbind(df$value, df$stigning)
# #   rownames(lc) <- df$variable
# #   # Kun hver anden linje så hvert land kun én gang
# #   lc <- lc[seq(from=2, to=10, by=2),]
# #   
# #   # Pile mod højre
# #   pil <- lc[,2] - lc[,1] > 0
# #   Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
# #          arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
# #          segment=!FALSE, col="blue")
# #   # Der skal kun tegnes et linjestykke hvis ændringen er større end pilen
# #   pil <- lc[,2] - lc[,1] > 0.3
# #   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*rightarrow2, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
# #            ljoin=1, col="blue")
# #   
# #   # Pile mod venstre
# #   pil <- lc[,2] - lc[,1] <  0
# #   Arrows(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2], (1:dim(lc)[1])[pil], lwd=1, lend=2, ljoin=1, 
# #          arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
# #          col="blue")
# #   pil <- lc[,2] - lc[,1] < -0.3
# #   segments(lc[pil,1], (1:dim(lc)[1])[pil],  lc[pil,2]*leftarrow2, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
# #            ljoin=1, col="blue")
# #   
# #   # Der er ikke sket nogen ændring, derfor ingen pil, men kun tyndt linjestykke
# #   pil <- lc[,2] - lc[,1] == 0 
# #   #Arrows(lc[!pil,1], (1:dim(lc)[1])[!pil],  lc[!pil,2], (1:dim(lc)[1])[!pil], lwd=1, lend=2, ljoin=1, 
# #   #	arr.length=.35, arr.width=.6, arr.adj=1, arr.type="triangle", 
# #   #	col="blue")
# #   segments(lc[pil,1]-.05, (1:dim(lc)[1])[pil],  lc[pil,2]+.05, (1:dim(lc)[1])[pil], lwd=20, lend=1, 
# #            ljoin=1, col="blue")
# #   
# #  
# #   
# #   
# #   title( main = paste(   overskrift, sygdom, gender, sep = " "), adj = 0 )
# #   legend("bottomleft", horiz = TRUE, legend = c("1999-2003 til 2004-2008",
# #                                                 "2004-2008 til 2009-2013"), fill = c("skyblue", "blue"),
# #          cex = 0.7, bty = "n")
# #   
# #   
# #   
# # }
# 
# 
# 
# 
