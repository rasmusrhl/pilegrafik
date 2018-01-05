# MAIN SCRIPT -------------------------------------------------------------

start_time <- Sys.time()


library(knitr)
library(rmarkdown)
library(shape)
library(dplyr)
library(reshape2 )
library(ggplot2)
library(scales)



DBI::dbListTables(con)


home_library         <- "C:/Users/rahela/Desktop/pilegrafik-master/pilegrafik-master/"               # bibliotek hvor scriptet afvikles fra
measurement_year     <- "data/2015"                                  # bibliotek med data 
disease_text         <- "c:/Users/rahela/Desktop/pilegrafik-master/pilegrafik-master/disease text/"
 

legend_english      <-  c("2001-2005 to 2006-2010"               , "2006-2010 to 2011-2015") 
legend_danish       <-  c("2001-2005 til 2006-2010"              , "2006-2010 til 2011-2015") 
legend_finnish      <-  c("Vuosista 2001-2005 vuosiin 2006-2010" , "Vuosista 2006-2010 vuosiin 2011-2015") 
legend_icelandic    <-  c("2001-2005 til 2006-2010"              , "2006-2010 til 2011-2015")
legend_norwegian    <-  c("2001-2005 til 2006-2010"              , "2006-2010 til 2011-2015") 
legend_swedish      <-  c("2001-2005 till 2006-2010"             , "2006-2010 till 2011-2015")
 



setwd( home_library )

unlink( x = "ny mappe", recursive = TRUE, force = TRUE) # slet evt. tidligere output filer.

unlink( x = "output/danish_files", recursive = TRUE, force = TRUE) # slet evt. tidligere output filer.
unlink( x = "output/english_files", recursive = TRUE, force = TRUE) # slet evt. tidligere output filer.
unlink( x = "output/swedish_files", recursive = TRUE, force = TRUE) # slet evt. tidligere output filer.
unlink( x = "output/norwegian_files", recursive = TRUE, force = TRUE) # slet evt. tidligere output filer.
unlink( x = "output/finnish_files", recursive = TRUE, force = TRUE) # slet evt. tidligere output filer.
unlink( x = "output/icelandic_files", recursive = TRUE, force = TRUE) # slet evt. tidligere output filer.

# english filenames and rstudio filenames are combined (so that I can rename the rstudio filenames to the english filenames, without relying on creationtime which goes wrong)

source( file = "english filenames.R")

# rstudio plots output 

rstudio_output <- c(paste0(  "male1year",  "-",  1:56, ".png" ) ,
                    paste0(  "female1year",  "-",  1:56, ".png" ), 
                    paste0(  "male5year",  "-",  1:56, ".png" ) ,
                    paste0(  "female5year",  "-",  1:56, ".png" ) ) 

rename_dataframe <- data.frame( english_filenames, rstudio_output, stringsAsFactors = FALSE ) 



# English -----------------------------------------------------------------



# english
setwd( home_library )
sprog_kode <- 1  # english
rmarkdown::render( input = "pilegrafik_6_sprog.Rmd", "word_document", output_file = "output/english.docx", clean = FALSE   ) 


setwd( paste0(home_library,  "output/english_files/figure-docx/") )


file.rename( rename_dataframe$rstudio_output ,  rename_dataframe$english_filenames ) 

gc()


# Danish ------------------------------------------------------------------


setwd( home_library)
sprog_kode <- 2  # danish
rmarkdown::render( input = "pilegrafik_6_sprog.Rmd", "word_document", output_file = "output/danish.docx",  clean = FALSE    )


setwd( paste0(home_library,  "output/danish_files/figure-docx/") )

file.rename( rename_dataframe$rstudio_output ,  rename_dataframe$english_filenames ) 


gc()



# Finnish -----------------------------------------------------------------



setwd( home_library)
sprog_kode <- 3  # finnish
rmarkdown::render( input = "pilegrafik_6_sprog.Rmd", "word_document", output_file = "output/finnish.docx",  clean = FALSE   )

setwd( paste0(home_library,  "output/finnish_files/figure-docx/") )

file.rename( rename_dataframe$rstudio_output ,  rename_dataframe$english_filenames ) 

gc()



# Icelandic ---------------------------------------------------------------


setwd( home_library)
sprog_kode <- 4  # icelandic
rmarkdown::render( input = "pilegrafik_6_sprog.Rmd", "word_document", output_file = "output/icelandic.docx",  clean = FALSE )


setwd( paste0(home_library,  "output/icelandic_files/figure-docx/") )


file.rename( rename_dataframe$rstudio_output ,  rename_dataframe$english_filenames ) 

gc()



# Norwegian ---------------------------------------------------------------




setwd( home_library)
sprog_kode <- 5  # norwegian
rmarkdown::render( input = "pilegrafik_6_sprog.Rmd", "word_document", output_file = "output/norwegian.docx",  clean = FALSE ) 



setwd( paste0(home_library,  "output/norwegian_files/figure-docx/") )

file.rename( rename_dataframe$rstudio_output ,  rename_dataframe$english_filenames ) 

gc()




# Swedish -----------------------------------------------------------------



setwd( home_library)

sprog_kode <- 6  # swedish
rmarkdown::render( input = "pilegrafik_6_sprog.Rmd", "word_document", output_file = "output/swedish.docx",  clean = FALSE ) 


setwd( paste0(home_library,  "output/swedish_files/figure-docx/") )




file.rename( rename_dataframe$rstudio_output ,  rename_dataframe$english_filenames ) 


gc()




# Copy to output folder ---------------------------------------




setwd( home_library)

unlink( x = "plot output", recursive = TRUE, force = TRUE) # slet evt. tidligere output filer.

dir.create( "plot output" )
setwd( paste0( home_library, "plot output/"))

dir.create( "swedish" )
dir.create( "danish" )
dir.create( "english" )
dir.create( "finnish" )
dir.create( "norwegian" )
dir.create( "icelandic" )



setwd( home_library)
file.copy( from = list.files("output/swedish_files/figure-docx/", full.names = TRUE), to = "plot output/swedish/" )
file.copy( from = list.files("output/danish_files/figure-docx/",  full.names = TRUE), to = "plot output/danish/")
file.copy( from = list.files("output/english_files/figure-docx/", full.names = TRUE), to = "plot output/english/")
file.copy( from = list.files("output/finnish_files/figure-docx/", full.names = TRUE), to = "plot output/finnish/")
file.copy( from = list.files("output/norwegian_files/figure-docx/", full.names = TRUE), to = "plot output/norwegian/")
file.copy( from = list.files("output/icelandic_files/figure-docx/", full.names = TRUE), to = "plot output/icelandic/")





# Copy/rename to Jacques folder ---------------------------------------


# copy


setwd( home_library)

unlink( x = "Jacques plots", recursive = TRUE, force = TRUE) # slet evt. tidligere output filer.

dir.create( "Jacques plots" )
setwd( paste0( home_library, "Jacques plots/") ) 

dir.create( "swedish" )
dir.create( "danish" )
dir.create( "english" )
dir.create( "finnish" )
dir.create( "norwegian" )
dir.create( "icelandic" )



setwd( home_library )

file.copy( from = list.files("output/swedish_files/figure-docx/", full.names = TRUE), to = "Jacques plots/swedish" )
file.copy( from = list.files("output/danish_files/figure-docx/",  full.names = TRUE), to = "Jacques plots/danish")
file.copy( from = list.files("output/english_files/figure-docx/", full.names = TRUE), to = "Jacques plots/english")
file.copy( from = list.files("output/finnish_files/figure-docx/", full.names = TRUE), to = "Jacques plots/finnish")
file.copy( from = list.files("output/norwegian_files/figure-docx/", full.names = TRUE), to = "Jacques plots/norwegian")
file.copy( from = list.files("output/icelandic_files/figure-docx/", full.names = TRUE), to = "Jacques plots/icelandic")




setwd( home_library)

source(  file = "Jacques plotnames.R") 


# danish
setwd(  paste0( home_library, "Jacques plots/danish/") )

file.rename(  from = jacques_names$long_png,  to =  jacques_names$danish_name_jac) 



# english

setwd(  paste0( home_library, "Jacques plots/english/") )


file.rename(  from = jacques_names$long_png,  to =  jacques_names$english_name_jac) 





# finnish
setwd(  paste0( home_library, "Jacques plots/finnish/") )

file.rename(  from = jacques_names$long_png,  to =  jacques_names$finnish_name_jac ) 


# Icelandic

setwd(  paste0( home_library, "Jacques plots/icelandic/") )

file.rename(  from = jacques_names$long_png,  to =  jacques_names$icelandic_name_jac) 



# Norwegian

setwd(  paste0( home_library, "Jacques plots/norwegian/") )
file.rename(  from = jacques_names$long_png,  to =  jacques_names$norwegian_name_jac  ) 


# Swedish

setwd(  paste0( home_library, "Jacques plots/swedish/") )
file.rename(  from = jacques_names$long_png,  to =  jacques_names$swedish_name_jac) 






# delete 888 plots --------------------------------------------------------

# The plots called 888 (bassocellular carcinomas) are deleted.

setwd(  paste0( home_library, "Jacques plots") )

plots_888 <- list.files( pattern = "888", recursive = TRUE ) 


plots_888_full_path <- paste0( home_library, "/Jacques plots", "/",  plots_888 ) 


file.remove(  plots_888_full_path ) 




stop_time <- Sys.time()

compute_time <- stop_time - start_time
compute_time



# Beskrivelse af scriptet:
# dette skript kører skriptet "pilegrafik 6 sprog", seks gange, én gang for hvert sprog.
# "pilegrafik 6 sprog" kører også andre skripts. 


# Af historiske grund så laver dette skript et 5 word dokumenter et for hvert af sprogene
# Svensk, Dansk, Engelsk, Finsk, Norsk og Islandsk. Til hvert document hører der en mappe 
# som indeholder png-filerne til hvert dokument. Det er disse filer vi er interesserede i.
#   Knitr døber filerne som male1year-1, male1year-2 etc. Dette skript omdøber filerne til engelske
# navne. 
#   Derefter kopierer den filerne til en anden mappe som hedder plot output, som indeholder en e
# mappe for hvert sprog. 
#   Derefter kopieres filerne til en mappe som hedder jacques plots, hvorefter disse filer omdøbes
# til et navnesystem som Jacques i frankrig har ønsket. 

# Filerne omdøbes ved at have en liste med de navne som knitr laver, sammen med en liste over de nye navne, 
# og så file.rename( from =, to). Oprindeligt havde jeg forsøgt at anvende filens oprettelsesdato, via file.info og ctime
# men ctime (creation time) blev forkert for filer der tog længere tid at lave. så hvis fil nummer 10 lang tid at lave, så kom 
# den bagefter i køen så at sige. 


# Til sidst i scriptet bliver filer som har en endelse på 888 slettet. De laves af scriptet, automatisk, og det er lettere at slette dem
# til sidst, end at ændre i scriptet så de slet ikke bliver lavet. 



