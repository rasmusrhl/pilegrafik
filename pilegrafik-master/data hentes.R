
setwd( paste0( home_library, measurement_year ) ) 


dk <-    read.csv( file = "DK_RS_file.csv", sep = ";", header = TRUE ) 
fi <-    read.csv( file = "FI_rs_file.csv", sep = ";", header = TRUE ) 
no <-    read.csv( file = "NO_rs_file.csv", sep = ";", header = TRUE ) 
se <-    read.csv( file = "SE_rs_file.csv", sep = ";", header = TRUE ) 


dk$variable <- "Danmark"
fi$variable <- "Finland"
no$variable <- "Norge"
se$variable <- "Sverige"



df <- rbind(  dk, fi, no, se  )



df <- df %>% filter(  age.group == 0 ) 


df <- df %>% select( period, variable, rs1yr, rs5yr, sex, entity  )


df$rs1yr <-  as.numeric(  as.character(  df$rs1yr ))
df$rs5yr <-  as.numeric(  as.character(  df$rs5yr ))



# df$period[   df$period=="1999"] <- "1999-2003" 
# df$period[   df$period=="2004"] <- "2004-2008" 
# df$period[   df$period=="2009"] <- "2009-2013" 

# df$period[   df$period=="2000"] <- "2000-2004" 
# df$period[   df$period=="2005"] <- "2005-2009" 
# df$period[   df$period=="2010"] <- "2010-2014" 

df$period[   df$period=="2001"] <- "2001-2005" 
df$period[   df$period=="2006"] <- "2006-2010" 
df$period[   df$period=="2011"] <- "2011-2015" 




# hvad er stigningen i forhold til næste år?
df$stigning_rs1yr <-   dplyr::lead( df$rs1yr, 1) 

df$stigning_rs5yr <-   dplyr::lead( df$rs5yr, 1) 


# 2009 gives NA fordi den ikke har en lead værdi, i stedet er det det andet køn som tages, hvilket jo bliver forkert.

# df$stigning_rs1yr[  df$period=="2009-2013"] <- NA
# df$stigning_rs5yr[  df$period=="2009-2013"] <- NA
# 

df$stigning_rs1yr[  df$period=="2011-2015"] <- NA
df$stigning_rs5yr[  df$period=="2011-2015"] <- NA





# df <- df %>% filter( period %in% c("1999-2003", "2004-2008" )) # den sidste periode er 2009-2013, men den findes i kolonnen stigning, så den 
#                                                                # kommer med alligevel selvom rækken ikke vælges(fordi den findes i samme række).

# df <- df %>% filter( period %in% c("2000-2004", "2005-2009" )) # den sidste periode er 2009-2013, men den findes i kolonnen stigning, så den 
#                                                                # kommer med alligevel selvom rækken ikke vælges(fordi den findes i samme række).

df <- df %>% filter( period %in% c("2001-2005", "2006-2010" )) # den sidste periode er 2009-2013, men den findes i kolonnen stigning, så den 
                                                               # kommer med alligevel selvom rækken ikke vælges(fordi den findes i samme række).





df_1year <- df %>% select( "Periode" = period, variable, "value" = rs1yr, "stigning" = stigning_rs1yr, "sex" = sex, entity ) 

df_5year <- df %>% select( "Periode" = period, variable, "value" = rs5yr, "stigning" = stigning_rs5yr, "sex" = sex, entity ) 

# Så er hoveddataframen, klar, så skal de enkelte frames bare trækkes ud.




df_1year_male     <- df_1year %>% filter(  sex == 1 )
df_1year_female   <- df_1year %>% filter(  sex == 2 )


df_5year_male     <- df_5year %>% filter(  sex == 1 )
df_5year_female   <- df_5year %>% filter(  sex == 2 )

 


# 1 year male -------------------------------------------------------------



Laebe_1year_male_10                                                                <-    df_1year_male        %>%  dplyr::filter(   entity == 10   ) 
Mundhule_1year_male_20                                                             <-    df_1year_male        %>%  dplyr::filter(   entity == 20   )
Spytkirtel_1year_male_30                                                           <-    df_1year_male        %>%  dplyr::filter(   entity == 30   )
Oropharynx_1year_male_40                                                           <-    df_1year_male        %>%  dplyr::filter(   entity == 40   )
Nasopharynx_1year_male_41                                                          <-    df_1year_male        %>%  dplyr::filter(   entity == 41   )
Hypopharynx_1year_male_50                                                          <-    df_1year_male        %>%  dplyr::filter(   entity == 50   )
Pharynx_daarligt_defineret_1year_male_51                                           <-    df_1year_male        %>%  dplyr::filter(   entity == 51   )
Spiseror_1year_male_60                                                             <-    df_1year_male        %>%  dplyr::filter(   entity == 60   )
Mave_1year_male_70                                                                 <-    df_1year_male        %>%  dplyr::filter(   entity == 70   )
Tyndtarm_1year_male_80                                                             <-    df_1year_male        %>%  dplyr::filter(   entity == 80   )
Tyktarm_1year_male_90                                                              <-    df_1year_male        %>%  dplyr::filter(   entity == 90   )
Endetarm_og_anus_1year_male_100                                                    <-    df_1year_male        %>%  dplyr::filter(   entity == 100   )
Lever_1year_male_110                                                               <-    df_1year_male        %>%  dplyr::filter(   entity == 110   )
Galdeblaere_og_galdeveje_1year_male_120                                            <-    df_1year_male        %>%  dplyr::filter(   entity == 120   )
Bugspytkirtel_1year_male_130                                                       <-    df_1year_male        %>%  dplyr::filter(   entity == 130   )
Naese_og_bihuler_1year_male_140                                                    <-    df_1year_male        %>%  dplyr::filter(   entity == 140   )
Strube_1year_male_150                                                              <-    df_1year_male        %>%  dplyr::filter(   entity == 150   )
Lunge_inkl_luftror_1year_male_160                                                  <-    df_1year_male        %>%  dplyr::filter(   entity == 160   )
Lungehinde_1year_male_170                                                          <-    df_1year_male        %>%  dplyr::filter(   entity == 170   )
Bryst_1year_male_180                                                               <-    df_1year_male        %>%  dplyr::filter(   entity == 180   )
Livmoderhals_1year_male_190                                                        <-    df_1year_male        %>%  dplyr::filter(   entity == 190   )
Livmoder_1year_male_200                                                            <-    df_1year_male        %>%  dplyr::filter(   entity == 200   )
Livmoder_uden_specifikation_1year_male_210                                         <-    df_1year_male        %>%  dplyr::filter(   entity == 210   )
Aeggestok_aeggeleder_mv_1year_male_220                                             <-    df_1year_male        %>%  dplyr::filter(   entity == 220   )
Ovrige_kvindelige_konsorganer_1year_male_230                                       <-    df_1year_male        %>%  dplyr::filter(   entity == 230   )
Prostata_1year_male_240                                                            <-    df_1year_male        %>%  dplyr::filter(   entity == 240   )
Testikel_1year_male_250                                                            <-    df_1year_male        %>%  dplyr::filter(   entity == 250   )
Penis_og_andre_mandlige_konsorganer_1year_male_260                                 <-    df_1year_male        %>%  dplyr::filter(   entity == 260   )
Nyre_1year_male_270                                                                <-    df_1year_male        %>%  dplyr::filter(   entity == 270   )
Blaere_og_andre_urinveje_1year_male_280                                            <-    df_1year_male        %>%  dplyr::filter(   entity == 280   )
Modermaerkekraeft_hud_1year_male_290                                               <-    df_1year_male        %>%  dplyr::filter(   entity == 290   )
Anden_hud_ikke_modermaerke_1year_male_300                                          <-    df_1year_male        %>%  dplyr::filter(   entity == 300   )
Oje_1year_male_310                                                                 <-    df_1year_male        %>%  dplyr::filter(   entity == 310   )
Hjerne_og_centralnervesystem_1year_male_320                                        <-    df_1year_male        %>%  dplyr::filter(   entity == 320   )
Skjoldbruskkirtel_1year_male_330                                                   <-    df_1year_male        %>%  dplyr::filter(   entity == 330   )
Knogle_1year_male_340                                                              <-    df_1year_male        %>%  dplyr::filter(   entity == 340   )
Bindevaev_1year_male_350                                                           <-    df_1year_male        %>%  dplyr::filter(   entity == 350   )
Non_Hodgkin_lymfom_1year_male_360                                                  <-    df_1year_male        %>%  dplyr::filter(   entity == 360   )
Hodgkins_lymfom_1year_male_370                                                     <-    df_1year_male        %>%  dplyr::filter(   entity == 370   )
Myelomatose_1year_male_380                                                         <-    df_1year_male        %>%  dplyr::filter(   entity == 380   )
Leukaemi_1year_male_400                                                            <-    df_1year_male        %>%  dplyr::filter(   entity == 400   )
Akut_lymfatisk_leukaemi_1year_male_401                                             <-    df_1year_male        %>%  dplyr::filter(   entity == 401   )
Kronisk_lymfatisk_leukaemi_1year_male_402                                          <-    df_1year_male        %>%  dplyr::filter(   entity == 402   )
Anden_og_uspecificeret_lymfatisk_leukaemi_1year_male_403                           <-    df_1year_male        %>%  dplyr::filter(   entity == 403   )
Akut_myeloid_leukaemi_1year_male_404                                               <-    df_1year_male        %>%  dplyr::filter(   entity == 404   )
Kronisk_myeloid_leukaemi_1year_male_405                                            <-    df_1year_male        %>%  dplyr::filter(   entity == 405   )
Anden_og_uspecificeret_myeloid_leukaemi_1year_male_406                             <-    df_1year_male        %>%  dplyr::filter(   entity == 406   )
Leukaemi_uspecificerede_celler_1year_male_407                                      <-    df_1year_male        %>%  dplyr::filter(   entity == 407   )
Andre_specificerede_1year_male_410                                                 <-    df_1year_male        %>%  dplyr::filter(   entity == 410   )
Ukendte_og_daarligt_definerede_1year_male_420                                      <-    df_1year_male        %>%  dplyr::filter(   entity == 420   )
Alle_kraeftformer_undtagen_anden_hud_bryst_og_prostata_1year_male_970              <-    df_1year_male        %>%  dplyr::filter(   entity == 970   )
Alle_kraeftformer_1year_male_980                                                   <-    df_1year_male        %>%  dplyr::filter(   entity == 980   )
Alle_kraeftformer_undtagen_anden_hud_1year_male_990                                <-    df_1year_male        %>%  dplyr::filter(   entity == 990   )
Laebe_mundhule_og_svaelg_1year_male_510                                            <-    df_1year_male        %>%  dplyr::filter(   entity == 510   )
Tyk_og_endetarm_1year_male_520                                                     <-    df_1year_male        %>%  dplyr::filter(   entity == 520   )
Basocellulaere_carcinomer_1year_male_888                                           <-    df_1year_male        %>%  dplyr::filter(   entity == 888   )



# 1 year female -----------------------------------------------------------






Laebe_1year_female_10                                                              <-    df_1year_female        %>%  dplyr::filter(   entity == 10   ) 
Mundhule_1year_female_20                                                           <-    df_1year_female        %>%  dplyr::filter(   entity == 20   )
Spytkirtel_1year_female_30                                                         <-    df_1year_female        %>%  dplyr::filter(   entity == 30   )
Oropharynx_1year_female_40                                                         <-    df_1year_female        %>%  dplyr::filter(   entity == 40   )
Nasopharynx_1year_female_41                                                        <-    df_1year_female        %>%  dplyr::filter(   entity == 41   )
Hypopharynx_1year_female_50                                                        <-    df_1year_female        %>%  dplyr::filter(   entity == 50   )
Pharynx_daarligt_defineret_1year_female_51                                         <-    df_1year_female        %>%  dplyr::filter(   entity == 51   )
Spiseror_1year_female_60                                                           <-    df_1year_female        %>%  dplyr::filter(   entity == 60   )
Mave_1year_female_70                                                               <-    df_1year_female        %>%  dplyr::filter(   entity == 70   )
Tyndtarm_1year_female_80                                                           <-    df_1year_female        %>%  dplyr::filter(   entity == 80   )
Tyktarm_1year_female_90                                                            <-    df_1year_female        %>%  dplyr::filter(   entity == 90   )
Endetarm_og_anus_1year_female_100                                                  <-    df_1year_female        %>%  dplyr::filter(   entity == 100   )
Lever_1year_female_110                                                             <-    df_1year_female        %>%  dplyr::filter(   entity == 110   )
Galdeblaere_og_galdeveje_1year_female_120                                          <-    df_1year_female        %>%  dplyr::filter(   entity == 120   )
Bugspytkirtel_1year_female_130                                                     <-    df_1year_female        %>%  dplyr::filter(   entity == 130   )
Naese_og_bihuler_1year_female_140                                                  <-    df_1year_female        %>%  dplyr::filter(   entity == 140   )
Strube_1year_female_150                                                            <-    df_1year_female        %>%  dplyr::filter(   entity == 150   )
Lunge_inkl_luftror_1year_female_160                                                <-    df_1year_female        %>%  dplyr::filter(   entity == 160   )
Lungehinde_1year_female_170                                                        <-    df_1year_female        %>%  dplyr::filter(   entity == 170   )
Bryst_1year_female_180                                                             <-    df_1year_female        %>%  dplyr::filter(   entity == 180   )
Livmoderhals_1year_female_190                                                      <-    df_1year_female        %>%  dplyr::filter(   entity == 190   )
Livmoder_1year_female_200                                                          <-    df_1year_female        %>%  dplyr::filter(   entity == 200   )
Livmoder_uden_specifikation_1year_female_210                                       <-    df_1year_female        %>%  dplyr::filter(   entity == 210   )
Aeggestok_aeggeleder_mv_1year_female_220                                           <-    df_1year_female        %>%  dplyr::filter(   entity == 220   )
Ovrige_kvindelige_konsorganer_1year_female_230                                     <-    df_1year_female        %>%  dplyr::filter(   entity == 230   )
Prostata_1year_female_240                                                          <-    df_1year_female        %>%  dplyr::filter(   entity == 240   )
Testikel_1year_female_250                                                          <-    df_1year_female        %>%  dplyr::filter(   entity == 250   )
Penis_og_andre_mandlige_konsorganer_1year_female_260                               <-    df_1year_female        %>%  dplyr::filter(   entity == 260   )
Nyre_1year_female_270                                                              <-    df_1year_female        %>%  dplyr::filter(   entity == 270   )
Blaere_og_andre_urinveje_1year_female_280                                          <-    df_1year_female        %>%  dplyr::filter(   entity == 280   )
Modermaerkekraeft_hud_1year_female_290                                             <-    df_1year_female        %>%  dplyr::filter(   entity == 290   )
Anden_hud_ikke_modermaerke_1year_female_300                                        <-    df_1year_female        %>%  dplyr::filter(   entity == 300   )
Oje_1year_female_310                                                               <-    df_1year_female        %>%  dplyr::filter(   entity == 310   )
Hjerne_og_centralnervesystem_1year_female_320                                      <-    df_1year_female        %>%  dplyr::filter(   entity == 320   )
Skjoldbruskkirtel_1year_female_330                                                 <-    df_1year_female        %>%  dplyr::filter(   entity == 330   )
Knogle_1year_female_340                                                            <-    df_1year_female        %>%  dplyr::filter(   entity == 340   )
Bindevaev_1year_female_350                                                         <-    df_1year_female        %>%  dplyr::filter(   entity == 350   )
Non_Hodgkin_lymfom_1year_female_360                                                <-    df_1year_female        %>%  dplyr::filter(   entity == 360   )
Hodgkins_lymfom_1year_female_370                                                   <-    df_1year_female        %>%  dplyr::filter(   entity == 370   )
Myelomatose_1year_female_380                                                       <-    df_1year_female        %>%  dplyr::filter(   entity == 380   )
Leukaemi_1year_female_400                                                          <-    df_1year_female        %>%  dplyr::filter(   entity == 400   )
Akut_lymfatisk_leukaemi_1year_female_401                                           <-    df_1year_female        %>%  dplyr::filter(   entity == 401   )
Kronisk_lymfatisk_leukaemi_1year_female_402                                        <-    df_1year_female        %>%  dplyr::filter(   entity == 402   )
Anden_og_uspecificeret_lymfatisk_leukaemi_1year_female_403                         <-    df_1year_female        %>%  dplyr::filter(   entity == 403   )
Akut_myeloid_leukaemi_1year_female_404                                             <-    df_1year_female        %>%  dplyr::filter(   entity == 404   )
Kronisk_myeloid_leukaemi_1year_female_405                                          <-    df_1year_female        %>%  dplyr::filter(   entity == 405   )
Anden_og_uspecificeret_myeloid_leukaemi_1year_female_406                           <-    df_1year_female        %>%  dplyr::filter(   entity == 406   )
Leukaemi_uspecificerede_celler_1year_female_407                                    <-    df_1year_female        %>%  dplyr::filter(   entity == 407   )
Andre_specificerede_1year_female_410                                               <-    df_1year_female        %>%  dplyr::filter(   entity == 410   )
Ukendte_og_daarligt_definerede_1year_female_420                                    <-    df_1year_female        %>%  dplyr::filter(   entity == 420   )
Alle_kraeftformer_undtagen_anden_hud_bryst_og_prostata_1year_female_970            <-    df_1year_female        %>%  dplyr::filter(   entity == 970   )
Alle_kraeftformer_1year_female_980                                                 <-    df_1year_female        %>%  dplyr::filter(   entity == 980   )
Alle_kraeftformer_undtagen_anden_hud_1year_female_990                              <-    df_1year_female        %>%  dplyr::filter(   entity == 990   )
Laebe_mundhule_og_svaelg_1year_female_510                                          <-    df_1year_female        %>%  dplyr::filter(   entity == 510   )
Tyk_og_endetarm_1year_female_520                                                   <-    df_1year_female        %>%  dplyr::filter(   entity == 520   )
Basocellulaere_carcinomer_1year_female_888                                         <-    df_1year_female        %>%  dplyr::filter(   entity == 888   )




# 5 year male -------------------------------------------------------------




Laebe_5year_male_10                                                                <-    df_5year_male        %>%  dplyr::filter(   entity == 10   ) 
Mundhule_5year_male_20                                                             <-    df_5year_male        %>%  dplyr::filter(   entity == 20   )
Spytkirtel_5year_male_30                                                           <-    df_5year_male        %>%  dplyr::filter(   entity == 30   )
Oropharynx_5year_male_40                                                           <-    df_5year_male        %>%  dplyr::filter(   entity == 40   )
Nasopharynx_5year_male_41                                                          <-    df_5year_male        %>%  dplyr::filter(   entity == 41   )
Hypopharynx_5year_male_50                                                          <-    df_5year_male        %>%  dplyr::filter(   entity == 50   )
Pharynx_daarligt_defineret_5year_male_51                                           <-    df_5year_male        %>%  dplyr::filter(   entity == 51   )
Spiseror_5year_male_60                                                             <-    df_5year_male        %>%  dplyr::filter(   entity == 60   )
Mave_5year_male_70                                                                 <-    df_5year_male        %>%  dplyr::filter(   entity == 70   )
Tyndtarm_5year_male_80                                                             <-    df_5year_male        %>%  dplyr::filter(   entity == 80   )
Tyktarm_5year_male_90                                                              <-    df_5year_male        %>%  dplyr::filter(   entity == 90   )
Endetarm_og_anus_5year_male_100                                                    <-    df_5year_male        %>%  dplyr::filter(   entity == 100   )
Lever_5year_male_110                                                               <-    df_5year_male        %>%  dplyr::filter(   entity == 110   )
Galdeblaere_og_galdeveje_5year_male_120                                            <-    df_5year_male        %>%  dplyr::filter(   entity == 120   )
Bugspytkirtel_5year_male_130                                                       <-    df_5year_male        %>%  dplyr::filter(   entity == 130   )
Naese_og_bihuler_5year_male_140                                                    <-    df_5year_male        %>%  dplyr::filter(   entity == 140   )
Strube_5year_male_150                                                              <-    df_5year_male        %>%  dplyr::filter(   entity == 150   )
Lunge_inkl_luftror_5year_male_160                                                  <-    df_5year_male        %>%  dplyr::filter(   entity == 160   )
Lungehinde_5year_male_170                                                          <-    df_5year_male        %>%  dplyr::filter(   entity == 170   )
Bryst_5year_male_180                                                               <-    df_5year_male        %>%  dplyr::filter(   entity == 180   )
Livmoderhals_5year_male_190                                                        <-    df_5year_male        %>%  dplyr::filter(   entity == 190   )
Livmoder_5year_male_200                                                            <-    df_5year_male        %>%  dplyr::filter(   entity == 200   )
Livmoder_uden_specifikation_5year_male_210                                         <-    df_5year_male        %>%  dplyr::filter(   entity == 210   )
Aeggestok_aeggeleder_mv_5year_male_220                                             <-    df_5year_male        %>%  dplyr::filter(   entity == 220   )
Ovrige_kvindelige_konsorganer_5year_male_230                                       <-    df_5year_male        %>%  dplyr::filter(   entity == 230   )
Prostata_5year_male_240                                                            <-    df_5year_male        %>%  dplyr::filter(   entity == 240   )
Testikel_5year_male_250                                                            <-    df_5year_male        %>%  dplyr::filter(   entity == 250   )
Penis_og_andre_mandlige_konsorganer_5year_male_260                                 <-    df_5year_male        %>%  dplyr::filter(   entity == 260   )
Nyre_5year_male_270                                                                <-    df_5year_male        %>%  dplyr::filter(   entity == 270   )
Blaere_og_andre_urinveje_5year_male_280                                            <-    df_5year_male        %>%  dplyr::filter(   entity == 280   )
Modermaerkekraeft_hud_5year_male_290                                               <-    df_5year_male        %>%  dplyr::filter(   entity == 290   )
Anden_hud_ikke_modermaerke_5year_male_300                                          <-    df_5year_male        %>%  dplyr::filter(   entity == 300   )
Oje_5year_male_310                                                                 <-    df_5year_male        %>%  dplyr::filter(   entity == 310   )
Hjerne_og_centralnervesystem_5year_male_320                                        <-    df_5year_male        %>%  dplyr::filter(   entity == 320   )
Skjoldbruskkirtel_5year_male_330                                                   <-    df_5year_male        %>%  dplyr::filter(   entity == 330   )
Knogle_5year_male_340                                                              <-    df_5year_male        %>%  dplyr::filter(   entity == 340   )
Bindevaev_5year_male_350                                                           <-    df_5year_male        %>%  dplyr::filter(   entity == 350   )
Non_Hodgkin_lymfom_5year_male_360                                                  <-    df_5year_male        %>%  dplyr::filter(   entity == 360   )
Hodgkins_lymfom_5year_male_370                                                     <-    df_5year_male        %>%  dplyr::filter(   entity == 370   )
Myelomatose_5year_male_380                                                         <-    df_5year_male        %>%  dplyr::filter(   entity == 380   )
Leukaemi_5year_male_400                                                            <-    df_5year_male        %>%  dplyr::filter(   entity == 400   )
Akut_lymfatisk_leukaemi_5year_male_401                                             <-    df_5year_male        %>%  dplyr::filter(   entity == 401   )
Kronisk_lymfatisk_leukaemi_5year_male_402                                          <-    df_5year_male        %>%  dplyr::filter(   entity == 402   )
Anden_og_uspecificeret_lymfatisk_leukaemi_5year_male_403                           <-    df_5year_male        %>%  dplyr::filter(   entity == 403   )
Akut_myeloid_leukaemi_5year_male_404                                               <-    df_5year_male        %>%  dplyr::filter(   entity == 404   )
Kronisk_myeloid_leukaemi_5year_male_405                                            <-    df_5year_male        %>%  dplyr::filter(   entity == 405   )
Anden_og_uspecificeret_myeloid_leukaemi_5year_male_406                             <-    df_5year_male        %>%  dplyr::filter(   entity == 406   )
Leukaemi_uspecificerede_celler_5year_male_407                                      <-    df_5year_male        %>%  dplyr::filter(   entity == 407   )
Andre_specificerede_5year_male_410                                                 <-    df_5year_male        %>%  dplyr::filter(   entity == 410   )
Ukendte_og_daarligt_definerede_5year_male_420                                      <-    df_5year_male        %>%  dplyr::filter(   entity == 420   )
Alle_kraeftformer_undtagen_anden_hud_bryst_og_prostata_5year_male_970              <-    df_5year_male        %>%  dplyr::filter(   entity == 970   )
Alle_kraeftformer_5year_male_980                                                   <-    df_5year_male        %>%  dplyr::filter(   entity == 980   )
Alle_kraeftformer_undtagen_anden_hud_5year_male_990                                <-    df_5year_male        %>%  dplyr::filter(   entity == 990   )
Laebe_mundhule_og_svaelg_5year_male_510                                            <-    df_5year_male        %>%  dplyr::filter(   entity == 510   )
Tyk_og_endetarm_5year_male_520                                                     <-    df_5year_male        %>%  dplyr::filter(   entity == 520   )
Basocellulaere_carcinomer_5year_male_888                                           <-    df_5year_male        %>%  dplyr::filter(   entity == 888   )




# 5 year female -----------------------------------------------------------



Laebe_5year_female_10                                                              <-    df_5year_female        %>%  dplyr::filter(   entity == 10   ) 
Mundhule_5year_female_20                                                           <-    df_5year_female        %>%  dplyr::filter(   entity == 20   )
Spytkirtel_5year_female_30                                                         <-    df_5year_female        %>%  dplyr::filter(   entity == 30   )
Oropharynx_5year_female_40                                                         <-    df_5year_female        %>%  dplyr::filter(   entity == 40   )
Nasopharynx_5year_female_41                                                        <-    df_5year_female        %>%  dplyr::filter(   entity == 41   )
Hypopharynx_5year_female_50                                                        <-    df_5year_female        %>%  dplyr::filter(   entity == 50   )
Pharynx_daarligt_defineret_5year_female_51                                         <-    df_5year_female        %>%  dplyr::filter(   entity == 51   )
Spiseror_5year_female_60                                                           <-    df_5year_female        %>%  dplyr::filter(   entity == 60   )
Mave_5year_female_70                                                               <-    df_5year_female        %>%  dplyr::filter(   entity == 70   )
Tyndtarm_5year_female_80                                                           <-    df_5year_female        %>%  dplyr::filter(   entity == 80   )
Tyktarm_5year_female_90                                                            <-    df_5year_female        %>%  dplyr::filter(   entity == 90   )
Endetarm_og_anus_5year_female_100                                                  <-    df_5year_female        %>%  dplyr::filter(   entity == 100   )
Lever_5year_female_110                                                             <-    df_5year_female        %>%  dplyr::filter(   entity == 110   )
Galdeblaere_og_galdeveje_5year_female_120                                          <-    df_5year_female        %>%  dplyr::filter(   entity == 120   )
Bugspytkirtel_5year_female_130                                                     <-    df_5year_female        %>%  dplyr::filter(   entity == 130   )
Naese_og_bihuler_5year_female_140                                                  <-    df_5year_female        %>%  dplyr::filter(   entity == 140   )
Strube_5year_female_150                                                            <-    df_5year_female        %>%  dplyr::filter(   entity == 150   )
Lunge_inkl_luftror_5year_female_160                                                <-    df_5year_female        %>%  dplyr::filter(   entity == 160   )
Lungehinde_5year_female_170                                                        <-    df_5year_female        %>%  dplyr::filter(   entity == 170   )
Bryst_5year_female_180                                                             <-    df_5year_female        %>%  dplyr::filter(   entity == 180   )
Livmoderhals_5year_female_190                                                      <-    df_5year_female        %>%  dplyr::filter(   entity == 190   )
Livmoder_5year_female_200                                                          <-    df_5year_female        %>%  dplyr::filter(   entity == 200   )
Livmoder_uden_specifikation_5year_female_210                                       <-    df_5year_female        %>%  dplyr::filter(   entity == 210   )
Aeggestok_aeggeleder_mv_5year_female_220                                           <-    df_5year_female        %>%  dplyr::filter(   entity == 220   )
Ovrige_kvindelige_konsorganer_5year_female_230                                     <-    df_5year_female        %>%  dplyr::filter(   entity == 230   )
Prostata_5year_female_240                                                          <-    df_5year_female        %>%  dplyr::filter(   entity == 240   )
Testikel_5year_female_250                                                          <-    df_5year_female        %>%  dplyr::filter(   entity == 250   )
Penis_og_andre_mandlige_konsorganer_5year_female_260                               <-    df_5year_female        %>%  dplyr::filter(   entity == 260   )
Nyre_5year_female_270                                                              <-    df_5year_female        %>%  dplyr::filter(   entity == 270   )
Blaere_og_andre_urinveje_5year_female_280                                          <-    df_5year_female        %>%  dplyr::filter(   entity == 280   )
Modermaerkekraeft_hud_5year_female_290                                             <-    df_5year_female        %>%  dplyr::filter(   entity == 290   )
Anden_hud_ikke_modermaerke_5year_female_300                                        <-    df_5year_female        %>%  dplyr::filter(   entity == 300   )
Oje_5year_female_310                                                               <-    df_5year_female        %>%  dplyr::filter(   entity == 310   )
Hjerne_og_centralnervesystem_5year_female_320                                      <-    df_5year_female        %>%  dplyr::filter(   entity == 320   )
Skjoldbruskkirtel_5year_female_330                                                 <-    df_5year_female        %>%  dplyr::filter(   entity == 330   )
Knogle_5year_female_340                                                            <-    df_5year_female        %>%  dplyr::filter(   entity == 340   )
Bindevaev_5year_female_350                                                         <-    df_5year_female        %>%  dplyr::filter(   entity == 350   )
Non_Hodgkin_lymfom_5year_female_360                                                <-    df_5year_female        %>%  dplyr::filter(   entity == 360   )
Hodgkins_lymfom_5year_female_370                                                   <-    df_5year_female        %>%  dplyr::filter(   entity == 370   )
Myelomatose_5year_female_380                                                       <-    df_5year_female        %>%  dplyr::filter(   entity == 380   )
Leukaemi_5year_female_400                                                          <-    df_5year_female        %>%  dplyr::filter(   entity == 400   )
Akut_lymfatisk_leukaemi_5year_female_401                                           <-    df_5year_female        %>%  dplyr::filter(   entity == 401   )
Kronisk_lymfatisk_leukaemi_5year_female_402                                        <-    df_5year_female        %>%  dplyr::filter(   entity == 402   )
Anden_og_uspecificeret_lymfatisk_leukaemi_5year_female_403                         <-    df_5year_female        %>%  dplyr::filter(   entity == 403   )
Akut_myeloid_leukaemi_5year_female_404                                             <-    df_5year_female        %>%  dplyr::filter(   entity == 404   )
Kronisk_myeloid_leukaemi_5year_female_405                                          <-    df_5year_female        %>%  dplyr::filter(   entity == 405   )
Anden_og_uspecificeret_myeloid_leukaemi_5year_female_406                           <-    df_5year_female        %>%  dplyr::filter(   entity == 406   )
Leukaemi_uspecificerede_celler_5year_female_407                                    <-    df_5year_female        %>%  dplyr::filter(   entity == 407   )
Andre_specificerede_5year_female_410                                               <-    df_5year_female        %>%  dplyr::filter(   entity == 410   )
Ukendte_og_daarligt_definerede_5year_female_420                                    <-    df_5year_female        %>%  dplyr::filter(   entity == 420   )
Alle_kraeftformer_undtagen_anden_hud_bryst_og_prostata_5year_female_970            <-    df_5year_female        %>%  dplyr::filter(   entity == 970   )
Alle_kraeftformer_5year_female_980                                                 <-    df_5year_female        %>%  dplyr::filter(   entity == 980   )
Alle_kraeftformer_undtagen_anden_hud_5year_female_990                              <-    df_5year_female        %>%  dplyr::filter(   entity == 990   )
Laebe_mundhule_og_svaelg_5year_female_510                                          <-    df_5year_female        %>%  dplyr::filter(   entity == 510   )
Tyk_og_endetarm_5year_female_520                                                   <-    df_5year_female        %>%  dplyr::filter(   entity == 520   )
Basocellulaere_carcinomer_5year_female_888                                         <-    df_5year_female        %>%  dplyr::filter(   entity == 888   )















