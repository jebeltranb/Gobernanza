library(psych)
library( ggplot2 )
library(xts)
library(readxl)
library(factoextra)
library(FactoMineR)
library('ca')
library(tables)
library( likert )
library( likert )
library('corrr')
library(rlang)
library( corrplot )
library(grid)
library( tidyr )
library(sqldf)
library( dplyr )

################################################
##                                            ##
##           Tamaño de la muestra             ##
##                                            ##
################################################

tam_muest_fr<-function(z,del,s)
{
  l<-length(z)
  m<-length(del)
  n_0=matrix(0,ncol =l ,nrow =m )
  n=n_0
  for (i in 1:l) 
  {
    for (j in 1:m) 
    {
      n_0[j,i]<-((z[i])^2*s^2)/(del[j]^2)
      n[j,i]<-round( floor(n_0[j,i])+1,0)
    }    
  }
  colnames(n)<-c("90%","95%","99%")
  row.names(n)<-del
  return(n)
}

z<-c(1.65,1.96,2.326)
delta<-c(0.2,0.15,0.1,0.05)
del<-delta
s=sqrt( 1.33201)

muestra<-tam_muest_fr(z,del,s)

prop<-c(0.242,0.18,0.176,0.15,0.15,0.075,0.027)
228*prop
muestra_f<-floor(228*prop)+1
muestra_f
sum(muestra_f)
bh <- read.csv("../data/Pilotaje_fray2.csv")
bh
length(bh)
dat_enc_darw1<-read_excel("../data/Pilotaje_fray2.xlsx")
dicc<-read_excel("../data/diccionariofray.xlsx") # carga los datos de un excel
v<-data.frame(dicc) 

length(dat_enc_darw1)
names(dat_enc_darw1)
m<-length(dat_enc_darw1)


for (h in 1:m) {   # dar los nombres a las variables con los nombres cortos del diccionario
  names(dat_enc_darw1)[h]<- dicc$item[h]  
}
daf<-dat_enc_darw1[-2,] # renombrar por practicidad y nombre corto
daf<-data.frame(daf) # poner el conjunto de datos como un data fracme
#str(daf)             # descripción de las variables

################################################
##                                            ##
##  Niveles de los factores en las preguntas  ##
##                                            ##
################################################
names(daf)
#filtt<-grep("^Q",names(daf))      dat_enc_darw1[,153]
#daf[,filtt]
##################
a_in<-c(17:20,49:57,111:129,148:161) # respuestas tipo 1

##########
b_in<-c(21:48) # respuestas tipo 2

##########
c_in<-c(62:70) # respuestas tipo 3

##########
d_in<-c(93:97,104) # respuestas tipo 4

##########
e_in<-c(88:92) # respuestas tipo 5

##########
f_in<-c() # respuestas tipo 6
##########

g_in<-c(58) # respuestas tipo 7

######################

ni_a1<-"1. Totalmente en desacuerdo"
ni_a2<-"2. En desacuerdo"
ni_a3<-"3. Ni de acuerdo, ni en desacuerdo"
ni_a4<-"4. De acuerdo"
ni_a5<-"5. Totalmente de acuerdo "
###############################
ni_b1<-"1. Sin importancia"
ni_b2<-"2. De poca importancia"
ni_b3<-"3. Moderadamente importante"
ni_b4<-"4. Importante"
ni_b5<-"5. Muy importante"
###############################
ni_c1<-"Nada importante"
ni_c2<-"Poco importanteimportante"
ni_c3<-"Indiferente"
ni_c4<-"Importante"
ni_c5<-"Muy importante"
###############################
ni_d1<-"Muy malo"
ni_d2<-"Malo"
ni_d3<-"Regular"
ni_d4<-"Bueno"
ni_d5<-"Muy bueno"

###############################
ni_e1<-"Ningún control"
ni_e2<-"Poco cobntrol"
ni_e3<-"Control medio"
ni_e4<-"Suficiente control"
ni_e5<-"Mucho control"

###############################
ni_f1<-"Ningún control"
ni_f2<-"Poco cobntrol"
ni_f3<-"Control medio"
ni_f4<-"Suficiente control"
###############################
ni_g1<-"Muy bajo"
ni_g2<-"Bajo"
ni_g3<-"Medio"
ni_g4<-"Alto"
ni_g5<-"Muy alto"

###############################################################
##                                                           ##
##   Asigna los niveles que falte y organizando los mismos   ##
##                                                           ##
###############################################################

daf<-dat_enc_darw1 # renombrar por practicidad y nombre corto
daf<-data.frame(daf)
names(daf)
daf[,153]
unique( c(levels(daf[ ,k]),ni_a1,ni_a2, ni_a3, ni_a4, ni_a5))
for (k in a_in) {  # reorganiza los niveles de los factores para que mantengan el orden
  #factor(daf[ ,k],exclude = NA)
  daf[ ,k] <- factor( daf[ ,k],exclude = NA ) # reorganiza
  levels(daf[ ,k])<- c(ni_a1,ni_a2, ni_a3, ni_a4, ni_a5) # si algu variable no toma todos los calores de lows factores
}
######################################################

for (k in b_in) {  # reorganiza los niveles de los factores para que mantengan el orden
  levels(daf[ ,k])<-unique( c(levels(daf[ ,k]),ni_b1,ni_b2, ni_b3, ni_b4, ni_b5)) # si algu variable no toma todos los calores de lows factores
  a1<-which(levels(daf[ ,k])==ni_b1)
  a2<-which(levels(daf[ ,k])==ni_b2)
  a3<-which(levels(daf[ ,k])==ni_b3)
  a4<-which(levels(daf[ ,k])==ni_b4)
  a5<-which(levels(daf[ ,k])==ni_b5)
  daf[ ,k] <- as.factor( daf[ ,k],exclude = NA ) # reorganiza
}

########################################################

for (k in c_in) {  # reorganiza los niveles de los factores para que mantengan el orden
  levels(daf[ ,k])<-unique( c(levels(daf[ ,k]),ni_c1,ni_c2, ni_c3, ni_c4, ni_c5)) # si algu variable no toma todos los calores de lows factores
  a1<-which(levels(daf[ ,k])==ni_c1)
  a2<-which(levels(daf[ ,k])==ni_c2)
  a3<-which(levels(daf[ ,k])==ni_c3)
  a4<-which(levels(daf[ ,k])==ni_c4)
  a5<-which(levels(daf[ ,k])==ni_c5)
  daf[ ,k] <- factor( daf[ ,k],exclude = NA, levels = levels( daf[ ,k] )[c(a1,a2,a3,a4,a5)] ) # reorganiza
}

########################################################

for (k in d_in) {  # reorganiza los niveles de los factores para que mantengan el orden
  levels(daf[ ,k])<-unique( c(levels(daf[ ,k]),ni_d1,ni_d2, ni_d3, ni_d4, ni_d5)) # si algu variable no toma todos los calores de lows factores
  a1<-which(levels(daf[ ,k])==ni_d1)
  a2<-which(levels(daf[ ,k])==ni_d2)
  a3<-which(levels(daf[ ,k])==ni_d3)
  a4<-which(levels(daf[ ,k])==ni_d4)
  a5<-which(levels(daf[ ,k])==ni_d5)
  daf[ ,k] <- factor( daf[ ,k],exclude = NA, levels = levels( daf[ ,k] )[c(a1,a2,a3,a4,a5)] ) # reorganiza
}

#####################################################################################
for (k in d_in) {  # reorganiza los niveles de los factores para que mantengan el orden
  levels(daf[ ,k])<-unique( c(levels(daf[ ,k]),ni_e1,ni_e2, ni_e3, ni_e4, ni_e5)) # si algu variable no toma todos los calores de lows factores
  a1<-which(levels(daf[ ,k])==ni_e1)
  a2<-which(levels(daf[ ,k])==ni_e2)
  a3<-which(levels(daf[ ,k])==ni_e3)
  a4<-which(levels(daf[ ,k])==ni_e4)
  a5<-which(levels(daf[ ,k])==ni_e5)
  daf[ ,k] <- factor( daf[ ,k],exclude = NA, levels = levels( daf[ ,k] )[c(a1,a2,a3,a4,a5)] ) # reorganiza
}

#####################################################################################
for (k in d_in) {  # reorganiza los niveles de los factores para que mantengan el orden
  levels(daf[ ,k])<-unique( c(levels(daf[ ,k]),ni_f1,ni_f2, ni_f3, ni_f4)) # si algu variable no toma todos los calores de lows factores
  a1<-which(levels(daf[ ,k])==ni_f1)
  a2<-which(levels(daf[ ,k])==ni_f2)
  a3<-which(levels(daf[ ,k])==ni_f3)
  a4<-which(levels(daf[ ,k])==ni_f4)
  daf[ ,k] <- factor( daf[ ,k],exclude = NA, levels = levels( daf[ ,k] )[c(a1,a2,a3,a4)] ) # reorganiza
}

########################################################

for (k in d_in) {  # reorganiza los niveles de los factores para que mantengan el orden
  levels(daf[ ,k])<-unique( c(levels(daf[ ,k]),ni_g1,ni_g2, ni_g3, ni_g4, ni_g5)) # si algu variable no toma todos los calores de lows factores
  a1<-which(levels(daf[ ,k])==ni_g1)
  a2<-which(levels(daf[ ,k])==ni_g2)
  a3<-which(levels(daf[ ,k])==ni_g3)
  a4<-which(levels(daf[ ,k])==ni_g4)
  a5<-which(levels(daf[ ,k])==ni_g5)
  daf[ ,k] <- factor( daf[ ,k],exclude = NA, levels = levels( daf[ ,k] )[c(a1,a2,a3,a4,a5)] ) # reorganiza
}

#####################################################################################


################################################
##                                            ##
##   Preguntas con los niveles establecidos   ##
##                                            ##
################################################

# Variables que contienes números pero son factores


str(daf)
names(daf)
daf[1]
tt <- tabular( ( Cargo  = daf[,5] ) * ( MCC = daf[,1] )  + ( Total = 1 )~ ( Frecuencia = 1 ), data = daf ) 


tt <- tabular( ( MCC  = daf[,1] ) , data = daf ) #crea la tabla de contingencias
?tabular
#####################################################################################

###############################################################
##                                                           ##
##    Renombra las variables que inicein por Q (preguntas)   ##
##                                                           ##
###############################################################

dfLikert <- daf[ , grep( "^Q", colnames( daf ) ) ]    # separa las variables que inician con Q questions
colnames( dfLikert ) <-  v[ grep( "^Q", v$item ), "Variable" ] # va al directoprio para renombrar
#length(dicc[ grep( "^Q", dicc$item ), "Variable" ])
names(dfLikert)

#names(dicc)daf
##############################################
###########                      #############
##############################################
names(daf)   #daf[,153]
bloque1a <- c(1:4)
bloque1b1 <- c(33:37)
bloque1b2 <- c(38:41)
bloque1c<- c(65:69)
bloque1d<- c(70:72)
bloque1e<- c(73:76)
bloque1f1<- c(77:79)
bloque1f2<- c(80:83)
bloque1g1<- c(84:87)
bloque1g2<- c(88:91)
bloque1h<- c(92:97)
bloque1i<- c(92:97)

bloque2a <- c(5,7,13)   # separa las varibales con los mismos niveles 1° bloque     
bloque3 <-c(6,8)
bloque4 <-11:12
bloque14<-14
bloque15<- 15  
bloque9<-9


items1a <- likert( items = dfLikert[ , bloque1a ] ) #Seleccion a las varibales para realizar graficas de barras comparativas
items1b1 <- likert( items = dfLikert[ , bloque1b1 ] )
items1b2 <- likert( items = dfLikert[ , bloque1b2 ] )
items1c <- likert( items = dfLikert[ , bloque1c ] ) 
items1d <- likert( items = dfLikert[ , bloque1d ] ) 
items1e <- likert( items = dfLikert[ , bloque1e ] ) 
items1f1 <- likert( items = dfLikert[ , bloque1f1 ] ) 
items1f2 <- likert( items = dfLikert[ , bloque1f2 ] ) 
items1g1 <- likert( items = dfLikert[ , bloque1g1 ] )
items1g2 <- likert( items = dfLikert[ , bloque1g2 ] )
items1h <- likert( items = dfLikert[ , bloque1h ] ) 




items2 <- likert( items = dfLikert[ , bloque2 ] )
#items2 <- likert( items = dfLikert[, bloque2, drop = FALSE] ) # para la selección de una única columna
items3 <- likert( items = dfLikert[ , bloque3 ] )
items4 <- likert( items = dfLikert[ , bloque4 ] )

########################################################
##                                                    ##
##    Gráficos de barras en los diferentes bloques    ##
##                                                    ##
########################################################

plot( items1a, centered = TRUE, group.order = colnames( items1a$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "15. Evalúe los siguientes enunciados",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))

#####################################

plot( items1b1, centered = TRUE, group.order = colnames( items1b1$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "18. Evalúe los siguientes enunciados (parte 1).",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))
#####################################

plot( items1b2, centered = TRUE, group.order = colnames( items1b2$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "18. Evalúe los siguientes enunciados (parte 2).",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))
#####################################

plot( items1c, centered = TRUE, group.order = colnames( items1c$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "41. Evalúe los siguientes enunciados",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))

#####################################

plot( items1d, centered = TRUE, group.order = colnames( items1d$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "42. Evalúe los siguientes enunciados",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))

#####################################
#####################################

plot( items1e, centered = TRUE, group.order = colnames( items1e$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "43. Evalúe los siguientes enunciados",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))

#####################################

plot( items1f1, centered = TRUE, group.order = colnames( items1f1$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "44. Evalúe los siguientes enunciados (parte 1)",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))
#####################################

plot( items1f2, centered = TRUE, group.order = colnames( items1f2$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "44. Evalúe los siguientes enunciados (parte 2)",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))

#####################################
plot( items1g1, centered = TRUE, group.order = colnames( items1g1$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "47. Evalúe los siguientes enunciados (parte 1)",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))
#####################################

plot( items1g2, centered = TRUE, group.order = colnames( items1g2$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "47. Evalúe los siguientes enunciados (parte 2)",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))
#####################################
daf[,153]
plot( items1h, centered = TRUE, group.order = colnames( items1g$items ), # plor de barras para el primer bloque 
      legend.position = "bottom" ) + labs(title = "47. Evalúe los siguientes enunciados",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic"))

#####################################



plot( items2, centered = TRUE, group.order = colnames( items2$items ),
      legend.position = "bottom"  ) +
  labs(title = "Respuestas del Segundo Bloque",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 12, hjust = 0 ),
         legend.text = element_text( size = 9 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic") )
#####################################
plot( items3, centered = TRUE, group.order = colnames( items3$items ),
      legend.position = "bottom" ) +
  labs(title = "Respuestas del Tercer Bloque",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 12.5, hjust = 0 ),
         legend.text = element_text( size = 9 ),
         legend.title = element_text( size = 10 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic") )
#####################################
plot( items4, centered = TRUE, group.order = colnames( items4$items ),
      legend.position = "bottom" ) +
  labs(title = "Respuestas del Cuarto Bloque",size=15,font=4,col="darkblue")+
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 11.5, hjust = 0 ),
         legend.text = element_text( size = 8.72 ),
         legend.title = element_text( size = 9.5 ), 
         plot.title = element_text(hjust = 0.5,color="deepskyblue4", size=17, face="bold.italic") )

########################################################
##                                                    ##
##   Gráficos de densidad en los diferentes bloques   ##
##                                                    ##
########################################################

plot( items1, type = "density" )
plot( items2, type = "density" )
plot( items3, type = "density" )
plot( items4, type = "density" )

