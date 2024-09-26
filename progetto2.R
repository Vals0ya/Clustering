# Scarico librerie e importo dataset --------------------------------------
require(tidyverse)
require(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)
require(ggplot2)
require(gridExtra)
remove(f.voles,coffee,wine)

#-------------------------------------------------------------------
is.na(olive)#verifichiamo se ci sono valori nulli

#rinomino le regioni 
olive$Region[olive$Region == 1] <- "Southern Italy"
olive$Region[olive$Region == 2] <- "Sardinia"
olive$Region[olive$Region == 3] <- "Northern Italy"
#rinomino le aree
olive$Area[olive$Area == 1] <- "North Apulia"
olive$Area[olive$Area == 2] <- "Calabria"
olive$Area[olive$Area == 3] <- "South Apulia"
olive$Area[olive$Area == 4] <- "Sicily"
olive$Area[olive$Area == 5] <- "Inland Sardinia"
olive$Area[olive$Area == 6] <- "Costal Sardinia"
olive$Area[olive$Area == 7] <- "East Liguria"
olive$Area[olive$Area == 8] <- "West Liguria"
olive$Area[olive$Area == 9] <- "Umbria"

#quante olive per ogni regione sono state analizzate? lo stesso numero?
conteggio_regioni <- table(olive$Area)

# Creo un dataframe dai risultati del conteggio
conteggio_df <- data.frame(regioni = names(conteggio_regioni), frequenza = as.numeric(conteggio_regioni))

# Creo un grafico a barre
grafico <- ggplot(data = conteggio_df, aes(x = regioni, y = frequenza)) +
  geom_bar(stat = "identity") +
  labs(x = "Regioni", y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# standardizziamo ---------------------------------------------------------

#Le prime due colonne non sono numeriche perciò le estraggo e standardizzo le altre per poi riunirle 
numeric_columns <- olive[3:ncol(olive)]
st_data <- scale(numeric_columns)#standardizzo
olive_data <- cbind(olive[, 1:2], st_data)
is.na(olive_data)
plot(olive_data)
#riesco già a vedere dei cluster? No, infatti non è supervisionato

# -- ----------------------------------------------------------------------
#Distanze: calcolo la matrice delle distanze tra le righe di olive_data utilizzando solo le colonne dalla terza in poi utilizzando i vari metodi 
x.dist1=dist(olive_data[-c(1, 2)],method="euclidean")

x.dist2=dist(olive_data[-c(1, 2)],method="manhattan")

x.dist3=dist(olive_data[-c(1, 2)],method="minkowski", p=3)

# dendrogrammi
hc.single1=hclust(x.dist1,method="single",members=NULL)
hc.single2=hclust(x.dist2,method="single",members=NULL)
hc.single3=hclust(x.dist3,method="single",members=NULL)
hc.ward=hclust(x.dist1,method="ward",members=NULL)

plot(hc.single1)
plot(hc.single2)
plot(hc.single3)
plot(hc.ward)

#Ancora non riesco a capire quanti cluster ci sono, con nessun metodo.

# -- ----------------------------------------------------------------------


# Metodo del gomito per capire quanti cluster ci sono  

# Utilizza il pacchetto "cluster" per calcolare l'inerzia
library(cluster)
set.seed(9910)

# Crea una serie di clustering con diversi numeri di cluster
k_values <- 1:10
inerzia <- numeric(length(k_values))

for (k in k_values) {
  kmeans_model <- kmeans(st_data, centers = k)#uso st_data che sono i dati standardizzati ma senza le prime due colonne
  inerzia[k] <- kmeans_model$tot.withinss
}

# Plot del metodo del gomito
plot(k_values, inerzia, type = "b", xlab = "Numero di Cluster", ylab = "Inerzia")


#Nel grafico, cerca il punto in cui l'inerzia inizia a diminuire a un ritmo più lento, creando una forma simile a un "gomito". Questo valore potrebbe essere un'indicazione del numero ottimale di cluster

#ricontrollo utilizzando la funzione NbClust sia con kmeans che con ward ( utilizza diversi criteri di validazione dei cluster)
library(NbClust)

zero<-NbClust(st_data,method= "ward.D")#utilizzando diversi criteri di validazione dei cluster.
uno<- NbClust(st_data,method= "kmeans",min.nc=3,max.nc=10)#Kmeans cerca di suddividere i dati in cluster di varianza minima


#provo a dividerlo in cinque gruppi 
hicluste1=cutree(hc.single1,k=5) #metodo euclideo
hicluste2=cutree(hc.single2,k=5)#metodo manhattan
hicluste3=cutree(hc.single3,k=5)#metodo minkowski

plot(olive_data[-c(1, 2)], col=hicluste1,main="Single Linkage - Euclidean distance")
plot(olive_data[-c(1, 2)], col=hicluste2,main="Single Linkage - Manhattan distance")
plot(olive_data[-c(1, 2)], col=hicluste3,main="Single Linkage - Minkowski distance")

hiward=cutree(hc.ward,k=5)
plot(olive_data[-c(1, 2)], col=hiward,main="Ward's minimum variance - Euclidean distance")
#Il migliore è ward 

#vedo la distribuzione delle osservazioni nei cluster generati dall'analisi di clustering..
table(hicluste1)#metodo euclideo
table(hicluste2)#metodo manhattan
table(hicluste3)#metodo minkowski
table(hiward) #metodo di ward
#capisco che la migliore è con il metodo ward, perchè distribuito più omogeneo anche se nel cluster 3 il numero rimane sempre maggiore anche se vado a dividere in piu gruppi. 

#Probabilmente la variabile significativa è l'area dato che le altre sono numeri decimali quindi è più improbabile che siano loro la variabile significativa. 
#Qual è l'area piu presente nel cluster tre?

# Seleziona le righe nel cluster 3
cluster_3_data <- olive_data[hiward == 3, ]

# Conta le diverse aree nel cluster 3
table(cluster_3_data$Area)
#Quindi ciò che le  accomuna è che sono quasi tutte del sud della puglia 

# -- ----------------------------------------------------------------------
#Qual è il grasso che porta più benefici a livello salutare?
# l' oleico è un acido grasso monoinsaturo presente in abbondanza nell'olio d'oliva. È noto per i suoi benefici per la salute cardiaca, inclusi i potenziali effetti positivi sul colesterolo e sulla riduzione del rischio di malattie cardiovascolari.

#Come è distributo questo grasso nei cluster? 

library(ggplot2)
library(dplyr)
library(plotly)

# Crea un nuovo dataframe contenente tutte le informazioni dei cluster
cluster_data <- data.frame(Oleic = olive_data$Oleic, Cluster = hiward)

# Calcola la frequenza per ciascun cluster
cluster_frequencies <- cluster_data %>%
  group_by(Cluster) %>%
  summarize(Frequency = n())

# Trova il cluster con la massima frequenza
most_informative_cluster <- cluster_frequencies %>%
  filter(Frequency == max(Frequency)) %>%  left_join(cluster_data, by = c("Cluster"))


# Crea un istogramma interattivo con plotly
plot <- ggplot(cluster_data, aes(x = factor(Cluster), y = Oleic, fill = factor(Cluster))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), alpha = 0.7) +
  labs(title = "Distribuzione di Oleic nei Cluster", x = "Cluster", y = "Valore di Oleic") +
  scale_fill_discrete(name = "Cluster") +
  theme_minimal()

# Rendi il grafico interattivo con plotly e aggiungi etichette
plotly_plot <- ggplotly(plot, tooltip = "y") %>%
  layout(annotations = list(
    x = most_informative_cluster$Cluster,
    y = max(most_informative_cluster$Oleic),
    text = paste("Cluster", most_informative_cluster$Cluster, " (max, n =", most_informative_cluster$Frequency, ")"),
    showarrow = TRUE,
    arrowhead = 2,
    arrowsize = 1.5,
    arrowwidth = 2,
    arrowcolor = "black"
  ))

plotly::config(plotly_plot, displayModeBar = F)
#perciò il cluster che rappresenta le olive con benifici salutari maggiori è il cluster tre. Ma in realtà è perchè è il cluster con più variabili. 


#Per essere più salutare il parametro deve essere il più vicino possibile allo zero. 
#identifico il cluster con il parametro "Oleic" uguale o vicino a 0 più frequente tra i cinque cluster e di conoscere il numero di osservazioni in questo cluster per vedere quale tra i cluster ha le olive più salutari

cluster_valmedi <- tapply(olive_data$Oleic, hiward, mean)#valori medi del parametro "Oleic" per ciascun cluster.

piuvicino_zero <- which.min(abs(cluster_valmedi - 0))#numero del cluster più vicino a 0 in termini di valore medio del parametro "Oleic".

cluster_size <- sum(hiward == piuvicino_zero)#numero di osservazioni nel cluster con il parametro "Oleic" più vicino a 0.

#infatti ritornando al grafico di prima, il cluster due è quello con più osservazioni che oscillano tra 0 e 1 

#perciò qual è l'aerea predominante nel cluster due che è quello con le olive piu salutari?
cluster_2_data <- olive_data[hiward == 2, ]

# Conta le diverse aree nel cluster 2
table(cluster_2_data$Area)
#perciò le olive più salutari le ha la calabria.
