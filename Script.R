library(dplyr)
library(ggplot2)
library(tidyr) 
library(lubridate)
library(gamlss)
library(MASS)
library(psych)
library(FactoMineR)
library(FactoInvestigate)
library(Factoshiny)
library(factoextra)
library(tidyverse)
library(gamlss.tr)
library(gamlss.dist)
library(cluster)
library(ClusterR)
library(Rtsne) 
library(kernlab)
library(clustMixType)
library(reshape2)

data <- ecommerceData

summary(data)
length(unique(data$User_ID))
length(unique(data$Product_ID))
table(as.factor(data$Category))
table(as.factor(data$Payment_Method))
table(as.factor(data$`Discount (%)`))
table(data$`Price (Rs.)`[data$Category=="Sports"])
table(data$`Price (Rs.)`[data$Category=="Beauty"])
str(data)
head(data)

summary(data$`Price (Rs.)`)
summary(data$`Final_Price(Rs.)`)

long_data1 <- melt(data, id.vars = NULL, measure.vars = c("Price (Rs.)", "Final_Price(Rs.)"),
                  variable.name = "Price_Type", value.name = "Price_Value")

# Creazione del boxplot
ggplot(long_data1, aes(x = Price_Type, y = Price_Value, fill = Price_Type)) +
  geom_boxplot() +
  labs(title = "Comparison of Boxplots: Price (Rs.) and Final_Price (Rs.)",
       x = "Type of price",
       y = "Price") +

  theme_minimal()

pairs.panels(data[c(4,6,8)], 
             method = "pearson", # correlation method
             hist.col = "#66A61E",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

data1 <- data


# Calcoli per mese --------------------------------------------------------


#trasformo la variabile Purchase_date in una variabile date
data1$Purchase_Date <- as.Date(data1$Purchase_Date, format= "%d-%m-%Y")
summary(data1$Purchase_Date)

#calcolo del fatturato mensile per tutti i mesi
mese_incasso_massimo <- data1 %>%
  mutate(Mese = format(as.Date(Purchase_Date), "%Y-%m")) %>%  
  group_by(Mese) %>%                                          
  summarise(Incasso_Totale = sum(`Final_Price(Rs.)`),  
            
            .groups = "drop") 

mese_incasso_massimo
giorni <- c(31,28,31,30,31,30,31,31,30,31,21)
mese_incasso_massimo$Media_incasso <- mese_incasso_massimo$Incasso_Totale/giorni
mese_incasso_massimo$giorni <- giorni
mese_incasso_massimo

mean(mese_incasso_massimo$Incasso_Totale)/30

ggplot(mese_incasso_massimo, aes(x = Mese, y = Incasso_Totale)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue") +
  labs(title = "Total Revenue per month",
       x = "Month",
       y = "Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mese_incasso_massimo, aes(x = Mese, y = Media_incasso)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue") +
  geom_hline(yintercept = 2294.782, color = "red", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Average Revenue per month",
       x = "Month",
       y = "Average revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#calcolo del numero di transazioni per ogni mese
transazioni_mensili <- data1 %>%
  mutate(Mese = format(as.Date(Purchase_Date), "%Y-%m")) %>%
  group_by(Mese) %>%
  summarise(NumVendite = n(),
            .groups = "drop")


transazioni_mensili
giorni <- c(31,28,31,30,31,30,31,31,30,31,21)
#aggiunta colonna che aggiusta il numero di vendite per il numero di giorni
transazioni_mensili$Media_mesi_giorni <- transazioni_mensili$NumVendite/giorni
transazioni_mensili$giorni <- giorni
transazioni_mensili

mean(datset_analisi_regressione$Nvendite)

#barplot delle vendite per mese
ggplot(transazioni_mensili, aes(x = Mese, y = NumVendite)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue") +
  labs(title = "Number of transactions per month",
       x = "Month",
       y = "Number of transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#barplot del numero di vendite aggiustato per il numero di giorni, aggiungendo una linea per indicare la media giornaliera
ggplot(transazioni_mensili, aes(x = Mese, y = Media_mesi_giorni)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue") +
  geom_hline(yintercept = 11.23, color = "red", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Average number of transactions per month",
       x = "Month",
       y = "Average number of transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#calcolo frequenze e percentuali per gli acquisti effettuati in un mese divisi per il metodo di pagamento
frequenze_metodi_mese <- data1 %>%
  mutate(Mese = format(Purchase_Date, "%Y-%m")) %>% 
  group_by(Mese, Payment_Method) %>%                 
  summarise(Frequenza = n(), .groups = "drop") %>%   
  group_by(Mese) %>%                                
  mutate(Totale_Mese = sum(Frequenza),              
         Percentuale = Frequenza / Totale_Mese * 100) %>% 
  ungroup() %>%                                     
  arrange(Mese)  

frequenze_metodi_mese

# Grafici a barre delle frequenze per ogni metodo di pagamento per mese
ggplot(frequenze_metodi_mese, aes(x = Mese, y = Frequenza, fill = Payment_Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequencies of payment methods per month",
       x = "Month",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grafici a barre delle Percentauli per ogni metodo di pagamento per mese
ggplot(frequenze_metodi_mese, aes(x = Mese, y = Percentuale, fill = Payment_Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentages of payment methods per month",
       x = "Month",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# studio per settimana ----------------------------------------------------


# Calcolo del numero di transazioni dividendo il dataset in settimane
transazioni_settimanali <- data1 %>%
  mutate(
    SettimanaInizio = as.Date(Purchase_Date) - as.numeric(format(Purchase_Date, "%u")) + 1,
    Settimana = ifelse(SettimanaInizio < as.Date("2020-01-01"), as.Date("2020-01-01"), SettimanaInizio)
  ) %>%
  group_by(Settimana) %>%
  summarise(NumVendite = n())

# Forzare il formato della colonna Settimana come data
transazioni_settimanali$Settimana
transazioni_settimanali$Settimana <- as.Date(transazioni_settimanali$Settimana)
transazioni_settimanali$Settimana
transazioni_settimanali
summary(transazioni_settimanali$NumVendite)

# Grafico settimanale con la settimana che parte dal 1 gennaio 2024
ggplot(transazioni_settimanali, aes(x = Settimana, y = NumVendite)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue") +
  labs(title = "Number of transaction per week",
       x = "Weeks",
       y = "Number of transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Ruota etichette per leggibilità
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") 


#calcolo percentuali di numero di vendite dei metodi di pagamento divisi per settimane
transazioni_settimanali_metodo <- data1 %>%
  mutate(
    SettimanaInizio = as.Date(Purchase_Date) - as.numeric(format(Purchase_Date, "%u")) + 1,
    Settimana = ifelse(SettimanaInizio < as.Date("2020-01-01"), as.Date("2020-01-01"), SettimanaInizio)
  ) %>%
  group_by(Settimana, Payment_Method) %>%      
  summarise(NumVendite = n(), .groups = "drop") %>% 
  group_by(Settimana) %>%                    
  mutate(Totale_Settimanale = sum(NumVendite),
         Percentuale = NumVendite / Totale_Settimanale * 100) %>% 
  ungroup()   

transazioni_settimanali_metodo
transazioni_settimanali_metodo$Settimana <- as.Date(transazioni_settimanali_metodo$Settimana)
transazioni_settimanali_metodo

#barplot delle percentuali di utilizzo dei metodi di pagamento per ogni settimana
ggplot(transazioni_settimanali_metodo, aes(x = Settimana, y = Percentuale, fill = Payment_Method)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barre affiancate per metodo di pagamento
  labs(title = "Sales by payment method per week",
       x = "Weeka",
       y = "Sales",
       fill = "Payment method") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# studio per categoria ----------------------------------------------------

#Calcolo media dei prezzi per ogni categoria
media_prezzi_per_categoria <- data %>%
  group_by(Category) %>%
  summarise(media_prezzo = mean(`Price (Rs.)`))

media_prezzi_per_categoria

#Distribuzione dei prezzi per ogni categoria
ggplot(data, aes(x = Category, y = data$`Price (Rs.)`)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Distribution of prices per category",
       x = "Category",
       y = "Price") +
  theme_minimal()

#Istogrammi delel distribuzioni dei prezzi per ogni categoria
ggplot(data, aes(x = data$`Price (Rs.)`, fill = Category)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  facet_wrap(~ Category, scales = "free_y") +
  labs(title = "Distribution of prices per category",
       x = "Price",
       y = "Probability") +
  theme_minimal()


#calcolo delle frequenze e percentuali dei metodi di pagamento divisi per categoria
frequenze_metodi_categoria <- data1 %>%
  group_by(Category, Payment_Method) %>%           
  summarise(Frequenza = n(), .groups = "drop") %>% 
  group_by(Category) %>%                          
  mutate(Totale_Categoria = sum(Frequenza),        
         Percentuale = Frequenza / Totale_Categoria * 100) %>% 
  ungroup() %>%                                    
  arrange(Category)                                

# Visualizza il risultato
print(frequenze_metodi_categoria) #UPI (Unified Payments Interface) è un sistema di pagamento digitale sviluppato in India che consente di effettuare transazioni bancarie in tempo reale tramite smartphone.

# Grafico delle frequenze per ogni metodo di pagamento per categoria di prodotto
ggplot(frequenze_metodi_categoria, aes(x = Category, y = Percentuale, fill = Payment_Method)) +
  geom_bar(stat = "identity", position = "dodge") +  # Le barre sono affiancate per ogni metodo di pagamento
  labs(title = "Percentages of payment methods per category",
       x = "Product category",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calcolare la media dei prezzi per categoria
prezzi_medi <- data1 %>%
  group_by(Category) %>%                      
  summarise(Prezzo_Medio = mean(`Price (Rs.)`)) %>%  
  arrange(Category)  

prezzi_medi

# Barplot dei prezzi medi per categoria
ggplot(prezzi_medi, aes(x = Category, y = Prezzo_Medio)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue") +
  labs(title = "Average Price per Category",
       x = "Category",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#confronto trai prezzi scontati e non per categoria (medi)
prezzi_confronto <- data %>%
  group_by(Category) %>%  
  summarise(
    Prezzo_Medio = mean(`Price (Rs.)`),            
    Prezzo_Medio_Sconto = mean(`Final_Price(Rs.)`)  
  ) %>%
  mutate(Differenza = Prezzo_Medio - Prezzo_Medio_Sconto) %>%  
  pivot_longer(cols = c(Prezzo_Medio, Prezzo_Medio_Sconto),   
               names_to = "Tipo_Prezzo", 
               values_to = "Prezzo") %>%
  arrange(Category)

prezzi_confronto

# Creare un barplot che confronti il prezzo medio e il prezzo medio scontato per categoria
ggplot(prezzi_confronto, aes(x = Category, y = Prezzo, fill = Tipo_Prezzo)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Comparison between Average price and discounted average price per category",
       x = "Category",
       y = "Price") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(prezzi_confronto, aes(x = Category, y = Differenza)) +
  geom_bar(stat = "identity", position = "dodge") +  # Le barre affiancate per ogni tipo di prezzo
  labs(title = "Difference between Average price and discounted average price per category",
       x = "Category",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#confronto trai prezzi scontati e non per categoria (assoluti)
prezzi_confronto_assoluto <- data %>%
  group_by(Category) %>%  
  summarise(
    Revenue_no_discount = sum(`Price (Rs.)`),           
    Final_revenue = sum(`Final_Price(Rs.)`) 
  ) %>%
  mutate(Differenza = Revenue_no_discount - Final_revenue) %>%  
  pivot_longer(cols = c(Revenue_no_discount, Final_revenue),    
               names_to = "Tipo_Prezzo", 
               values_to = "Prezzo") %>%
  arrange(Category)

prezzi_confronto_assoluto

# Barplot per confrontare la somma dei prezzi scontati e non assoluti
ggplot(prezzi_confronto_assoluto, aes(x = Category, y = Prezzo, fill = Tipo_Prezzo)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Comparison between the sum of the prices discounted and not, per category",
       x = "Category",
       y = "Price") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Barplot per confrontare i prezzi scontati e non assoluti (differenza)
ggplot(prezzi_confronto_assoluto, aes(x = Category, y = Differenza)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Difference between the sum of prices not discounted and discounted, per category",
       x = "Category",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#calcolo frequenze e percentuali dei tipi di sconti divisi per categorie
frequenze_sconti_categoria <- data1 %>%
  group_by(Category, `Discount (%)`) %>%           
  summarise(Frequenza = n(), .groups = "drop") %>%
  group_by(Category) %>%                          
  mutate(Totale_Categoria = sum(Frequenza),     
         Percentuale = Frequenza / Totale_Categoria * 100) %>% 
  ungroup() %>%                                  
  arrange(Category, `Discount (%)`)  

frequenze_sconti_categoria

ggplot(frequenze_sconti_categoria, aes(x = Category, y = Percentuale, fill = as.factor(`Discount (%)`))) +
  geom_bar(stat = "identity", position = "dodge") +  # Le barre sono affiancate per ogni metodo di pagamento
  labs(title = "Percentages of the discounts per category",
       x = "Category",
       y = "Percentages") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#nuovo dataset di backup, creando una colonna aggiuntiva nella quale si inserisce il prezzo della transazioni in una categoria di prezzo (10 categorie)
data12 <- data1 %>%
  mutate(
    Prezzo_Range = cut(
      `Price (Rs.)`,               
      breaks = seq(10, 500, by = (500 - 10) / 10),
      labels = 1:10,           
      include.lowest = TRUE      
    )
  )
table(data12$Prezzo_Range)
#aggiunta colonna per inserire il nome del mese invece del numero
data12 <- data12 %>%
  mutate(
    Mese = format(Purchase_Date, "%m"), 
    Mese_nome = case_when(
      Mese == "01" ~ "Gennaio",
      Mese == "02" ~ "Febbraio",
      Mese == "03" ~ "Marzo",
      Mese == "04" ~ "Aprile",
      Mese == "05" ~ "Maggio",
      Mese == "06" ~ "Giugno",
      Mese == "07" ~ "Luglio",
      Mese == "08" ~ "Agosto",
      Mese == "09" ~ "Settembre",
      Mese == "10" ~ "Ottobre",
      Mese == "11" ~ "Novembre",
      Mese == "12" ~ "Dicembre"
    )
  )
table(as.factor(data12$Mese_nome))

#aggiunta colonna per aggiungere due colonne che descrivono il giorno della settimana ed il numero della settimana
data12 <- data12 %>%
  mutate(
    Giorno_Setimana = wday(Purchase_Date, label = TRUE, abbr = FALSE), 
    Settimana_Anno = isoweek(Purchase_Date) 
  )

#creazione nuovo dataset di backup per aggiungere una colonna che raprpesenti l'intervallo di prezzo (5 intervalli)
data123 <- data1 %>%
  mutate(
    Prezzo_Range = cut(
      `Price (Rs.)`,           
      breaks = seq(10, 500, by = (500 - 10) / 5), 
      labels = c("10-107", "108-205", "206-303", "304-401", "402-500"),            
      include.lowest = TRUE     
    )
  )
table(data123$Prezzo_Range)
head(data123)

#calcolo delle transazioni mensili corrispettive per ogni range di prezzo e le percentuali
transazioni_mensili_range <- data123 %>%
  mutate(Mese = format(as.Date(Purchase_Date), "%Y-%m")) %>%  
  group_by(Mese, Prezzo_Range) %>%                          
  summarise(
    NumTransazioni = n(), 
    .groups = "drop"
  ) %>%
  group_by(Mese) %>%  # Raggruppa per mese per calcolare il totale mensile
  mutate(
    Percentuale = (NumTransazioni / sum(NumTransazioni)) * 100
  ) %>%
  ungroup()

transazioni_mensili_range

#barplot delle transazioni mensili corrispettive per ogni range di prezzo
ggplot(transazioni_mensili_range, aes(x = Mese, y = NumTransazioni, fill = Prezzo_Range)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Number of transactions by the price range per month",
    x = "Month",
    y = "Number of transactions",
    fill = "Price range"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#barplot delle transazioni mensili corrispettive per ogni range di prezzo (percentuale)
ggplot(transazioni_mensili_range, aes(x = Mese, y = Percentuale, fill = Prezzo_Range)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Number of transactions by the price range per month",
    x = "Month",
    y = "Percentage",
    fill = "Price range"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#calcolo della colonna Season per descrivere la stagione della transazione (estate-inverno...)
data123 <- data123 %>%
  mutate(Season = case_when(
    as.Date(Purchase_Date) >= as.Date("2024-03-21") & as.Date(Purchase_Date) <= as.Date("2024-06-20") ~ "Spring", 
    as.Date(Purchase_Date) >= as.Date("2024-06-21") & as.Date(Purchase_Date) <= as.Date("2024-09-20") ~ "Summer", 
    as.Date(Purchase_Date) >= as.Date("2024-09-21") & as.Date(Purchase_Date) <= as.Date("2024-12-20") ~ "Autumn", 
    (as.Date(Purchase_Date) >= as.Date("2024-01-01") & as.Date(Purchase_Date) <= as.Date("2024-03-20")) ~ "Winter"  
  ))

data123$Season

#calcolo numero di transazioni per stagione
transazioni_stagionali <- data123 %>%
  mutate(Stagione = data123$Season) %>%  
  group_by(Stagione) %>%                        
  summarise(NumTransazioni = n(), .groups = "drop") 

transazioni_stagionali

#barplot delle transazioni stagionali
ggplot(transazioni_stagionali, aes(x = Stagione, y = NumTransazioni)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Number of transaction per season",
    x = "Season",
    y = "Number of transactions",
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#transazioni divise per range di prezzo su base settimanale
transazioni_stagionali_range <- data123 %>%
  mutate(Stagione = data123$Season) %>%  
  group_by(Stagione, Prezzo_Range) %>%                            
  summarise(NumTransazioni = n(), .groups = "drop")   

transazioni_stagionali_range

#barplot delle ransazioni divise per range di prezzo su base settimanale
ggplot(transazioni_stagionali_range, aes(x = Stagione, y = NumTransazioni, fill = Prezzo_Range)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Number of transactions divided by price range per season",
    x = "Season",
    y = "Numero di Transazioni",
    fill = "Price range"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#calcolo di: somma final_price, numero vendite e media di prezzo per vendita per ogni giorno del dataset
ricavi_giornalieri <- data12 %>%
  mutate(Giorno_Anno = format(Purchase_Date, "%Y-%m-%d")) %>%
  group_by(Giorno_Anno) %>%
  summarise(Somma_Final_Price = sum(`Final_Price(Rs.)`),
            Somma_Price = sum(`Price (Rs.)`),
            Nvendite = n(),
            Media_per_vendita_giornaliera = Somma_Final_Price / Nvendite)

ricavi_giornalieri$Giorno_Anno <- as.Date(ricavi_giornalieri$Giorno_Anno, format = "%Y-%m-%d")
ricavi_giornalieri

#serie storica della somma dei final price
ggplot(ricavi_giornalieri, aes(x = Giorno_Anno, y = Somma_Final_Price)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Trend of the sum of final prices",
    x = "Time",
    y = "Total revenue"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#serie storica del ricavo medio da vendite per giorno
ggplot(ricavi_giornalieri, aes(x = Giorno_Anno, y = Media_per_vendita_giornaliera)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Trend of the average sale",
    x = "Time",
    y = "Average price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# regressione  ------------------------------------------------------------

#creazione dataset per effettuare analisi di regressione, non sul dataset originale ma in uno che spieghi i trend giornalieri
datset_analisi_regressione <- data.frame(ricavi_giornalieri)
datset_analisi_regressione <- data.frame(datset_analisi_regressione, ricavi_giornalieri$Giorno_Anno)
datset_analisi_regressione$Nome_del_giorno <- weekdays(datset_analisi_regressione$Giorno_Anno)
datset_analisi_regressione$Nome_del_mese <- months(datset_analisi_regressione$Giorno_Anno)
datset_analisi_regressione <- datset_analisi_regressione %>%
  mutate(
    Day = as.numeric(Giorno_Anno - min(Giorno_Anno)), # Trend lineare
    Weekday = weekdays(Giorno_Anno),          # Giorno della settimana
    IsWeekend = ifelse(Weekday %in% c("sabato", "domenica"), 1, 0) # Dummy weekend
  )
datset_analisi_regressione$Day <- datset_analisi_regressione$Day +1


# Aggiunta colonne booleane per facilitare l'analisi (possibilità di togliere un mese/giorno per volta)
datset_analisi_regressione <- datset_analisi_regressione %>%
  mutate(Is_lunedi = weekdays(Giorno_Anno) == "lunedì")%>%
  mutate(Is_martedi = weekdays(Giorno_Anno) == "martedì")%>%
  mutate(Is_mercoledi = weekdays(Giorno_Anno) == "mercoledì")%>%
  mutate(Is_giovedi = weekdays(Giorno_Anno) == "giovedì")%>%
  mutate(Is_venerdi = weekdays(Giorno_Anno) == "venerdì")%>%
  mutate(Is_sabato = weekdays(Giorno_Anno) == "sabato")%>%
  mutate(Is_domenica = weekdays(Giorno_Anno) == "domenica")%>%
  mutate(Is_gennaio = Nome_del_mese == "gennaio")%>%
  mutate(Is_febbraio = Nome_del_mese == "febbraio")%>%
  mutate(Is_marzo = Nome_del_mese == "marzo")%>%
  mutate(Is_aprile = Nome_del_mese == "aprile")%>%
  mutate(Is_maggio = Nome_del_mese == "maggio")%>%
  mutate(Is_giugno = Nome_del_mese == "giugno")%>%
  mutate(Is_luglio = Nome_del_mese == "luglio")%>%
  mutate(Is_agosto = Nome_del_mese == "agosto")%>%
  mutate(Is_settembre = Nome_del_mese == "settembre")%>%
  mutate(Is_ottobre = Nome_del_mese == "ottobre")%>%
  mutate(Is_novembre = Nome_del_mese == "novembre")



#serie storica evidenziando ogni osservazione in baso al giorno della settimana
ggplot(datset_analisi_regressione, aes(x = Giorno_Anno, y = Media_per_vendita_giornaliera)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(aes(color = Nome_del_giorno), size = 4) +  # Colore diverso per ogni giorno
  scale_color_manual(values = c("lunedì" = "red", "martedì" = "orange", "mercoledì" = "yellow", 
                                "giovedì" = "green", "venerdì" = "blue", "sabato" = "purple", "domenica" = "pink")) +
  labs(
    title = "Trend of the average sale per day",
    x = "Time",
    y = "Average price",
    color = "Days of the week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datset_analisi_regressione, aes(x = Giorno_Anno, y = Nvendite)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(aes(color = Nome_del_giorno), size = 4) +  # Colore diverso per ogni giorno
  scale_color_manual(values = c("lunedì" = "red", "martedì" = "orange", "mercoledì" = "yellow", 
                                "giovedì" = "green", "venerdì" = "blue", "sabato" = "purple", "domenica" = "pink")) +
  labs(
    title = "Trend of sales per day",
    x = "Time",
    y = "Sales",
    color = "Days of the week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calcola il totale delle vendite per giorno e categoria
ricavi_giornalieri1 <- data12 %>%
  mutate(Giorno_Anno = format(Purchase_Date, "%Y-%m-%d")) %>%
  group_by(Giorno_Anno, Category) %>%
  summarise(
    Totale_per_categoria = n(),
    
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Category,  # Nome delle colonne basato sulle categorie
    values_from = Totale_per_categoria,  # Riempie con i totali delle vendite
    values_fill = 0  # Riempie i valori mancanti con 0
  ) 

summary(datset_analisi_regressione)
summary(ricavi_giornalieri1)

ricavi_giornalieri_percentuali <- data12 %>%
  mutate(Giorno_Anno = format(Purchase_Date, "%Y-%m-%d")) %>%
  group_by(Giorno_Anno) %>%
  summarise(
    Somma_Final_Price = sum(`Final_Price(Rs.)`),
    Nvendite = n(),
    Media_per_vendita_giornaliera = Somma_Final_Price / Nvendite,
    Perc_Beauty = sum(ifelse(Category == "Beauty", 1, 0)) / Nvendite * 100,
    Perc_Clothing = sum(ifelse(Category == "Clothing", 1, 0)) / Nvendite * 100,
    Perc_Electronics = sum(ifelse(Category == "Electronics", 1, 0)) / Nvendite * 100,
    Perc_Home_Kitchen = sum(ifelse(Category == "Home & Kitchen", 1, 0)) / Nvendite * 100,
    Perc_Sports = sum(ifelse(Category == "Sports", 1, 0)) / Nvendite * 100,
    Perc_Books = sum(ifelse(Category == "Books", 1, 0)) / Nvendite * 100,
    Perc_Toys = sum(ifelse(Category == "Toys", 1, 0)) / Nvendite * 100
  )
summary(as.factor(data12$Payment_Method))
ricavi_giornalieri_percentuali

ricavi_giornalieri_percentuali2 <- data12 %>%
  mutate(Giorno_Anno = format(Purchase_Date, "%Y-%m-%d")) %>%
  group_by(Giorno_Anno) %>%
  summarise(
    Somma_Final_Price = sum(`Final_Price(Rs.)`),
    Nvendite = n(),
    Media_per_vendita_giornaliera = Somma_Final_Price / Nvendite,
    Perc_Beauty = sum(ifelse(Category == "Beauty", 1, 0)) / Nvendite * 100,
    Perc_Clothing = sum(ifelse(Category == "Clothing", 1, 0)) / Nvendite * 100,
    Perc_Electronics = sum(ifelse(Category == "Electronics", 1, 0)) / Nvendite * 100,
    Perc_Home_Kitchen = sum(ifelse(Category == "Home & Kitchen", 1, 0)) / Nvendite * 100,
    Perc_Sports = sum(ifelse(Category == "Sports", 1, 0)) / Nvendite * 100,
    Perc_Books = sum(ifelse(Category == "Books", 1, 0)) / Nvendite * 100,
    Perc_Toys = sum(ifelse(Category == "Toys", 1, 0)) / Nvendite * 100,
    Perc_Cash = sum(ifelse(Payment_Method == "Cash on Delivery", 1, 0)) / Nvendite * 100,
    Perc_Credit_Card = sum(ifelse(Payment_Method == "Credit Card", 1, 0)) / Nvendite * 100,
    Perc_Debit_Card = sum(ifelse(Payment_Method == "Debit Card", 1, 0)) / Nvendite * 100,
    Perc_Net_Banking = sum(ifelse(Payment_Method == "Net Banking", 1, 0)) / Nvendite * 100,
    Perc_UPI = sum(ifelse(Payment_Method == "UPI", 1, 0)) / Nvendite * 100
  )
#aggiunta di colonne aggiuntive per ogni categoria che indicano frequenza e percentuale di vendita per ogni specifico giorno
datset_analisi_regressione1 <- datset_analisi_regressione
datset_analisi_regressione1$Beauty <- ricavi_giornalieri1$Beauty
datset_analisi_regressione1$Clothing <- ricavi_giornalieri1$Clothing
datset_analisi_regressione1$Electronics <- ricavi_giornalieri1$Electronics
datset_analisi_regressione1$`Home & Kitchen` <- ricavi_giornalieri1$`Home & Kitchen`
datset_analisi_regressione1$Sports <- ricavi_giornalieri1$Sports
datset_analisi_regressione1$Books <- ricavi_giornalieri1$Books
datset_analisi_regressione1$Toys <- ricavi_giornalieri1$Toys
datset_analisi_regressione1$Beautyp <- ricavi_giornalieri_percentuali$Perc_Beauty
datset_analisi_regressione1$Clothingp <- ricavi_giornalieri_percentuali$Perc_Clothing
datset_analisi_regressione1$Electronicsp <- ricavi_giornalieri_percentuali$Perc_Electronics
datset_analisi_regressione1$`Home & Kitchenp` <- ricavi_giornalieri_percentuali$Perc_Home_Kitchen
datset_analisi_regressione1$Sportsp <- ricavi_giornalieri_percentuali$Perc_Sports
datset_analisi_regressione1$Booksp <- ricavi_giornalieri_percentuali$Perc_Books
datset_analisi_regressione1$Toysp <- ricavi_giornalieri_percentuali$Perc_Toys
datset_analisi_regressione1$cashp <- ricavi_giornalieri_percentuali2$Perc_Cash
datset_analisi_regressione1$creditcardp <- ricavi_giornalieri_percentuali2$Perc_Credit_Card
datset_analisi_regressione1$debitp <- ricavi_giornalieri_percentuali2$Perc_Debit_Card
datset_analisi_regressione1$netp <- ricavi_giornalieri_percentuali2$Perc_Net_Banking
datset_analisi_regressione1$UPIp <- ricavi_giornalieri_percentuali2$Perc_UPI

summary(datset_analisi_regressione1)

pairs.panels(datset_analisi_regressione1[c(2,3,4)], 
             method = "pearson", # correlation method
             hist.col = "#66A61E",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#istogrammi delle variabili: numero di vendite, somma final price e prezzo medio per vendite (somma_final_price/n_vendite) per vedere le distribuzioni di tali variabili
par(mfrow = c(1,3))
hist(datset_analisi_regressione$Nvendite, breaks = 50)
hist(datset_analisi_regressione$Somma_Final_Price, breaks= 50)
hist(datset_analisi_regressione$Media_per_vendita_giornaliera, breaks= 50)
summary(datset_analisi_regressione1$Somma_Final_Price)


dispersion_vendite <- var(datset_analisi_regressione1$Nvendite) / mean(datset_analisi_regressione1$Nvendite)
dispersion_vendite


gen.Family("NO", type="log")

model <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ 1, family= LOGNO
, data = datset_analisi_regressione1, control = gamlss.control(n.cyc = 100)) # Modello iniziale
model <- gamlss(datset_analisi_regressione1$Nvendite ~ 1, family= GPO
                , data = datset_analisi_regressione1, control = gamlss.control(n.cyc = 100)) 

step_model <- stepGAIC(model, scope = ~  Is_lunedi + Is_martedi + Is_mercoledi + 
                         Is_giovedi + Is_venerdi + Is_sabato + Is_domenica + 
                         Is_gennaio + Is_febbraio + Is_marzo + Is_aprile + 
                         Is_maggio + Is_giugno + Is_luglio + Is_agosto + 
                         Is_settembre + Is_ottobre + Is_novembre + 
                         Beautyp + Clothingp + Electronicsp + `Home & Kitchenp` + 
                         Sportsp + Booksp + Toysp + cashp+ creditcardp+ debitp+ netp+ UPIp, 
                       data = datset_analisi_regressione1, k = 3)
step_model <- stepGAIC(model, scope = ~   Is_lunedi + Is_martedi + Is_mercoledi + 
                         Is_giovedi + Is_venerdi + Is_sabato + Is_domenica + 
                         Is_gennaio + Is_febbraio + Is_marzo + Is_aprile + 
                         Is_maggio + Is_giugno + Is_luglio + Is_agosto + 
                         Is_settembre + Is_ottobre + Is_novembre + 
                         Beautyp + Clothingp + Electronicsp + `Home & Kitchenp` + 
                         Sportsp + Booksp + Toysp + cashp+ creditcardp+ debitp+ netp+ UPIp, 
                       data = datset_analisi_regressione1, k = 3) #Somma_Final_Price+ Media_per_vendita_giornaliera+ 
summary(step_model)
plot(step_model)

summary(datset_analisi_regressione1$Somma_Final_Price)

# regressione lineare, somma final price target ---------------------------


#modello completo di regressione (normale)
model40 <- gamlss(datset_analisi_regressione1$Nvendite ~ datset_analisi_regressione1$Somma_Final_Price+ datset_analisi_regressione1$Is_lunedi + datset_analisi_regressione1$Is_martedi+ datset_analisi_regressione1$Is_mercoledi+ datset_analisi_regressione1$Is_giovedi+ datset_analisi_regressione1$Is_venerdi+ datset_analisi_regressione1$Is_sabato+ datset_analisi_regressione1$Is_domenica+   datset_analisi_regressione1$Is_gennaio+ datset_analisi_regressione1$Is_febbraio+ datset_analisi_regressione1$Is_marzo+ datset_analisi_regressione1$Is_aprile+ datset_analisi_regressione1$Is_maggio+ datset_analisi_regressione1$Is_giugno+ datset_analisi_regressione1$Is_luglio+ datset_analisi_regressione1$Is_agosto+ datset_analisi_regressione1$Is_settembre+ datset_analisi_regressione1$Is_ottobre+ datset_analisi_regressione1$Is_novembre+ datset_analisi_regressione1$Beautyp+ datset_analisi_regressione1$Clothingp+ datset_analisi_regressione1$Electronicsp+ datset_analisi_regressione1$`Home & Kitchenp`+ datset_analisi_regressione1$Sportsp+ datset_analisi_regressione1$Booksp+ datset_analisi_regressione1$Toysp,  data = datset_analisi_regressione1, control = gamlss.control(n.cyc = 100))
summary(model40)
plot(model40) 

#NO (modello migliore)
mod_NORMALE <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio+     datset_analisi_regressione1$Booksp,  data = datset_analisi_regressione1,family=NO, control = gamlss.control(n.cyc = 100))
summary(mod_NORMALE)
plot(mod_NORMALE) 
par(mfrow=c(1,1))
histDist(datset_analisi_regressione1$Nvendite)
table(datset_analisi_regressione1$Nvendite)

#SN1
mod_SN1 <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio+     datset_analisi_regressione1$Booksp,  data = datset_analisi_regressione1, family=SN1, control = gamlss.control(n.cyc = 100))
summary(mod_SN1)
plot(mod_SN1) 

#BCCG
mod_BCCG <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio+     datset_analisi_regressione1$Booksp,  data = datset_analisi_regressione1, family=BCCG, control = gamlss.control(n.cyc = 100))
summary(mod_BCCG)
plot(mod_BCCG) 

#GA_SOMMA
mod_GA_SOMMA <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio,  data = datset_analisi_regressione1, family=GA, control = gamlss.control(n.cyc = 100))
summary(mod_GA_SOMMA)
plot(mod_GA_SOMMA) 

#LNO
mod_LNO <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio,  data = datset_analisi_regressione1, family=LNO, control = gamlss.control(n.cyc = 100))
summary(mod_LNO)
plot(mod_LNO) 
histDist(mod_LNO)

#LOGNO
mod_LOGNO <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio,  data = datset_analisi_regressione1, family=LOGNO, control = gamlss.control(n.cyc = 100))
summary(mod_LOGNO)
plot(mod_LOGNO) 


#IG
mod_IG <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio+ datset_analisi_regressione1$Electronicsp+ datset_analisi_regressione1$Beautyp,  data = datset_analisi_regressione1, family=IG, control = gamlss.control(n.cyc = 100))
summary(mod_IG)
plot(mod_IG) 

#GG
mod_GG <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio+datset_analisi_regressione1$Is_novembre++  datset_analisi_regressione1$Beautyp ,  data = datset_analisi_regressione1, family=GG, control = gamlss.control(n.cyc = 100))
summary(mod_GG)
plot(mod_GG)

#+datset_analisi_regressione1$Is_novembre++  datset_analisi_regressione1$Beautyp

#GIG
mod_GIG <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio,  data = datset_analisi_regressione1, family=GIG, control = gamlss.control(n.cyc = 100))
summary(mod_GIG)
plot(mod_GIG) 

#WEI
mod_WEI <- gamlss(datset_analisi_regressione1$Somma_Final_Price ~ datset_analisi_regressione1$Nvendite+         datset_analisi_regressione1$Is_febbraio+ datset_analisi_regressione1$Beautyp+ datset_analisi_regressione1$Is_novembre,  data = datset_analisi_regressione1, family=WEI, control = gamlss.control(n.cyc = 100))
summary(mod_WEI)
plot(mod_WEI) 

AIC(mod_NORMALE, mod_GA_SOMMA, mod_SN1, mod_BCCG, mod_LNO, mod_IG, mod_GG, mod_GIG, mod_WEI,mod_LOGNO)
BIC(mod_NORMALE, mod_GA_SOMMA, mod_SN1, mod_BCCG, mod_LNO, mod_IG, mod_GG, mod_GIG, mod_WEI, mod_LOGNO)

AIC( mod_GA_SOMMA,  mod_LNO, mod_IG, mod_GG, mod_GIG, mod_WEI, mod_LOGNO)
BIC( mod_GA_SOMMA,  mod_LNO, mod_IG, mod_GG, mod_GIG, mod_WEI, mod_LOGNO)

GAIC(mod_GA_SOMMA,  mod_LNO, mod_IG, mod_GG, mod_GIG, mod_WEI, mod_LOGNO)


AIC(mod_GG, mod_GA_SOMMA)
BIC(mod_GG, mod_GA_SOMMA)
logLik(mod_GA_SOMMA)
logLik(mod_GG)

logLik(mod_GG)
logLik(mod_GA_SOMMA)
logLik(mod_LNO)
logLik(mod_IG)
logLik(mod_GIG)
logLik(mod_WEI)
logLik(mod_LOGNO)

par(mfrow=c(1,2))
histDist(datset_analisi_regressione1$Somma_Final_Price, family=GG, main = "GG", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=GA, main = "GA", nbins = 20)


par(mfrow=c(3,4))
histDist(datset_analisi_regressione1$Somma_Final_Price, family=NO, main= "NO", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=GA, main = "GA", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=SN1, main = "SN1", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=BCCG, main= "BCCG", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=LNO, main = "LNO", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=IG, main = "IG", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=GG, main = "GG", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=GIG, main = "GIG", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=WEI, main = "WEI", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=LOGNO, main ="LOGNO", nbins = 20)

par(mfrow=c(2,4))
histDist(datset_analisi_regressione1$Somma_Final_Price, family=GA, main = "GA", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=LNO, main = "LNO", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=IG, main = "IG", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=GG, main = "GG", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=GIG, main = "GIG", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=WEI, main = "WEI", nbins = 20)
histDist(datset_analisi_regressione1$Somma_Final_Price, family=LOGNO, main ="LOGNO", nbins = 20)




#istogramma con le frequenze di numeri di vendite
ggplot(datset_analisi_regressione1, aes(x = Nvendite)) +
  geom_histogram(bins = 19, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogramm of total sales",
    x = "Sales",
    y = "Frequency"
  ) +
  theme_minimal()

#istogramma con le frequenze di numeri di vendite
ggplot(datset_analisi_regressione1, aes(x = Somma_Final_Price)) +
  geom_histogram(bins = 25, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogramm of total sum of prices",
    x = "Prices",
    y = "Frequency"
  ) +
  theme_minimal()

#istogramma con le frequenze di numeri di vendite
ggplot(datset_analisi_regressione1, aes(x = Beautyp)) +
  geom_histogram(bins = 25, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogramm of the percentages of items sold per day (Beauty)",
    x = "Percentages of item sold per day",
    y = "Frequency"
  ) +
  theme_minimal()

datset_analisi_regressione1_grafico<- datset_analisi_regressione1 %>%
  mutate(
    Evidenziato = case_when(
      Nome_del_giorno == "domenica" ~ "Domenica",
      format(Giorno_Anno, "%m") == "02" ~ "Febbraio",
      TRUE ~ "Altro"
    )
  )

ggplot(datset_analisi_regressione, aes(x = Giorno_Anno, y = Nvendite)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(aes(color = Nome_del_giorno), size = 5) +  # Colore diverso per ogni giorno
  scale_color_manual(values = c("lunedì" = "red", "martedì" = "orange", "mercoledì" = "yellow", 
                                "giovedì" = "green", "venerdì" = "blue", "sabato" = "purple", "domenica" = "pink")) +
  labs(
    title = "Trend of sales",
    x = "Time",
    y = "Sales",
    color = "Days of the week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datset_analisi_regressione1_grafico, aes(x = Giorno_Anno, y = Nvendite)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(aes(color = Evidenziato), size = 5) +  # Evidenzia in base alla nuova colonna
  scale_color_manual(
    values = c(
      "Domenica" = "pink",    # Colore per la domenica
      "Febbraio" = "purple", # Colore per febbraio
      "Other" = "grey"       # Colore per gli altri giorni
    )
  ) +
  labs(
    title = "Trend of sales",
    x = "Time",
    y = "Sales",
    color = "Evidenziato"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datset_analisi_regressione1_grafico, aes(x = Giorno_Anno, y = Somma_Final_Price)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(aes(color = Evidenziato), size = 4) +  # Evidenzia in base alla nuova colonna
  scale_color_manual(
    values = c(
  
      "Febbraio" = "purple", # Colore per febbraio
      "Other" = "grey"       # Colore per gli altri giorni
    )
  ) +
  labs(
    title = "Trend of total revenue",
    x = "Time",
    y = "Sum Final_Price",
    color = "Evidenziato"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#barplot delle vendite per mese
ggplot(datset_analisi_regressione1, aes(x = Giorno_Anno, y = Nvendite)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue") +
  geom_hline(yintercept = 11.23, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Number of transactions per day",
       x = "Month",
       y = "Number of transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# studio per giorno della settimana ---------------------------------------

#calcolo di: somma dei final_price, media dei final_price e numero di vendite per ogni giorno della settimana
ricavi_settimana_giorno <- data12 %>%
  mutate(Giorno = Giorno_Setimana) %>% 
  group_by(Giorno) %>%
  summarise(
    Somma_Final_Price = sum(`Final_Price(Rs.)`),                 
    Media_Final_Price = mean(`Final_Price(Rs.)`),  
    Nvendite = n(),
    .groups = "drop"
  )
ricavi_settimana_giorno
frequenza_giorno <- as.data.frame(table(datset_analisi_regressione1$Nome_del_giorno))
frequenza_giorno
ricavi_settimana_giorno$ricorrenza_giorno <- frequenza_giorno$Freq
ricavi_settimana_giorno
ricavi_settimana_giorno$media_sum_ricorrenza <- ricavi_settimana_giorno$Somma_Final_Price/ricavi_settimana_giorno$ricorrenza_giorno
ricavi_settimana_giorno$media_vendite_ricorrenza <- ricavi_settimana_giorno$Nvendite/ricavi_settimana_giorno$ricorrenza_giorno
ricavi_settimana_giorno

ricavi_settimana_giorno[c(1,5)]
#barplot con la somma dei final_price per giorno della settimana
ggplot(ricavi_settimana_giorno, aes(x = Giorno, y = Somma_Final_Price, fill= Giorno)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Total revenue per day",
    x = "Days",
    y = "Revenue",
    fill = "Days of the week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#barplot con media della somma dei final_price per giorno della settimana
ggplot(ricavi_settimana_giorno, aes(x = Giorno, y = media_sum_ricorrenza, fill= Giorno)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Average total revenue per day",
    x = "Days",
    y = "Average total revenue",
    fill = "Days of the week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#barplot con il numero di transazioni per giorno della settimana
ggplot(ricavi_settimana_giorno, aes(x = Giorno, y = Nvendite, fill= Giorno)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot con barre affiancate
  labs(
    title = "Number of transactions per day",
    x = "Days",
    y = "Number of transactions",
    fill = "Days of the week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#barplot con la media del numero di transazioni per giorno della settimana
ggplot(ricavi_settimana_giorno, aes(x = Giorno, y = media_vendite_ricorrenza, fill= Giorno)) +
  geom_hline(yintercept = 11.23, color = "red", linetype = "dashed", size = 0.5) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot con barre affiancate
  labs(
    title = "Average number of transactions per day",
    x = "Days",
    y = "Number of transactions",
    fill = "Days of the week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#barplot con la media del final_price per giorno della settimana
ggplot(ricavi_settimana_giorno, aes(x = Giorno, y = Media_Final_Price, fill= Giorno)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot con barre affiancate
  labs(
    title = "Mean of the final prices per day",
    x = "Days",
    y = "Average price",
    fill = "Days of the week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

par(mfrow=c(1,1))
#calcolo nuovo colonna col il prezzo scontato dal prezzo pieno
data12$PrezzoScontato <- data12$`Price (Rs.)`*(data12$`Discount (%)`/100)
hist(data12$PrezzoScontato, breaks = 1000)
table(data12$PrezzoScontato)

par(mfrow=c(1,2))
#rappresentazione delle distribuzioni dei due prezzi
hist(data12$`Price (Rs.)`, breaks = 500)
hist(data12$`Final_Price(Rs.)`, breaks = 500)

# Clustering --------------------------------------------------------------

#preparazione del dataset per adatatrlo all'analisi
dati_clustering <- data12
summary(dati_clustering)

dati_cluster1 <- subset(dati_clustering, select= c(3,4,5,6,7,11,12))
summary(dati_cluster1)

#funzione per normalizzare le variabili numeriche in un range da 0 a 1 
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
dati_cluster1[c(2,4)] <- lapply(dati_cluster1[c(2,4)], normalize)

#trasformazione delle variabili character in fattoriali, decisione di gestire la variabile Discount come una variabile fattoriale ordinale (insieme anche ai mesi e ai giorni dell'anno)
dati_cluster1$Category <- as.factor(dati_cluster1$Category)
dati_cluster1$`Discount (%)` <- factor(dati_clustering$`Discount (%)`, 
                                       levels = c(0, 5, 10, 15, 20, 25, 30, 50), 
                                       ordered = TRUE)
dati_cluster1$Payment_Method <- as.factor(dati_cluster1$Payment_Method)
dati_cluster1$Mese_nome<- factor(dati_cluster1$Mese_nome, 
                                 levels = c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", 
                                            "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre"), 
                                 ordered = TRUE)
dati_cluster1$Giorno_Setimana<- factor(dati_cluster1$Giorno_Setimana, 
                                       levels = c("lunedì", "martedì", "mercoledì", "giovedì", 
                                                  "venerdì", "sabato", "domenica"), 
                                       ordered = TRUE)

summary(dati_cluster1)

#utilizzo distanza gower per gestire variabili numeriche e categoriche
gower_dist <- daisy(dati_cluster1, metric = "gower")

#prova di tutti i metodi
ris1 <- hclust(gower_dist, method = "single")
ris2 <- hclust(gower_dist, method = "complete") 
ris3 <- hclust(gower_dist, method = "average") 
ris4 <- hclust(gower_dist, method = "ward.D") 

#dendogrammi
par(mfrow=c(2,2))
plot(ris1)
plot(ris2)
plot(ris3)
plot(ris4)

par(mfrow=c(1,1))
plot(ris3)
rect.hclust(ris3, k = 5)


# Prova diversi numeri di cluster con clustering gerarchico
wss <- numeric()
for (k in 1:10) {
  pam_fit <- pam(gower_dist, k = k, diss=TRUE)
  wss[k] <- pam_fit$objective[2]
}

par(mfrow=c(1,1))
# Grafico dell'Elbow
plot(1:10, wss[1:10], type = "b", xlab = "Number of cluster", ylab = "PAM")



# avg_sil_width <- numeric()
# for (k in 2:10) {
#   pam_fit <- pam(gower_dist, k = k)
#   avg_sil_width[k] <- pam_fit$silinfo$avg.width
# }
# pam_fit$clusinfo
# 
# # Grafico del coefficiente di silhouette
# plot(2:10, avg_sil_width[2:10], type = "b", xlab = "Number of cluster", ylab = "Coefficient of Average silhouette")



#taglio dei dendogrammi a 5 cluster
z1 <- cutree(ris1, k = 5)  
z2 <- cutree(ris2, k = 5)  
z3 <- cutree(ris3, k = 5)  
z4 <- cutree(ris4, k = 5)  

par(mfrow=c(1,1))

plot(dati_clustering1, col= z1)
plot(dati_clustering1, col= z2)
plot(dati_clustering1, col= z3)#buono "average"
plot(dati_clustering1, col= z4)

par(mfrow=c(2,2))
plot(dati_cluster1[c(4,5)], col = z1, main= "Single")
plot(dati_cluster1[c(4,5)], col = z2, main = "Complete")
plot(dati_cluster1[c(4,5)], col = z3, main= "Average")
plot(dati_cluster1[c(4,5)], col = z4,  main= "Ward.D")


clust_ris <- dati_cluster1
clust_ris$clustering <- z3

summary(clust_ris)

par(mfrow=c(1,1))
table(clust_ris[c(5,8)])
risultati <- dati_clustering
risultati$Clustering <- as.factor(clust_ris$clustering)
risultati$Payment_Method <- as.factor(risultati$Payment_Method)
boxplot(risultati$`Final_Price(Rs.)`~ risultati$Clustering)

summary(risultati)

# Seleziona le variabili numeriche
numeric_vars <- c("Price (Rs.)", "Final_Price(Rs.)")

# Trasformazione in formato long per ggplot2
long_data <- melt(risultati, id.vars = "Clustering", measure.vars = numeric_vars)

# Boxplot multipli
ggplot(long_data, aes(x = as.factor(Clustering), y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplot delle Variabili Numeriche per Cluster",
       x = "Cluster",
       y = "Valore",
       fill = "Variabile") +
  theme_minimal()

table_data <- as.data.frame(table(risultati$Payment_Method, risultati$Clustering))
colnames(table_data) <- c("Payment_Method", "Cluster", "Frequency")

# Crea una colonna combinata per Payment_Method e Cluster
table_data$Payment_Cluster <- paste(table_data$Payment_Method, table_data$Cluster, sep = " - Cluster ")

# Ordina i fattori per una migliore leggibilità
table_data$Payment_Cluster <- factor(table_data$Payment_Cluster, 
                                     levels = unique(table_data$Payment_Cluster))

# Grafico classificazione cluster per payment methods
ggplot(table_data, aes(x = Payment_Cluster, y = Frequency, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequencies of payment methods in the clusters",
       x = "Payment method - Cluster",
       y = "Frequency",
       fill = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())  # Rimuove le linee verticali


