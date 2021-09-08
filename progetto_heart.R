### Segalini Michele & Manzoni Nicolò
### Progetto Heart Disease

# carico la library di tidyverse
library(tidyverse)
library(dbplyr)
library(ggplot2)

# carico il dataset
heart <- read.csv("heart.csv", header = TRUE, stringsAsFactors = FALSE)

# analizzo la struttura del dataset
summary(heart)
# usando tibble
heart_tibble <- as_tibble(heart)
arrange_all(heart_tibble)

# trasformo i valori unspecified nella colonna sex in NA
heart[heart == "unspecified"] <- NA
heart[heart == "undefined"] <- NA


# trasformo i dati in modo tecnicamente corretto
heart$sex <- as.factor(heart$sex)

heart$cp <- as.factor(heart$cp)

# trasformo il tipo di dati della colonna chol da chr a int
heart$chol <- as.numeric(heart$chol)

# rendo factor la colonna fbs
heart$fbs <- as.factor(heart$fbs)

# rendo factor la colonna restecg
heart$restecg <- as.factor(heart$restecg)

# rendo factor la colonna exang
heart$exang <- as.factor(heart$exang)

# rendo factor la colonna slope
heart$slope <- as.factor(heart$slope)

# rendo factor la colonna ca
heart$ca <- as.factor(heart$ca)

# rendo factor la colonna fbs
heart$target <- as.factor(heart$target)

# rendo factor la colonna tal
heart$thal <- as.factor(heart$thal)
# cancello il livello 0 dalla caolonna thal poiche inutile
heart$thal[heart$thal == 0] <- NA
heart$thal <- droplevels(heart$thal)
levels(heart$thal)
str(heart)



# rinomino i livelli delle colonne che sono fattore
heart <- heart %>%
  mutate(
    sex = recode_factor(sex, "0" = "female", "1" = "male"),
    cp = recode_factor(cp, "0" = "typical", "1" = "atypical", "2" = "non-anginal", "3" = "asymptomatic"),
    fbs = recode_factor(fbs, "0" = "<120mg/dl", "1" = ">120mg/dl"),
    restecg = recode_factor(restecg, "0" = "normal", "1" = "stt", "2" = "hypertrophy"),
    slope = recode_factor(slope, "0" = "upsloping", "1" = "flat", "2" = "downsloping"),
    thal = recode_factor(thal, "1" = "normal", "2" = "fixed", "3" = "reversable"),
    heart_disease = recode_factor(target, "0" = "No", "1" = "Yes")
  )

# controllo se ci sono valori NA
summary(heart)

# elimino i valori NA dal dataset
heart <- na.omit(heart)

# rimouovo le colonne ritenute inutili
heart <- subset(heart, select = -x)
str(heart)



# controllo la consistenza dei dati delle colonne
# visualizzo l'istogramma delle frequenze cardiache "thalach"
hist(heart$thalach)
summary(heart$thalach)


# assegno ai valori maggiori di 222 nella colonna thalach la media della colonna stessa
heart$thalach[heart$thalach > 222] <- mean(heart$thalach)

# visualizzo istogramma dopo la modifica
hist(heart$thalach)

# visualizzo una panoramica generica degli outliers del dataset "heart"
boxplot(heart)

# visualizzo il boxplot della colonna "trestbps", "age", "chol", "thalach" e "oldpeak"  prima della modifica
boxplot(heart$trestbps)
boxplot(heart$age)
boxplot(heart$chol)
boxplot(heart$thalach)
boxplot(heart$oldpeak)


# procedo con la rimozione degli outliers
summary(heart$trestbps)
summary(heart$age)
summary(heart$chol)
summary(heart$thalach)
summary(heart$oldpeak)

# terzo quartile
q3_trestbps <- quantile(heart$trestbps, .75)
q3_age <- quantile(heart$age, .75)
q3_chol <- quantile(heart$chol, .75)
q3_thalach <- quantile(heart$thalach, .75)
q3_oldpeak <- quantile(heart$oldpeak, .75)



# primo quartile
q1_trestbps <- quantile(heart$trestbps, .25)
q1_age <- quantile(heart$age, .25)
q1_chol <- quantile(heart$chol, .25)
q1_thalach <- quantile(heart$thalach, .25)
q1_oldpeak <- quantile(heart$oldpeak, .25)

# differenza interquartile
iqr_trestbps <- (q3_trestbps - q1_trestbps)
iqr_age <- (q3_age - q1_age)
iqr_chol <- (q3_chol - q1_chol)
iqr_thalach <- (q3_thalach - q1_thalach)
iqr_oldpeak <- (q3_oldpeak - q1_oldpeak)

# valore minimo
minimum_trestbps <- (q1_trestbps - (1.5 * iqr_trestbps))
minimum_age <- (q1_age - (1.5 * iqr_age))
minimum_chol <- (q1_chol - (1.5 * iqr_chol))
minimum_thalach <- (q1_thalach - (1.5 * iqr_thalach))
minimum_oldpeak <- (q1_oldpeak - (1.5 * iqr_oldpeak))

# valore massimo
maximum_trestbps <- (q3_trestbps + (1.5 * iqr_trestbps))
maximum_age <- (q3_age + (1.5 * iqr_age))
maximum_chol <- (q3_chol + (1.5 * iqr_chol))
maximum_thalach <- (q3_thalach + (1.5 * iqr_thalach))
maximum_oldpeak <- (q3_oldpeak + (1.5 * iqr_oldpeak))

# rimuovo gli outlier attraverso un filtro
heart <- heart[heart$trestbps > minimum_trestbps & heart$trestbps < maximum_trestbps, ]
heart <- heart[heart$age > minimum_age & heart$age < maximum_age, ]
heart <- heart[heart$chol > minimum_chol & heart$chol < maximum_chol, ]
heart <- heart[heart$thalach > minimum_thalach & heart$thalach < maximum_thalach, ]
heart <- heart[heart$oldpeak > minimum_oldpeak & heart$oldpeak < maximum_oldpeak, ]

# visualizzo i boxplot
boxplot(heart$trestbps)
boxplot(heart$age)
boxplot(heart$chol)
boxplot(heart$thalach)
boxplot(heart$oldpeak)



## Analisi descrittiva approfondita
# analizziamo la variabile sex  attraverso il grafico a torta
sex_pie <- ggplot(heart, aes(x = "", fill = sex)) +
  geom_bar(width = 1) +
  labs(title = "Sex Frequency", x = "", y = "") +
  coord_polar(theta = "y") +
  scale_fill_manual(
    values = c("pink", "blue"),
    name = "Legend",
    labels = c("Female", "Male")
  )
sex_pie

# analizziamo la variabile "age" in relazione con sex
age_barplot <- ggplot(heart, aes(x = age)) +
  geom_bar(aes(fill = sex), width = 0.5) +
  labs(title = " Age & Sex", x = "Age", y = "frequence") +
  scale_fill_manual(
    values = c("pink", "blue"),
    name = "Legend",
    labels = c("Female", "Male")
  )
age_barplot


# analizziamo la variabile "cp" in relazione con sex
cp_barplot <- ggplot(heart, aes(x = cp)) +
  geom_bar(aes(fill = sex), width = 0.5) +
  labs(title = "Cp Level", subtitle = " What type of chest pain the patient have?", x = "Cp", y = "frequence") +
  scale_fill_manual(
    values = c("pink", "blue"),
    name = "Legend",
    labels = c("Female", "Male")
  )
cp_barplot

# secondo grafico per cp e heart_disease
cp_disease <- ggplot(data = heart, mapping = aes(x = heart_disease, fill = cp)) +
  geom_bar(stat = "count", position = "fill") +
  labs(
    title = "Heart Disease Prevalence by Chest Pain",
    x = "Heart Disease Status",
    y = "Proportion of Patients"
  )
cp_disease

# analizziamo la variabile "fbs" in relazione con sex
fbs_barplot <- ggplot(heart, aes(x = fbs)) +
  geom_bar(aes(fill = sex), width = 0.5) +
  labs(title = "Fasting Blood Sugar level", subtitle = " After the fasting blood sugar test, the glucose level in the blood was > 120mg/dl?", x = "fbs", y = "frequence") +
  scale_fill_manual(
    values = c("pink", "blue"),
    name = "Legend",
    labels = c("Female", "Male")
  )
fbs_barplot


# analizziamo la variabile "restecg" in relazione con sex
restecg_barplot <- ggplot(heart, aes(x = restecg)) +
  geom_bar(aes(fill = sex, ), width = 0.5) +
  labs(title = "Rest Ecg", subtitle = " What type of result we got from the patient's resitng electrocardiographic?", x = "RestEcg", y = "frequence") +
  scale_fill_manual(
    values = c("pink", "blue"),
    name = "Legend",
    labels = c("Female", "Male")
  )
restecg_barplot


# analizziamo la variabile "trestbps" in relazione con "heart_disease" e "thal"
trestbps_histo <- ggplot(data = heart, mapping = aes(x = trestbps, fill = heart_disease)) +
  geom_histogram(color = "white", bins = 15) +
  facet_grid(heart_disease ~ thal) +
  labs(
    title = "Distribution of Resting Blood Pressure",
    x = "Resting Blood Pressure",
    y = "Number of Patients"
  )
trestbps_histo



# analizziamo la variabile "chol" in relazione con "heart_disease" e "thal"
chol_ggplot <- ggplot(data = heart, mapping = aes(x = age, y = chol, color = heart_disease)) +
  geom_point() +
  facet_grid(heart_disease ~ fbs) +
  labs(title = "Cholesteral vs Age by Heart Disease and Fasting Blood Sugar Levels")
chol_ggplot


# andiamo a visualizzare un grafico lineare
heart_summary_line <- heart %>%
  group_by(age) %>%
  summarise(
    patients = n(),
    avg_max_hr = mean(thalach)
  ) %>%
  arrange(age) %>%
  filter(patients >= 5) # Keep Ages with at least 5 patients

heart_summary_line

# creiamo il grafico lineare con "age" e "avg_max_hr"
thalach_line <- ggplot(data = heart_summary_line, mapping = aes(x = age, y = avg_max_hr)) +
  geom_line(color = "#0072B2") +
  labs(title = "Age vs Avg_max_hr")
thalach_line





## Regressione Lineare

plot(heart$age, heart$chol, xlab = "age ", ylab = "chol (num)")
# inverto l'ordine
reg <- lm(heart$chol ~ heart$age)
# disegno la retta di regressione lineare
abline(reg, col = "blue")
# visualizzo i residui
segments(heart$age, fitted(reg), heart$age, heart$chol, col = "red", lty = 2)
title(main = "Regr. lin tra Age e Colesterolo")
#
summary(reg)
#
confint(reg)
reg_2 <- lm(heart$chol ~ I(heart$age - mean(heart$age)))
summary(reg_2)
# verifico il tipo di relazione tramite r
r <- cor(heart$chol, heart$age)
r
# calcolo r^2
r^2

# analisi dei residui
plot(reg$fitted, reg$residuals, main = "Residui")
abline(0, 0)
## da grafico non mi sembra di vedere pattern particolari

# distribuzione quantili
qqnorm(reg$residuals)
qqline(reg$residuals)








## creo un nuovo dataframe per le previsioni

age <- heart$age

reg <- lm(heart$chol ~ age)

heart_prevision <- data.frame("age" = c(47, 36, 56, 31, 29, 40, 42, 50, 35, 58))


predict(reg, heart_prevision)
predict(reg, heart_prevision, interval = "confidence")






## Algoritmo Machine Learning
install.packages("caret")
library(caret)
install.packages("e1071", dependencies = TRUE)

# creo il dataset
heart_ml <- heart[c(-2:-4, -6:-12, -14)]

dim(heart_ml)

# eseguo alcuni comandi per vedere la struttura del dataset
sapply(heart_ml, class)
head(heart_ml)
str(heart_ml)

# verifichiamo la distribuzione dei vari valori in ogni classe
percentage <- prop.table(table(heart_ml$thal)) * 100
cbind(freq = table(heart_ml$thal), percentage = percentage)

# diamo una occhiata al dataset col comando summary che mostra la media, il mnimo, il massimo, ecc..
summary(heart_ml)

# splitto l'input e l'output
x <- heart_ml[, 1:2]
dim(x)
y <- heart_ml[, 3]
dim(t(y))

# tramite la visualizzazione di un boxplot abbiamo una idea più chiara della distribuzione degli attributi
par(mfrow = c(1, 2))
for (i in 1:2) {
  boxplot(x[, i], main = names(heart_ml)[i])
}

# barplot per la distribuzione delle classi
plot(y)

# boxplot and whiskers plots
# questo grafico ci da una idea piu chiara per vedere le differenze di distribuzioni degli attributi per ogni classe
featurePlot(x = x, y = y, plot = "box")
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = x, y = y, plot = "density", scales = scales, auto.key = list(columns = 3))

# test set
set.seed(1777)

# creo una matrice che contiene 80% delle osservazioni del dataset originale per poi utilizzarla come training set
training_index <- createDataPartition(heart_ml$thal, p = .80, list = FALSE)

# seleziono 80% dei dati per il training dei 6 modelli di algoritmi
training_set <- heart_ml[training_index, ]
nrow(training_set)

# usiamo il rimanente 20% dei dati per il test
test_set <- heart_ml[-training_index, ]
nrow(test_set)

## Utilizziamo il metodo *"Accuracy"* per creare i diversi modelli.
# Questa rappresenta il rapporto tra il numero delle osservazioni corrette previste 
# diviso per il numero totale di osservazionicontenute nel dataset, moltiplicato per 100(e.g. 95% accurate).
seed <- set.seed(1777)
control <- trainControl(method = "cv", number = 10, seed = seed)
metric <- "Accuracy"

## proviamo gli algoritmi

# algoritmi lineari
## LDA
fit_lda <- train(thal ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "lda"
)

# algoritmi non lineari
## CART
fit_cart <- train(thal ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "rpart"
)
## kNN
fit_knn <- train(thal ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "knn"
)
## MLP
fit_mlp <- train(thal ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "mlp"
)

# algoritmi avanzati
## Random Forest
fit_rf <- train(thal ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "rf"
)
## SVM
fit_svm <- train(thal ~ .,
  data = training_set, metric = metric, trControl = control,
  method = "svmRadial"
)

# visualizziamo tramite il comando summary accuracy of models
# qui di seguito visualizziamo l'accuratezza di ogni modello and also other metrics like Kappa
results <- resamples(list(lda = fit_lda, cart = fit_cart, knn = fit_knn, mlp = fit_mlp, rf = fit_rf, svm = fit_svm))
summary(results)

# plot per vedere meglio i risultati degli algoritmi
dotplot(results)

# summarize best results
fit_knn$results

# Il modello LDA risulta essere il più accurato 


