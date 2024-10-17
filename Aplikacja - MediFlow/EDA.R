# Wczytywanie potrzebnych bibliotek
library(dygraphs)
library(shiny)
library(tidyverse)
library(ggplot2)

# Wczytanie danych do ramki danych 
theData <- read.csv("HR-Employee-Attrition.csv")

## EDA
# Sprawdzenie struktury danych
str(theData)

# Sprawdzanie zmiennych numerycznych 
names(theData)[sapply(theData, is.numeric)]

# Sprawdzanie zmiennych tekstowych
names(theData)[sapply(theData,function(x) !is.numeric(x))]

# Podsumowanie zmiennych
summary(theData)

# Sprawdzenie brakujących wartości 
anyNA(theData)

# Sprawdzanie duplikatów
theData[duplicated(theData),]

# Wyświetlenie 6 pierwszych wierszy
head(theData)

# Zmiana typów danych
## Konwersja wybranych kolumn na "factor" 
theData$Attrition <- as.factor(theData$Attrition)
theData$BusinessTravel <- as.factor(theData$BusinessTravel)
theData$Department <- as.factor(theData$Department)
theData$EducationField <- as.factor(theData$EducationField)
theData$Gender <- as.factor(theData$Gender)
theData$JobRole <- as.factor(theData$JobRole)
theData$MaritalStatus <- as.factor(theData$MaritalStatus)
theData$Over18 <- as.factor(theData$Over18)
theData$OverTime <- as.factor(theData$OverTime)

# Dodanie przedziałów wiekowych 
theData <- theData %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(0, 25, 35, 45, 55, 65, Inf), 
                        labels = c("0-25", "26-35", "36-45", "46-55", "56-65", "65+"),
                        right = FALSE))

# Dodanie przedziałów dochodów miesięcznych 
theData <- theData %>%
  mutate(IncomeGroup = cut(MonthlyIncome, 
                           breaks = c(0, 2000, 4000, 6000, 8000, 10000, Inf), 
                           labels = c("0-2000", "2001-4000", "4001-6000", "6001-8000", "8001-10000", "10000+"),
                           right = FALSE))

# Dodanie przedziałów dla ogólnej liczby lat pracy 
theData <- theData %>%
  mutate(ExperienceGroup = cut(TotalWorkingYears, 
                               breaks = c(0, 5, 10, 15, 20, Inf), 
                               labels = c("0-5", "6-10", "11-15", "16-20", "20+"),
                               right = FALSE))

# Dodanie przedziałów dla lat pracy w firmie
theData <- theData %>%
  mutate(YearsAtCompanyGroup = cut(YearsAtCompany, 
                                   breaks = c(0, 2, 5, 10, 15, Inf), 
                                   labels = c("0-2", "3-5", "6-10", "11-15", "15+"),
                                   right = FALSE))

# Dodanie przedziałów dla stawek godzinowych 
theData <- theData %>%
  mutate(HourlyRateGroup = cut(HourlyRate, 
                               breaks = c(0, 30, 60, 90, 120, 150, 180, 210, Inf), 
                               labels = c("0-30", "31-60", "61-90", "91-120", "121-150", "151-180", "181-210", "210+"),
                               right = FALSE))

# Dodanie przedziałów odległości od domu
theData <- theData %>%
  mutate(DistanceGroup = cut(DistanceFromHome, 
                             breaks = c(0, 5, 10, 20, 30, Inf), 
                             labels = c("0-5", "6-10", "11-20", "21-30", "30+"),
                             right = FALSE))

# Wizualizacja danych 
## Wiek
ggplot(theData, aes(x=Age)) +   geom_histogram(bins = 40, fill = "lightblue", color = "white") +
  scale_x_continuous(breaks = seq(min(theData$Age), max(theData$Age), by = 1)) +
  labs(title = "Histogram wieku", x = "Wiek", y = "Częstotliwość") +
  theme_minimal()

## Rozkład pracowników ze względu na departament 
departamenty_procentowo <- theData %>%
  count(Department, name = "Liczba") %>%
  mutate(Procent = round((Liczba / sum(Liczba)) * 100,1))


ggplot(departamenty_procentowo, aes(x="", y=Liczba, fill=Department)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Procent,"%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Rozkład departamentów", x = NULL, y = NULL) +
  theme_void()

## Rozkład pracowników ze względu na kierunek ukończonych studiów

edukacja_procentowo <- theData %>% 
  count(EducationField, name = "Liczba") %>% 
  mutate(Procent = round((Liczba / sum(Liczba))*100,1))

ggplot(edukacja_procentowo, aes(x="", y=Liczba, fill=EducationField)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Procent, "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Rozkład kierunku ukończonych studiów", x = NULL, y = NULL) +
  theme_void()      

## Rozkład pracowników ze względu na stanowisko

### Tworzę ramkę danych zawierającą stanowiska i ich ilości w firmie
ilosc_danego_stanowiska <- theData %>% count(JobRole, name = "Ilość") %>% arrange(desc(Ilość))

### Zmieniam typ danej JobRole na czynnik oraz porządkuje na podstawie ilości wystąpień
ilosc_danego_stanowiska$JobRole <- factor(ilosc_danego_stanowiska$JobRole,
                                          levels = ilosc_danego_stanowiska$JobRole[order(-ilosc_danego_stanowiska$Ilość)])

ggplot(ilosc_danego_stanowiska, aes(x=JobRole, y=Ilość)) + geom_col(fill="lightgreen") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90")) + 
  labs(x="Stanowisko", y="Ilość obsadzonych stanowisk w firmie", 
       title="Rozkład pracowników ze względu na stanowisko")

## Zależność pomiędzy miesięcznym przychodem a zajmowanym stanowiskiem i płcią

ggplot(theData, aes(x=JobRole, y=MonthlyIncome, fill=Gender)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90")) + 
  labs(x="Stanowisko", y="Miesięczny przychód", 
       title="Zależność pomiędzy miesięcznym przychodem a zajmowanym stanowiskiem i płcią")


## Zależność pomiędzy miesięcznym przychodem a zajmowanym stanowiskiem 

ggplot(theData, aes(x=MonthlyIncome, y=JobRole)) + geom_point(color="orange") + 
  theme_minimal() + labs(x="Miesięczny przychód", y="Stanowisko", 
                         title="Zależność pomiędzy miesięcznym przychodem a stanowiskiem")

## Zależność pomiędzy miesięcznym przychodem a wiekiem

ggplot(theData, aes(x=Age, y=MonthlyIncome)) + geom_point(color="purple") + 
  geom_hline(yintercept = min(theData$MonthlyIncome), 
             linetype = "dashed", color = "red", size = 1) +  # linia dla minimum
  geom_hline(yintercept = max(theData$MonthlyIncome), 
             linetype = "dashed", color = "darkgreen", size = 1) + # linia dla maximum
  theme_minimal() + labs(x="Wiek", y="Miesięczny przychód", 
                         title="Zależność pomiędzy miesięcznym przychodem a wiekiem")

## Zapisanie danych w pliku zewnętrznym
write_csv2(theData, "HR-Employee-Attrition.csv")
