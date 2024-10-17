####
library(tidyverse)
####

# Wczytywanie danych
data <- read_csv2("HR-Employee-Attrition.csv")

## Zamiana języka danych
data <- data %>% 
  mutate(across(c(Department, BusinessTravel, JobRole, Gender, MaritalStatus, OverTime, Attrition), ~ case_match(
  .,
  # Zmiana nazw w kolumnie Department
  "Sales" ~ "Sprzedaż",
  "Human Resources" ~ "Zasoby ludzkie",
  "Research & Development" ~ "Badania i rozwój",
  
  # Zmiana nazw w kolumnie BusinessTravel
  "Travel_Rarely" ~ "Rzadkie podróże",
  "Travel_Frequently" ~ "Częste podróże",
  "Non-Travel" ~ "Brak podróży",
  
  # Zmiana nazw w kolumnie JobRole
  "Sales Executive" ~ "Przedstawiciel handlowy",
  "Research Scientist" ~ "Naukowiec badawczy",
  "Laboratory Technician" ~ "Technik laboratoryjny",
  "Manufacturing Director" ~ "Dyrektor ds. produkcji",
  "Healthcare Representative" ~ "Przedstawiciel służby zdrowia",
  "Manager" ~ "Menedżer",

  # Zmiana nazw w kolumnie Gender
  "Female" ~ "Kobieta",
  "Male" ~ "Mężczyzna",
  
  # Zmiana nazw w kolumnie MaritialStatus
  "Single" ~ "Wolny",
  "Married" ~ "Żonaty/Zamężna",
  "Divorced" ~ "Rozwiedziony/Rozwiedziona", 
  
  "Yes" ~ "Tak",
  "No" ~ "Nie", 
  
  "Yes" ~ "Tak",
  "No" ~ "Nie",
  
  .default = .
))) %>% 
  rename(Dział = Department, PodróżeSłużbowe = BusinessTravel, Stanowisko = JobRole, Płeć = Gender,
         StanCywilny = MaritalStatus, Rotacja = Attrition, ZadowolenieZPracy = JobSatisfaction,
         MiesięcznyPrzychód = MonthlyIncome, Nadgodziny = OverTime, GrupaWiekowa = AgeGroup,
         GrupaDochodowa = IncomeGroup, GrupaDoświadczenia = ExperienceGroup,
         PrzedziałLatWFirmie = YearsAtCompanyGroup, StawkaNaGodzine = HourlyRateGroup,
         OdległośćOdDomu = DistanceGroup)


# Zmiana typów danych ----------------------
## Konwersja zmiennych na factor
data$Rotacja <- as.factor(data$Rotacja)
data$PodróżeSłużbowe <- as.factor(data$PodróżeSłużbowe)
data$Dział <- as.factor(data$Dział)
data$Płeć <- as.factor(data$Płeć)
data$Stanowisko <- as.factor(data$Stanowisko)
data$StanCywilny <- as.factor(data$StanCywilny)
data$Nadgodziny <- as.factor(data$Nadgodziny)
data$GrupaWiekowa <- factor(data$GrupaWiekowa, ordered = TRUE)
data$GrupaDochodowa<- factor(data$GrupaDochodowa, levels = c("0-2000","2001-4000","4001-6000","6001-8000",
                                                        "8001-10000","10000+"),
                                                        ordered = TRUE)
data$GrupaDoświadczenia <- factor(data$GrupaDoświadczenia, levels = c("0-5","6-10","11-15","16-20","20+"),
                               ordered = TRUE)
data$PrzedziałLatWFirmie <- factor(data$PrzedziałLatWFirmie, c("0-2","3-5","6-10","11-15","15+"),
                                   ordered = TRUE)
data$StawkaNaGodzine <- factor(data$StawkaNaGodzine, ordered = TRUE)
data$OdległośćOdDomu <- factor(data$OdległośćOdDomu, c("0-5","6-10","11-20","21-30"),
                             ordered = TRUE)


## Tworzenie zmiennych globalnych -------------------
## Wskaźnik rotacji pracowników 
attritionRate <- round((sum(data$Rotacja == "Tak") / nrow(data)) * 100, 2)

## Ilość osób rotujących według płci 
genderAttrition <- data %>% 
  filter(Rotacja=="Tak") %>% 
  group_by(Płeć) %>% 
  summarise(Count = n())

## Procent osób rotujących, które pracowały w godzinach nadliczbowych
overtimeAttrition <- data %>% 
  filter(Rotacja=="Tak") %>% 
  summarise(PercentOvertime = round(mean(Nadgodziny=="Tak")*100,0))

## Średni wynik Work Life Balance osób rotujących
workLifeBalanceAvgAttrition <- data %>% 
  filter(Rotacja=="Tak") %>% 
  summarise(AvgWorkLifeBalance=round(mean(WorkLifeBalance),2))

## Obliczanie średniej oceny pracy dla rotujących pracowników
workRateAvgAtrittion <- data %>%
  filter(Rotacja == "Tak") %>%
  mutate(AvgWorkRate = (EnvironmentSatisfaction + ZadowolenieZPracy + RelationshipSatisfaction) / 3) %>%
  summarise(AvgWorkRate = round(mean(AvgWorkRate), 2))

## Obliczanie średniej oceny pracy wszystkich pracowników
workRateAvgAll <- data %>% 
  mutate(AvgWorkRate = (EnvironmentSatisfaction + ZadowolenieZPracy + RelationshipSatisfaction) / 3) %>%
  summarise(AvgWorkRate = round(mean(AvgWorkRate), 2))

## Obliczanie średniego wieku osób rotujących
avgAgeAttrition <- data %>% 
  filter(Rotacja=="Tak") %>% 
  summarise(AvgAge=round(mean(Age)))

## Obliczanie średniej ilości lat przepracowanych w firmie dla osób rotujących
avgYearsAtCompanyAttrition <- data %>% 
  filter(Rotacja=="Tak") %>% 
  summarise(avgYearsAtCompany = round(mean(YearsAtCompany),0))

## Obliczanie ilości rotujących osob
numberOfAttrPp <- data %>% 
  filter (Rotacja=="Tak") %>% 
  summarise(Count = n())

## Obliczanie średniego przychodu rotujących pracowników
avgIncome <- data %>% 
  filter(Rotacja=="Tak") %>% 
  summarise(AvgIncome = round(mean(MiesięcznyPrzychód),2))

## Obliczanie średniego dystansu z domu do biura
avgDistanceFromHome <- data %>% 
  filter(Rotacja=="Tak") %>% 
  summarise(AvgDistance = round(mean(DistanceFromHome),2))

## Obliczanie, na którym stanowisku rotuje najwięcej osób
jobRole <- data %>% 
  filter(Rotacja=="Tak") %>% 
  group_by(Stanowisko) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  top_n(1, Count) 


# -------------
customSentence_share <- function(numItems, type) {
  paste("Love it? Share it!")
}


## -----------
dropdownMenuCustom <- function(customSentence = NULL, icon = NULL, ...) {
  items <- list(...)
  
  numItems <- length(items)
  
  tags$li(
    class = "dropdown messages-menu",  # Ustal klasę dla typu "messages"
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon,
      tags$span(class = "label label-primary", numItems)  
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}


# ---------------------
choices <- c(
             "Dział",
             "Stanowisko",
             "StanCywilny",
             "GrupaWiekowa",
             "GrupaDochodowa",
             "GrupaDoświadczenia",
             "PrzedziałLatWFirmie",
             "StawkaNaGodzine",
             "OdległośćOdDomu",
             "PodróżeSłużbowe")




