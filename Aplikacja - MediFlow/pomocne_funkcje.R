####
library(tidyverse)
library(shiny)
####

# Wczytywanie danych
data <- read_csv2("HR-Employee-Attrition.csv")


# Zmiana typów danych ----------------------
## Konwersja zmiennych na factor
data$Attrition <- as.factor(data$Attrition)
data$BusinessTravel <- as.factor(data$BusinessTravel)
data$Department <- as.factor(data$Department)
data$EducationField <- as.factor(data$EducationField)
data$Gender <- as.factor(data$Gender)
data$JobRole <- as.factor(data$JobRole)
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$Over18 <- as.factor(data$Over18)
data$OverTime <- as.factor(data$OverTime)
data$AgeGroup <- factor(data$AgeGroup, ordered = TRUE)
data$IncomeGroup <- factor(data$IncomeGroup, levels = c("0-2000","2001-4000","4001-6000","6001-8000",
                                                        "8001-10000","10000+"),
                                                        ordered = TRUE)
data$ExperienceGroup <- factor(data$ExperienceGroup, levels = c("0-5","6-10","11-15","16-20","20+"),
                               ordered = TRUE)
data$YearsAtCompanyGroup <- factor(data$YearsAtCompanyGroup, c("0-2","3-5","6-10","11-15","15+"),
                                   ordered = TRUE)
data$HourlyRateGroup <- factor(data$HourlyRateGroup, ordered = TRUE)
data$DistanceGroup <- factor(data$DistanceGroup, c("0-5","6-10","11-20","21-30"),
                             ordered = TRUE)

## Tworzenie zmiennych globalnych -------------------
## Wskaźnik rotacji pracowników 
attritionRate <- round((sum(data$Attrition == "Yes") / nrow(data)) * 100, 2)


## Ilość osób rotujących według płci 
genderAttrition <- data %>% 
  filter(Attrition=="Yes") %>% 
  group_by(Gender) %>% 
  summarise(Count = n())

## Procent osób rotujących, które pracowały w godzinach nadliczbowych
overtimeAttrition <- data %>% 
  filter(Attrition=="Yes") %>% 
  summarise(PercentOvertime = round(mean(OverTime=="Yes")*100,0))

## Średni wynik Work Life Balance osób rotujących
workLifeBalanceAvgAttrition <- data %>% 
  filter(Attrition=="Yes") %>% 
  summarise(AvgWorkLifeBalance=round(mean(WorkLifeBalance),2))

## Obliczanie średniej oceny pracy dla rotujących pracowników
workRateAvgAtrittion <- data %>%
  filter(Attrition == "Yes") %>%
  mutate(AvgWorkRate = (EnvironmentSatisfaction + JobSatisfaction + RelationshipSatisfaction) / 3) %>%
  summarise(AvgWorkRate = round(mean(AvgWorkRate), 2))

## Obliczanie średniej oceny pracy wszystkich pracowników
workRateAvgAll <- data %>% 
  mutate(AvgWorkRate = (EnvironmentSatisfaction + JobSatisfaction + RelationshipSatisfaction) / 3) %>%
  summarise(AvgWorkRate = round(mean(AvgWorkRate), 2))

## Obliczanie średniego wieku osób rotujących
avgAgeAttrition <- data %>% 
  filter(Attrition=="Yes") %>% 
  summarise(AvgAge=round(mean(Age)))

## Obliczanie średniej ilości lat przepracowanych w firmie dla osób rotujących
avgYearsAtCompanyAttrition <- data %>% 
  filter(Attrition=="Yes") %>% 
  summarise(avgYearsAtCompany = round(mean(YearsAtCompany),0))

## Obliczanie ilości rotujących osob
numberOfAttrPp <- data %>% 
  filter (Attrition=="Yes") %>% 
  summarise(Count = n())

## Obliczanie średniego przychodu rotujących pracowników
avgIncome <- data %>% 
  filter(Attrition=="Yes") %>% 
  summarise(AvgIncome = round(mean(MonthlyIncome),2))

## Obliczanie średniego dystansu z domu do biura
avgDistanceFromHome <- data %>% 
  filter(Attrition=="Yes") %>% 
  summarise(AvgDistance = round(mean(DistanceFromHome),2))

## Obliczanie, na którym stanowisku rotuje najwięcej osób
jobRole <- data %>% 
  filter(Attrition=="Yes") %>% 
  group_by(JobRole) %>% 
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
             "Department",
             "JobRole",
             "MaritalStatus",
             "AgeGroup",
             "IncomeGroup",
             "ExperienceGroup",
             "YearsAtCompanyGroup",
             "HourlyRateGroup",
             "DistanceGroup",
             "BusinessTravel")




