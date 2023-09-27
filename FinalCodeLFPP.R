# R code for 'When marriage become unattainable: a cohort analysis --------

setwd("C:/Users/hanna/OneDrive/Finals/Diss/R")


library(gtsummary)
library(webshot)
library(haven)
library(gt)
library(tidyverse)

#load Socio Dem Surveillance 2019 release dataset
base <-read_dta('AHRI.dta')
load('base') #SocioDem dataset
load('pregSEM') #Pregnancies 2019 dataset
load('wgh') #Women's General Health dataset

# Figure 1 ----------------------------------------------------------------
# Lexis diagram displaying the cohorts used in analysis and the period over 
# which data is collected in the AHRI HDSS

library('LexisPlotR')
library('pBrackets')


lexisGrid <- lexis_grid(year_start = 1945, year_end = 2020, age_start = 0, age_end = 75, d=10, lwd=0.5)
lexisGridFill <- lexis_year(lg = lexisGrid, delta = 19, year = 2000, alpha = 0.7, fill = "grey55")

lexisGridFill <- lexis_cohort(lg = lexisGridFill, cohort = 1945, delta = 20, alpha = .7, fill = "#54438E")
lexisGridFill <- lexis_cohort(lg = lexisGridFill, cohort = 1965, delta = 20, alpha = .7, fill = '#587DBA')
lexisGridFill <- lexis_cohort(lg = lexisGridFill, cohort = 1985, delta = 10, alpha = .7, fill = "#20934A")


lexisGridFill + 
  labs(y="Age", x="Year") + 
  theme(axis.title.x = element_text(margin = margin(t = 25), size = 25, 
                                    colour = grey(level = 0.4), hjust = 0.1),
        axis.title.y = element_text(margin = margin(r = 25), size = 25, 
                                    colour = grey(level = 0.4),hjust = 0.1),
        axis.text = element_text(size = 14))+
  geom_segment(aes(x = as.Date(7400-01-01, origin = '1970-01-01'), y = 0, xend = as.Date(12900, origin = '1970-01-01'), yend = 15),
               arrow = arrow(), size = 3,  linejoin = c('round', 'mitre', 'bevel'), colour = 'grey10')+
  geom_segment(aes(x = as.Date(1975, origin = '1970-01-01'), y = 0, xend = as.Date(12900, origin = '1970-01-01'), yend = 30),
               arrow = arrow(), size = 3,  linejoin = c('round', 'mitre', 'bevel'), colour = 'grey10')+
  geom_segment(aes(x = as.Date(-5300, origin = '1970-01-01'), y = 0, xend = as.Date(12950, origin = '1970-01-01'), yend = 50),
               arrow = arrow(), size = 3,  linejoin = c('round', 'mitre', 'bevel'), colour = 'grey10')+
  theme(plot.tag.position = c(0.9, 0.03), 
        plot.tag = element_text(colour = grey(0.3), size = 20))



# Table 1 ----------------------------------------------------------------



tbl1_base <- base %>%
  #restrict dataset to selected cohorts
  filter(DoB < '1995-01-01' & DoB >'1945-01-01')%>%
  #create highest education and Generation variable
  group_by(IndividualId)%>%
  fill(MaritalStatus, direciton = 'updown')%>%
  #select only those with at least 1 obs of marital status and education
  filter(!is.na(MaritalStatus))%>%
  mutate(EduMax = max(Education, na.rm = T))%>%
  ungroup()%>%
  mutate(Edugroupmax = 
           ifelse(EduMax < 8, 'Primary or less',
                  ifelse(EduMax >=8 & EduMax < 12, 'Some secondary', 
                         ifelse(EduMax == 12, 'Matric', 'Some tertiary')))) %>%
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse(DoB > '1985-01-01' & DoB < '1995-01-01','Gen3', 'Other'))))%>%
  mutate(Edugroupmax = factor(Edugroupmax, 
                              levels = c('Primary or less', 'Some secondary', 'Matric', 'Some tertiary'),
                              ordered = T))%>%
  filter(Generation != 'Other')%>%
  #Label each observation with Year of obs
  separate(StartDate, into = 'Year', sep = '-', remove = F)

tbl1_Female <- tbl1_base %>%
  group_by(IndividualId)%>%
  slice(1)%>%
  ungroup()%>%
  select(Sex, Generation, Edugroupmax)%>%
  mutate(Sex = unclass(Sex))%>%
  ungroup()

tbl1_FHHH <- tbl1_base %>%
  filter(HHRelation == 1)%>%
  group_by(HouseholdId, IndividualId)%>%
  slice(1)%>%
  ungroup()%>%
  mutate(FHHH = ifelse(Sex == 2, 'Female-headed HHs', '0'))%>%
  select(FHHH)


tbl1_Employ <-   tbl1_base %>%
  filter(HHRelation == 1)%>%
  filter(!is.na(CurrentlyEmployed), CurrentlyEmployed !=0)%>%
  group_by(HouseholdId, Year)%>%
  slice_min(CurrentlyEmployed, with_ties = F)%>%
  ungroup()%>%
  select(CurrentlyEmployed, HouseholdId, Generation)%>%
  mutate(CurrentlyEmployed = ifelse(CurrentlyEmployed == 1, 'Full-time',
                                    ifelse(CurrentlyEmployed == 2, 'Part-time',
                                           'Unemployed')))%>%
  select(CurrentlyEmployed)


tbl1_Nonresident <- tbl1_base %>%
  group_by(IndividualId, Year)%>%
  slice_max(NonResident, with_ties = F)%>%
  ungroup()%>%
  select(Generation, NonResident)%>%
  mutate( NonResident = unclass(NonResident))




tbl1_row1 <- tbl_summary( tbl1_Female,
                          by = Generation,
                          value = list(Sex ~ '2'),
                          label  = list(Sex ~ 'Female',
                                        Edugroupmax ~ "Highest level of education achieved"),
                          statistic = list(
                            all_categorical() ~ "{p}% ({n})"))%>%
  add_overall()%>%
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()%>%
  modify_spanning_header(
    c(stat_1, stat_2, stat_3) ~ 
      "**Generation**")%>%
  modify_header(
    stat_1 = "   **1945-65**,    N = 22,720",
    stat_2 = "   **1965-85**,    N = 56,971",
    stat_3 = "   **1985-95**,    N = 45,216",
    stat_0 = "   **Overall**,   N = 124,907",
    label = '')

tbl1_row2 <- tbl_summary(tbl1_FHHH,
                         value = list(FHHH ~ 'Female-headed HHs'),
                         label = list(FHHH ~ 'Female-headed HHs'),
                         statistic = FHHH ~ "{p}% ({n})")%>%
  bold_labels() 

tbl1_row3 <- tbl_summary(tbl1_Nonresident,
                         by = Generation,
                         label= list(NonResident ~ 'Non-resident from their household per year'),
                         value = list(NonResident ~ 1),
                         statistic = NonResident ~ "{p}%")%>%
  add_overall()%>%
  bold_labels() 

tbl1_row4 <- tbl_summary(tbl1_Employ,
                         label= list(CurrentlyEmployed ~ 'Household head currently employed'),
                         statistic = list(CurrentlyEmployed ~ "{p}%"))%>%
  bold_labels()


#Table 1 
tbl1_stack_summary <- tbl_stack(list(tbl1_row1, tbl1_row2, tbl1_row3, tbl1_row4))%>%
  as_gt()%>%
  cols_width(label ~ pct(40),
             stat_0 ~pct(15))%>%
  tab_options(table.width = pct(55))

gtsave(data = tbl1_stack_summary, filename = 'summarytbl.png', vheight = 1800)


# Figure 2 ----------------------------------------------------------------
library(lubridate)
library(ggpubr)

base <- read_dta('AHRI.dta')%>%
  #create calendar year variable
  separate(StartDate, into = 'Year', remove = F, sep = '-')

AllMarEdu <- base %>%
  filter(!is.na(MaritalStatus))%>% 
  mutate(WasMarried = ifelse(grepl('2|3|4', MaritalStatus), '1', '0'))%>%
  group_by(IndividualId)%>%
  mutate(EverMarried = max(WasMarried))%>%
  ungroup()%>%
  transform(Year = as.numeric(Year))%>%
  mutate(Age = floor(Year - decimal_date(DoB)))

AllMarEdu1 <- AllMarEdu %>%
  mutate(AgeAtObs = decimal_date(EducationObsDate) - decimal_date(DoB))


AllMarEdu2  <- AllMarEdu1 %>%
  group_by(IndividualId)%>%
  mutate(EduMax = max(Education, na.rm = T))%>%
  ungroup()

AllMarEdu3 <- AllMarEdu2 %>%
  group_by(IndividualId)%>%
  slice_max(EducationObsDate, with_ties = F)

AllMarEdugroup <- AllMarEdu3%>%
  ungroup()%>%
  mutate(Edugroupmax = 
           ifelse(EduMax < 8, 'Primary\nor less',
                  ifelse(EduMax >=8 & EduMax < 12, 'Some \nsecondary', 
                         ifelse(EduMax == 12, 'Matric', 'Some \ntertiary')))) %>%
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse(DoB > '1985-01-01' & DoB < '1995-01-01','Gen3', 'Other'))))

AllMarEdugroup1 <- AllMarEdugroup%>%
  filter(AgeAtObs > 23)


AllMarEduPropEdu <- AllMarEdugroup1 %>%
  ungroup()%>%
  count(Generation, Sex, Edugroupmax)

EverMarEduPropEdu <- AllMarEdugroup1 %>%
  ungroup()%>%
  filter(EverMarried == 1)%>%
  count(Generation, Sex, Edugroupmax)%>%
  rename(MarEver = n)

MarEverAggrEdu <- left_join(EverMarEduPropEdu, AllMarEduPropEdu, by =  c('Sex', 'Generation',
                                                                         'Edugroupmax'))%>%
  mutate(MarProp = MarEver/n)


GenNames <- c('Gen1' = 'Generation 1945-65',
              'Gen2' = 'Generation 1965-85' ,
              'Gen3' = 'Generation 1985-95')

MarEverAggrEdu$Edugroupmax <- factor(MarEverAggrEdu$Edugroupmax, levels = c('Primary\nor less', 'Some \nsecondary', 'Matric', 'Some \ntertiary'),
                                     ordered = T)


#Figure 2a
grd_yminor1 <- seq(0,1, length.out = 11)
grd_y1 <- seq(0.05,0.95, length.out = 10)

ggEduMarEver <- ggplot(subset(MarEverAggrEdu, Generation != 'Other'))+
  geom_col(mapping = aes(Edugroupmax, MarProp, fill = factor(Sex)),
           position = 'dodge')+
  facet_wrap(~Generation, labeller = as_labeller(GenNames))+
  theme(axis.title.y = element_text(margin = margin(r = 10), size = 15, 
                                    colour = 'grey30'),
        axis.title.x = element_text(margin = margin(t = 20), size = , 
                                    colour = 'grey30'),
        legend.title = element_text(colour = 'grey30', size = 15),
        legend.text = element_text(colour = 'grey30', size = 15),
        axis.text = element_text(colour = 'grey30', size = 13),
        legend.key.size = unit(1, 'cm'),
        legend.position = 'none',
        strip.text.x = element_text(size = 20, colour = 'grey30'),
        strip.background = element_rect('white'),
        plot.margin = unit(c(0.5,0.5,0,1),'cm'),
        #      panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  )+
  labs( x= '', y = 'Proportion of people who have \never been married')+
  scale_fill_manual(name = '', labels = c('Male', 'Female'),values = c( "#5ab4ac", '#D15439'))+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), limit = c(0,1))+
  #geom_hline(yintercept = grd_y1, col = "white", linewidth = 0.5)+
  geom_hline(yintercept = grd_yminor1, col = "white", linewidth = 0.05)




# relative advantage of edu for marriage 
MarEverAggrEdu2 <- MarEverAggrEdu %>%
  group_by(Generation, Sex)%>%
  mutate(Ref = MarProp[Edugroupmax == 'Matric'])%>%
  ungroup()%>%
  mutate(Adv = MarProp/Ref,
         Adv = Adv - 1)

MarEverAggrEdu2$Edugroupmax <- factor(MarEverAggrEdu2$Edugroupmax, 
                                      levels = c('Primary\nor less', 'Some \nsecondary', 'Matric', 'Some \ntertiary'),
                                      ordered = T)

#Figure 2b
grd_yminor <- seq(-0.75,1.75, length.out = 6)
grd_y <- seq(-1,2, length.out = 7)

ggEduMarEverAdv <- ggplot(subset(subset(MarEverAggrEdu2, Generation != 'Other'), Edugroupmax != 'Matric'))+
  geom_col(mapping = aes(x = Edugroupmax, y = Adv, fill = factor(Sex)),
           position = 'dodge')+
  facet_wrap(~Generation, labeller = as_labeller(GenNames))+
  theme(axis.title.y = element_text(margin = margin(r = 10), size = 15, 
                                    colour = 'grey30'),
        axis.title.x = element_text(margin = margin(t = 20), size = 20, 
                                    colour = 'grey30'),
        legend.title = element_text(colour = 'grey30', size = 15),
        legend.text = element_text(colour = 'grey30', size = 15),
        axis.text = element_text(colour = 'grey30', size = 13),
        legend.key.size = unit(1, 'cm'),
        legend.position = 'bottom',
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        plot.margin = unit(c(0.75,0.5,0.5,0.75),'cm'))+
  scale_fill_manual(name = '', labels = c('Male', 'Female'),values = c( "#5ab4ac", '#D15439'))+
  labs( x= 'Education level', y = 'Difference in likelihood of being ever \nmarried relative to Matric marriage rate')+
  scale_y_continuous(breaks = c(-0.5,0,0.5,1,1.5), limit = c(-0.75,1.75))+
  geom_hline(yintercept = grd_y, col = "white", linewidth = 0.5) +
  geom_hline(yintercept = grd_yminor, col = "white", linewidth = 0.5)




#Figure 2 combined
ggarrange(ggEduMarEver, ggEduMarEverAdv, ncol = 1, labels = c("a)","b)"),
          font.label = list(size = 20, color = 'grey30'), hjust = 0.075)

# Figure 3 ----------------------------------------------------------------

Mothers <- AllMarEdugroup1

Daughters <- AllMarEdugroup1 %>%
  filter(DoB >= '1985-01-01' & DoB <= '1995-01-01') %>%
  filter(!is.na(MotherId)) %>%
  select(IndividualId, MotherId, DoB, Sex, Education, EducationObsDate,
         MaritalStatus, EverMarried)


MothersDaughters <- left_join(x = Mothers, y= Daughters, 
                              by = c("IndividualId" = 'MotherId'))


MothersDaughters <- MothersDaughters %>%
  rename(DoB = DoB.x,
         childDoB = DoB.y,
         Sex = Sex.x,
         SexOfChild = Sex.y) %>%
  filter(!is.na(childDoB))%>%
  mutate(AgeAtBirth = decimal_date(childDoB)-decimal_date(DoB))%>%
  filter(!is.na(MaritalStatus.x))%>% #this is fine, because only need last obs MarStatus in the end
  mutate(WasMarried.x = ifelse(grepl('2|3|4', MaritalStatus.x), '1', '0'))%>%
  group_by(IndividualId)%>%
  mutate(EverMarried.x = max(WasMarried.x)) # creating evermarriage for mothers

MothersDaughtersslice <- MothersDaughters %>%
  group_by(IndividualId.y)%>%
  slice(1) %>%
  select(-c(MotherId:LocationId))%>% # selecting only one obs of a daughter-mother pair
  filter(SexOfChild == 2) # selecitng only women 

# Graph for education - 1985 cohort

EduNames <- c("Primary\nor less" = "Primary\nor less",
              "Some \nsecondary" = "Some \nsecondary", 
              "Matric" = "Matric", 
              'Some \ntertiary' = 'Some \ntertiary')



AggrMotherDaugthersMar <- MothersDaughtersslice %>%
  ungroup()%>%
  count(EverMarried.x, EverMarried.y, Edugroupmax)%>%
  mutate( EverMarried.x = ifelse(EverMarried.x == 0, 'Never \nMarried', 'Ever \nMarried'))

AggrMotherDaugthersMar$Edugroupmax <- factor(AggrMotherDaugthersMar$Edugroupmax, 
                                             levels = c('Primary\nor less', 'Some \nsecondary', 'Matric', 'Some \ntertiary'),
                                             ordered = T)


Figure3 <- ggplot(subset(AggrMotherDaugthersMar, !is.na(Edugroupmax)))+
  geom_col(aes(x = EverMarried.x, y= n, group = EverMarried.y, 
               fill = EverMarried.y), position = 'fill')+
  facet_wrap(~Edugroupmax, labeller = as_labeller(EduNames), ncol = 4)+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))+
  scale_fill_manual(name = "Daughter's marital status", labels = c('Never married', 'Ever married'),
                    values = c('grey75', '#20934A'))+
  theme(strip.background = element_rect('#587DBA'),
        legend.position = 'bottom',
        panel.spacing = unit(0.1, "lines"),
        axis.title.y = element_text(margin = margin(r = 18), size = 18, 
                                    colour = 'grey30'),
        axis.title.x = element_text(margin = margin(t = 10), size = 20, 
                                    colour = 'grey30'),
        plot.subtitle = element_text( size = 19, 
                                      colour = 'grey30', hjust = 0.5, margin = margin(b = 10)),
        legend.title = element_text(colour = 'grey30', size = 15),
        legend.text = element_text(colour = 'grey30', size = 15),
        axis.text = element_text(colour = 'grey30', size = 15),
        legend.key.size = unit(1, 'cm'),
        strip.text.x = element_text(size =20, colour = 'white'))+
  labs( x= "Mother's marital status", y = 'Proportion of daughters ever married', 
        subtitle = "Mother's education level")


ggplot_build(Figure3)


# Figure 4 ----------------------------------------------------------------
library('lubridate')

base1 <- base %>%
  separate(StartDate, into = 'Year', sep = '-', remove = F)


preDate <- data.frame(expand.grid(1945: 2000,
                                  IndividualId = unique(base$IndividualId)))%>%
  rename(Year = Var1)

columns <- data.frame(x = colnames(base1), y = NA)%>%
  pivot_wider(names_from = x, values_from = y)

joinable_rows <- left_join(preDate, columns, by = c('IndividualId', 'Year'))%>%
  select(c(Node, IndividualId, Sex:StartDate, Year, EndDate:LabourObsDate))

base2 <- rbind(base1, joinable_rows)


#done!

base3 <- base2 %>%
  group_by(IndividualId)%>%
  fill(DoB,DoD, MotherId, .direction = 'downup')%>%
  ungroup()%>%
  transform(Year = as.numeric(Year))%>%
  filter(Year >= decimal_date(DoB))%>%
  filter(Year <= decimal_date(DoD) | is.na(DoD))

base5 <- base3 %>%
  arrange(IndividualId, Year)%>%
  group_by(IndividualId)%>%
  fill(Episode, .direction = 'up')%>%
  fill(Sex, .direction = 'updown')%>%
  ungroup()%>%
  filter(!is.na(Episode))

base6 <- base5 %>%
  group_by(IndividualId, Year)%>%
  slice(1)


base11 <- base6%>%
  ungroup()%>%
  mutate(Age = as.numeric(Year) - decimal_date(DoB))

base12 <- base11%>%
  mutate(Agegroup = ifelse(Age < 15, 'xu15',
                           ifelse(Age >=15 & Age <20, 'x1519',
                                  ifelse(Age >=20 & Age <25, 'x2024',
                                         ifelse(Age >=25 & Age <30, 'x2529',
                                                ifelse(Age >=30 & Age <35, 'x3034',
                                                       ifelse(Age >=35 & Age <40, 'x3539',
                                                              ifelse(Age >=40 & Age <45, 'x4044',
                                                                     ifelse(Age >=45 & Age <50, 'x4549', 'xo50')))))))))%>%
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse (DoB >= '1985-01-01' & DoB < '1995-01-01','Gen3','Other'))))

base12<- base12 %>%
  select(-c(EndDate:Memberships))%>%
  arrange(IndividualId, Year)

base15 <- base12 %>%
  ungroup()%>%
  filter(!grepl('xu15|xo50', Agegroup))%>%
  filter(Generation != 'Other')

base16 <- base15 %>%
  ungroup()%>%
  filter(IndividualId %in% pregSEM$WomanId)

ASpersonyears <- base16%>%
  ungroup()%>%
  filter(Year < 2018)%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)%>%
  select(-Generation)


EvMar <- pregSEM %>%
  select(WomanId, EverMarried)%>%
  group_by(WomanId)%>%
  slice(1)

ASpersonyearsM <- base16%>%
  ungroup()%>%
  left_join(EvMar, by = c('IndividualId' = 'WomanId'))%>%
  filter(Year < 2018,
         EverMarried == 1)%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)%>%
  select(-Generation)

ASpersonyearsNM <- base16%>%
  ungroup()%>%
  left_join(EvMar, by = c('IndividualId' = 'WomanId'))%>%
  filter(Year < 2018,
         EverMarried == 0)%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)%>%
  select(-Generation)

ASbirthsM <- pregSE2 %>%
  filter(!grepl('xu15|xo50', Agegroup))%>%
  filter(grepl('L|M|F', Outcome))%>%
  filter(Generation != 'Other', 
         EverMarried == 1)%>%
  ungroup()%>%
  filter(EverMarried ==1)%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)

ASbirthsNM <- pregSE2 %>%
  filter(!grepl('xu15|xo50', Agegroup))%>%
  filter(grepl('L|M|F', Outcome))%>%
  filter(Generation != 'Other', 
         EverMarried == 0)%>%
  ungroup()%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)

ASFRcalcM <- cbind(ASbirthsM, ASpersonyearsM)%>%
  mutate(ASFR1519 = x1519B *5/x1519,
         ASFR2024 = x2024B *5/x2024,
         ASFR2529 = x2529B *5/x2529,
         ASFR3034 = x3034B *5/x3034,
         ASFR3539 = x3539B *5/x3539,
         ASFR4044 = x4044B *5/x4044,
         ASFR4549 = x4549B *5/x4549)%>%
  mutate(TFR = ASFR1519+ ASFR2024+ ASFR2529+ ASFR3034+ ASFR3539+ ASFR4044+ ASFR4549)

ASFRcalcNM <- cbind(ASbirthsNM, ASpersonyearsNM)%>%
  mutate(ASFR1519 = x1519B *5/x1519,
         ASFR2024 = x2024B *5/x2024,
         ASFR2529 = x2529B *5/x2529,
         ASFR3034 = x3034B *5/x3034,
         ASFR3539 = x3539B *5/x3539,
         ASFR4044 = x4044B *5/x4044,
         ASFR4549 = x4549B *5/x4549)%>%
  mutate(TFR = ASFR1519+ ASFR2024+ ASFR2529+ ASFR3034+ ASFR3539+ ASFR4044+ ASFR4549)



ASFRM <- ASFRcalcM %>%
  rename('15-19' = ASFR1519,
         '20-24' = ASFR2024,
         '25-29' = ASFR2529,
         '30-34' = ASFR3034,
         '35-39' = ASFR3539,
         '40-44' = ASFR4044,
         '45-49' = ASFR4549)%>%
  select(c(Generation, '15-19':TFR))%>%
  pivot_longer(cols = '15-19':'45-49', names_to = 'Agegroup')%>%
  mutate(EverMarried = 'Y')

ASFRM[18,4] <- NA
ASFRM[14,4] <-NA       #removing age-groups for which less than half women in generation have attained



ASFRNM <- ASFRcalcNM %>%
  rename('15-19' = ASFR1519,
         '20-24' = ASFR2024,
         '25-29' = ASFR2529,
         '30-34' = ASFR3034,
         '35-39' = ASFR3539,
         '40-44' = ASFR4044,
         '45-49' = ASFR4549)%>%
  select(c(Generation, '15-19':TFR))%>%
  pivot_longer(cols = '15-19':'45-49', names_to = 'Agegroup')%>%
  mutate(EverMarried = 'N')

ASFRNM[18,4] <- NA
ASFRNM[14,4] <-NA       #removing age-groups for which less than half women in generation have attained


X <- rbind(ASFRNM, ASFRM)
# WORKS!

ggplot(X)+
  geom_line(mapping = aes(Agegroup, value, group = interaction(Generation, EverMarried),
                          colour = Generation, 
                          linetype = EverMarried),
            lwd = 2, alpha = 0.75)+
  geom_point(mapping = aes(Agegroup, value, group = Generation, colour = Generation,
                           shape = Generation), 
             size = 5, alpha = 0.9)+
  scale_color_manual(name = 'Generation', labels = c('1945-65', '1965-85', '1985-95'),
                     values = c('#54438E', "#587DBA", '#20934A'))+
  scale_linetype_manual(name = 'Marital Status', labels = c('Never Married', 'Ever Married'),
                        values = c(3,1))+
  scale_shape_manual(name = 'Generation', labels = c('1945-65', '1965-85', '1985-95'), 
                     values = c(15, 16, 17))+
  theme(axis.title.y = element_text(margin = margin(r = 20), size = 20, 
                                    colour = 'grey30'),
        axis.title.x = element_text(margin = margin(t = 20), size = 20, 
                                    colour = 'grey30'),
        title = element_text(margin = margin(r = 20), size = 20, 
                             colour = 'grey30'),
        legend.title = element_text(colour = 'grey30', size = 15),
        legend.text = element_text(colour = 'grey30', size = 15),
        axis.text = element_text(colour = 'grey30', size = 15),
        legend.key.size = unit(1, 'cm'))+
  labs( x = 'Age group', y = 'Age-specific fertility rate')+
  guides(line = guide_legend(override.aes = list(colour = "grey30") ) )+
  ylim(0,1.25)
?scale_shape_manual


# Table 2 -----------------------------------------------------------------


cumsumASFRNM <- ASFRNM %>%
  group_by(Generation)%>%
  mutate(cumsum = cumsum(value))%>%
  select(-c(value, TFR))%>%
  filter(!is.na(cumsum))%>%
  pivot_wider(names_from = Agegroup, values_from = cumsum)%>%
  ungroup()%>%
  mutate_if(is.numeric, round, 2)%>%
  mutate(EverMarried = 'Never Married')%>%
  select(EverMarried, Generation, `15-19`:`45-49`)


CumFertTableMar <- ASFRM %>%
  group_by(Generation)%>%
  mutate(cumsum = cumsum(value))%>%
  select(-c(value, TFR))%>%
  filter(!is.na(cumsum))%>%
  pivot_wider(names_from = Agegroup, values_from = cumsum)%>%
  ungroup()%>%
  mutate_if(is.numeric, round, 2)%>%
  mutate(EverMarried = 'Ever Married')%>%
  select(EverMarried, Generation, `15-19`:`45-49`)%>%
  rbind(cumsumASFRNM)%>%
  mutate(Generation = ifelse(Generation == 'Gen1', '1945-65',
                             ifelse(Generation == 'Gen2', '1965-85', '1985-95')))%>%
  group_by(Generation)%>%
  gt()%>%
  tab_header('Cumulative TFR')%>%
  tab_style(locations = cells_column_labels(columns = everything()),
            style = list( cell_borders(sides = "bottom", weight = px(3)),
                          cell_text(weight = "bold")))%>%
  tab_style(locations = cells_title(groups = "title"),
            style     = list(cell_text(weight = "bold", size = 24)  ) )%>%
  cols_align(align = 'left', columns = Generation)%>%
  tab_source_note(source_note = "Data: Author's calculations from AHRI PIP cohort")%>%
  tab_spanner(label = 'Age group', columns = `15-19`:`45-49`  )%>%
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "---"
  )%>%
  tab_style(locations = cells_row_groups(groups = everything()),
            style = list(
              cell_fill(color = "grey90"),
              cell_text(style = "italic")))%>%
  tab_style(style = cell_text(color = 'white'),
            locations = cells_column_labels(EverMarried))

gtsave(filename = 'cumfertmar.png', data = CumFertTableMar, vheight = 1200)

# Figure 5 ----------------------------------------------------------------



pregEdu <- preg %>%
  separate(MotherDoB, into= 'MotherYoB', sep ='-', remove = F)%>%
  filter( DDate > '1945-01-01') %>%
  select(WomanId, MotherDoB, MotherYoB, DDate, MotherAge, DeliverySetting,
         Outcome, LCnt, BirthOrder, FirstRecordedDate) %>%
  mutate(AgeAtBirth = decimal_date(DDate)-decimal_date(MotherDoB)) %>%
  filter(MotherAge !=3)%>% #an outlier
  transform(MotherYoB = as.numeric(MotherYoB))

baseSmall <- base %>%
  mutate(Age = decimal_date(StartDate) - decimal_date(DoB),
         AgeAtEduObs = decimal_date(EducationObsDate) - decimal_date(DoB))%>%
  filter(Sex == 2)%>%
  group_by(IndividualId)%>%
  mutate(EduMax = max(Education, na.rm = T))%>%
  ungroup()

mutate(Edugroupmax = 
         ifelse(EduMax < 8, 'Primary\nor less',
                ifelse(EduMax >=8 & EduMax < 12, 'Some \nsecondary', 
                       ifelse(EduMax == 12, 'Matric', 'Some \ntertiary'))))%>%
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse(DoB > '1985-01-01' & DoB < '1995-01-01',
                                   'Gen3', 'Other'))))%>%
  select(c(Age, IndividualId, Sex, Education, Edugroupmax, MaritalStatus,
           AgeAtEduObs, Generation))%>%
  group_by(IndividualId)%>%
  mutate(MaxAgeAtObs = max(AgeAtEduObs, na.rm = T))
#dataset with generations, max education, marital and one observation 
#per individual
baseSmallSlice <- baseSmall %>%
  group_by(IndividualId)%>%
  slice(1)


#pregSEM is pregnancy with socieconomic variables
pregSEM <- left_join(pregEdu, baseSmallSlice, 
                     by = c('WomanId' = 'IndividualId'))




AgeFirstBirthDotPlot <- subset(pregSEM, Generation != 'Other') %>%
  filter(BirthOrder == 1,
         AgeAtBirth < 25 & AgeAtBirth >= 12,
         !is.na(Edugroupmax))%>%
  filter(AgeAtEduObs > 20)%>%
  group_by(Edugroupmax, Generation)%>%
  summarise(Median = format(round(median(AgeAtBirth, na.rm = T), 2), nsmall = 2),
            Q1 = format(round(quantile(probs = 0.25, AgeAtBirth),2), nsmall = 2),
            Q3 = format(round(quantile( probs = 0.75, AgeAtBirth),2), nsmall = 2),
            Mean = format(round(mean(AgeAtBirth, na.rm = T),2), nsmall = 2),
            s.d. = format(round(sd(AgeAtBirth, na.rm = T),2), nsmall = 2),
            n = n())%>%
  mutate(Generation = factor(Generation, levels = c('Gen1', 'Gen2', 'Gen3'),
                             labels = c('1945-65', '1965-85', '1985-95')))


AgeFirstBirthDotPlot$Edugroupmax <- factor(AgeFirstBirthDotPlot$Edugroupmax, levels = c('Primary\nor less', 'Some \nsecondary', 'Matric', 'Some \ntertiary'),
                                           ordered = T)
EduNames2 <- c('Primary\nor less' = 'Primary or less',
               'Some \nsecondary' = 'Some secondary',
               'Matric' = 'Matric',
               'Some \ntertiary' = 'Some tertiary')
#in diss
ggplot(AgeFirstBirthDotPlot)+
  geom_segment(aes(y = Generation, yend = Generation, 
                   x = as.numeric(Q1), xend= as.numeric(Q3)), lwd = 2, colour = 'grey40', alpha = 0.9)+
  geom_point(aes(x = as.numeric(Median), y = Generation, colour = Generation), size = 9, alpha = 0.9)+
  facet_wrap(~Edugroupmax, ncol = 1, labeller = as_labeller(EduNames2))+
  scale_color_manual(name = 'Generation', labels = c('1945-65', '1965-85', '1985-95'),
                     values = c('#54438E', '#587DBA', '#20934A'))+
  theme(strip.background = element_rect('transparent'),
        legend.position = 'none',
        panel.spacing = unit(0.1, "lines"),
        axis.title.y = element_text(margin = margin(r = 20), size = 20, 
                                    colour = 'grey30'),
        axis.title.x = element_text(margin = margin(t = 20), size = 20, 
                                    colour = 'grey30'),
        title = element_text(margin = margin(r = 20), size = 20, 
                             colour = 'grey30'),
        legend.title = element_text(colour = 'grey30', size = 15),
        legend.text = element_text(colour = 'grey30', size = 15),
        axis.text = element_text(colour = 'grey30', size = 15),
        legend.key.size = unit(1, 'cm'),
        strip.text.x = element_text(size =20, colour = 'grey30'),
        strip.text = element_text(hjust = 0))+
  labs( y= 'Generation', x = 'Median age at first birth')+
  scale_y_discrete(limits=rev)

#ANOVA proof of significance
AllMarEduAOV <- subset(pregSEM, Generation != 'Other') %>%
  filter(BirthOrder == 1,
         AgeAtBirth < 25 & AgeAtBirth >= 12,
         !is.na(Edugroupmax))%>%
  filter(AgeAtEduObs > 20)

AllMarEduAOV1 <- AllMarEduAOV%>%
  filter(Edugroupmax == 'Primary\nor less')
AllMarEduAOV2 <- AllMarEduAOV%>%
  filter(Edugroupmax == 'Some \nsecondary')
AllMarEduAOVM <- AllMarEduAOV%>%
  filter(Edugroupmax == 'Matric')
AllMarEduAOV3 <- AllMarEduAOV%>%
  filter(Edugroupmax == 'Some \ntertiary')

Edu1AOV <- summary(aov(AgeAtBirth ~ Generation, data = AllMarEduAOV1))
Edu2AOV <- summary(aov(AgeAtBirth ~ Generation, data = AllMarEduAOV2))
MatricAOV <- summary(aov(AgeAtBirth ~ Generation, data = AllMarEduAOVM))
Edu3AOV <- summary(aov(AgeAtBirth ~ Generation, data = AllMarEduAOV3))

GenAOV <- summary(aov(AgeAtBirth ~ Generation, data = AllMarEduAOV))
EduAOV <- summary(aov(AgeAtBirth ~ Edugroupmax, data = AllMarEduAOV))


# Figure 6 ----------------------------------------------------------------


#downloading datasets
load('base') #SocioDem dataset
load('preg') #Pregnancies 2019 dataset
load('wgh') #Women's General Health dataset


load('pregSEM') #Pregnancies 2019 dataset
preg <- pregSEM 
wgh <- read_dta('wgh.dta')#Women's General Health dataset
base <- read_dta('AHRI.dta') #SocioDem dataset

#combining education markers from base with pregnancy dataset
pregEdu <- preg %>%
  separate(MotherDoB, into= 'MotherYoB', sep ='-', remove = F)%>%
  filter( DDate > '1945-01-01') %>%
  select(WomanId, MotherDoB, MotherYoB, DDate, MotherAge, DeliverySetting, Outcome, LCnt, BirthOrder, FirstRecordedDate) %>%
  mutate(AgeAtBirth = decimal_date(DDate)-decimal_date(MotherDoB)) %>%
  filter(MotherAge !=3)%>% #an outlier
  transform(MotherYoB = as.numeric(MotherYoB))

baseSmall <- base %>%
  mutate(Age = decimal_date(StartDate) - decimal_date(DoB),
         AgeAtEduObs = decimal_date(EducationObsDate) - decimal_date(DoB))%>%
  filter(Sex == 2)%>%
  group_by(IndividualId)%>%
  mutate(EduMax = max(Education, na.rm = T))%>%
  ungroup()%>%
  mutate(Edugroupmax = 
           ifelse(EduMax < 8, 'Primary\nor less',
                  ifelse(EduMax >=8 & EduMax < 12, 'Some \nsecondary', 
                         ifelse(EduMax == 12, 'Matric', 'Some \ntertiary')))) %>%
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse(DoB > '1985-01-01' & DoB < '1995-01-01','Gen3', 'Other'))))%>%
  select(c(Age, IndividualId, Sex, Education, Edugroupmax, MaritalStatus, AgeAtEduObs, Generation))%>%
  group_by(IndividualId)%>%
  mutate(MaxAgeAtObs = max(AgeAtEduObs, na.rm = T))

#dataset with generations, max education, marital and one observation 
#per individual
baseSmallSlice <- baseSmall %>%
  group_by(IndividualId)%>%
  slice(1)


#pregSEM is pregnancy with socieconomic variables
pregSEM <- left_join(pregEdu, baseSmallSlice, 
                     by = c('WomanId' = 'IndividualId'))


#Building figure 6

#Union1 computes the oldest age at which an individual was single
Union1 <- base %>%
  filter(Sex ==2,
         DoB >'1985-01-01' & DoB <'1995-01-01',
         MaritalStatus == 1)%>%
  select(IndividualId, DoB, StartDate, EndDate, MaritalStatus)%>%
  mutate(AgeAtSingle = decimal_date(EndDate) - decimal_date(DoB))%>%
  group_by(IndividualId)%>%
  slice_max(AgeAtSingle, with_ties = F)%>%
  pivot_wider(values_from = EndDate, names_from = MaritalStatus)%>%
  select(IndividualId, DoB, AgeAtSingle, `1` )%>%
  rename(SingleDate = `1` )

# Union5 computes the minimum age at which an individual entered marriage or an
# informal union
Union5 <- base %>%
  filter(Sex ==2,
         DoB >'1985-01-01' & DoB <'1995-01-01',
         MaritalStatus == 5 | MaritalStatus == 2)%>%
  mutate(UnionStatus = ifelse(MaritalStatus == 2 |MaritalStatus == 5, '1', '0'))%>%
  select(IndividualId, DoB, StartDate, EndDate, UnionStatus)%>%
  mutate(AgeAtUnion = decimal_date(StartDate) - decimal_date(DoB))%>%
  group_by(IndividualId)%>%
  slice_min(AgeAtUnion, with_ties = F)%>%
  pivot_wider(values_from = StartDate, names_from = UnionStatus)%>%
  select(IndividualId, AgeAtUnion, `1`)%>%
  rename(UnionDate = `1` )

#NotUnion1 retains people who were never in a union and were observed after age 18
# nd assigns their age at union 'NA'
NotUnion <- base %>%
  filter(Sex ==2,
         DoB >'1985-01-01' & DoB <'1995-01-01')%>%
  select(IndividualId, DoB, StartDate, EndDate, MaritalStatus)%>%
  mutate(AgeAtUnion = decimal_date(StartDate) - decimal_date(DoB))%>%
  filter(AgeAtUnion <25)%>% #so only those in unions before 25
  group_by(IndividualId)%>%
  mutate(MaritalMax = max(MaritalStatus, na.rm =T),
         AgeMax = max(AgeAtUnion, na.rm= T))

NotUnion1 <- NotUnion %>%
  filter(MaritalMax == 1, 
         AgeMax >18)%>%
  group_by(IndividualId)%>%
  slice_max(AgeAtUnion, with_ties = F)%>%
  filter(!is.na(MaritalStatus))%>%
  mutate(AgeAtUnion = NA)%>%
  select(IndividualId, AgeAtUnion)

#union gives the age and date for which a person first entered a union after
# being observed being single, and maintians those who never entered a union
Union <- left_join(Union1, Union5, by = c('IndividualId'))%>%
  mutate(gap = decimal_date(UnionDate) - decimal_date(SingleDate))%>%
  filter(gap < 2| AgeAtSingle > AgeAtUnion)%>%
  select(IndividualId, AgeAtUnion, UnionDate)%>%
  rbind(NotUnion1)

#Education12 selects the youngest age associated with being in grade 12
Education12 <- base %>%
  filter(Sex ==2,
         DoB >'1985-01-01' & DoB <'1995-01-01')%>%
  select(IndividualId, DoB, StartDate, EndDate, Education, EducationObsDate)%>%
  mutate(Age = decimal_date(EducationObsDate) - decimal_date(DoB))%>%
  filter(Education == 12 )%>%
  group_by(IndividualId)%>%
  slice_min(Age, with_ties = F)

#Education 11 selects the youngest age associated with being in 
#grade 10 or grade 11
Education11 <- base %>%
  filter(Sex ==2,
         DoB >'1985-01-01' & DoB <'1995-01-01')%>%
  select(IndividualId, DoB, StartDate, EndDate, Education, EducationObsDate)%>%
  mutate(Age = decimal_date(EducationObsDate) - decimal_date(DoB))%>%
  filter(Education == 11 | Education == 10)

Education11$Education[Education11$Education == 10] <-11

Education11 <- Education11%>%
  group_by(IndividualId)%>%
  slice_max(Age, with_ties = F)

#EducationNot1 retains those who were aged over 18 at last observation but
# never reached grade 12 and assigns them 'NA' for 'DateMatric' variable
EducationNot <- base %>%
  filter(Sex ==2,
         DoB >'1985-01-01' & DoB <'1995-01-01')%>%
  select(IndividualId, DoB, StartDate, EndDate, Education, EducationObsDate)%>%
  mutate(Age = decimal_date(EducationObsDate) - decimal_date(DoB))%>%
  filter(Age <25)%>%
  group_by(IndividualId)%>%
  mutate(EduMax = max(Education, na.rm = T))%>%
  filter(EduMax != 12,
         EduMax != 15,
         !is.na(EduMax))

EducationNot1 <- EducationNot %>%
  filter(Age >= 18)%>%
  group_by(IndividualId)%>%
  slice_max(Age,with_ties = F)%>%
  mutate(DateMatric = NA)



#Education computes the date on which indiiduals are first recorded being in 
#grade 12, contingent on them preivously being observed in grade 10 or 11 
# within the last 2 years, and also includes all individuals who never
# attained grade 12
Education <- rbind(Education12, Education11)%>%
  select(IndividualId, DoB, Education, EducationObsDate)%>%
  pivot_wider(names_from = Education, values_from = EducationObsDate)%>%
  filter(!is.na(`11`),
         !is.na(`12` ))%>%
  mutate(gap = decimal_date(`12`) - decimal_date(`11` ))%>%
  filter(gap <2)%>%
  rename(DateMatric = `12`)%>%
  mutate(AgeMatric = decimal_date(DateMatric) - decimal_date(DoB))%>%
  bind_rows(EducationNot1)

#FirstBirth  selects the age at first birth available for all women in
#the 1985-95 generation
FirstBirth <- pregSEM %>%
  filter(MotherDoB > '1985-01-01' & MotherDoB <'1995-01-01',
         BirthOrder ==1)%>%
  select(WomanId, DDate, AgeAtBirth)

#FirstSex selects the minimum age at which an individual reports first having
#had sex
FirstSex <- wgh %>%
  select(IIntId, DSRound, AgeAtFirstSex, AgeAtVisit)%>%
  filter(!is.na(AgeAtFirstSex),
         AgeAtFirstSex <25 & AgeAtFirstSex >11)%>%
  group_by(IIntId)%>%
  slice_min(AgeAtFirstSex, with_ties = F)%>%
  select(IIntId, AgeAtFirstSex)

#edugroupmax selects the maximum education an individual is observed as having 
#over all survye rounds
Edugroupmax <- base %>%
  select(IndividualId, Education)%>%
  group_by(IndividualId)%>%
  mutate(Edugroupmax = max(Education, na.rm=T))%>%
  slice(1)

#LifeEvents joins an individual's date at reaching grade 12, having sex, and
#entering a union for all women in the generation who gave birth before age 25
LifeEvents <- FirstBirth %>%
  left_join(Education, by = c('WomanId' = 'IndividualId'))%>%
  left_join(Union, by = c('WomanId' = 'IndividualId'))%>%
  left_join(FirstSex, by = c('WomanId' = 'IIntId'))%>%
  filter(AgeAtBirth < 25 & AgeAtBirth >11,
         AgeMatric < 25 | is.na(AgeMatric),
         AgeAtUnion < 25 | is.na(AgeAtUnion),
         AgeAtFirstSex < AgeAtBirth)%>% #first sex cannot occur after first birth
  left_join(Edugroupmax,  by = c('WomanId' = 'IndividualId'))

unique(LifeEvents$WomanId)


# Figure 6 

#calculate the standard deviation around each life event
LifeEventsCountSd <-LifeEvents %>%
  filter(AgeAtFirstSex >14)%>%
  group_by(AgeAtFirstSex)%>%
  summarise(sdBirth = sd(AgeAtBirth, na.rm = T),
            sdMatric = sd(AgeMatric, na.rm = T),
            sdUnion = sd(AgeAtUnion, na.rm = T),
            n = n())%>%
  pivot_longer(cols = c(sdBirth, sdUnion, sdMatric), names_to = 'EventTypesd',
               values_to = 'sd')%>%
  select(sd)


#calclulate the mean age at each life event across the sample
LifeEventsCount <-LifeEvents %>%
  group_by(AgeAtFirstSex)%>%
  summarise(BirthMean = mean(AgeAtBirth),
            MatricMean = mean(AgeMatric, na.rm = T),
            UnionMean = mean(AgeAtUnion, na.rm = T),
            n = n())%>%
  filter(AgeAtFirstSex >=15 & AgeAtFirstSex <25)%>% # because sample is too small
  pivot_longer(cols = c(BirthMean, UnionMean, MatricMean), names_to = 'EventType',
               values_to = 'AgeAtEvent')%>%
  cbind(LifeEventsCountSd)%>%
  mutate(perc = n *100/ (sum(n)/3))


#plot the variaion in life event by age at sexual debut
Figure6 <- ggplot(LifeEventsCount, aes(x = AgeAtFirstSex, y = AgeAtEvent, 
                                       group = EventType))+
  geom_point(aes(colour= EventType), lwd = 1.5)+
  geom_point(aes( ymin = AgeAtEvent - sd, ymax = AgeAtEvent + sd,
                  fill = EventType), alpha = 0.3)+
  scale_colour_manual(name = '', labels = c('First birth',
                                            'Achieves matric',
                                            'First union'),
                      values = c( '#54438E',  "#587DBA", '#D15439'))+
  scale_fill_manual(name = '', labels = c('First birth',
                                          'Achieves matric',
                                          'First union'),
                    values = c( '#54438E',  "#587DBA", '#D15439'))+
  theme(axis.title.y = element_text(margin = margin(r = 20), size = 20, 
                                    colour = 'grey30'),
        axis.title.x = element_text(margin = margin(b=13, t = 10), size = 20, 
                                    colour = 'grey30'),
        title = element_text(margin = margin(r = 20), size = 20, 
                             colour = 'grey30'),
        legend.title = element_text(colour = 'grey30', size = 15),
        legend.text = element_text(colour = 'grey30', size = 15),
        axis.text = element_text(colour = 'grey30', size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.position = 'top',
        plot.margin = unit(c(0,1,0,1), 'cm'))+
  labs( x= 'Age when woman first had sex', y = 'Age at life event')+
  scale_x_continuous(breaks = 15:25)+
  scale_y_continuous(breaks = 15:25)


# App3 Table 1 ------------------------------------------------------------

AllMarPropT <- AllMarEdugroup1 %>%
  ungroup()%>%
  count(Generation, Sex)

EverMarPropT <- AllMarEdugroup1 %>%
  ungroup()%>%
  filter(EverMarried == 1)%>%
  count(Generation, Sex)%>%
  rename(MarEver = n)

CurMarT<- AllMarEdu2 %>%
  ungroup()%>%
  filter(MaritalStatus == 2)%>%
  filter(Age > 23)%>%
  filter(StartDate > '2018-01-01' & StartDate <'2019-01-01')%>%
  group_by(IndividualId)%>%
  slice(1)%>%
  ungroup()%>%
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse(DoB > '1985-01-01' & DoB < '1995-01-01','Gen3', 'Other'))))%>%
  count(Generation, Sex)%>%
  rename(CurMar = n)

CurMarAllT<- AllMarEdu2 %>%
  ungroup()%>%
  filter(StartDate > '2018-01-01' & StartDate <'2019-01-01')%>%
  filter(Age > 23)%>%
  group_by(IndividualId)%>%
  slice(1)%>%
  ungroup()%>%
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse(DoB > '1985-01-01' & DoB < '1995-01-01','Gen3', 'Other'))))%>%
  count(Generation, Sex)%>%
  rename(CurMarN = n)
#in diss
MarEverAggrEduT <- left_join(EverMarPropT, AllMarPropT, by =  c('Sex', 'Generation'))%>%
  left_join(CurMarT, by = c('Sex', 'Generation'))%>%
  left_join(CurMarAllT, by = c('Sex', 'Generation'))%>%
  mutate(MarProp = MarEver*100/n,
         CurMarProp = CurMar*100/CurMarN)%>%
  select(Generation, Sex, MarProp, n, CurMarProp, CurMarN)%>%
  filter(Generation != 'Other')%>%
  transform(Sex = ifelse(Sex == 1, 'Male', 'Female'))%>%
  mutate_if(is.numeric, round, 2)%>%
  gt()%>%
  tab_header('Marriage rates in the AHRI PIP cohort')%>%
  tab_row_group(label = 'cohort 1945-65', 
                rows = Generation == 'Gen1') %>%
  tab_row_group(label = 'cohort 1965-85', 
                rows = Generation == 'Gen2') %>%
  tab_row_group(label = 'cohort 1985-95', 
                rows = Generation == 'Gen3')%>%
  tab_style(locations = cells_column_labels(columns = everything()),
            style = list( cell_borders(sides = "bottom", weight = px(3)),
                          cell_text(weight = "bold")))%>%
  tab_style(locations = cells_title(groups = "title"),
            style     = list(cell_text(weight = "bold", size = 24)  ) )%>%
  cols_align(align = 'left', columns = Generation)%>%
  cols_align(align = 'center', columns = c(MarProp, CurMarProp))%>%
  tab_style(locations = cells_row_groups(groups = everything()),
            style = list(
              cell_text(style = "italic") ))%>%
  tab_style(style = cell_text(color = 'white'),
            locations = cells_column_labels(columns = Sex))%>%
  tab_style(style = cell_text(indent = pct(30)),
            locations = cells_body(
              columns = Sex))%>%
  cols_label(
    MarProp = "Ever \nMarried (2000-2019) % (n)",
    CurMarProp = "Currently \nMarried (2018) % (n)"
  )%>%
  cols_hide(Generation)%>%
  cols_merge_n_pct(MarProp,n)%>%
  cols_merge_n_pct(CurMarProp,CurMarN)%>%
  cols_width(
    Sex ~ pct(30))%>%
  tab_options(table.width = pct(40))%>%
  tab_source_note(source_note = "Data: Author's calculations from AHRI PIP cohort 2000-2019")%>%
  row_group_order(groups = c("cohort 1945-65", "cohort 1965-85", "cohort 1985-95"))


gtsave(filename = 'MarriageRateTable.png', data = MarEverAggrEduT, vwidth = 800)

?gtsave
library(gt)
library(tidyverse)
coldata <- ggplot_build(ggEduMarEver)

avgAgeGraph <- AllMarEdu1group%>%
  group_by(Generation, Edugroup, Sex)%>%
  summarise(Age = median(Age))%>%
  print(n=30)

# App4 Table 1 ------------------------------------------------------------



birthagesEduTable <- AgeFirstBirth%>%
  gt()%>%
  tab_header('Age at first birth among under 25 year-olds')%>%
  tab_row_group(label = 'Primary or less  ***', 
                rows = Edugroupmax == 'Primary\nor less') %>%
  tab_row_group(label = 'Some secondary  ***', 
                rows = Edugroupmax == 'Some \nsecondary') %>%
  tab_row_group(label = 'Matric  ***', 
                rows = Edugroupmax == 'Matric')%>%
  tab_row_group(label = 'Some tertiary  **', 
                rows = Edugroupmax == 'Some \ntertiary')%>%
  tab_style(locations = cells_column_labels(columns = everything()),
            style = list( cell_borders(sides = "bottom", weight = px(3)),
                          cell_text(weight = "bold")))%>%
  tab_style(locations = cells_title(groups = "title"),
            style     = list(cell_text(weight = "bold", size = 24)  ) )%>%
  cols_align(align = 'left', columns = Generation)%>%
  cols_align( align = 'left', columns = IQR)%>%
  tab_style(locations = cells_row_groups(groups = everything()),
            style = list(
              cell_fill(color = "grey90"),
              cell_text(style = "italic") ))%>%
  tab_source_note(source_note = "One-way ANOVA: ***p <0.001, **p <0.01, *p <0.05")%>%
  tab_source_note(source_note = "Data: Author's calculations from AHRI PIP cohort")%>%
  row_group_order(groups = c('Some tertiary  **','Matric  ***', 'Some secondary  ***',
                             'Primary or less  ***'))


gtsave(filename = 'AgeFirstBirth.png', data = birthagesEduTable, vwidth = 1200, vheight = 3200)

library(gt)
library(webshot)
library(tidyverse)



# App5 Table 1 ------------------------------------------------------------


library(gt)
library(webshot2)
library(tidyverse)

baseDOB <- base %>%
  select(IndividualId, DoB)%>%
  mutate(Generation = ifelse(DoB >= '1945-01-01' & DoB < '1965-01-01', '1945-65',
                             ifelse(DoB >='1965-01-01' & DoB < '1985-01-01', '1965-85', 
                                    ifelse(DoB >='1985-01-01' & DoB < '1995-01-01', '1985-95', 'Other'))))%>%
  select(-DoB)



FirstSexTable1 <- FirstSex %>%
  left_join(baseDOB, by = c('IIntId' = 'IndividualId'))%>%
  group_by(IIntId)%>%
  slice(1)

FirstSexTable2 <- FirstSexTable1 %>%
  filter(Generation != 'Other', 
         !is.na(Generation))%>%
  group_by(Generation)%>%
  summarise(Mean = mean(AgeAtFirstSex, na.rm = T),
            s.d. = sd(AgeAtFirstSex, na.rm = T),
            n = n())%>%
  mutate_if(is.numeric, round, 2)%>%
  gt()%>%
  tab_header('Age at first sex')%>%
  tab_style(locations = cells_column_labels(columns = everything()),
            style = list( cell_borders(sides = "bottom", weight = px(3)),
                          cell_text(weight = "bold")))%>%
  tab_style(locations = cells_title(groups = "title"),
            style     = list(cell_text(weight = "bold", size = 24)  ) )%>%
  cols_align(align = 'left', columns = Generation)%>%
  tab_source_note(source_note = "Data: Author's calculations from AHRI PIP cohort")%>%
  tab_style(locations = cells_row_groups(groups = everything()),
            style = list(
              cell_fill(color = "grey90"),
              cell_text(style = "italic")))

gtsave(filename = 'sexualDebut.png', data = FirstSexTable2, vheight = 800)
