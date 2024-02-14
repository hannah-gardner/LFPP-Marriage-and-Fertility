#######################################################################
## R code for 'When marriage become unattainable: a cohort analysis  ##
##                of fertility in rural South Africa                 ##
#######################################################################

#Set-up------------------------------------------------------------------------
setwd()

library(lubridate)
library(ggpubr)
library(gtsummary)
library(webshot)
library(haven)
library(gt)
library(tidyverse)
library(LexisPlotR)
library(pBrackets)
 

load('DataDem') %>%      #load Socio Dem Surveillance 2019 release dataset
  select(-c(Node, FatherID, Days, LocationId, Resident, 
          Enumeration: Memberships, SpouseId:SEObsDate))

load('DataPreg')%>%  #load Pregnancies 2023 release dataset
  filter(DDate < '2019-01-01')

# Figure 1 ----------------------------------------------------------------
# Lexis diagram displaying the cohorts used in analysis and the period over 
# which data is collected in the AHRI HDSS


LexisGrid <- lexis_grid(year_start = 1945, year_end = 2020, age_start = 0, age_end = 75, d=10, lwd=0.5)
LexisGridFill <- lexis_year(lg = LexisGrid, delta = 19, year = 2000, alpha = 0.7, fill = "grey55")

LexisGridFill <- lexis_cohort(lg = LexisGridFill, cohort = 1945, delta = 20, alpha = .7, fill = "#54438E")
LexisGridFill <- lexis_cohort(lg = LexisGridFill, cohort = 1965, delta = 20, alpha = .7, fill = '#587DBA')
LexisGridFill <- lexis_cohort(lg = LexisGridFill, cohort = 1985, delta = 10, alpha = .7, fill = "#20934A")


Figure1 <- LexisGridFill + 
  labs(y="Age", x="Year") + 
  theme(axis.title.x = element_text(margin = margin(t = 20), size = 32, 
                                    colour = grey(level = 0.1), hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 20), size = 32, 
                                    colour = grey(level = 0.1), hjust = 0.5),
        axis.text = element_text(size = 26, colour = grey(level = 0.1)))
 


# Creating base demographic dataframe used for most analyses- -----------------


# Demographic dataset with ever-marriage, education and generation variables
DataDem1 <- DataDem %>% 
  mutate(AgeAtObs = decimal_date(StartDate) - decimal_date(DoB))%>%
  #create calendar year variable
  separate(StartDate, into = 'Year', remove = F, sep = '-')%>%
  transform(Year = as.numeric(Year))


DataDemMEG1 <- DataDem1 %>%
  #identify maximum level of education, last date of observation and last
  #date edu observation
  mutate(AgeAtEduObs = decimal_date(EducationObsDate) - decimal_date(DoB))%>% 
  group_by(IndividualId)%>%
  mutate(EduMax = max(Education, na.rm = T),
         AgeEduObsMax = max(AgeAtEduObs, na.rm=T), 
         AgeAtObsMax = max(AgeAtObs, na.rm=T))%>%
  ungroup()
  

DataDemMEG2 <- DataDemMEG1%>%
  #create ever-married variable
  filter(!is.na(MaritalStatus))%>% 
  mutate(WasMarried = ifelse(grepl('2|3|4', MaritalStatus), '1', '0'))%>%
  group_by(IndividualId)%>%
  mutate(EverMarried = max(WasMarried))%>%
  ungroup()


DataDemMEG3 <- DataDemMEG2 %>%
  #label and group maximum education variable
  ungroup()%>%
  filter(EduMax != -Inf)%>% #removes those for whom Education was NA
  mutate(EduMaxLbl = 
           ifelse(EduMax < 8, 'Primary\nor less',
                  ifelse(EduMax >=8 & EduMax < 12, 'Some \nsecondary', 
                         ifelse(EduMax == 12, 'Matric', 'Some \ntertiary')))) %>%
  #make and label generation variable
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse(DoB > '1985-01-01' & DoB < '1995-01-01','Gen3', 'Other'))))%>%
  filter(Generation != 'Other')
  
DataDemMEG3$EduMaxLbl <- factor(DataDemMEG3$EduMaxLbl, 
                               levels = c('Primary\nor less', 'Some \nsecondary', 
                                          'Matric', 'Some \ntertiary'),
                                   ordered = T)

#filter for adequate age at observation, justified in appendix
DataDemMEG <-DataDemMEG3 %>%
  group_by(IndividualId)%>%
  slice(1, with_ties = F)%>%
  ungroup()%>%
  filter(AgeEduObsMax >= 21,
         AgeAtObsMax >=23)

#no filtering on education variable, used for fertility-only analyses
DataDemMEGfert <-DataDemMEG3 %>%
  group_by(IndividualId)%>%
  slice(1, with_ties = F)%>%
  ungroup()%>%
  filter(AgeAtObsMax >=23)



# Table 1 -----------------------------------------------------------------

tbl1_Female <- DataDemMEG %>%
  select(Sex, Generation, EduMaxLbl)%>%
  mutate(Sex = unclass(Sex))%>%
  ungroup()

tbl1_Female$EduMaxLbl <- factor(tbl1_Female$EduMaxLbl, 
                                levels = c('Primary\nor less', 'Some \nsecondary', 
                                           'Matric', 'Some \ntertiary'),
                               ordered = T)

tbl1_FHHH <- DataDemMEG3 %>%
  filter(IndividualId %in%DataDemMEG$IndividualId)%>%
  filter(HHRelation == 1)%>%
  group_by(HouseholdId, IndividualId)%>%
  slice(1)%>%
  ungroup()%>%
  mutate(FHHH = ifelse(Sex == 2, 'Female-headed households', '0'))%>%
  select(FHHH, Generation)


tbl1_Employ <-   DataDemMEG3 %>%
  filter(IndividualId %in%DataDemMEG$IndividualId)%>%
  filter(HHRelation == 1)%>%
  filter(!is.na(CurrentlyEmployed))%>%
  group_by(HouseholdId, Year)%>%
  slice_min(CurrentlyEmployed, with_ties = F)%>%
  ungroup()%>%
  mutate(CurrentlyEmployed = ifelse(CurrentlyEmployed == 1, 'Full-time',
                                    ifelse(CurrentlyEmployed == 2, 'Part-time',
                                           'Unemployed')))%>%
  select(CurrentlyEmployed, Generation)

tbl1_Employ <-   DataDemMEG3 %>%
  filter(IndividualId %in%DataDemMEG$IndividualId)%>%
  filter(!is.na(CurrentlyEmployed))%>%
  group_by(IndividualId, Year)%>%
  slice_min(CurrentlyEmployed, with_ties = F)%>%
  ungroup()%>%
    select(CurrentlyEmployed, Generation)%>%
  mutate(CurrentlyEmployed = ifelse(CurrentlyEmployed == 1 | CurrentlyEmployed ==2, 1, 0))

  


tbl1_Nonresident <- DataDemMEG3 %>%
  filter(IndividualId %in%DataDemMEG$IndividualId)%>%
  group_by(IndividualId, Year)%>%
  slice_max(NonResident, with_ties = F)%>%
  ungroup()%>%
  select(Generation, NonResident)%>%
  mutate( NonResident = unclass(NonResident))

DataDemMEG %>%
  count(Generation)

tbl1_row1 <- tbl_summary( tbl1_Female,
                          by = Generation,
                          value = list(Sex ~ '2'),
                          label  = list(Sex ~ 'Female, % (n)',
                                        EduMaxLbl ~ "Highest level of education achieved, % (n)"),
                          statistic = list(
                            all_categorical() ~ "{p}% ({n})"))%>%
  add_overall()%>%
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()%>%
  modify_spanning_header(
    c(stat_1, stat_2, stat_3) ~ 
      "**Generation**")%>%
  modify_header(
    stat_1 = "   **1945-64**,    N = 20,130",
    stat_2 = "   **1965-84**,    N = 47,871",
    stat_3 = "   **1985-94**,    N = 32,088",
    stat_0 = "   **Overall**,   N = 100,089",
    label = '')%>%
  modify_footnote(all_stat_cols() ~NA)



tbl1_row2 <- tbl_summary(tbl1_FHHH,
                         by = Generation,
                           value = list(FHHH ~ 'Female-headed households'),
                         label = list(FHHH ~ 'Female-headed households, %'),
                         statistic = FHHH ~ "{p}%")%>%
  add_overall()%>%
  bold_labels() 

tbl1_row3 <- tbl_summary(tbl1_Nonresident,
                         by = Generation,
                         label= list(NonResident ~ 'Non-resident from their household in a given year, %'),
                         value = list(NonResident ~ 1),
                         statistic = NonResident ~ "{p}%")%>%
  add_overall()%>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~NA)



tbl1_row4 <- tbl_summary(tbl1_Employ,
                          by = Generation,
                          label= list(CurrentlyEmployed ~ 'Employed in a given year, %'),
                          statistic = list(CurrentlyEmployed ~ "{p}%"),
                         value = list(CurrentlyEmployed ~ 1))%>%
  add_overall()%>%
  bold_labels()%>%
  modify_footnote(all_stat_cols() ~NA)


#Table 1 
Table1 <- tbl_stack(list(tbl1_row1, tbl1_row2, tbl1_row3, tbl1_row4))%>%
  as_gt()%>%
  cols_width(label ~ pct(40),
             stat_0 ~pct(15))%>%
  tab_options(table.width = pct(84))%>% 
  tab_options(table_body.hlines.color = "transparent")%>%
  tab_style(style = cell_borders(sides = c("bottom"),  weight = px(0.5), color = grey(0.7)),
                                 locations = cells_body(rows = c(1, 6, 7, 8 ,9)))

gtsave(data = Table1, filename = 'summarytbl.rtf', vheight = 1800)


 #Figure 2 ----------------------------------------------------------------

AllPropEdu <- DataDemMEG %>%
  #proportion of all indv in gender, generation and education categories
  ungroup()%>%
  count(Generation, Sex, EduMaxLbl)

EverMarPropEdu <- DataDemMEG %>%
  #proportion of indv who are ever-married in each gender and education category
  ungroup()%>%
  filter(EverMarried == 1)%>%
  count(Generation, Sex, EduMaxLbl)%>%
  rename(MarEver = n)

Fig2_BothProps <- left_join(EverMarPropEdu, AllPropEdu, by =  c('Sex', 'Generation',
                                                                         'EduMaxLbl'))%>%
  mutate(MarProp = MarEver/n)

#labels for display
GenNames <- c('Gen1' = 'Generation 1945-64',
              'Gen2' = 'Generation 1965-84',
              'Gen3' = 'Generation 1985-94')
Fig2_BothProps$EduMaxLbl <- factor(Fig2_BothProps$EduMaxLbl, 
                                   levels = c('Primary\nor less', 'Some \nsecondary', 
                                              'Matric', 'Some \ntertiary'),
                                     ordered = T)


#Figure 2a
Fig2_grd_yminor1 <- seq(0,1, length.out = 11)
fig2_grd_y1 <- seq(0.05,0.95, length.out = 10)

ggEduMarEver <- ggplot(subset(Fig2_BothProps, Generation != 'Other'))+
  geom_col(mapping = aes(EduMaxLbl, MarProp, fill = factor(Sex)),
           position = 'dodge')+
  facet_wrap(~Generation, labeller = as_labeller(GenNames))+
  theme(axis.title.y = element_text(margin = margin(r = 10), size = 15, 
                                    colour = 'grey30'),
        axis.title.x = element_text(margin = margin(t = 20), size = , 
                                    colour = 'grey30'),
        axis.text = element_text(colour = 'grey30', size = 13),
        legend.position = 'none',
        strip.text.x = element_text(size = 20, colour = 'grey30'),
        strip.background = element_rect('white'),
        plot.margin = unit(c(0.5,0.5,0,1),'cm'), 
        panel.grid.minor = element_blank()  )+
  labs( x= '', y = 'Proportion of people who have \never been married')+
  scale_fill_manual(name = '', labels = c('Male', 'Female'),values = c( "#5ab4ac", '#D15439'))+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), limit = c(0,1))+
  geom_hline(yintercept = Fig2_grd_yminor1, col = "white", linewidth = 0.05)+
  geom_hline(yintercept = 0, col = "grey30", linewidth = 0.7)


#Figure 2b

# relative advantage of edu for marriage 
Fig2_BothProps2 <- Fig2_BothProps %>%
  group_by(Generation, Sex)%>%
  mutate(Ref = MarProp[EduMaxLbl == 'Matric'])%>%
  ungroup()%>%
  mutate(Adv = MarProp/Ref,
         Adv = Adv - 1)

Fig2_BothProps2$EduMaxLbl <- factor(Fig2_BothProps2$EduMaxLbl, 
                                      levels = c('Primary\nor less', 'Some \nsecondary', 
                                                 'Matric', 'Some \ntertiary'),
                                      ordered = T)

#Figure 2b
Fig2_grd_yminor <- seq(-0.75,1.75, length.out = 6)
Fig2_grd_y <- seq(-1,2, length.out = 7)

ggEduMarEverAdv <- ggplot(subset(Fig2_BothProps2, Generation != 'Other'))+
  geom_col(mapping = aes(x = EduMaxLbl, y = Adv, fill = factor(Sex)),
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
  labs( x= 'Education level', 
        y = 'Relative difference in proportion of those \never-married when compared with Matric category')+
  scale_y_continuous(breaks = c(-0.5,0,0.5,1,1.5), limit = c(-0.75,1.75))+
  geom_hline(yintercept = Fig2_grd_y, col = "white", linewidth = 0.5) +
  geom_hline(yintercept = Fig2_grd_yminor, col = "white", linewidth = 0.5)+
  geom_hline(yintercept = 0, col = "grey30", linewidth = 0.7)+
  geom_point(aes(x='Matric', y=0), color = 'grey30')




#Figure 2 combined
Figure2 <- ggarrange(ggEduMarEver, ggEduMarEverAdv, ncol = 1, labels = c("a)","b)"),
          font.label = list(size = 20, color = 'grey30'), hjust = 0.075)

# Figure 3 ----------------------------------------------------------------

Mothers <- DataDemMEG%>%
  filter(Sex==2)

Daughters <- DataDemMEG %>%
  filter(Generation =='Gen3',
         Sex==2,
         !is.na(MotherId)) %>%
  select(IndividualId, MotherId, DoB, Sex, EverMarried)


Fig3_MumDaught <- left_join(x = Mothers, y= Daughters, 
                              by = c("IndividualId" = 'MotherId'))


Fig3_MumDaught <- Fig3_MumDaught %>%
  rename(MotherIndividualId = IndividualId,
         ChildIndividualId = IndividualId.y,
         MotherDoB = DoB.x,
         ChildDoB = DoB.y,
         MotherEverMarried = EverMarried.x,
         ChldEverMarried = EverMarried.y)%>%
  filter(!is.na(ChildDoB))%>%
  mutate(AgeAtBirth = decimal_date(ChildDoB)-decimal_date(MotherDoB))%>%
  group_by(ChildIndividualId)%>%
  slice(1)%>%  # selecting only one obs of a daughter-mother pair
  ungroup

# Graph for education - 1985-95 Generation
EduNames <- c("Primary\nor less" = "Primary\nor less",
              "Some \nsecondary" = "Some \nsecondary", 
              "Matric" = "Matric", 
              'Some \ntertiary' = 'Some \ntertiary')


Fig3_props <- Fig3_MumDaught %>%
  ungroup()%>%
  count(MotherEverMarried, ChldEverMarried, EduMaxLbl)%>%
  mutate( MotherEverMarried = ifelse(MotherEverMarried == 0,
                                     'Never- \nMarried', 'Ever- \nMarried'))

Fig3_props$EduMaxLbl <- factor(Fig3_props$EduMaxLbl,
                                levels = c('Primary\nor less', 'Some \nsecondary', 
                                           'Matric', 'Some \ntertiary'),
                                  ordered = T)


Figure3 <- ggplot(Fig3_props)+
  geom_col(aes(x = MotherEverMarried, y= n, group = ChldEverMarried, 
               fill = ChldEverMarried), position = 'fill')+
  facet_wrap(~EduMaxLbl, labeller = as_labeller(EduNames), ncol = 4)+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))+
  scale_fill_manual(name = "Daughter's marital status", 
                    labels = c('Never-married', 'Ever-married'),
                    values = c('grey75', '#20934A'))+
  theme(strip.background = element_rect('#587DBA'),
        legend.position = 'bottom',
        panel.spacing = unit(0.1, "lines"),
        axis.title.y = element_text(margin = margin(r = 18), size = 18, 
                                    colour = 'grey30'),
        axis.title.x = element_text(margin = margin(t = 10), size = 20, 
                                    colour = 'grey30'),
        plot.subtitle = element_text( size = 19, 
                                      colour = 'grey30', hjust = 0.5, 
                                      margin = margin(b = 10)),
        legend.title = element_text(colour = 'grey30', size = 15),
        legend.text = element_text(colour = 'grey30', size = 15),
        axis.text = element_text(colour = 'grey30', size = 15),
        legend.key.size = unit(1, 'cm'),
        strip.text.x = element_text(size =20, colour = 'white'))+
  labs( x= "Mother's marital status", y = 'Proportion of daughters ever-married', 
        subtitle = "Mother's education level")

ggplot_build(Figure3)


# Figure 4 ----------------------------------------------------------------

###Create Births of Interest Variable

#prepare for combining pregnancy and demographic datasets
DataPreg1 <- DataPreg %>%
  separate(MotherDoB, into= 'MotherYoB', sep ='-', remove = F)%>%
  transform(MotherYoB = as.numeric(MotherYoB))%>%
  select(WomanId, MotherDoB, MotherYoB, DDate, MotherAge, Outcome, LCnt, 
         BirthOrder, FirstRecordedDate) %>%
  mutate(AgeAtBirth = decimal_date(DDate)-decimal_date(MotherDoB))

subset_DataDemMEG <- DataDemMEG %>%
  ungroup()%>%
  filter(Sex == 2)%>%
  select(c(IndividualId, Sex, EduMaxLbl, AgeAtObsMax, Generation, EverMarried))

#DataPregMEG is DataPreg with marital, education, generation
DataPregMEG <- left_join(DataPreg1, subset_DataDemMEG, 
                         by = c('WomanId' = 'IndividualId'))        

#adding rows for multiple births
PregAgeGrp <- DataPregMEG %>%
  ungroup()%>%
  filter(LCnt ==2 | LCnt ==3| LCnt ==4)%>%
  bind_rows(DataPregMEG)
PregAgeGrp <- DataPregMEG%>%
  filter(LCnt ==3 | LCnt ==4)%>%
  bind_rows(PregAgeGrp)
PregAgeGrp <- DataPregMEG%>%
  filter(LCnt ==4)%>%
  bind_rows(PregAgeGrp)%>%
#restrict births
  filter(AgeAtBirth >15,
         AgeAtBirth <50,
         DDate > '1945-01-01',
         grepl('L|M|F', Outcome),
         WomanId %in% DataDemMEGfert$IndividualId)%>%
  #create age categories in births dataset
  mutate( Agegroup = cut(AgeAtBirth, breaks = c(0,15, 20,25,30,35,40,45,50,100), 
                         labels = c('xu15B', 'x1519B', 'x2024B', 'x2529B', 'x3034B',
                                    'x3539B', 'x4044B', 'x4549B', 'xo50B')))



#create frame with a row for each year lived by someone in DataDemMEG 
YearsAlive <- data.frame(expand.grid(1945: 2019,
                                  IndividualId = unique(DataDemMEG$IndividualId)))%>%
  rename(Year = Var1)

DataDemY <- DataDem1 %>%
  select(IndividualId, Year, DoB, DoD, MotherId, Episode, Sex, StartDate) %>%
  filter(Sex ==2,
         DoB <'1995-01-01' & DoB > '1945-01-01')

Columns <- data.frame(x = colnames(DataDemY), y = NA)%>%
  pivot_wider(names_from = x, values_from = y)

ColYearsAlive <- left_join(YearsAlive, Columns, by = c('IndividualId', 'Year'))

Fig4_py <- rbind(DataDemY, ColYearsAlive)


#fill in created rows
Fig4_py1 <- Fig4_py %>%
  group_by(IndividualId)%>%
  fill(DoB, DoD, Sex, .direction = 'downup')%>%
  ungroup()%>%
  filter(Year >= decimal_date(DoB))%>%
  filter(Year <= decimal_date(DoD) | is.na(DoD))

#take out those after loss to followup
Fig4_py2 <- Fig4_py1 %>%
  arrange(IndividualId, Year)%>%
  group_by(IndividualId)%>%
  fill(Episode, .direction = 'up')%>%
  ungroup()%>%
  filter(!is.na(Episode)) 


Fig4_py3 <- Fig4_py2 %>%
 #ensure one row per year, delete duplicates
  group_by(IndividualId, Year)%>%
  slice(1)%>%
  ungroup()%>%
#create age categories in personyears dataset
  mutate(Age = Year - decimal_date(DoB),
         Agegroup = cut(Age, breaks = c(0,15, 20,25,30,35,40,45,50,100), 
                        labels = c('xu15', 'x1519', 'x2024', 'x2529', 'x3034', 
                                   'x3539', 'x4044', 'x4549', 'xo50')))%>%
  filter(Agegroup != 'xu15',
         Agegroup !='xo50')

Fig4_py4<- Fig4_py3 %>%
  #Create generations variable and restrict
  ungroup()%>%
  mutate( Generation= cut(DoB, breaks = as.Date( c('0000-01-01', '1945-01-01', 
                                          '1965-01-01', '1985-01-01', 
                                          '1995-01-01', '2050-01-01')),
                          labels = c('Other', 'Gen1', 'Gen2', 'Gen3', 'Other')))%>%
  filter(Generation != 'Other')

EvMar <- DataDemMEG %>%
  #evermarried variable selection
  select(IndividualId, EverMarried)%>%
  group_by(IndividualId)%>%
  slice(1)

Fig4_py5 <- Fig4_py4 %>%
  ungroup()%>%
  filter(IndividualId %in% DataPregMEG$WomanId)%>%
  ungroup()%>%
  left_join(EvMar, by = c('IndividualId' = 'IndividualId'))

#create final dataframes for all, evermarried, nevermarried women and births

ASpersonyearsM <- Fig4_py5%>%
  ungroup()%>%
  filter( EverMarried == 1)%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)%>%
  select(-Generation)

ASpersonyearsNM <- Fig4_py5%>%
  ungroup()%>%
  filter(EverMarried == 0)%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)%>%
  select(-Generation)

ASbirthsM <- PregAgeGrp %>%
  filter( EverMarried == 1)%>%
  ungroup()%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)

ASbirthsNM <- PregAgeGrp %>%
  filter( EverMarried == 0)%>%
  ungroup()%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)

ASFRcalcM <- cbind(ASbirthsM, ASpersonyearsM)%>%
  mutate(ASFR1519 = x1519B /x1519,
         ASFR2024 = x2024B /x2024,
         ASFR2529 = x2529B /x2529,
         ASFR3034 = x3034B /x3034,
         ASFR3539 = x3539B /x3539,
         ASFR4044 = x4044B /x4044,
         ASFR4549 = x4549B /x4549)%>%
  mutate(TFR = 5*( ASFR1519+ ASFR2024+ ASFR2529+ ASFR3034+ ASFR3539+ ASFR4044+ ASFR4549))

ASFRcalcNM <- cbind(ASbirthsNM, ASpersonyearsNM)%>%
  mutate(ASFR1519 = x1519B /x1519,
         ASFR2024 = x2024B /x2024,
         ASFR2529 = x2529B /x2529,
         ASFR3034 = x3034B /x3034,
         ASFR3539 = x3539B /x3539,
         ASFR4044 = x4044B /x4044,
         ASFR4549 = x4549B /x4549)%>%
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

#removing age-groups for which less than half women in generation have attained
ASFRM[18,4] <- NA
ASFRM[14,4] <-NA


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

#removing age-groups for which less than half women in generation have attained
ASFRNM[18,4] <- NA
ASFRNM[14,4] <-NA   

Fig4_ASFRcomb <- rbind(ASFRNM, ASFRM)

Figure4 <- ggplot(Fig4_ASFRcomb)+
  geom_line(mapping = aes(Agegroup, value, group = interaction(Generation, EverMarried),
                          colour = Generation, 
                          linetype = EverMarried),
            lwd = 2, alpha = 0.75)+
  geom_point(mapping = aes(Agegroup, value, group = Generation, colour = Generation,
                           shape = Generation), 
             size = 5, alpha = 0.9)+
  scale_color_manual(name = 'Generation', labels = c('1945-64', '1965-84', '1985-94'),
                     values = c('#54438E', "#587DBA", '#20934A'))+
  scale_linetype_manual(name = 'Marital Status', labels = c('Never-married', 'Ever-married'),
                        values = c(3,1))+
  scale_shape_manual(name = 'Generation', labels = c('1945-64', '1965-84', '1985-94'), 
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
  ylim(0,1.25)+
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3))


# Table 2 -----------------------------------------------------------------


cumsumASFRNM <- ASFRNM %>%
  group_by(Generation)%>%
  mutate(cumsum = 5*cumsum(value))%>%
  select(-c(value, TFR))%>%
  filter(!is.na(cumsum))%>%
  pivot_wider(names_from = Agegroup, values_from = cumsum)%>%
  ungroup()%>%
  mutate_if(is.numeric, round, 2)%>%
  mutate(EverMarried = 'Never-married')%>%
  select(EverMarried, Generation, `15-19`:`45-49`)


CumFertTableMar <- ASFRM %>%
  group_by(Generation)%>%
  mutate(cumsum = 5* cumsum(value))%>%
  select(-c(value, TFR))%>%
  filter(!is.na(cumsum))%>%
  pivot_wider(names_from = Agegroup, values_from = cumsum)%>%
  ungroup()%>%
  mutate_if(is.numeric, round, 2)%>%
  mutate(EverMarried = 'Ever-married')%>%
  select(EverMarried, Generation, `15-19`:`45-49`)%>%
  rbind(cumsumASFRNM)%>%
  mutate(Generation = ifelse(Generation == 'Gen1', '1945-64',
                             ifelse(Generation == 'Gen2', '1965-84', '1985-94')))%>%
  group_by(Generation)%>%
  gt()%>%
  tab_header('Cumulative TFR')%>%
  tab_style(locations = cells_column_labels(columns = everything()),
            style = list( cell_borders(sides = "bottom", weight = px(3)),
                          cell_text(weight = "bold")))%>%
  tab_style(locations = cells_title(groups = "title"),
            style     = list(cell_text(weight = "bold", size = 24)  ) )%>%
  cols_align(align = 'left', columns = Generation)%>%
  tab_source_note(source_note = "Data: Author's calculations from AHRI PIP data")%>%
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

gtsave(filename = 'Table2.rtf', data = CumFertTableMar, vheight = 1200)

# Figure 5 ----------------------------------------------------------------

#calculate summary statistics for age at first birth
Fig5_data <- subset(DataPregMEG, Generation != 'Other') %>%
  filter(BirthOrder == 1,
         AgeAtBirth < 25 & AgeAtBirth >= 12,
         WomanId %in% DataDemMEG$IndividualId)

Fig5_sum <- Fig5_data%>%
  group_by(EduMaxLbl, Generation)%>%
  summarise(Median = format(round(median(AgeAtBirth, na.rm = T), 2), nsmall = 2),
            Q1 = format(round(quantile(probs = 0.25, AgeAtBirth),2), nsmall = 2),
            Q3 = format(round(quantile( probs = 0.75, AgeAtBirth),2), nsmall = 2),
            Mean = format(round(mean(AgeAtBirth, na.rm = T),2), nsmall = 2),
            sd = format(round(sd(AgeAtBirth, na.rm = T),2), nsmall = 2),
            n = n())%>%
  mutate(Generation = factor(Generation, levels = c('Gen1', 'Gen2', 'Gen3'),
                             labels = c('1945-64', '1965-84', '1985-94')),
         IQR = as.numeric(Q3) - as.numeric(Q1))

#create education labels and set levels
Fig5_sum$EduMaxLbl <- factor(Fig5_sum$EduMaxLbl, 
                             levels = c('Primary\nor less', 'Some \nsecondary',
                                        'Matric', 'Some \ntertiary'),
                                           ordered = T)
EduNames2 <- c('Primary\nor less' = 'Primary or less',
               'Some \nsecondary' = 'Some secondary',
               'Matric' = 'Matric',
               'Some \ntertiary' = 'Some tertiary')

#build Figure 5
Figure5 <- ggplot(Fig5_sum)+
  geom_segment(aes(y = Generation, yend = Generation, 
                   x = as.numeric(Q1), xend= as.numeric(Q3)), lwd = 2, 
               colour = 'grey40', alpha = 0.9)+
  geom_point(aes(x = as.numeric(Median), y = Generation, colour = Generation), 
             size = 9, alpha = 0.9,
             shape = 15)+
  facet_wrap(~EduMaxLbl, ncol = 1, labeller = as_labeller(EduNames2))+
  scale_color_manual(name = 'Generation', labels = c('1945-64', '1965-84', '1985-94'),
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
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits = c(17,23.5), breaks = c(17:23))

ggplot_build(Figure5)




# Figure 6 ----------------------------------------------------------------

#restricting only to individuals who have been under surveillance since age 15
Observed15 <-DataDem %>%
  mutate(Age = decimal_date(StartDate) - decimal_date(DoB))%>%
  filter(IndividualId %in%DataDemMEG$IndividualId,
         Age<15,
         Sex ==2,
         DoB <'1995-01-01' & DoB >'1985-01-01')%>%
  group_by(IndividualId)%>%
  slice(1, with_ties = F)%>%
  ungroup()%>%
  select(IndividualId)


#Education12 selects the youngest age associated with being in grade 12
Education12 <- DataDem %>%
  filter(IndividualId %in% Observed15$IndividualId,
         Sex ==2,
         DoB >'1985-01-01' & DoB <'1995-01-01')%>%
  select(IndividualId, DoB, StartDate, EndDate, Education, EducationObsDate)%>%
  mutate(Age = decimal_date(EducationObsDate) - decimal_date(DoB))%>%
  filter(Education == 12,
         Age<25)%>%
  group_by(IndividualId)%>%
  slice_min(Age, with_ties = F)

#Education 11 selects the youngest age associated with being in 
#grade 10 or grade 11
Education11 <- DataDem %>%
  filter(IndividualId %in% Observed15$IndividualId,
         Sex ==2,
         DoB >'1985-01-01' & DoB <'1995-01-01')%>%
  select(IndividualId, DoB, StartDate, EndDate, Education, EducationObsDate)%>%
  mutate(Age = decimal_date(EducationObsDate) - decimal_date(DoB))%>%
  filter(Education == 11 | Education == 10)

Education11$Education[Education11$Education == 10] <-11

Education11 <- Education11%>%
  group_by(IndividualId)%>%
  slice_max(Age, with_ties = F)


#Education computes the date on which indiiduals are first recorded being in 
#grade 12, contingent on them preivously being observed in grade 10 or 11 
# within the last 2 years, and also includes all individuals who never
# attained grade 12
Education <- rbind(Education12, Education11)%>%
  select(IndividualId, DoB, Education, EducationObsDate)%>%
  pivot_wider(names_from = Education, values_from = EducationObsDate)%>%
  filter(!is.na(`11`),
         !is.na(`12` ))%>%
  separate(`12`, into="DateMatric", remove=F, sep="-")%>%
  mutate(DateMatric = as.numeric(DateMatric))%>%
  mutate(DateMatric = DateMatric-1)%>%
separate(`11`, into="Year11", remove=F, sep="-")%>%
  mutate(Year11 = as.numeric(Year11))%>%
  mutate(Year11 = Year11 - 1)%>%
  mutate(YearEnd = '12-15')%>%
  unite(DateMatric, c(DateMatric, YearEnd), sep='-', remove =F)%>%
  unite(Year11, c(Year11, YearEnd), remove=F, sep='-')%>%
  transform(DateMatric = as.Date(DateMatric),
            Year11 = as.Date(Year11))%>%
  mutate(AgeMatric = decimal_date(DateMatric) - decimal_date(DoB))%>%
  mutate(gap = decimal_date(DateMatric) - decimal_date(Year11))%>%
  filter(gap <2.1)%>%
  group_by(IndividualId)%>%
  slice_max(DateMatric, with_ties = F)%>%
  ungroup()



# Union5 computes the minimum age at which an individual entered marriage or an
# informal union
Union5 <- DataDem %>%
  filter(IndividualId %in% Observed15$IndividualId,
         MaritalStatus == 5 | MaritalStatus == 2)%>%
  mutate(UnionStatus = ifelse(MaritalStatus == 2 |MaritalStatus == 5, '1', '0'))%>%
  select(IndividualId, DoB, StartDate, EndDate, UnionStatus)%>%
  mutate(AgeAtUnion = decimal_date(StartDate) - decimal_date(DoB))%>%
  group_by(IndividualId)%>%
  slice_min(AgeAtUnion, with_ties = F)%>%
  ungroup()%>%
  pivot_wider(values_from = StartDate, names_from = UnionStatus)%>%
  select(IndividualId, AgeAtUnion, `1`)%>%
  rename(UnionDate = `1` )


#Union1 computes the oldest age at which an individual was single
Union1 <- DataDem %>%
  filter(IndividualId %in% Union5$IndividualId,
         MaritalStatus == 1)%>%
  select(IndividualId, DoB, StartDate, EndDate, MaritalStatus)%>%
  mutate(AgeAtSingle = decimal_date(StartDate) - decimal_date(DoB))%>%
  left_join(Union5, by= c('IndividualId'))%>%
  filter(AgeAtSingle <AgeAtUnion)%>%
  group_by(IndividualId)%>%
  slice_max(AgeAtSingle, with_ties = F)%>%
  ungroup()%>%
  pivot_wider(values_from = StartDate, names_from = MaritalStatus)%>%
  select(IndividualId, DoB, AgeAtSingle, `1` )%>%
  rename(SingleDate = `1` )



UnionBirths <- DataPregMEG%>%
  filter(BirthOrder == 1)%>%
  group_by(WomanId)%>%
  slice(1)%>%
  ungroup%>%
  select(WomanId, AgeAtBirth)

UnionEdu <- Education%>%
  select(IndividualId, AgeMatric)

#Union before observation period
UnionBefore <- DataDem%>%
  filter(IndividualId %in% Observed15$IndividualId,
         !is.na(MaritalStatus))%>%
  group_by(IndividualId)%>%
  slice_min(Episode)%>%
  ungroup%>%
  filter(MaritalStatus ==5 | MaritalStatus ==2)%>%
  left_join(UnionBirths, by = c('IndividualId' = 'WomanId'))%>%
  left_join(UnionEdu, by = c('IndividualId'))%>%
  filter(decimal_date(StartDate) - decimal_date(DoB) <AgeAtBirth,
         decimal_date(StartDate) - decimal_date(DoB) <AgeMatric)%>%
  mutate(AgeAtUnion = decimal_date(StartDate) - decimal_date(DoB),
         UnionDate = StartDate)%>%
  select(IndividualId, AgeAtUnion, UnionDate)


#union gives the age and date for which a person first entered a union after
# being observed being single
Union <- left_join(Union1, Union5, by = c('IndividualId'))%>%
  mutate(gap = decimal_date(UnionDate) - decimal_date(SingleDate))%>%
  filter(gap < 2,
         AgeAtUnion <25)%>%
  select(IndividualId, AgeAtUnion, UnionDate)%>%
  rbind(UnionBefore)%>%
  group_by(IndividualId)%>%
  slice_max(AgeAtUnion, with_ties = F)%>%
  ungroup()

#FirstBirth  selects the age at first birth available for all women in
#the 1985-94 generation
FirstBirth <- DataPregMEG %>%
  filter(WomanId %in% Observed15$IndividualId,
         MotherDoB > '1985-01-01' & MotherDoB <'1995-01-01',
         BirthOrder ==1,
         AgeAtBirth <25 & AgeAtBirth > 15)%>%
  select(WomanId, AgeAtBirth)%>%
  rename(AgeFirstBirth = AgeAtBirth)%>%
  #get rid of rows for multiple births
  group_by(WomanId)%>%
  slice(1, with_ties = F)%>%
  ungroup()
  

#EduMaxLbl selects the maximum education an individual is observed as having 
#over all survey rounds
EduMaxLbl <- DataDemMEG %>%
  select(IndividualId, EduMaxLbl)%>%
  group_by(IndividualId)%>%
  slice(1)

#LifeEvents joins an individual's date at reaching grade 12, having sex, and
#entering a union for all women in the generation who gave birth before age 25
LifeEvents <- Observed15 %>%
  left_join(FirstBirth, by = c('IndividualId' = 'WomanId'))%>%
  left_join(Education, by = c('IndividualId'))%>%
  left_join(Union, by = c('IndividualId'))%>%
  left_join(EduMaxLbl,  by = c('IndividualId'))%>%
  select(IndividualId, EduMaxLbl, AgeFirstBirth, AgeMatric, AgeAtUnion, DoB)

save(LifeEvents, file = 'LifeEvents')


#Figure 6 table and grpah ------------------------------------------------------

LifeEventsTable <- LifeEvents %>%
  select(IndividualId, AgeMatric, EduMaxLbl, AgeFirstBirth, AgeAtUnion)%>%
  mutate(GotMatric = EduMaxLbl == 'Some tertiary'| EduMaxLbl=='Matric')%>%
  filter(!is.na(AgeFirstBirth), 
         !is.na(AgeAtUnion),
         !is.na(AgeMatric))

tbl3_MBU <-LifeEventsTable %>%
  count(MBU = AgeMatric < AgeFirstBirth & AgeFirstBirth < AgeAtUnion)%>%
  pivot_wider(names_from = MBU, values_from = n)%>%
  mutate(n = `TRUE`+`FALSE`,
         perc = 100*(`TRUE`/n),
         Label = 'Matric, Birth, Union')
tbl3_MUB <-LifeEventsTable %>%
  count(MUB = AgeMatric < AgeAtUnion & AgeAtUnion < AgeFirstBirth)%>%
  pivot_wider(names_from = MUB, values_from = n)%>%
  mutate(n = `TRUE`+`FALSE`,
         perc = 100*(`TRUE`/n),
         Label = 'Matric, Union, Birth')
tbl3_UMB <-LifeEventsTable %>%
  count(UMB = AgeAtUnion <AgeMatric & AgeMatric <AgeFirstBirth)%>%
  pivot_wider(names_from = UMB, values_from = n)%>%
  mutate(n = `TRUE`+`FALSE`,
         perc = 100*(`TRUE`/n),
         Label = 'Union, Matric, Birth')
tbl3_UBM <-LifeEventsTable%>%
  count(UBM = AgeAtUnion < AgeFirstBirth & AgeFirstBirth <AgeMatric)%>%
  pivot_wider(names_from = UBM, values_from = n)%>%
  mutate(n = `TRUE`+`FALSE`,
         perc = 100*(`TRUE`/n),
         Label = 'Union, Birth, Matric')
tbl3_BMU <-LifeEventsTable %>%
  count(BMU = AgeFirstBirth < AgeMatric & AgeMatric < AgeAtUnion)%>%
  pivot_wider(names_from = BMU, values_from = n)%>%
  mutate(n = `TRUE`+`FALSE`,
         perc = 100*(`TRUE`/n),
         Label = 'Birth, Matric, Union')
tbl3_BUM <-LifeEventsTable %>%
  count(BUM = AgeFirstBirth < AgeAtUnion & AgeAtUnion < AgeMatric)%>%
  pivot_wider(names_from = BUM, values_from = n)%>%
  mutate(n = `TRUE`+`FALSE`,
         perc = 100*(`TRUE`/n),
         Label = 'Birth, Union, Matric')


Table3 <- bind_rows(tbl3_BUM, tbl3_BMU, tbl3_UBM, tbl3_UMB, tbl3_MUB, tbl3_MBU )%>%
  select(Label, perc)%>%
  rename(' ' = Label,
         '%' = perc) %>%
  mutate_if(is.numeric, round, 0)%>%
  gt()%>%
  tab_header('Order of vital conjunctures among women giving birth between ages 15 and 25')%>%
  tab_style(locations = cells_title(groups = "title"),
            style     = list(cell_text(weight = "bold", size = 24)  ))%>%
  tab_source_note(source_note = "Data: Author's calculations from AHRI PIP data")

gtsave(filename = 'LifeEventsTable.rtf', data = Table3, vwidth = 300)


#Fig6 graph 

LifeEventsTable1 <- LifeEvents%>%
  mutate(Order = ifelse(AgeMatric < AgeFirstBirth & AgeFirstBirth <AgeAtUnion, 'MBU',
                        ifelse(AgeMatric < AgeAtUnion & AgeAtUnion < AgeFirstBirth, 'MUB',
                               ifelse(AgeAtUnion <AgeMatric & AgeMatric <AgeFirstBirth, 'UMB',
                                      ifelse( AgeAtUnion < AgeFirstBirth & AgeFirstBirth <AgeMatric, 'UBM',
                                              ifelse(AgeFirstBirth < AgeMatric & AgeMatric < AgeAtUnion, 'BMU', 'BUM'))))))%>%
  filter(!is.na(Order))%>%
  group_by(Order)%>%
  summarise(MedianBirth = median(AgeFirstBirth),
            MedianUnion = median(AgeAtUnion),
            MedianMatric = median(AgeMatric))%>%
  pivot_longer(cols = c(MedianBirth, MedianUnion, MedianMatric), names_to = 'Event' )

LifeEventsTable1$Order <- factor(LifeEventsTable1$Order,
                               levels = c('BUM', 'BMU', 'UBM', 'UMB', 'MUB', 'MBU'),
                               ordered = T)

  
Table3plot <- ggplot(LifeEventsTable1)+
  geom_segment(aes(y = Order, yend = Order, 
                   x = 17, xend= 22), lwd = 2, colour = 'grey40', alpha = 0.9)+
  geom_point(aes(x = as.numeric(value), y = Order, colour = Event), 
             size = 9, alpha = 0.9,
             shape = 15)+
  scale_color_manual(name = 'Event', labels = c('First Birth', 'Achieved Matric', 'First Union'),
                     values = c('#54438E', '#D15439', '#5ab4ac'))+
  theme(panel.background = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        axis.title= element_blank(),
        title = element_text(margin = margin(r = 20), size = 20, 
                             colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(colour = 'black', size = 15),
        axis.text.x = element_text(colour = 'black', size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour='black', size=1.5),
        legend.key.size = unit(1.2, 'cm'),
        legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.key = element_rect(fill = alpha("white", 0.0)))+
  labs( y= '', x = '')+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits = c(17,22), breaks = c(17:22))


#APPENDIX-----------------------------------------------------------------------
## App2 Table 1 -----------------------------------------------------------------

AllMarPropT <- DataDemMEG %>%
  ungroup()%>%
  count(Generation, Sex)

EverMarPropT <- DataDemMEG %>%
  ungroup()%>%
  filter(EverMarried == 1)%>%
  count(Generation, Sex)%>%
  rename(MarEver = n)

CurMarT<- DataDem %>%
  ungroup()%>%
  filter(MaritalStatus == 2,
         IndividualId %in% DataDemMEG$IndividualId)%>%
  filter(StartDate > '2018-01-01' & StartDate <'2019-01-01')%>%
  group_by(IndividualId)%>%
  slice_max(MaritalStatus, with_ties = F)%>%
  ungroup()%>%
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse(DoB > '1985-01-01' & DoB < '1995-01-01','Gen3', 'Other'))))%>%
  count(Generation, Sex)%>%
  rename(CurMar = n)

CurMarAllT<- DataDem %>%
  ungroup()%>%
  filter(StartDate > '2018-01-01' & StartDate <'2019-01-01',
         IndividualId %in% DataDemMEG$IndividualId)%>%
  group_by(IndividualId)%>%
  slice(1)%>%
  ungroup()%>%
  mutate( Generation= 
            ifelse (DoB > '1945-01-01' & DoB < '1965-01-01', 'Gen1', 
                    ifelse (DoB >= '1965-01-01' & DoB < '1985-01-01','Gen2',
                            ifelse(DoB > '1985-01-01' & DoB < '1995-01-01','Gen3', 'Other'))))%>%
  count(Generation, Sex)%>%
  rename(CurMarN = n)


Fig2_TableApp <- left_join(EverMarPropT, AllMarPropT, by =  c('Sex', 'Generation'))%>%
  left_join(CurMarT, by = c('Sex', 'Generation'))%>%
  left_join(CurMarAllT, by = c('Sex', 'Generation'))%>%
  mutate(MarProp = MarEver*100/n,
         CurMarProp = CurMar*100/CurMarN)%>%
  select(Generation, Sex, MarProp, MarEver, CurMarProp, CurMar)%>%
  filter(Generation != 'Other')%>%
  transform(Sex = ifelse(Sex == 1, 'Male', 'Female'))%>%
  mutate_if(is.numeric, round, 2)%>%
  gt()%>%
  tab_header('Marriage rates in the AHRI PIP data')%>%
  tab_row_group(label = 'Generation 1945-64', 
                rows = Generation == 'Gen1') %>%
  tab_row_group(label = 'Generation 1965-84', 
                rows = Generation == 'Gen2') %>%
  tab_row_group(label = 'Generation 1985-94', 
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
    MarProp = "Ever-\nmarried (2000-2019) % (n)",
    CurMarProp = "Currently \nMarried (2018) % (n)"
  )%>%
  cols_hide(Generation)%>%
  cols_merge_n_pct(MarProp,MarEver)%>%
  cols_merge_n_pct(CurMarProp,CurMar)%>%
  cols_width(
    Sex ~ pct(30))%>%
  tab_options(table.width = pct(40))%>%
  tab_source_note(source_note = "Data: Author's calculations from AHRI PIP data 2000-2019")%>%
  row_group_order(groups = c("Generation 1945-64", "Generation 1965-84", "Generation 1985-94"))


gtsave(filename = 'MarriageRateTable.rtf', data = Fig2_TableApp, vwidth = 800)


## App3 Table 1 ------------------------------------------------------------
#ANOVA proof of significance
DataDemMEG_AOV <- Fig5_data

DataDemMEG_AOV1 <- DataDemMEG_AOV%>%
  filter(EduMaxLbl == 'Primary\nor less')
DataDemMEG_AOV2 <- DataDemMEG_AOV%>%
  filter(EduMaxLbl == 'Some \nsecondary')
DataDemMEG_AOVM <- DataDemMEG_AOV%>%
  filter(EduMaxLbl == 'Matric')
DataDemMEG_AOV3 <- DataDemMEG_AOV%>%
  filter(EduMaxLbl == 'Some \ntertiary')

Edu1AOV <- summary(aov(AgeAtBirth ~ Generation, data = DataDemMEG_AOV1))
Edu2AOV <- summary(aov(AgeAtBirth ~ Generation, data = DataDemMEG_AOV2))
MatricAOV <- summary(aov(AgeAtBirth ~ Generation, data = DataDemMEG_AOVM))
Edu3AOV <- summary(aov(AgeAtBirth ~ Generation, data = DataDemMEG_AOV3))

GenAOV <- summary(aov(AgeAtBirth ~ Generation, data = DataDemMEG_AOV))
EduAOV <- summary(aov(AgeAtBirth ~ EduMaxLbl, data = DataDemMEG_AOV))

##table 1 appendix4
Appendix3 <- Fig5_sum%>%
  mutate(a = ' (',
        b = ')')%>%
   unite(IQR, c(a,Q1), sep = "", remove = T)%>%
  unite(IQR, c(IQR,Q3), sep = ", ", remove = T)%>%
  unite(IQR, c(IQR, b), sep = "", remove = T)%>%
  select(Generation, EduMaxLbl, Median, IQR, Mean, sd, n)%>%
  rename(s.d. = sd)%>%
  gt()%>%
  tab_header('Age at first birth among 12 - 25 year-olds')%>%
  tab_row_group(label = 'Primary or less  ***', 
                rows = EduMaxLbl == 'Primary\nor less') %>%
  tab_row_group(label = 'Some secondary  ***', 
                rows = EduMaxLbl == 'Some \nsecondary') %>%
  tab_row_group(label = 'Matric  *', 
                rows = EduMaxLbl == 'Matric')%>%
  tab_row_group(label = 'Some tertiary  **', 
                rows = EduMaxLbl == 'Some \ntertiary')%>%
  tab_style(locations = cells_column_labels(columns = everything()),
            style = list( cell_borders(sides = "bottom", weight = px(3)),
                          cell_text(weight = "bold")))%>%
  tab_style(locations = cells_title(groups = "title"),
            style     = list(cell_text(weight = "bold", size = 24)  ) )%>%
  cols_align(align = 'left', columns = Generation)%>%
  cols_align( align = 'center', columns = c(n,IQR))%>%
  tab_style(locations = cells_row_groups(groups = everything()),
            style = list(
              cell_fill(color = "grey90"),
              cell_text(style = "italic") ))%>%
  tab_source_note(source_note = "One-way ANOVA: ***p <0.001  **p <0.01  *p <0.05")%>%
  tab_source_note(source_note = "Data: Author's calculations from AHRI PIP data")%>%
  row_group_order(groups = c('Some tertiary  **','Matric  *', 'Some secondary  ***',
                             'Primary or less  ***'))


gtsave(filename = 'AgeFirstBirth.rtf', data = Appendix4, vwidth = 1200, vheight = 3200)


#Calculations not shown--------------------------------------------------------
## overall TFR ----------------------------------------------------------------

ASpersonyears <- Fig4_py5%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)%>%
  select(-Generation)

ASbirths <-  PregAgeGrp %>%
  ungroup()%>%
  filter(!is.na(Generation))%>%
  count(Generation, Agegroup)%>%
  pivot_wider(names_from = Agegroup, values_from = n)

ASFRcalc <- cbind(ASbirths, ASpersonyears)%>%
  mutate(ASFR1519 = x1519B /x1519,
         ASFR2024 = x2024B /x2024,
         ASFR2529 = x2529B /x2529,
         ASFR3034 = x3034B /x3034,
         ASFR3539 = x3539B /x3539,
         ASFR4044 = x4044B /x4044,
         ASFR4549 = x4549B /x4549)%>%
  mutate(TFR = 5*( ASFR1519+ ASFR2024+ ASFR2529+ ASFR3034+ ASFR3539+ ASFR4044+ ASFR4549))

ASFRall <- ASFRcalc %>%
  rename('15-19' = ASFR1519,
         '20-24' = ASFR2024,
         '25-29' = ASFR2529,
         '30-34' = ASFR3034,
         '35-39' = ASFR3539,
         '40-44' = ASFR4044,
         '45-49' = ASFR4549)%>%
  select(c(Generation, '15-19':TFR))%>%
  pivot_longer(cols = '15-19':'45-49', names_to = 'Agegroup')

ASFRall[18,4] <- NA
ASFRall[14,4] <-NA  


CumFertTableAll <- ASFRall %>%
  group_by(Generation)%>%
  mutate(cumsum = 5* cumsum(value))%>%
  select(-c(value, TFR))%>%
  filter(!is.na(cumsum))%>%
  pivot_wider(names_from = Agegroup, values_from = cumsum)%>%
  ungroup()%>%
  mutate_if(is.numeric, round, 2)%>%
  select(Generation, `15-19`:`45-49`)%>%
  mutate(Generation = ifelse(Generation == 'Gen1', '1945-64',
                             ifelse(Generation == 'Gen2', '1965-84', '1985-94')))%>%
  group_by(Generation)%>%
  gt()%>%
  tab_header('Cumulative TFR')%>%
  tab_style(locations = cells_column_labels(columns = everything()),
            style = list( cell_borders(sides = "bottom", weight = px(3)),
                          cell_text(weight = "bold")))%>%
  tab_style(locations = cells_title(groups = "title"),
            style     = list(cell_text(weight = "bold", size = 24)  ) )%>%
  cols_align(align = 'left', columns = Generation)%>%
  tab_source_note(source_note = "Data: Author's calculations from AHRI PIP data")%>%
  tab_spanner(label = 'Age group', columns = `15-19`:`45-49`  )%>%
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "---"
  )%>%
  tab_style(locations = cells_row_groups(groups = everything()),
            style = list(
              cell_fill(color = "grey90"),
              cell_text(style = "italic")))

## Births to never-married women-----------------------------------------------
PregAgeGrp%>%
  filter(Generation == 'Gen2' | Generation == 'Gen3')%>%
  count(EverMarried==0)

## Median age at birth all gens --------------------------------------------
Fig5_data%>%
  group_by(Generation)%>%
  summarise(median(AgeAtBirth))

Fig5_data%>%
  summarise(median(AgeAtBirth))

## Births under 20 in each generation------------------------------------------
DataPregMEG%>%
  filter(BirthOrder==1)%>%
  count(Generation, MotherAge<20)%>%
  pivot_wider(names_from = 'MotherAge < 20', values_from = n)%>%
  mutate(perc = `TRUE` / (`TRUE`+`FALSE`))

## Teenage pregnanacies to children of teenage pregnaancies--------------------
TeenPreg <- DataPregMEG %>%
  filter(MotherAge <20)

MothersPreg <- DataDemMEG%>%
  filter(IndividualId %in% DataPregMEG$WomanId) %>%
  mutate(TeenagePreg = ifelse(IndividualId %in% TeenPreg$WomanId, '1', '0'))%>%
  select(IndividualId, TeenagePreg, EduMaxLbl)%>%
  filter(!is.na(TeenagePreg))

DaughtersPreg <- DataDemMEG %>%
  filter(Generation =='Gen3',
         !is.na(MotherId),
         IndividualId %in% DataPregMEG$WomanId) %>%
  mutate(TeenagePreg = ifelse(IndividualId %in% TeenPreg$WomanId, '1', '0'))%>%
  select(IndividualId, MotherId, TeenagePreg)


Fig6_MumDaughtPreg <- left_join(x = MothersPreg, y= DaughtersPreg, 
                            by = c("IndividualId" = 'MotherId'))


Fig6_MumDaughtPreg <- Fig6_MumDaughtPreg %>%
  rename(MotherIndividualId = IndividualId,
         ChildIndividualId = IndividualId.y,
         MotherTeenagePreg = TeenagePreg.x,
         ChildTeenagePreg = TeenagePreg.y)%>%
  group_by(ChildIndividualId)%>%
  slice(1)%>%  # selecting only one obs of a daughter-mother pair
  ungroup

# Graph for education - 1985-95 Generation
EduNames <- c("Primary\nor less" = "Primary\nor less",
              "Some \nsecondary" = "Some \nsecondary", 
              "Matric" = "Matric", 
              'Some \ntertiary' = 'Some \ntertiary')


Fig6_propsPreg <- Fig6_MumDaughtPreg %>%
  ungroup()%>%
  count(MotherTeenagePreg, ChildTeenagePreg, EduMaxLbl)%>%
  mutate (MotherTeenagePreg = ifelse(MotherTeenagePreg == 0, 'No', 'Yes'))

Fig6_propsPreg$EduMaxLbl <- factor(Fig6_propsPreg$EduMaxLbl,
                               levels = c('Primary\nor less', 'Some \nsecondary', 'Matric', 'Some \ntertiary'),
                               ordered = T)


Figure6 <- ggplot(Fig6_propsPreg)+
  geom_col(aes(x = MotherTeenagePreg, y= n, group = ChildTeenagePreg, 
               fill = ChildTeenagePreg), position = 'fill')+
  facet_wrap(~EduMaxLbl, labeller = as_labeller(EduNames), ncol = 4)+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))+
  scale_fill_manual(name = "Daughter's marital status", labels = c('Never-married', 'Ever-married'),
                    values = c('grey75', '#20934A'))+
  theme(strip.background = element_rect('#587DBA'),
        legend.position = 'bottom',
        panel.spacing = unit(0.1, "lines"),
        axis.title.y = element_text(margin = margin(r = 18), size = 18, 
                                    colour = 'grey30'),
        axis.title.x = element_text(margin = margin(t = 10), size = 20, 
                                    colour = 'grey30'),
        plot.subtitle = element_text( size = 19, 
                                      colour = 'grey30', hjust = 0.5, 
                                      margin = margin(b = 10)),
        legend.title = element_text(colour = 'grey30', size = 15),
        legend.text = element_text(colour = 'grey30', size = 15),
        axis.text = element_text(colour = 'grey30', size = 15),
        legend.key.size = unit(1, 'cm'),
        strip.text.x = element_text(size =20, colour = 'white'))+
  labs( x= "Mother had teenage birth", y = 'Proportion of daughters with teenage birth', 
        subtitle = "Mother's education level")

ggplot_build(Figure3)



