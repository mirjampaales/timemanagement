#Title: Time management skills as predictor of first semester grades
##Authors: 

### SETUP ###
library(dplyr) #pipe %>% things to nice df-s
library(psych) #for factor analysis
library(QuantPsyc) #for factor analysis
library(GPArotation) #for factor analysis
library(Hmisc)
library(lavaan) 
library(skimr) #for quick looks

### IMPORT THE DATA ###

load('data/clean_data_15.06.RData')

### INITIAL OVERVIEW OF THE SAMPLE
# 1. Age and gender #
age_gender <- study_data %>% 
  dplyr::select(student_id, gender, birthyear)
age_gender <- unique(age_gender)

age_gender$age <- 2019-age_gender$birthyear
table(age_gender$gender,age_gender$age)


#59 boys and 28 girls were 19, 41 boys and 6 girls were 20 years old
#134 students were 19 or 20 years old at the year of this study


#create a vector of courses that are compulsory during the first semester
first_compulsory <- c("Sissejuhatus erialasse LTAT.03.002", #A.K.A. 'Introduction to Speciality'
                      "Matemaatiline maailmapilt MTMM.00.342",  #A.K.A. 'Introduction to Discrete Mathematics'
                      "Kõrgem matemaatika I MTMM.00.340", #A.K.A. 'Introduction to Calculus'
                      "Arvuti arhitektuur ja riistvara I LOFY.03.079",  #A.K.A. 'Computer Architecture and Hardware'
                      "Programmeerimine LTAT.03.001")  #A.K.A. 'Python Programming'


# 2. courses and grades #

# students' workload in number of courses and ECTS
registrations <- study_data %>% 
  group_by(student_id, course) %>% #group by courses, to lose the repetitions
  summarise(points = max(ects)) #using max helps to have each course only once


workload <- registrations %>% 
  group_by(student_id) %>% 
  summarise(no_of_courses = length(course),
            total_credit_points = sum(points)) %>% 
  arrange(desc(no_of_courses))

#take a quick look at the workload
barplot(table(workload$total_credit_points))
barplot(table(workload$no_of_courses))
summary(workload)
#it is fairly comparable, mostly 5-6 courses and 30 credit points:
# student_id        no_of_courses    total_credit_points
# Length:189         Min.   : 1.000   Min.   : 3.00      
# Class :character   1st Qu.: 5.000   1st Qu.:30.00      
# Mode  :character   Median : 5.000   Median :30.00      
#                   Mean   : 5.407   Mean   :31.14      
#                   3rd Qu.: 6.000   3rd Qu.:33.00      
#                   Max.   :11.000   Max.   :50.00


#since some of the students take some exams twice, we need to look at the higher result for each course
final_grades <- study_data %>% 
  filter(grade_no >= -1 & grade_no <= 5) %>% #leave out the courses with pass-fail grade that are coded as 6 / -6
  mutate(grade_no = replace(grade_no, grade_no == -1, 0)) %>% 
  group_by(student_id, course) %>% 
  summarise(finalgrade = max(grade_no)) #even when taking the exam twice the highest grade is the final

summary(final_grades)


#now, using the final grades for each course, we calculate the grade point average (gpa) for each student
GPA <- final_grades %>% 
  filter(course %in% first_compulsory) %>% #only look at grades from compulsory courses 
#  mutate(grade_no <- replace(finalgrade, finalgrade == -1, 0)) %>% #???asenda -1 j2lle nullidega, et keskmist arvutada
  group_by(student_id) %>%
  summarise(average_grade = mean(finalgrade))

summary(GPA)
# student_id        average_grade  
# Length:188         Min.   :0.000  
# Class :character   1st Qu.:2.400  
# Mode  :character   Median :3.200  
#                   Mean   :3.102  
#                   3rd Qu.:4.000  
#                   Max.   :5.000

### TIME USE ###
first_monday <- as.Date('2019-09-30', format = "%Y-%m-%d")

# Create the main data frame we are going to use later, the Study Time: ###
std_time <- time_data %>% 
  group_by(student_id) %>% 
  filter(study_time_amount != 24) %>% #this would most probably a sign that the student made an entry to test the environment
  filter(course %in% first_compulsory) %>% 
  summarise(total_hours_reported = sum(study_time_amount),
            average_time_per_day = sum(study_time_amount)/length(unique(reported_date)),
            no_of_reports = length(unique(reported_date)),
            fill_later = as.numeric(mean(answer_date - reported_date)),
            no_of_methods = length(unique(learning_method)))

time_data$day <- weekdays(time_data$reported_date)

# ? for each student find how much they study during each day
daily_students <- time_data %>% 
  group_by(student_id, reported_date) %>% 
  #filter(study_time_amount != 0.0) %>% 
  filter(study_time_amount != 24) %>% 
  filter(course %in% first_compulsory) %>% 
  summarise(days_total = sum(study_time_amount),
            day = day)

daily_students <- unique(daily_students)
summary(daily_students)

# student_id        reported_date          days_total         day           
# Length:38945      Min.   :2019-09-29   Min.   : 0.000   Length:38945      
# Class :character  1st Qu.:2019-10-09   1st Qu.: 3.000   Class :character  
# Mode  :character  Median :2019-10-17   Median : 4.500   Mode  :character  
#                   Mean   :2019-10-17   Mean   : 4.638                     
#                   3rd Qu.:2019-10-26   3rd Qu.: 6.500                     
#                   Max.   :2019-11-06   Max.   :18.500                     



#the days have a strange order to them, lets fix it
daily_students$day <- factor(daily_students$day, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


#? how does the (average) week look like in terms of study time
weeks <- daily_students %>% 
  group_by(day) %>% 
  summarise(all_time = sum(days_total),
            avg_time = mean(days_total),
            min_time = min(days_total),
            max_time = max(days_total)) %>% 
  arrange(day)

weeks

#the weeks are fine. tuesdays are the busiest days, saturdays are least



  ###################
#### FACTORANALYSIS ####
  ###################

tmq_items <- read.table(file = "data/tmq.txt", sep = "\t", header=TRUE, stringsAsFactors = FALSE)


efa_data = tmq_data %>% dplyr::select(tmq1:tmq28)
efa_fit_3 = fa(efa_data, nfactors = 3)
loadings_3 <- loadings(efa_fit_3, sort = T)

#hand-selecting only those with a loading >0.5
efa_data_3 <- efa_data %>% dplyr::select(tmq1, tmq3, tmq6, tmq8, tmq9, 
                                  tmq10, tmq12, tmq13, tmq14, tmq15, tmq16, tmq17, tmq18, tmq19, 
                                  tmq20, tmq21, tmq22, tmq24, tmq26, tmq27)

efa_fit_3_again <- fa(efa_data_3, nfactors = 3)
loadings_3_again <- loadings(efa_fit_3_again, sort = T)

######lets make it prettier better to read and contextualize ####
loads <- as.data.frame(unclass(efa_fit_3_again$loadings))
code <- row.names(loads)
loads$code <- code

items <- tmq_items %>% dplyr::select(code, name)
loads_items <- merge(x=items,y=loads, by="code")

loads_items <- as.data.frame(unclass(loads_items))
loads_items[loads_items<0.5]<-""

grouped_items <- loads_items %>% 
  mutate_if(is.numeric, round,3) %>% 
  arrange(desc(MR1), desc(MR2), desc(MR3)) 

#using the gruped_items we list the items into factors as following:
f1 <- c('tmq8', 'tmq13', 'tmq19', 'tmq17', 'tmq24', 'tmq15', 'tmq26', 'tmq6', 'tmq27')
f2 <- c('tmq16', 'tmq14', 'tmq10', 'tmq21', 'tmq9', 'tmq20', 'tmq22')
f3 <- c('tmq12','tmq18','tmq3')


#### USING THE RESULTS OF FACTOR ANALYSIS WE TURN BACK TO GRADES ####

time_grades <- merge(x = std_time, y = GPA, by = 'student_id')
background <- study_data %>% 
  dplyr::select(student_id, gender, entry_score)
background <- unique(background)


time_grades_background <- merge(x = time_grades, y = background, by = 'student_id')


q_1 <- tmq_data %>% 
  group_by(student_id) %>% 
  summarise(F1 = mean(tmq8, tmq13, tmq19, tmq17, tmq24, tmq15, tmq26, tmq6, tmq27),
            F2 = mean(tmq16,tmq14, tmq10, tmq21, tmq9, tmq20, tmq22),
            F3 = mean(tmq12,tmq18,tmq3))


to_corelate <- merge(x = time_grades_background, y = q_1, by = 'student_id')

#now that the data is merged we can lose the student id
to_corelate <- to_corelate %>% 
  dplyr::select(-student_id)

#create a table, export to file to look at it more closely
cor2 <- rcorr(as.matrix(to_corelate), type = 'spearman')
write.csv(cor2$r, file = 'cor.csv')
write.csv(cor2$P, file = 'pvalue.csv')


#we see that the  correlations that are >= |0.4| are the following:
#
#
#

### MODELS ###
m1 = lm(average_grade ~ total_hours_reported, data = to_corelate)
summary(m1)

model_1 = lm(average_grade ~ no_of_reports+entry_score+F1+F2+gender, data = to_corelate)
summary(model_1)
model_2 = lm(average_grade ~ total_hours_reported+F2+gender, data = to_corelate)
summary(model_2)


myModel <- '
average_grade ~ total_hours_reported + entry_score
total_hours_reported ~ F1+F2+F3+gender+entry_score
'
fit <- sem(model = myModel,
           data  = to_corelate)

summary(fit)
plot(fit)
