#23-0320 mon 19:25

#
install.packages("clipr")
library(clipr)
library(tidyverse)
install.packages("clean_names")
library("clean_names")

#student
Student ID,Full Name,favourite.food,mealPlan,AGE
1,Sunil Huffmann,Strawberry yoghurt,Lunch only,4
2,Barclay Lynn,French fries,Lunch only,5
3,Jayendra Lyne,N/A,Breakfast and lunch,7
4,Leon Rossini,Anchovies,Lunch only,
5,Chidiegwu Dunkel,Pizza,Breakfast and lunch,five
6,Güvenç Attila,Ice cream,Lunch only,6

#
example("read.csv")


read.csv(file = "clipboard")
read.table(file = "clipboard", sep = "\t")

#
read_csv("a,b,c
         1,2,3
         4,5,6")

read_csv("a,b,c
         1,2,3
         4,5,6
         7,8,9
         10,11,12", skip = 2)

read_csv("#a,b,c
         1,2,3
         #4,5,6
         7,8,9
         10,11,12", comment = '#')  # '#'붙은 곳은 출력 안 함,

#
read_csv("1,2,3\n4,5,6",col_names = FALSE)  #열 이름을 R에서 자동으로 할당
read_csv("1,2,3\n4,5,6",col_names =c("갈","김","목"))  #열 이름을 사용자가 지정

read_csv("x,y,z
         2,5,.", na = '.')  #결측값을 명시적으로 표시

read_csv("x,y,z
         2,5,4
         2,/,4", na = '/')  #문자는 상관없음

#
install.packages("janitor")   #clean_names() 함수 쓰기 위해 필요함
library(janitor)              #clean_names() 함수 쓰기 위해 필요함

student
student |> is.na()
student |> clean_names()  # 한 번에 모든 열의 이름 snakecase로 바꿈
#snakecase: 열 이름의 첫 글자는 소문자로, 언더바로 연결

#
# example(clipr)
# args(clipr)
# read_clip() -> a
# a |> view()
# read.csv("clipboard", sep = ",")

(read_clip_tbl() -> eungee1) #|> view()
str(eungee1)
eungee1



# ---------
#read.csv('clipboard', sep = ',', na = c("N/A","")) -> student_2


typeof(eungee1$Student.ID)
typeof(eungee1$mealPlan) 

#str() 혹은 glimpse()
  #데이터셋 모든 컬럼의 자료형을 일괄 조회
str(eungee1)
glimpse(eungee1)

#
eungee1 |> mutate(
  meal_plan = as.factor(mealPlan)
) #|> str()

eungee1 |> mutate(
  meal_plan = as.factor(mealPlan)
) |> glimpse()

#
eungee1
eungee1 |> complete(AGE, AGE)


#null제거
is.na(eungee1)
str_detect(eungee1$favourite.food, "N/A")
str_replace_all(eungee1$favourite.food, "N/A", NA)


#
student_2
typeof(student_2$mealPlan)      #character
typeof(student_2$Student.ID)    #integer
typeof(student_2$Full.Name)     #character
typeof(student_2$favourite.food)    #character
typeof(student_2$AGE)    #character

student_2 |> clean_names() |> 
  mutate(meal_plan = factor(meal_plan)) -> student_3  #meal_plan열의 데이터형을 factor로 바꾸기

student_3
typeof(student_3$meal_plan)  #integer..?








