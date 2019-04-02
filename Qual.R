setwd("D://Data Analytics/Project 1")
data<-read.csv("qfc.csv")

require(stringr)


demo1 <- strsplit(as.character(data$Qualification),"\\,|\\-")

len = length(demo1)
valuecol=0
qscore=0
value=0
i=1
j=1
for(i in 1:len)
{
  
  len1 = length(demo1[[i]])
  j=1
  for(j in 1:len1)
  {
    a=demo1[[i]][j]
    a=str_trim(a)
    qscore=findvalue(a)
    qscore
    value[j]=qscore
    
  #  data$qualmaxvalue=maxvalue
    j=j+1
  }
  valuecol[i]=max(value)
  value=0
  i=i+1
}
data$Quali=valuecol


#function
findvalue <- function(a){
  
  #if a is null value - return 0
  #if a is not null check further condition, else return 0
#condition to check Value 1

val=0
value=0

r1=grep("diploma", a,ignore.case = TRUE)
r1=length(r1)
r2=grep("Certificate", a,ignore.case = TRUE)
r2=length(r2)
r3=grep("fellowship", a,ignore.case = TRUE)
r3=length(r3)
r4=0


if(r1==0 | r2==0 | r3== 0) {
  test = substr(a,1,1)
  if(test == "d" | test == "D"){ 
    r4=1 
  } else{ r4=0
    } 
}



if(r1 == 1 | r2 == 1 | r3 == 1 | r4 == 1){
  val=1
} else {
  val = 0
}
  
#condition to check Value 2

r5=grep("Certificates", a,ignore.case = TRUE)
r5=length(r5)
r6=grep("Fellowship", a,ignore.case = TRUE)
r6=length(r6)
r7=0

if(r5==0 | r6==0 ){
  test = substr(a,1,1)
  if(test == "F" | test =="f") {
    r7=1
  } else {r7==0}
  
}

if(r5 == 1 | r6 == 1 | r7 == 1)
{
  val=2
} else {val =val}

#Condition to check value 3

r8=grep("MBBS", a,ignore.case = TRUE)
r8=length(r8)
r9=0

if(r8==0){
  test = substr(a,1,1)
  if(test == "B" | test =="b") {
    r9=1
  } else {r9=0}
  
}

if(r8 == 1 | r9 == 1)
{
  val=3
} else {val =val}

#Condition to check value 4

r10=grep("Doctor", a,ignore.case = TRUE)
r10=length(r10)
r11=0
r12=0

if(r10==0){
  test = substr(a,1,1)
  if(test == "M" && a !="MBBS") {
    r10=1
  } else {r10=0}
  
}

if(r10==0 & r11 == 0){
  test = substr(a,1,1)
  if(test == "P" && a !="PhD") {
    r12=1
  } else {r12=0}
  
}

if(r10 == 1 | r11 == 1 | r12 == 1)
{
  val=4
} else {val =val}

#Condition to check Value 5


r13=grep("PhD", a,ignore.case = TRUE)
r13=length(r13)
r14=grep("Doctrate", a,ignore.case = TRUE)
r14=length(r14)


if(r13 == 1 | r14 == 1)
{
  val=5
} else {val =val}

return(val)

}
