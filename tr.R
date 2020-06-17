args <- commandArgs(trailingOnly = TRUE)

library("RPostgreSQL")
library("survival")
library("survminer")
library("tidyverse")
library("lubridate")
library("flexsurv")

#f <- args[1]
se <-  "SELECT * from preprocessing NATURAL join dimension  "
f <- "WHERE (dimension.\"BRAND\" = 'PHONGPIMARN' OR dimension.\"BRAND\" = 'SAHABHANT') AND (dimension.\"MATERIAL\" = '1-05-001-0005' OR dimension.\"MATERIAL\" = '1-05-000-0001')
AND dimension.\"COUNTY\" ='A'"
er <- "order by preprocessing.\"PEANO\",preprocessing.\"NUM\";"

#y <- str_replace_all(f, "BRAND", "\"BRAND\"")
#v <- str_replace_all(f, "[\\]","[\"]")
#v <- (gsub("[\\]", "\"",f))
r <- paste(se,f,er,sep = " ")

con=dbConnect(PostgreSQL(), user="postgres",dbname="dbtest",password="au122798")
df_3<- dbGetQuery(con,r)



df_3 <- df_3 %>% mutate(date_time = as.POSIXct(Sys.Date()))


df_3$time <- NA



first_broken_found <- FALSE
for (row in 1:nrow(df_3)){
    
    peano <- df_3[row, "PEANO"]
    
    if (first_broken_found == FALSE) {
        if (df_3[row, "BROKEN"] == TRUE)  {
            df_3[row, "time"] <- df_3[row, "BROKENDATE"] - df_3[row, "FROM"]
            first_broken_found <- TRUE
            
        }else{
            if(df_3[row, "TO"] == as.POSIXct("9999-12-31")){
                df_3[row, "time"] <- df_3[row, "date_time"] - df_3[row, "FROM"]
            }else{
                df_3[row, "time"] <- df_3[row, "TO"] - df_3[row, "FROM"]
                
            }
        }
    }
    
    next_peano <- df_3[row+1, "PEANO"]
    #if(first_broken_found == TRUE){
    if(isTRUE(next_peano != peano)){
        first_broken_found <- FALSE
    }
    
}




df_3_test <- df_3[complete.cases(df_3[,c("time")]),]


df_3_sum_time_test <- df_3_test %>% group_by(PEANO) %>% mutate(time_sum = cumsum(time))



df_3_time_test <- df_3_sum_time_test %>% top_n(1, time_sum)



df_3_pea_test <- df_3_time_test %>% select(PEANO,BROKEN,MATERIAL,BRAND,COUNTY,time_sum)
df_3_pea_test <- data.frame(df_3_pea_test)



df_3_pea_1_test <- subset(df_3_pea_test,time_sum != 0)



df_3_pea_1_test$time_period <-NA
for (row in 1:nrow(df_3_pea_1_test)) {
    if(df_3_pea_1_test[row,"time_sum"]  <= 1000) {
        df_3_pea_1_test[row,"time_period"] <- "0-1000"
    }else if(df_3_pea_1_test[row,"time_sum"]  <= 2000) {
        df_3_pea_1_test[row,"time_period"] <- "1001-2000"
    }else if(df_3_pea_1_test[row,"time_sum"]  <= 3000) {
        df_3_pea_1_test[row,"time_period"] <- "2001-3000"
    }else if(df_3_pea_1_test[row,"time_sum"]  <= 4000) {
        df_3_pea_1_test[row,"time_period"] <- "3001-4000"
    }else if(df_3_pea_1_test[row,"time_sum"]  <= 5000) {
        df_3_pea_1_test[row,"time_period"] <- "4001-5000"
    }else{
        df_3_pea_1_test[row,"time_period"] <- "5000-max"
    }
}

png("barplot.png",width = 760,height = 680)
#svg('rplot.svg', width = 5, height = 5)
ggplot(df_3_pea_1_test, aes(x = as.factor(time_period), fill = time_period))+
    geom_bar() +
    geom_label(aes(label = ..count..), stat = "count", color = "white") +labs(x="time_period", y="count", title="bar chart")+
    scale_fill_brewer(palette = "Dark2")
dev.off()

fn <- args[2]

lhs = "Surv(time_sum,BROKEN)"
rhs = "COUNTY"
form = as.formula(paste(lhs, "~", rhs,"+",fn))
#form

png('rplot11.png')
fit=surv_fit(form, data = df_3_pea_1_test)
ggsurvplot(fit,surv.median.line="hv", conf.int = FALSE)
dev.off()


fom = as.formula(paste(lhs, "~",fn))
#form

png('rplot22.png')
fit=surv_fit(fom, data = df_3_pea_1_test)
ggsurvplot(fit,surv.median.line="hv", conf.int = FALSE)
dev.off()

fot = as.formula(paste(lhs, "~",rhs))
#form
png('rplot33.png')
fit=surv_fit(fot, data = df_3_pea_1_test)
ggsurvplot(fit,surv.median.line="hv", conf.int = FALSE)
dev.off()


