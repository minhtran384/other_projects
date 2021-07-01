install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("data.table")
library(dplyr)
library(plyr)
library(ggplot2)
library(data.table)  

files <- list.files(path = ".",pattern = "DRUG")
files <- list.files(path = ".")
temp <- lapply(files, fread, sep="$", header=TRUE, stringsAsFactors=FALSE, quote="", fill = TRUE )
data <- rbindlist(temp, fill=TRUE)

df.drug <- read.table("DRUG19Q1.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )
df.drug2 <- read.table("DRUG19Q2.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )
df.drug3 <- read.table("DRUG19Q3.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )
df.drug4 <- read.table("DRUG19Q4.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )

df.drug <- rbind(df.drug, df.drug2, df.drug3, df.drug4)

df.react = read.table("REAC19Q1.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )
df.react2 = read.table("REAC19Q2.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )
df.react3 = read.table("REAC19Q3.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )
df.react4 = read.table("REAC19Q4.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )

df.react <- rbind(df.react, df.react2, df.react3, df.react4)

df.demo = read.table("DEMO19Q1.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )
df.demo2 = read.table("DEMO19Q2.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )
df.demo3 = read.table("DEMO19Q3.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )
df.demo4 = read.table("DEMO19Q4.txt",sep="$", header=TRUE, comment.char="#", stringsAsFactors=FALSE, quote="", fill = TRUE )

df.demo <- rbind(df.demo, df.demo2, df.demo3, df.demo4)

df.tramal = df.drug[df.drug$drugname == "TRAMAL", ]
df.lyrica = df.drug[df.drug$drugname == "LYRICA", ]

df.tramal <- left_join(df.tramal, df.react, by = "caseid", copy = FALSE)
df.tramal <- left_join(df.tramal, df.demo, by = "caseid", copy = FALSE)
df.tramal <- df.tramal[c("caseid","drugname","pt","sex","age","age_grp")]

df.tramal.demo <- df.tramal %>%
  group_by(sex, pt) %>%
  dplyr::summarise(case_count = n()) %>%
  arrange(desc(case_count))

df.tramal.F <- filter(df.tramal.demo, sex == "F") %>% top_n(10)
df.tramal.M <- filter(df.tramal.demo, sex == "M"  ) %>% top_n(10)
df.tramal.top.demo <- rbind(df.tramal.F, df.tramal.M)


df.lyrica <- left_join(df.lyrica, df.react, by = "caseid", copy = FALSE)
df.lyrica <- left_join(df.lyrica, df.demo, by = "caseid", copy = FALSE)
df.lyrica <- df.lyrica[c("caseid","drugname","pt","sex","age","age_grp")]

df.tramal.top <- data.frame(table(df.tramal$pt))
df.tramal.top <- head(arrange(df.tramal.top,desc(Freq)), n = 10)
names(df.tramal.top)[1] <- "Effect"
names(df.tramal.top)[2] <- "Count"

df.tramal.top.demo <- head(arrange(df.tramal.demo,desc(case_count)), n = 20)

df.lyrica.top <- data.frame(table(df.lyrica$pt))
df.lyrica.top <- head(arrange(df.lyrica.top,desc(Freq)), n = 10)
names(df.lyrica.top)[1] <- "Effect"
names(df.lyrica.top)[2] <- "Count"

ggplot(df.tramal.top, aes(x=reorder(Effect, -Count), y=Count)) + 
  geom_bar(stat = "identity", fill = "#038C8C") + 
  theme_minimal() +
  labs( x = "Adverse effects", y = "Case count",
        title ="Top 10 adverse effects of Tramal") +
  theme(text = element_text(size=10),
       axis.text.x = element_text(angle=30, hjust=1)) 

ggplot(df.tramal.F, aes(x=reorder(pt, -case_count), y=case_count)) + 
  geom_bar(stat = "identity", fill = "#BF463B") + 
  theme_minimal() +
  labs( x = "Adverse effects", y = "Case count",
        title ="Top 10 adverse effects of Tramal in Female patients") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=30, hjust=1)) 

ggplot(df.tramal.M, aes(x=reorder(pt, -case_count), y=case_count)) + 
  geom_bar(stat = "identity", fill = "#0367A6") + 
  theme_minimal() +
  labs( x = "Adverse effects", y = "Case count",
        title ="Top 10 adverse effects of Tramal in Male patients") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=30, hjust=1)) 

ggplot(df.lyrica.top, aes(x=reorder(Effect, -Count), y=Count)) + 
  geom_bar(stat = "identity", fill = "#F29F05") + 
  theme_minimal() +
  labs( x = "Adverse effects", y = "Case count",
        title ="Top 10 adverse effects of Lyrica") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=30, hjust=1)) 
