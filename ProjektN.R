library(dplyr)
library(stringr)
library(gtools)
library(xml2)
library(tidyverse)
#install.packages("mgsub")
library(mgsub)
#install.packages("utf8")
library(utf8)
#install.packages("rcompanion")
library(rcompanion)
#install.packages("VIM")
library(VIM)
library(caTools)
library(randomForest)
library(rpart)
library(e1071)
library(kknn)
library(caret)
library(RSelenium)
library(seleniumPipes)
library(rvest)
library(ggplot2)

##################################
# pobranie danych
##################################

# Nawiązanie połączenia z serwerem Selenium
remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port= 4444,
                  browserName = "chrome",
                  newSession = TRUE)


#przejście do strony flixable,com i przewinięcie jej do końca, by wyświetlić wszyskie filmy i seriale
remDr %>% go("https://flixable.com/")
scrollHeight<- remDr %>%executeScript("return document.body.scrollHeight", args = list(""))
lastScrollHeight <- scrollHeight[[1]]
repeat{
  remDr %>% executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(1)
  scrollHeight<- remDr %>% executeScript("return document.body.scrollHeight", args = list(""))
  remDr %>% executeScript("window.scrollBy(0,-100);")
  newScrollHeight <- scrollHeight[[1]]
  if (lastScrollHeight == newScrollHeight){
    break
  }
}

# pobranie wszystkich linków do produkcji zapisanych na stronie, a następnie zapisanie do pliku csv
wektorLinkow <- c()
elems <- remDr %>% findElements(using="class name", "card-header-image")
for(j in 1:length(elems)){
  e<-findElementsFromElement(elems[[j]], using = "tag name", "a")
  if(length(e)>0){
    link<- e[[1]]%>%getElementAttribute("href")
    wektorLinkow<-c(wektorLinkow,link)
  }
}
write.csv(wektorLinkow, file ="myVectors.csv", row.names=FALSE)


length(wektorLinkow) # 5965 filmy sieriale, stan na 12.08.2021 
?smartbind
df1<- NULL
# pobranie danych filmów na podstawie pozyskanych wczesniej linkow
zrobWierszRvest<-function(w,wektorLinkow,remDr){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  page<-read_html("https://flixable.com/title/lf-mbrwk/", encoding="UTF-8")
  #details<-html_node(page,".card-category")%>%html_text()
  movie_name <- html_node(page,".title")%>%html_text()
  year <- page%>%xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/h6/span[1]")%>%html_text()
  length<- page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/h6/span[3]")%>%html_text()
  rating<- page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/h6/span[5]")%>%html_text()
  if (identical(rating, character(0))){
    rating <-NA
  }
  v1<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[2]/*")%>%html_text()%>%na.omit() #genres
  v2<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[3]/*")%>%html_text()%>%na.omit() #director
  v3<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[4]/*")%>%html_text()%>%na.omit() #cast
  v4<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[5]/*")%>%html_text()%>%na.omit() #country
  v5<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[6]/*")%>%html_text()%>%na.omit() #rate
  v6<-page %>% xml_find_all("/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div[1]/div[2]/p[7]/*")%>%html_text()%>%na.omit() #added to netlix
  
  v <- c(v1,v2,v3,v4,v5,v6)
  #print(v)
  indexy<-seq(1,length(v),1)
  nazwyKolumn<-v[indexy%%2==1]
  nazwykolumn<- gsub(":","",nazwyKolumn)
  wartosci<-v[indexy%%2==0]
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  #df1<-data.frame(1,1)
  df1<-cbind(movie_name,df1)
  df1<-cbind(length,df1)
  df1<-cbind(rating,df1)
  df1<-cbind(year,df1)
  df1
}

#pobranie danych ze wszystkich linków 
movies<-NULL
for(w in 1: length(wektorLinkow)){
  skip<-FALSE
  tryCatch(
    df2<-zrobWierszRvest(w,wektorLinkow,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(movies)){
    movies<-df2
  }else{
    movies<-smartbind(movies,df2)
    
  }
}
View(movies)
write.csv(movies, file ="mymovies.csv", row.names=FALSE, fileEncoding = "UTF-8")


?smartbind
?data.frame

#############################
# przygotwanie danych
#############################
movies <- read.csv("mymovies.csv", header = TRUE, sep = ",")

# oczyszczenie danych 
movies_bck <- movies
View(movies_bck)

movies$movie_name <- mgsub(movies$movie_name, c("'", "!","#","[()]"), c("", "","", ""))
movies <- subset (movies, select = -Rate.) 

# dodanie kolumny rok z rokiem dodania filmu do serwisu
movies$Added.to.Netflix.year <-  str_replace_all(movies$Added.to.Netflix., "^.*,\\s+", "") # wybranie roku z pola tesktowego

# dodanie do kolumny rok roku dodania ostatniego sezonu serialu do serwisu
for (i in 1: nrow(movies)) {
  if (is.na(movies$Added.to.Netflix.year[i])){
    
    movies$Added.to.Netflix.year[i] <-  str_replace(movies$New.Season.Added.[i], "^.*,\\s+", "")
  }
  
}
movies$rating <- as.numeric(str_replace_all(movies$rating, "\\/\\d+", "") ) # zmiana ratingu na numeric i usuniecie /10
movies$Added.to.Netflix.year <- as.numeric(movies$Added.to.Netflix.year)

# dodanie main_genre do ramki danych 
movies$main_genre <- str_replace_all(movies$Genres., "^.*,", "")
#zapisanie oczyszczonej ramki do pliku
write.csv(movies, file ="mymovies_clean.csv", row.names=FALSE, fileEncoding = "UTF-8")

#####################################################################
# Analiza przygotowanych danych
#####################################################################


moviesPoland <- movies%>%filter(`Production.Country.`=="Poland")%>%collect() # filmy z Polski dostępne w serwisie
#View(moviesPoland)

summary(movies)
str(movies)

# rozdzeilenie obsady na pojedynczych aktorów tak, by policzyć ich wystąpienia. 
# Zwiększamy liczbę wierszy, by każdy aktor występował w osobnym wierszu 
moviesCast <- movies%>%
  mutate(unpacked = str_split(Cast., ",")) %>%
  unnest(cols = c(unpacked)) %>%
  mutate(Cast. = str_trim(unpacked))

View(moviesCast)
mostPopActor <- names(which.max(table(moviesCast$Cast.)))

moviesActor <- moviesCast%>%filter(Cast.==mostPopActor)%>%collect() 
actorsList <-  moviesCast%>% group_by(Cast.)%>%tally(sort = TRUE)%>%na.omit()
View(actorsList)
View(actorsList[1:10,]) #top 10 aktorów, którzy najczęściej są obecni w obsadzie filmów biblioteki Netfliksa


topRateMovies <-  movies%>% group_by(rating)%>%tally(sort = TRUE)%>%na.omit() # najczęściej dawane oceny
topRatedMovies <- movies %>% arrange(desc(rating)) %>% slice(1:10) # 10 najelpiej ocenianych filmow
topRateMovies2 <- topRateMovies%>% drop_na()
par(mfrow=c(1,1))
hist(movies$rating, topRateMovies2$n,col = 'steelblue',breaks=100,main='Rozkład ocen',xlab = 'Rating') # rozkład ocen filmów
?hist


noYearMovies <- movies%>% group_by(year)%>%tally(sort = TRUE)%>%na.omit() # liczba filmów dostępnych w serwisie wyprodukowanych w danym roku
moviesAddedToNetflixTop <-  movies%>% group_by(Added.to.Netflix.year)%>%tally(sort = TRUE)%>%na.omit() # liczba dodanych tytułów do Netfliksa per rok
View(moviesAddedToNetflixTop)
?ggplot2
plot <- ggplot(moviesAddedToNetflixTop, aes(x=as.numeric(Added.to.Netflix.year), y=n)) + 
  geom_point()+
  geom_smooth()+
  xlab("year")+
  ylab("amount")
plot


avgRatingPerYear <- movies%>% group_by(year)%>%
  summarize(mean_size = mean(rating , na.rm = TRUE), count_film= n()) # średnia ocen dla filmów per rok


moviesProdCountry <- movies%>%
  mutate(unpacked = str_split(`Production.Country.`, ",")) %>%
  unnest(cols = c(unpacked)) %>%
  mutate(`Production.Country.` = str_trim(unpacked))

moviesProdCountryList <-  moviesProdCountry%>% group_by(`Production.Country.`)%>%tally(sort = TRUE)%>%na.omit() # liczba produkowanych tytułów per kraj

plot <- ggplot(moviesProdCountryList[1:10,], aes(x=Production.Country., y=n)) + 
  geom_point()+
  xlab("country")+
  ylab("amount")
plot


#najpopularniejsze gatunki
moviesTopGenre <- movies%>%
  mutate(unpacked = str_split(Genres., ",")) %>%
  unnest(cols = c(unpacked)) %>%
  mutate(Genres.= str_trim(unpacked))

moviesTopGenreList <-  moviesTopGenre%>% group_by(Genres.)%>%tally(sort = TRUE)%>%na.omit() # lnajpopularniejsze gatunki na Netfliksie
plot <- ggplot(moviesTopGenreList[1:10,], aes(x=Genres., y=n)) + 
  geom_bar(stat="identity")+
  xlab("genre")+
  ylab("amount")
plot


moviesTopGenreByYear2018 <-  moviesTopGenre%>% filter(`year`=="2018")%>%collect() %>%group_by(year, Genres.)%>%tally(sort = TRUE)%>%na.omit() # lnajpopularniejsze gatunki na Netfliksie po dacie produkcji
moviesTopGenreByYear2019 <-  moviesTopGenre%>% filter(`year`=="2019")%>%collect() %>%group_by(year, Genres.)%>%tally(sort = TRUE)%>%na.omit() # lnajpopularniejsze gatunki na Netfliksie po dacie produkcji
moviesTopGenreByYear2020 <-  moviesTopGenre%>% filter(`year`=="2020")%>%collect() %>%group_by(year, Genres.)%>%tally(sort = TRUE)%>%na.omit() # lnajpopularniejsze gatunki na Netfliksie po dacie produkcji
moviesTopGenreByYear2021 <-  moviesTopGenre%>% filter(`year`=="2021")%>%collect() %>%group_by(year, Genres.)%>%tally(sort = TRUE)%>%na.omit() # lnajpopularniejsze gatunki na Netfliksie po dacie produkcji



#######################################################################
# liczenie korelacji pomiędzy kolumnami w celu zbudowania modelu 
# budowanie modelu: random forest regression, knn, linear regression 
#######################################################################

# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, 
# numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437

mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% rename(X1=Var1, X2=Var2)
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
} 
movies_assoc <- movies%>%select(-Added.to.Netflix., -New.Season.Added.) # usuwamy kolumny, które mają dużo NA, w ich miejsce została utworzona kolumna Added.to.Netflix.year
View(movies_assoc)
assoc <- mixed_assoc(movies_assoc)

View(assoc) # korelacja pomiędzy zmiennymi wywołana różnymi metodami zależnymi od typu zmiennej (numeric czy nominal)


# sprawdzenie jak dużo mamy NA w poszczególnych kolumnach 
aggr(movies_assoc, numbers=TRUE,
     sortVars=TRUE,
     labels=names(data),
     cex.axis=.7)

movies_assoc2 <- movies_assoc %>% mutate_all(funs(replace_na(.,0))) # zamiana Na na 0 w celu dalszej analizy i testownaia modeli
movies_assoc2 <- subset(movies_assoc2, rating!=0.0) # usuniecie wierszu, gdzie rating czyli zmienna celu jest NA
?select
View(movies_assoc2)
?randomForest


# zbudujemy teraz model przy użyciu randomForest dla zmiennej rating 
#w tym celu stworzony został zbiór testowy i treningowy, w 1 przykładzoe usunięte zostały wartości NA

movies_noProdCountry <- movies_assoc %>% select(-Production.Country.) 

moviesNoNa <- movies_assoc %>% drop_na()
moviesNoNa <- movies_noProdCountry %>% drop_na()

write.csv(moviesNoNa, file ="mymovies_nona.csv", row.names=FALSE, fileEncoding = "UTF-8") # zapis ramki danych do pliku bez wierszy z NA
assocNoNA <- mixed_assoc(moviesNoNa)
View(assocNoNA)


movies_model <- moviesNoNa #dane bez NA
movies_model <- movies_assoc2 #dane z zerem zamiast NA

movies_model <- movies_model%>% select(-Production.Country.)

set.seed(123)

sample<-sample.split(Y=movies_model,SplitRatio=.75)
trains<-subset(movies_model,sample=TRUE)
tests<-subset(movies_model,sample=FALSE)
?sample.split

# randomForest model
regrRF<-randomForest(rating~.,data=trains)
predictionsRF <- predict(regrRF, tests)

myMAE <- mean(abs(tests$rating - predictionsRF)) # średni błąd bezwzględny 0.339 dla bez NA, dla danych z 0 zamiast NA 0.474
myMAE
myRSQR <- R2(tests$rating, predictionsRF) # mean squared error 
myRSQR
myMSE <- mean((tests$rating - predictionsRF)^2)
myMSE


# model z użyciem KNN
regrKNN<-train.kknn(rating~.,data=trains,kmax=5)
predictionsKNN<-predict(regrKNN,tests)

myMAE <- mean(abs(tests$rating - predictionsKNN)) # średni błąd bezwzględny 0.339 dla bez NA, dla danych z 0 zamiast NA 0.474
myMAE
myRSQR <- R2(tests$rating, predictionsKNN)  
myRSQR
myMSE <- mean((tests$rating - predictionsKNN)^2)# mean squared error
myMSE


