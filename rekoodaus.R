load("data/data.rda")
library(dplyr)
library(tm)
library(qdap)
library("stringdist")
library(rvest)

stopwords1 <- read_html("http://www.ranks.nl/stopwords/finnish") %>% 
    html_table() %>% 
    lapply(flatten_chr) %>% 
    flatten_chr() %>% 
    paste(collapse=" ") %>%
    strsplit(" ") %>% 
    flatten_chr()

stopwords2 <- read_html("http://countwordsfree.com/stopwords/finnish") %>% 
    html_table() %>% 
    .[[1]] %>% 
    select(2) %>% 
    flatten_chr()

stopwords3 <- stopwords("fi")

load("data/finnish-stopwords.RData")

stopwords <- union(stopwords, stopwords1)  %>% 
    union(stopwords2) %>% 
    union(stopwords3)

rm(stopwords1, stopwords2, stopwords3)

#muuta 

# lähihoitataja
# päiväkodinopettaja to lastentarhanopettaja
# hallintojohtaja talousj
# koulunjohtaja
# palveluvastaava
# ruokapalveluvastaava
# atk-vastaava
# asiakaspalveluvastaava
# tiimivastaava
# ruokapalveluesimies
# ravitsemisesimies
# palveluesimies
# paloesimies
# mittausesimies: 
# lääkintäesimies: johtaja
# ensihoitoesimies: Sairaankuljetuksen ensihoitajat
#  ravitsemistyöntekijä: Avustavat keittiö- ja ruokatyöntekijät
# atk-suunnitelija, -tukihenkilö


# ammattinimiinSotkeutuvat <- c("amk","erikoistuva","työterveys",
#                               "terveyskeskus","päiväkodin", "vastaava", 
#                               "siivoustyön", "ravitsemistyön","puistotyön","mittaustyön",
# "varhaiskasvatus","liikuntatoimen","museotoimen",
# "tulosalue","kehittämis", "kulttuuritoimen",
# "nuorisotoimen","palvelualue","vastuualue","hanke","johtava",
# "kiertävä", "päivystävä")
# 
# turhiaSanoja <- c("amk","erikoistuva","työterveys",
#                   "terveyskeskus", "vastaava")


# poistot <- c(ammattinimiinSotkeutuvat, turhiaSanoja) %>% 
#     paste(collapse="|")

data2 <- data$v07_taus_nimek %>% 
    iconv(., "latin1", "UTF-8") %>% 
    tolower() %>%
    removeNumbers() %>%
    scrubber() %>% 
    gsub("\\.|,|/|\\(|\\)|:|\\?|'|‹|%|;|\\+|=", " ", .) %>% 
    #removePunctuation() %>% 
    gsub("( |$)vs(\\.| |$)|( |$)va(\\.| |$)|sij(\\.|ainen)", " ", .) %>% 
    gsub(" amk |\\(amk\\)","", .) %>%
    #genetiivien poisto
    gsub("( |^)[a-zäöå\\-]*n( |$)", " ", .) %>% 
    gsub("\\.|,| ja "," ", .) %>% 
    Trim() %>%
    clean() %>% 
    comma_spacer() %>%
    gsub("^sh$","sairaanhoitaja",.) %>% 
    data.frame(syote=. ) 
    

    


    # gsub("^lehtori.*|.*lehtori","lehtori", .) %>%
    #     gsub("^rehtori.*|.*rehtori","rehtori", .) %>% 

#alkaa sana,

    
    
luokitus <- read.delim("http://tilastokeskus.fi/meta/luokitukset/ammatti/001-2010/luokitusavain_2_teksti.txt", 
                       header = T, skip=3, encoding="latin1")
luokitus_se <- read.delim("http://www.stat.fi/meta/luokitukset/ammatti/001-2010/tekstitiedosto_sv.txt",
                          header = T, skip=3, encoding="latin1")
koonti <- data2 %>% 
    count(syote) %>% 
    arrange(desc(n)) %>% 
    ungroup() %>% 
    mutate(Ammattinimikeselite = as.character(syote),
           vastine = amatch(syote, tolower(luokitus$nimike.1), maxDist=4),
           vastine2 = ifelse(is.na(vastine),
                             amatch(syote, tolower(luokitus$nimike.2), maxDist=4),
                             NA),
           vastine3 = ifelse(is.na(vastine) & is.na(vastine2), 
                             amatch(syote, tolower(luokitus_se$benämning), maxDist=4),
                             NA),
           puuttuu = is.na(vastine) & is.na(vastine2) & is.na(vastine3) )

testi <- koonti %>% 
    filter(puuttuu==T & syote!="" & !is.na(syote)) %>%  
    head(1000) %>% 
    group_by(syote) %>% 
    mutate(loytyva = paste( (grep(syote, luokitus$nimike.1, 
                                 value=T, ignore.case=T), 
                             collapse="|"))) %>%
    


amlLuokitteluKeva <- read.csv2("data/amlKoodaus.csv", stringsAsFactors = F, encoding = "latin1") %>% 
    mutate(kirkko = substr(Ammattinimikekoodi,1,1)=="E") %>% 
    filter(Ammattinimikeselite!="") 

amlLuokitteluKeva %>% 
    mutate(Ammattinimikeselite =tolower(Ammattinimikeselite)) %>% 
    filter(kirkko==F) %>% 
    select(-Ammattinimikekoodi) %>% 
    right_join(koonti)

                    