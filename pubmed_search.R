##http://gastonsanchez.com/work/webdata/getting_web_data_r4_parsing_xml_html.pdf

# load XML
install.packages("XML")
library(XML)




pubmed_data<-function(qjournal, qyear){
  query<-sprintf("%s[jo]+%s[pdat]", qjournal, qyear)
  qIDs<-esearch_ids(query)
  # control
  nIDs<-length(qIDs)
  #create variables first, save data in vector, then create dataframe
  ID<-character()
  NoAuthors<-numeric()
  NoCollaborations<-numeric()
  Origin.Country<-character()
  Int.Collab<-numeric()
  Countries<-numeric()

  for(i in 1:length(qIDs)){
    print (paste(i, "of", nIDs))
    tryCatch({
      xfetch<-efetch_id(qIDs[i])  
      #xfetch<-efetch_id(25533960)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    abstract<-xmlSApply(getNodeSet(xfetch, "//Abstract"), xmlValue)
    if(length(abstract)==1){
      authors<-xmlSApply(getNodeSet(xfetch,"//LastName"), xmlValue)
      no.authors<-length(authors)
      affiliations<-xmlSApply(getNodeSet(xfetch, "//Affiliation"), xmlValue)
      if(length(affiliations)>=1){
        affiliations<-unique(affiliations)
        no.collaborations<-length(affiliations)
        countries<-country_split2(affiliations)
        no.countries<-length(countries)
        origin.country<-countries[1]
      }else{
        no.collaborations<-NA
        origin.country<-NA
        no.countries<-NA
        countries<-NA
      }
      
    # control
    ID[i]<-qIDs[i]
    NoAuthors[i]<-no.authors
    NoCollaborations[i]<-no.collaborations
    Origin.Country[i]<-origin.country
    Int.Collab[i]<-no.countries
    Countries[i]<-paste(countries, sep="", collapse=",")
    }
  }
  
  return(data.frame(ID, NoAuthors,
                    NoCollaborations,
                    Origin.Country, Int.Collab,
                    Countries,
                    stringsAsFactors=FALSE))
}  



##helper functions
#esearch
esearch_ids<-function(query){
  base_url<-"http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  esearch<-sprintf("esearch.fcgi?db=pubmed&term=%s&retmax=10000",query)
  search_url<-paste(base_url, esearch,sep='')
  search_doc<-xmlParse(search_url)
  ids<-xpathSApply(search_doc, path = "//IdList/Id", fun='xmlValue')
  return(ids)
}
#works...
esearch_ids("huels d")

#efetch
efetch_id<-function(pmid){
  efetch<-sprintf("efetch.fcgi?db=pubmed&id=%s&retmode=xml", pmid)
  base_url<-"http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  sum_url=paste(base_url, efetch,sep='')
  summary_doc<-xmlParse(sum_url)
  return(summary_doc)
}
#works...

#affiliations split...returns unique countries, and last email affiliation


country_split2<-function(aff_id){
  df<-character()
  for (i in 1:length(aff_id)){
    country1<-country_find(aff_id[i])
    if(length(country1)==1){
      df[i]<-country1
    }
  }
  return(unique(df))
}

country_find<-function(single_string){
  library(countrycode)
  countries<-c(countrycode_data[-122,1], 
               "UK", "Russia", "USA", "Taiwan",
               "Venezuela", "Vietnam",
               "United States of America", "Bolivia" )
  for (i in 1:length(countries)){
    x<-grep(countries[i], single_string)
    if (length(x)==1){
      return(countries[i])
    }
  }
}


# run function across all journals
journals<-c("cell",
            "embo j",
            "mol cell",
            "oncogene",
            "development",
            "cell cycle",
            "j exp med",
            "j cell biol",
            "mol biol cell",
            "lancet",
            "nature",
            "Proc Natl Acad Sci U S A",
            "science",
            "plos one",
            "plos biol",
            "plos genet")

journals2<-c("N Engl J Med",
             "Nat Genet",
             "Nat Med",
             "cancer cell",
             "cell stem cell",
             "Nat Cell Biol",
             "Gastroenterology",
             "Neuron")

journal_pubmed<-function(jour){
  df<-data.frame()
  for (i in 1:length(jour)){
    x1<-pubmed_data(jour[i], "2014")
    x2<-x1[complete.cases(x1),]
    x2$journal<-jour[i]
    df<-rbind(df, x2)
  }
  return(df)
}


full_trial<-journal_pubmed(journals)

full_journals2<-journal_pubmed(journals2)
write.csv(full_journals2, "journals2014_addition.csv")
ncol(pubmed)
pubmed2<-pubmed[,2:8]
head(pubmed2)
colnames(full_journals2)
pubmed_allJournals<-rbind(pubmed2, full_journals2)
nrow(pubmed_allJournals)

write.csv(pubmed_allJournals, "pubmed_allJournals.csv")


