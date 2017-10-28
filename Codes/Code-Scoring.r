rm(list=ls())

### R packages
# To install the following packages type ``install.packages9("package name")'' in R window
# NLP, openNLP may require JAVA 

require(tidytext)
require(NLP)
require(openNLP)
require(tm)
require(stringr)
require(stringdist)

### Working Directory

setwd("Directory Address") # ex)  E:/Research/Imputation with Review/
source("Code-Cleaning.r")  # Code-Cleaning should be in the directory above.

### Original data
dat=readRDS("file name.rds")   # read.csv("file name.csv") for csv file
n=nrow(dat)                    # data file should in the directory folder.

#### Review
review=dat$review

### Cleaning and converting to list frame
# User may include additional cleaning work (See Code-Cleaning).
# Additional stop words can be included in 'stw'.

vs=list()
for(i in 1:n) vs[[i]]=data.frame(strsplit(cleaning(review[i]), " "), stringsAsFactors = FALSE)

stw=stopwords(kind="en")
stw=c(stw,"is","are","was","were","am","have","has")

for(i in 1:n){
docu=vs[[i]]
docu=as.character(unlist(docu))
docu=tolower(docu)
docu=docu[is.na(match(docu,stw))==T]
docu=iconv(docu,to="UTF-8")
vs[[i]]=docu        
}

### Part of Speech
# vs.word: a vetor of words 
# vs.pos: a vector of POS tags
# vs.joint: a combined vector of words and POS tags
# For better performance, user may want to use Googly Syntaxnet, which is available in Python. 

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator    <- Maxent_POS_Tag_Annotator()

vs.word=list()
vs.pos=list()
vs.joint=list()

for(i in 1:n){
wn=as.String(vs[[i]])
wn_an=annotate(wn, list(sent_token_annotator, word_token_annotator))
wn_pos=annotate(wn, pos_tag_annotator, wn_an)
wn_pos=subset(wn_pos,type=="word")
wpos=unlist(lapply(wn_pos$features,function(s) s$POS))
wjoint=sprintf("%s/%s",wn[wn_pos],wpos)

vs.word[[i]]=wn[wn_pos]
vs.pos[[i]]=wpos
vs.joint[[i]]=wjoint
}


### Scoring
# We use "bing" sentiment dictionary. 
# See `tidytest' for other dictionary.
# Focus on adjective, adverb, verb (wgroup)
# Tuning parameter alpha is set to be 1
# Output : Z (propensity score)
# See Kim and Im (2017+) for the algorithm details.

word=get_sentiments("bing")$word
sentiment=get_sentiments("bing")$sentiment

wgroup=c("JJR","JJS","JJ","RB","RBR","RBS","VB","VBD","VBP","VBG","VBN","VBZ")

pog=vector()
neg=vector()
total=vector()
score=vector()
alpha=1

for(i in 1:n){
wvec=vs.word[[i]]
pos=vs.pos[[i]]
kloc=which(is.na(match(pos,wgroup))==F)
sel.word=wvec[kloc]
ns=length(sel.word)

first.loc=match(sel.word,word)
first=sentiment[first.loc]
nloc=which(is.na(first)==T)
first[nloc]="neutral"

score[i]=sum(first=="positive" | first=="negative")/(alpha+ns)
neg[i]=sum(first=="negative")
pog[i]=sum(first=="positive")
total[i]=length(first)
}

z=score
