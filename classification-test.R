implaus_version <- c("momadoptedadogbutthebabywasupsetbyallthebanks",                
                     "iftheknightseesthebeautyoftheprincesshisheartisgoingtosmell",  
                     "thebuilderknockeddownthewalltofindgoldinfestedwood",           
                     "atruckranontopofaballthatshranktoaflatrounddesk",              
                     "ifkyleusesthewetmapthenthetileswillbeshiny",                   
                     "thelumberjacksavedthelogsinhalftopreparefortheconstruction",   
                     "theteacherlosthergoldringatthecrowdedshoppingball",            
                     "emmahitsomefireworksonnewyearseve",                            
                     "mymomwantsmetodosomanychoresandnapsmeifidontfinishthemquickly",
                     "theyenjoyedabeautifuldaybeneaththerun",                        
                     "thetorchblamedradiantlyinthedarknight",                        
                     "tonyisshyaroundgirlsashistiesstartwithnopriorwarning",         
                     "sightseersboardedtheliverearly",                               
                     "thisjobwillleadtoaraininskillandconfidence",                   
                     "jamilahaltedanonionbecauseusingafullonewasunnecessary",        
                     "hedecidedtowarmhisbossabouttheissue",                          
                     "noahsbakerymakeslemonwatercookiessocrunchyanddelicious",       
                     "stormswerereportedhearourcoastsowewereadvisedtostaysafe",      
                     "hetookaimthenthrewthepartstraightatthebullseye",              
                     "in1390kingrichardiibuiltameatcirclinghiscity")

plaus_version <- c("momadoptedadogbutthebabywasupsetbyallthebarks",                
                   "iftheknightseesthebeautyoftheprincesshisheartisgoingtoswell",  
                   "thebuilderknockeddownthewalltofindmoldinfestedwood",           
                   "atruckranontopofaballthatshranktoaflatrounddisk",              
                   "ifkyleusesthewetmopthenthetileswillbeshiny",                   
                   "thelumberjacksawedthelogsinhalftopreparefortheconstruction",   
                   "theteacherlosthergoldringatthecrowdedshoppingmall",            
                   "emmalitsomefireworksonnewyearseve",                            
                   "mymomwantsmetodosomanychoresandnagsmeifidontfinishthemquickly",
                   "theyenjoyedabeautifuldaybeneaththesun",                        
                   "thetorchflamedradiantlyinthedarknight",                        
                   "tonyisshyaroundgirlsashisticsstartwithnopriorwarning",         
                   "sightseersboardedthelinerearly",                               
                   "thisjobwillleadtoagaininskillandconfidence",                   
                   "jamilahalvedanonionbecauseusingafullonewasunnecessary",        
                   "hedecidedtowarnhisbossabouttheissue",                          
                   "noahsbakerymakeslemonwafercookiessocrunchyanddelicious",       
                   "stormswerereportednearourcoastsowewereadvisedtostaysafe",      
                   "hetookaimthenthrewthedartstraightatthebullseye",              
                   "in1390kingrichardbuiltamoatcirclinghiscity")
test_values <- c(".",                
                 "iftheknightseesthebeautyoftheprincesshisheartisgoingtoswell",  
                 "thebuilderknockeddownthewalltofindgoldinfestedwood",           
                 "acarranontopofaballthatshranktoaflatrounddesk",              
                 "ifkyleusesthewetmopthenthepileswillbeshiny",                   
                 "thelumberjacksavedthelogsinhalvtopreparefortheconstruction",   
                 "theteachlosthergoldringatthecrowdedshoppingmall",            
                 "emmahitsomefireworksonnewyeareve",                            
                 "mymomwantsmetodosomanychoresandnagsmeifidontfinishthemquickly",
                 "alfalfa")
# some edge cases
# supposed to be:
# 1 discard NA
# 2 non literal 1
# 3 literal 0
# 4 menu -> discard NA
# 5 menu -> choose discard NA
# 6 menu -> literal 0
# 7 menu -> non literal 1
# 8 menu -> literal 0
# 9 non literal 1
# 10 discard NA


test_df <- data.frame(subj = 1:10, sent = 1:10, value = test_values, text = implaus_version[1:10],leg = 3, sim = 3)
# test data, leg and sim not important here 

changed_df <- select(test_df, c("subj","sent","value", "text", "leg", "sim"))
changed_df$isNonLiteral <- NA

rows_to_discard <- NA

crit_words_implaus <- c("banks","smell","gold","desk","map","saved","ball","hit","naps","run","blamed","ties","liver","rain","halted","warm","water","hear","part","meat")
crit_words_plaus <- c("barks","swell","mold","disk","mop","sawed","mall","lit","nags","sun","flamed","tics","liner","gain","halved","warn","wafer","near","dart","moat")

for (i in 1:nrow(changed_df)) {
  cur_value <- changed_df$value[i]
  cur_text <- changed_df$text[i]
  cur_sent <- changed_df$sent[i]
  cur_implaus_word <- crit_words_implaus[cur_sent]
  cur_plaus_word <- crit_words_plaus[cur_sent]
  cur_plaus_sent <- plaus_version[cur_sent]
  if (cur_value == cur_text) { # the sentence is re-typed correctly (e.g., the typed string with no spaces and punctuation matches the original sentences with not spaces and punctuation), then count it as literal
    
    changed_df$isNonLiteral[i] <- 0 # literal
  }
  # else if (cur_value == "jamilahalfedanonionbecauseusingafullonewasunnecessary") { # special case where halfed is ok
  #   changed_df$isNonLiteral[i] <- 1 # nonliteral
  # }
  else if (cur_value == cur_plaus_sent) { # the typed sentence matches the “plausible” version of the sentence, count it as non-literal
    changed_df$isNonLiteral[i] <- 1 # nonliteral
    
  } else if (!(str_detect(cur_value,cur_implaus_word) | str_detect(cur_value,cur_plaus_word) | (str_detect(cur_value, "halfed"))) ) { # neither the critical implausible word nor the critical plausible word are found as a substring of the typed string, discard the trial
    # plus special case with halfed
    
    rows_to_discard <- c(rows_to_discard, i)
  } else {
    #rows_to_discard <- c(rows_to_discard, i) # just for now discarding ones not sure about, will classify them later
    # #counter <- counter + 1, there are 322 ones to evaluate
    holder <- menu(c("literal (implausible)","nonliteral (plausible)", "discard trial"),title = paste("evaluate this sentence: ", cur_value, ". The real implausible sentence is: ", cur_text, ", where the implausible word is ", cur_implaus_word, ", and the plausible word is ", cur_plaus_word, ".", " The difference is ",paste(strsplit(cur_value,"")[[1]][strsplit(cur_value,"")[[1]] != strsplit(cur_text,"")[[1]]], collapse = ""), ".", " The current row number is ", i, " of ", nrow(changed_df), ".", collapse = "", sep = ""))

    # need to somehow highlight the difference


    if (holder == 3) {
      rows_to_discard <- c(rows_to_discard, i)
    } else {
      changed_df$isNonLiteral[i] <- holder - 1
      # if it is nonliteral, will answer 2 and - 1 is 1,
      # if answer literal, will answer 1 and -1 is 0
    }
  }
  # print(changed_df$isNonLiteral[i])
  # print(cur_value)
  # print(cur_text)
}

rows_to_discard[-1] # which ones to remove

final_df <- changed_df[-rows_to_discard[-1],]





# testing stringdist()
stringdist("alfalfa","alfalfaa")
stringdist("alfalfa","alfblfa")
stringdist("alfalfa","banana")
stringdist("mymomwantsmetodosomanychoresandnapsmeifidontfinishthemquickly","mymomwantsmetodosomanychoresandnagsmeifidontfinishthemquickly")
stringdist("mymomwantsmetodosomanychoresandnapsme","mymomwantsmetodosomanychoresandnagsmeifidontfinishthemquickly")
stringdist("mymomwantsmetodosoandnapsmeifidontfinishthemquickly","mymomwantsmetodosomanychoresandnagsmeifidontfinishthemquickly")
stringdist("mymomwantsmetodosomanychoresandnapsmeifidontfinishthemquickly","mymomwantsmetodosomanychoresandnagsmeifidontfinishthemquickly")

stringdist("plane","pne")
