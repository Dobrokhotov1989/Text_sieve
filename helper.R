#Function to remove html tags from the text of paper
cleanFun = function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#Function to highlight text
highlight <- function(text, dic, color) {
  x <- unlist(strsplit(text, split = " ", fixed = TRUE))
  x[tolower(x) %in% tolower(dic)] <- paste0("<strong><mark style = 'background-color: ",
                                             color, "'>",
                                             x[tolower(x) %in% tolower(dic)],
                                             "</mark></strong>")
  paste(x, collapse = " ")
  
}

#Download records list function
sieving = function (srch_term = input$srch_term,
                    que_limit = input$que_limit,
                    dic1 = input$Dic_1,
                    dic2 = input$Dic_2) {
  notif = showNotification("Loading records from EuropePMC")
  
  search_results = europepmc::epmc_search(query = srch_term,
                                          limit = que_limit,
                                          verbose = FALSE)
  
  #remove cases when pmcid is NA
  search_results = search_results[complete.cases(search_results[,"pmcid"]),]
  
  #remove non open access cases
  search_results = search_results[which(search_results$isOpenAccess == "Y"),]
  
  removeNotification(notif)
  
  if (!is.null(search_results)) {
    collected.data = tibble()
    #https://shiny.rstudio.com/articles/progress.html
    withProgress(message = 'Analyzing data', value = 0, {
      for (i in 1:nrow(search_results)){
        the_text = europepmc::epmc_ftxt(search_results$pmcid[i])
        paragraphs = trimws(xml_find_all(xml_children(xml_children(the_text)), "//p"))
        the_tibble = tibble(doi = search_results$doi[i],
                            title = search_results$title[i],
                            author = search_results$authorString[i],
                            pubYear = search_results$pubYear[i],
                            pmid = search_results$pmid[i],
                            pmcid = search_results$pmcid[i],
                            journalTitle = search_results$journalTitle[i],
                            issue = search_results$issue[i],
                            journalVolume = search_results$journalVolume[i],
                            parag_num = seq(from = 1, length.out = length(paragraphs)),
                            paragraphs = cleanFun(paragraphs),
                            sentences = cleanFun(paragraphs))
        
        the_tibble = the_tibble %>%
          unnest_tokens(sentence,
                        sentences,
                        token = "regex",
                        pattern = "(?<!\\..|fig|Fig|i|e|al|Cat|cat|no|No|St|ver|Ver|Dr|dr)[.?!]\\s+")
        #(?<!\\b(Fig|fig|i|e|St|al))\\.")
        
        for (m in 1:length(dic1)){
          
          matches.dic_1 = stringr::str_extract(string = the_tibble$sentence, pattern = sprintf("(?=.*\\b%s\\b).*",
                                                                                               dic1[m]))
          matches.dic_1 = matches.dic_1[!is.na(matches.dic_1)]
          
          for (n in 1:length(dic2)){
            
            matches.dic_2 = stringr::str_extract(string = matches.dic_1, pattern = sprintf("(?=.*\\b%s\\b).*",
                                                                                           dic2[n]))
            matches.dic_2 = matches.dic_2[!is.na(matches.dic_2)]
            
            the_tibble = unique(the_tibble[which(the_tibble$sentence %in% matches.dic_2),])
            
            collected.data = rbind(collected.data, the_tibble)
            
          }
        }
        
        #find duplicated sentences
        duplicates = data.frame(table(collected.data$sentence))
        dupl.data = collected.data[collected.data$sentence %in% duplicates$Var1[duplicates$Freq > 1],]
        
        #identify which of the duplicated sentences derived from the same paragraph
        #that was somehow repeated
        u_pars = tapply(dupl.data$paragraphs, dupl.data$sentence, FUN = function(x){
          #set the longest paragraph as a test string
          a_string = x[which(nchar(x) == max(nchar(x)))]
          tr_fl = c()
          #check which paragraphs are substring of the longest paragraph
          for (i in 1:length(x)){tr_fl = c(tr_fl,
                                           grepl(x[i], a_string, fixed = TRUE))}
          #make a list of longest or unique paragraphs
          ret_st = x[which(nchar(x) == max(nchar(x)) | tr_fl == FALSE)]
          return(ret_st)
        })
        
        #Remove the duplicated paragraphs
        collected.data[collected.data$sentence %in% duplicates$Var1[duplicates$Freq > 1], "parag_num"] = 
          collected.data[collected.data$sentence %in% duplicates$Var1[duplicates$Freq > 1] & collected.data$paragraphs %in% u_pars, "parag_num"]
        
        collected.data[collected.data$sentence %in% duplicates$Var1[duplicates$Freq > 1], "paragraphs"] = 
          collected.data[collected.data$sentence %in% duplicates$Var1[duplicates$Freq > 1] & collected.data$paragraphs %in% u_pars, "paragraphs"] 
        
        collected.data = unique(collected.data)
        
        collected.data$sentence = sapply(collected.data$sentence,
                                         FUN = function(x){highlight(text = x, dic = dic1, color = "red")})
        collected.data$sentence = sapply(collected.data$sentence,
                                         FUN = function(x){highlight(text = x, dic = dic2, color = "yellow")})
        collected.data$paragraphs = sapply(collected.data$paragraphs,
                                         FUN = function(x){highlight(text = x, dic = dic1, color = "red")})
        collected.data$paragraphs = sapply(collected.data$paragraphs,
                                           FUN = function(x){highlight(text = x, dic = dic2, color = "yellow")})
        
        
        #https://shiny.rstudio.com/articles/progress.html
        incProgress(1/nrow(search_results))
      }
    })  
    
    if (nrow(collected.data) == 0) {
      collected.data = "No matches"
    }
    
  } else {
    
    collected.data = "No papers"
    
  }
  
  return(collected.data)
}

csv_to_vector = function(file){
  ext <- tools::file_ext(file$datapath)
  
  req(file)
  validate(need(ext == "csv", "Please upload a csv file or type the search term"))
  
  list_of_terms = read.csv(file$datapath, header = TRUE)
  
  list_of_terms = unlist(list_of_terms)
  
  list_of_terms = unique(list_of_terms)
  
  return(list_of_terms)
}

##https://github.com/rasmusab/tidyverse-in-a-table/blob/master/tidyverse_in_a_table.R
datatable_callback <- JS("
  var format = function(d) {
    return '<div style=\"padding: .5em;\">' +
           '<p><h4>' + d[4] + '</h4></p>' +
           '<p><h5>' + d[3] + '</h5></p>' +
           '<br>' + 
           '<p>' + d[6] + '</p>' + 
           '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"
)