#Function to remove html tags from the text of paper
cleanFun = function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#Function to highlight text
highlight <- function(text, dic, color) {
  for (i in 1:length(dic)){
  text = gsub(pattern = paste0("(?!(^<\\/mark><\\/strong>$))(\\b", dic[i], "\\b)(\\.|,|;|:)?"),
              replacement = paste0("<strong><mark style = 'background-color: ",
                                   color, "'>",
                                   "\\2",
                                   "</mark></strong>",
                                   "\\3"),
              x = text,
              ignore.case = TRUE,
              perl = TRUE)
  }
  return(text)
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
  search_results = search_results[which(!is.na(search_results$pmcid)),]
  
  #remove non open access cases
  search_results = search_results[which(search_results$isOpenAccess == "Y"),]
  
  removeNotification(notif)
  
  if (!is.null(search_results) & nrow(search_results >= 1)) {
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
                            paragraphs = paragraphs,
                            sentences = paragraphs)
       
         the_tibble = the_tibble %>%
          unnest_tokens(sentence,
                        sentences,
                        token = "regex",
                        to_lower = FALSE,
                        pattern = "(?<!\\..|fig|Fig|i|e|al|Cat|cat|no|No|St|ver|Ver|Dr|dr)[.?!]\\s+")
        
        the_tibble$token = cleanFun(stringr::str_to_lower(the_tibble$sentence))
        

        the_tibble$matched_dic1 = grepl(x = the_tibble$token, pattern = paste0("\\b(",
                                                                               paste(dic1, collapse = "|"),
                                                                               ")\\b"),
                                        ignore.case = TRUE)
        the_tibble$matched_dic2 = grepl(x = the_tibble$token, pattern = paste0("\\b(",
                                                                               paste(dic2, collapse = "|"),
                                                                               ")\\b"),
                                        ignore.case = TRUE)
        
        collected.data = rbind(collected.data,
                               unique(the_tibble[which(the_tibble$matched_dic1 == TRUE &
                                                  the_tibble$matched_dic2 == TRUE),]))
        
        
        
        #find duplicated tokens
        duplicates = data.frame(table(collected.data$token))
        dupl.data = collected.data[collected.data$token %in% duplicates$Var1[duplicates$Freq > 1],]
        
        #identify which of the duplicated tokens derived from the same paragraph
        #that was somehow repeated
        u_pars = tapply(dupl.data$paragraphs, dupl.data$token, FUN = function(x){
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
        collected.data = 
          collected.data[which(!(collected.data$token %in% duplicates$Var1[duplicates$Freq > 1] & !(collected.data$paragraphs %in% u_pars))),]


        if (nrow(collected.data > 0)){
          collected.data$sentence = sapply(collected.data$sentence,
                                           FUN = function(x){highlight(text = x, dic = dic1, color = "red")})
          collected.data$sentence = sapply(collected.data$sentence,
                                           FUN = function(x){highlight(text = x, dic = dic2, color = "yellow")})
          collected.data$paragraphs = sapply(collected.data$paragraphs,
                                             FUN = function(x){highlight(text = x, dic = dic1, color = "red")})
          collected.data$paragraphs = sapply(collected.data$paragraphs,
                                             FUN = function(x){highlight(text = x, dic = dic2, color = "yellow")})
          
        }
        #https://shiny.rstudio.com/articles/progress.html
        incProgress(1/nrow(search_results))
      }
    })  
    
    if (nrow(collected.data) == 0) {
      
      collected.data = "No sentences matches both dictionary were found"
    }
    
  } else {
    
    collected.data = "No papers matching your query were found"
    
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
           '<p><h5><em>' + d[8] + ' ' + d[2] + ', ' + d[11] +
              ' (' + d[10] + ')</em></p></h5>' +
           '<p><h6><em><u>' + ' doi: ' + d[9] + '</u></em></h6></p>' +
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