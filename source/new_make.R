mk_pub_new <- function(data, exclude = NULL) {
  
  indentation <- ""
#     "\\hangindent=2em
# \\hangafter=1"
  
  note <- data %>% arrange(year, journal, note, author) %>% pull(note)
  
  data <- data %>% 
    mutate(
      note = factor(note, c(NA,"In print", "Under review", "In preparation"))
    ) %>% 
    arrange(year, note, journal, author) # %>% select(all_of(selection))
  
  
  
  if(!is.null(exclude)) {
    
    for(ex in exclude) {
      data <- data %>% 
        mutate(!!as.name(ex) := NA)
    }
  }
  
  
  data <- 
    data %>% 
    mutate(author = str_replace(author, "Sooyong Lee", fixed("\\\\textbf{Sooyong Lee}")),
           journal = paste0("\\emph{",journal,"}."),
           volum = paste0(" ",volum,","),
           number = paste0(" ",number,"."),
           doi = paste0(" DOI:", doi,"."),
           note = paste0(" ",note,".")
    ) %>% 
    mutate_all(~ as.character(.x)) %>% 
    mutate_all(~ case_when(is.na(.x) ~ "", 
                           str_detect(.x, "NA") ~ "", 
                           # str_detect(.x, "DOI:NA") ~ "", 
                           str_detect(.x, "NULL") ~ "", 
                           TRUE ~ .x)) %>% 
    mutate(
      out = 
        glue::glue(" <<author>> (<<year>>). <<title>>. <<journal>><<volum>><<number>><<doi>><<note>>",
                   .open = "<<", .close = ">>")
    )
  
  # out <- data %>% pull(out)
  # out <- paste(out, collapse = "\n\n")
  data
  # cat(out)
}

# Presentation
mk_presen_new <- function(data, exclude = NULL) {
  
  indentation <- 
    "\\hangindent=2em
\\hangafter=1"
  
  data <- data %>% arrange(year, author) # %>% select(all_of(selection))
  
  if(!is.null(exclude)) {
    
    for(ex in exclude) {
      data <- data %>% 
        mutate(!!as.name(ex) := NA)
    }
  }
  
  data <- 
    data %>% 
    mutate(author = str_replace(author, "Sooyong Lee", fixed("\\\\textbf{Sooyong Lee}")),
           title = paste0("\\emph{",title,"}"),
           conference = paste0(conference,","),
           location = paste0(" ",location,".")
    ) %>% 
    mutate_all(~ as.character(.x)) %>% 
    mutate_all(~ case_when(is.na(.x) ~ "", 
                           str_detect(.x, "NA") ~ "", 
                           str_detect(.x, "NULL") ~ "", 
                           TRUE ~ .x)) %>% 
    mutate(
      out = 
        glue::glue("<<author>> (<<year>>). <<title>>. <<conference>><<location>>",  .open = "<<", .close = ">>")
    )
  
  out <- data %>% pull(out)
  out <- paste(out, collapse = "\n\n")
  
  cat(out)
}


# mk_pub_new <- function(data, exclude = NULL) {
#   
#   indentation <- 
#     "\\hangindent=2em
# \\hangafter=1"
#   
#   note <- data %>% arrange(year, journal, note, author) %>% pull(note)
#   
#   data <- data %>% 
#     mutate(
#       note = factor(note, c(NA,note[str_detect(note, "In print")], "Under review", "In preparation"))
#     ) %>% 
#     arrange(year, note, journal, author) # %>% select(all_of(selection))
#   
#   
#   
#   if(!is.null(exclude)) {
#     
#     for(ex in exclude) {
#       data <- data %>% 
#         mutate(!!as.name(ex) := NA)
#     }
#   }
#   
#   
#   data <- 
#     data %>% 
#     mutate(
#       volum = paste0(volum, ","),
#       number = paste0(number, "."),
#       note = paste0(note, "."),
#     ) %>% 
#     mutate_all(~ as.character(.x)) %>% 
#     mutate_all(~ case_when(is.na(.x) ~ "", 
#                            str_detect(.x, "NA") ~ "", 
#                            # str_detect(.x, "DOI:NA") ~ "", 
#                            str_detect(.x, "NULL") ~ "", 
#                            TRUE ~ .x)) %>% 
#     # mutate(
#     #   out = 
#     #     glue::glue("{indentation}\n{author} ({year}). {title}. {journal}. {volum} {number} {doi}. {note}")
#     # ) %>% 
#     mutate(
#       out = pub_entry("-", year, author, title, journal, glue('{volum} {number} {note}'))
#     )
#     
#   
#   out <- data %>% pull(out)
#   
#   # out <- paste(out, collapse = "\n\n")
#   
#   out
# }