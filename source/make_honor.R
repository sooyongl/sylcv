mk_newentry <- function(data, what=NA, when=NA, with=NA, where=NA, why=NA, more_why = NA, showcat = T, entry_nm = "cvhonor") {
  # data = a1
  cols <- c("what"=what, "when"=when, "with"=with, "where"=where, "why"=why, "more_why" = more_why)
  
  check_name<-function(data, cols) {
    for(a in cols) {
      names(data)[which(names(data)==a)] <- names(cols)[which(cols==a)]
    }
    data
  }
  
  out <- check_name(data, cols)
  
  if(sum(any(names(out)=="what"), na.rm = T) == 0) {
    out$what <- ""
  }
  
  if(sum(any(names(out)=="when"), na.rm = T) == 0) {
    out$when <- ""
  }
  
  if(sum(any(names(out)=="with"), na.rm = T) == 0) {
    out$with <- ""
  }
  if(sum(any(names(out)=="where"), na.rm = T) == 0) {
    out$where <- ""
  }
  
  if(sum(any(names(out)=="why"), na.rm = T) == 0) {
    
    why0 <- ""
    
  } else {
    why_part <- str_split(out$why, ";", simplify = F) 
    why0 <- c(); i <- 0
    for(wp in why_part){
      i <- i + 1
      
      if(is.na(wp)) {
        why_itemize <- ""
      } else {
        why_itemize <- ""  
        for(wpp in wp) {
          why_itemize <- paste0(why_itemize,"\\item ", wpp," ")
        }
      }
      
      why0[i] <- why_itemize
    }
  }
  
  if(sum(any(names(out)=="more_why"), na.rm = T) == 0) {
    
    more_why0 <- ""
    
  } else {
    morewhy_part <- str_split(out$more_why, ";", simplify = F) 
    more_why0 <- c(); i <- 0
    for(wp in morewhy_part){
      i <- i + 1
      
      if(is.na(wp)) {
        
        morewhy_itemize <- ""
        
      } else {
        morewhy_itemize <- " \\begin{itemize}"
        for(wpp in wp) {
          morewhy_itemize <- paste0(morewhy_itemize,"\\item item ", wpp," ")
        }
        morewhy_itemize <- paste0(morewhy_itemize, " \\end{itemize}")
        
      }
      more_why0[i] <- morewhy_itemize
    }
  }
  
  
  out <- out %>% 
    mutate(why = why0) %>% 
    mutate(more_why = more_why0) %>% 
    mutate_all(~ as.character(.x)) %>% 
    mutate_all(~ case_when(is.na(.x) ~ "", 
                           str_detect(.x, "NA") ~ "", 
                           str_detect(.x, "NULL") ~ "", 
                           TRUE ~ .x)) %>% 
    mutate(
      tex = paste0("\\",entry_nm, "{",
                   what,"}{",
                   with,"}{",
                   where,"}{",
                   when,"}{\\begin{cvitems}",
                   why, 
                   
                   more_why,
                   
                   "\\end{cvitems}}")
    ) %>% 
    pull(tex)
  
  out <- paste(out, collapse = "\n\n")
  out <- paste(
    c(paste0("\\begin{", entry_nm, "s}"),
      out,
      paste0("\\end{", entry_nm, "s}"),
    collapse = "\n"))
  
  if(showcat) {
    
    cat(out)  
    
  } else {
    out
  }
  
}