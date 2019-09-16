#### FUNCTIONS FOR DATA WRANGLING ####

basic_exclusions <- function(d) {
  d1 <- d %>% 
    # remove observations that meet exclusion criteria
    filter(created >= as.Date("2018-11-22")) %>%  # filter out data from testing phase
    filter(created <= as.Date("2019-02-28"))      # filter out data from outside sampling period
  
  d2 <- d1 %>% filter(!is.na(session), !is.na(ended)) 
  d3 <- d2 %>% # unfinished sessions
    filter(data_ok == 1) 
  
  d4 <- d3 %>% # participant indicated unserious answers (up to here: 1495 observations)
    filter(is.na(study_degree_year) | study_degree_year < 3) # finished studying previous to 2017 (up to here: 1476 observations)
  
  n0 <- nrow(d)
  n1 <- nrow(d1)
  n2 <- nrow(d2)
  n3 <- nrow(d3)
  n4 <- nrow(d4)
  return(list(data = d4, exclusions = list(raw = n0, in_period = n1, 
                                           fin = n2, ok = n3, students = n4)))
}

german_unis <- function(d) {
  d1 <- d %>% filter(!(uni_current %contains% "Amsterdam") | is.na(uni_current)) %>% 
    filter(!(uni_current %contains% "[Gg]roningen") | is.na(uni_current))
  return(d1)
}

only_psych <- function(d) {
  d1 <- d %>% filter(field_of_study_mc == "Psychology" | study_degree_field == "Psychology")
  return(d1)
}

add_n_projects <- function(d) {
  d1 <- d %>% 
    # n_of_projects
    mutate(n_projects = str_count(exp_specific, "[0-9]")) %>% 
    mutate(n_projects = ifelse(is.na(n_projects), 0, n_projects)) %>% 
    
    # project dummies
    mutate(n_emp.intern = str_count(exp_specific, "1"),
           n_project =    str_count(exp_specific, "2"),
           n_thesis.bsc = str_count(exp_specific, "3"),
           n_thesis.msc = str_count(exp_specific, "4"),
           n_other =      str_count(exp_specific, "5"))
  
  return(d1)
}

fix_typos <- function(d) {
  string_replacement <- c(
    ".*[rR]eg.*" = "Universität Regensburg", 
    ".*[Kk]öln.*" = "Universität zu Köln",
    ".*[Ss][Rr][Hh].*" = "SRH Heidelberg",
    ".*[Gg]ueri.*" = "Otto-von-Guericke-Universität Magdeburg",
    ".*[Ww]ürz.*" = "Julius-Maximilians-Universität Würzburg",
    ".*[Rr][Ww][Tt][Hh].*" = "RWTH Aachen",
    ".*[Aa]a?chen.*" = "RWTH Aachen",
    ".*[Ll][Mm][Uu].*" = "Ludwig-Maximilians-Universität München",
    ".*[oO]snabrück.*" = "Universität Osnabrück",
    ".*[Gg]roningen.*" = "University of Groningen",
    ".*fresenius.*" = "Hochschule Fresenius Düsseldorf")
  
  # The code below now uses the `string_replacement` vector from above to 
  # correct typos in the main data frame.
  d1 <- d %>% 
    mutate(uni_current = str_replace_all(uni_current, string_replacement),
           uni_bachelor = str_replace_all(uni_bachelor, string_replacement))
  
  # The code below replaces answers like `----` in `uni_current` with `NA`.
  d1$uni_current[grepl(".*--.*", d1$uni_current)] <- NA
  
  return(d1)
}

spread_practices <- function(research_practice, name) {
  
  # helper function
  find_one_rp <- function(research_practice, project_index) {
    out <- ifelse(research_practice %contains% project_index, TRUE, FALSE)
    return(out)
  }
  
  # define projects vector
  projects <- c("_emp.intern" = "1", 
                "_project" = "2",
                "_thesis.bsc" = "3", 
                "_thesis.msc" = "4", 
                "_other" = "5")
  
  # apply helper function for each project
  out <- map_dfc(projects, find_one_rp, research_practice = research_practice)
  
  # add information about research practice to colname
  names(out) <- paste0(name, names(projects))
  
  # return the object
  return(out)
}

#### FUNCTIONS FOR ANALYSIS ####
qrp_in_lectures <- function(d) {
  d <- d %>% separate(specific_qrps_teaching, into = paste0("qrp.t", 1:11)) %>% 
    gather(qrp.t1:qrp.t11, key = "answer_number", value = "qrp_id") %>% 
    drop_na(qrp_id) %>% 
    mutate(qrp = dplyr::recode(qrp_id, 
                               "1" = "SR of Variables", 
                               "2" = "No Sample Planning", 
                               "3" = "Flexible Sample Size", 
                               "4" = "SR of Conditions", 
                               "5" = "Flexible Analysis", 
                               "6" = "Flexible Exclusion", 
                               "7" = "SR of Hypotheses", 
                               "8" = "HARKing", 
                               "9" = "Rounding p-Values", 
                               "10" = "None", 
                               "11" = "Not Sure")) %>% 
    select(id, qrp) %>% arrange(id)
  
  return(d)
}

#### FUNCTIONS FOR PRINTING ####

# function for german number printing
co <- function(x, nsmall = 2, nround = 2, ger = T) {
  if (ger == T) {
    out <- x %>% round(nround) %>%  format(nsmall = nsmall, decimal.mark = ",")
  } else {
    out <- x %>% round(nround) %>%  format(nsmall = nsmall)
  }
  
  return(out)
}

co.data <- function(data, x, ...) {
  out <- data %>% pull(x) %>% co(...)
  return(out)
}

# function for printing of percentage numbers
cop <- function(data, nsmall = 1, nround = 1, ger = T, space = T) {
  x <- ifelse(space, " %", "%")
  out <- paste0(co(data, nround = nround, nsmall = nsmall, ger = ger), x)
  return(out)
}

cop2 <- function(data, nsmall = 1, nround = 1, ger = T) {
  out <- co(data*100, nround = nround, nsmall = nsmall, ger = ger)
  return(out)
}

# function for apa p value printing
format_p <- function(p, zero = FALSE, stars = "yes", alpha = NULL) {
  z <- ifelse(zero, "0.", ".")
  
  if (stars == "yes") {
    s1 <- "*  "; s2 <- "** "; s3 <- "***"
  } else if (stars == "space") {
    s1 <- "   "; s2 <- "   "; s3 <- "   "
  } else if (stars == "no") {
    s1 <- ""; s2 <- ""; s3 <- ""
  } else {
    warning("Parameter 'stars' must be one of 'yes', 'space', 'no'.")
  }
  
  if (!is.null(alpha)) {
    if (length(alpha) == 1) {
      a1 <- alpha
      a2 <- alpha / 5
      a3 <- alpha / 50
    } else if (length(alpha == 3)) {
      a1 <- alpha[1]
      a2 <- alpha[2]
      a3 <- alpha[3]
    } else warning("alpha needs to be of length 1 or 3.")
  } else {
    a1 <- 0.05
    a2 <- 0.01
    a3 <- 0.001
  }
  
  single_p <- function(x) {
    if (round(x, 3) < 0.001) {
      out <- paste0("<", z, "001")
    } else {
      out <- x %>% round(3) %>% format(nsmall = 3)
      out <- out %>% str_replace("0\\.", z)
    }
    
    if (is.na(x)) {
      return(NA)
    } else if (x < a3) {
      out <- paste0(out, s3)
    } else if (x < a2 & x > a3){
      out <- paste0(out, s2)
    } else if (x < a1 & x > a2) {
      out <- paste0(out, s1)
    } 
    return(out)
  }
  
  out <- map_chr(p, single_p)
  return(out)
}

printp <- function(p, zero = FALSE, stars = "no", alpha = NULL) {
  if (p < 0.001) {
    out <- paste("p", format_p(p, zero = zero, stars = stars, alpha = alpha))
  } else {
    out <- paste("p =", format_p(p, zero = zero, stars = stars, alpha = alpha))
  }
  return(out)
}

# create a table from GLMM for printing in report
glmer_table <- function(m, alpha = 0.05, names, ci_level = 0.95, bonferroni = T, ger = T) {
  su <- summary(m)
  co <- su$coefficients
  k <- nrow(co) - 1
  a1 <- ifelse(bonferroni, alpha / k, alpha)
  a2 <- a1 / 5
  a3 <- a1 / 50
  
  ci_sep <- ifelse(ger == T, ";", ",")
  
  if (is.numeric(ci_level)) {
    ci <- m %>% confint(method = "Wald", level = ci_level) %>% exp()
    ci_level <- paste0("KI (", (ci_level*100) %>% format(nsmall = 2), " %)")
  } else if (ci_level == "adjusted") {
    ci <- m %>% confint(method = "Wald", level = 1-a1) %>% exp()
    ci_level <- paste0("KI (", (round(1-a1, 4)*100) %>% format(nsmall = 2), " %)")
  }
  
  n.na <- sum(is.na(ci[,1])) + 1
  ci_fix <- ci[n.na:nrow(ci),] %>% round(2)
  
  d1 <- co %>% as_tibble() %>% 
    mutate(OR = exp(Estimate))
  names(d1) <- c("B", "SE", "z", "p", "OR")
  
  d2 <- ci_fix %>% as_tibble()
  names(d2) <- c("lo", "up")
  
  d <- bind_cols(d1, d2)
  out <- d %>% mutate(predictor = rownames(co)) %>% 
    mutate(p = format_p(p, alpha = a1)) %>%
    mutate(CI = glue::glue("[{co(lo, nround = 2, nsmall = 2)}{ci_sep} {co(up, nround = 2, nsmall = 2)}]")) %>% 
    add_column(ger_names = names) %>%
    select(predictor, ger_names, B, SE, OR, CI, z, p) %>%
    mutate_at(c(3, 4, 5, 7), round, 2)
  
  names(out) <- c("predictor", "Prädiktor", "B", "SE", "OR", ci_level, "z", "p")
  
  return(list(table = out, alpha = c(a1, a2, a3)))
}

# create a table from LMM for printing in report
lmer_table <- function(m, names, alpha = 0.05, bonferroni = T, ger = T) {
  su <- summary(m)
  k <- nrow(su$coefficients) - 1
  
  a1 <- ifelse(bonferroni, alpha / k, alpha)
  a2 <- a1 / 5
  a3 <- a1 / 50
  
  d <- su$coefficients %>% as_tibble() %>% select(-df)
  names(d) <- c("B", "SE", "t", "p")
  
  out <- d %>% mutate(p = format_p(p, alpha = a1)) %>% 
    add_column(predictor = rownames(su$coefficients), .before = "B") %>% 
    add_column(" " = names, .before = "B")
  
  return(list(table = out, alpha = c(a1, a2, a3)))
}

# format a correlation table
format_cor <- function(x, names=NULL, zero = TRUE, alpha = NULL, bonferroni = FALSE) {
  # takes a Hmisc::rcorr object as input
  r <- x$r
  p <- x$P
  
  if (is.null(names)) {
    na <- colnames(r)  
  } else if (length(names) != ncol(r)) {
    warning("names must have a name for each variable.")
    return("aborted")
  } else {
    na <- names
  }
  
  if (!is.null(alpha)) {
    if (length(alpha) == 1) {
      a1 <- alpha
      a2 <- alpha / 5
      a3 <- alpha / 50
    } else if (length(alpha == 3)) {
      a1 <- alpha[1]
      a2 <- alpha[2]
      a3 <- alpha[3]
    } else warning("alpha needs to be of length 1 or 3.")
  } else {
    a1 <- 0.05
    a2 <- 0.01
    a3 <- 0.001
  }
  
  if (bonferroni) {
    a1 <- a1 / ncol(r)
    a2 <- a1 / 5
    a3 <- a1 / 50
  }
  
  out <- matrix(NA, ncol = ncol(r), nrow = nrow(r))
  for (col in 1:ncol(r)) {
    for (rw in 1:nrow(r)) {
      px <- p[rw, col]
      rx <- r[rw, col] %>% round(2)
      if (is.na(px)) {
        out[rw, col] <- format(1, nsmall = 2)
        if (rx != 1) warning("P value was NA, but correlation not 1.")
      } else if (px > a1) {
        out[rw, col] <- paste0(format(rx, nsmall = 2)) 
      } else if (a2 < px & px < a1) {
        out[rw, col] <- paste0(format(rx, nsmall = 2), "*") 
      } else if (a3 < px & px < a2) {
        out[rw, col] <- paste0(format(rx, nsmall = 2), "**") 
      } else if (px < a3) {
        out[rw, col] <- paste0(format(rx, nsmall = 2), "***") 
      }
    }
  }
  
  if (!zero) {
    out <- out %>% apply(2,function(x) str_replace(x, "0\\.", "."))
  }
  
  out[upper.tri(out)] <- ""
  colnames(out) <- c(na)
  out <- as_tibble(out)
  out <- bind_cols(" " = na, out)
  
  return(list(table = out, alpha = c(a1, a2, a3)))
}

# quick-p-value-formatting for tables
quick.p <- function(p) {
  return(format(p, digits = 1, nsmall = 3, zero.print = FALSE, scientific = FALSE,
                decimal.mark = ","))
}

# glue abbreviations together
abk.f <- function(general, specific = "") {
  out <- paste0("Abkürzungen: ", general, ifelse(nchar(specific) > 1, ", ", ""), specific, "\n")
  return(out)
}


# prepare an anova object for printing
prep_anova <- function(x, keep = 6:8) {
  x %>% 
    as_tibble() %>% 
    rename(df = `Chi Df`) %>% 
    rename(chisq = Chisq) %>% 
    rename(p = `Pr(>Chisq)`) %>% 
    mutate(p = format_p(p)) %>% 
    select(keep)  
}