


#############
# rigorous flattening of rd element (concatenating all text / rcode / verb elements) 
flatten_rd <- function(r) {
  tag = attr(r, "Rd_tag")
  if (is.null(tag)) {
    tag = ""
  }
  if (tag == "TEXT") {
    return(as.character(r)) # TODO: convert to md?
  }
  else if (tag == "RCODE") {
    return(as.character(r)) # TODO: convert to md?
  }
  else if (tag == "VERB") {
    return(as.character(r)) # TODO: convert to md?
  }
  
  else if (is.list(r)) {
    if (tag == "\\code") {
      a = flatten_rd(r[[1]])
      if (grepl("\n", a, fixed = TRUE)) {
        return(paste0("\n```\n\n", flatten_rd(r[[1]]) , "\n```\n\n"))
      }
      else {
        return(paste0("`", flatten_rd(r[[1]]) , "`"))
      }
    }
    else if (tag %in% c("\\bold", "\\strong")) {
      return(paste0("**", flatten_rd(r[[1]]) , "**"))
    }
    
    else if (tag == "\\emph") {
      return(paste0("_", flatten_rd(r[[1]]) , "_"))
    }
    else if (tag == "\\link") {
      link1 = flatten_rd(r[[1]])
      link2 = attr(r, "Rd_option")
      
      dest = ""
      name = ""
      pkg = NULL
      if (is.null(link2)) {
        name = link1
        dest = paste0(link1, ".Rmd")
      }
      else {
        if (startsWith(link2, "=")) {
          name = link1
          dest = paste0(link2, ".Rmd")
        }
        else {
          a = unlist(strsplit(link2,":"))
          if (length(a) == 1) {
            dest = paste0(link2, "::", link1) # make ready for downlit
            name = dest
          }
          else if (length(a) == 2) {
            dest = paste0(a[1], "::", a[2]) # make ready for downlit
            name = link1
          }
          else {
            dest = link2
            name = link1
          }
          ext_dest = downlit::autolink(dest)
          if (!is.na(ext_dest)) {
            dest = ext_dest
          }
          else {
            dest = ""
          }
        }
      }
      
      return(paste0("[", name , "](", dest , ")")) # TODO: apply resolve links
    }
    else if (tag == "\\href") {
      url =  flatten_rd(r[[1]])
      linkname =  flatten_rd(r[[2]])
      return(paste0("[", linkname , "](", url , ")")) # TODO: apply resolve links
    }
    else if (tag == "\\url") {
      url =  flatten_rd(r[[1]])
      return(paste0("[", url , "](", url , ")")) # TODO: apply resolve links
    }
    else if (tag %in% c("\\method","\\S3method", "\\S4method" )) {
      return(paste0(flatten_rd(r[[1]]), ".", flatten_rd(r[[2]])))
    }
    else {
      out = ""
      if (length(r) > 0) {
        for (i in 1:length(r)) {
          out = paste0(out, flatten_rd(r[[i]]))
        }
      }
      return(out)
    }
  }
  else {
    return("")
  }
}

fix_code_links <- function(a) {
  gsub("`\\[(.+?)\\]\\((.+?)\\)`", "[`\\1`](\\2)", a)
}


extract_rd_fields <- function(f) {
  rd = tools::parse_Rd(f)
  
  out = list()
  for (i in 1:length(rd)) {
    r = rd[[i]]
    tag = attr(r, "Rd_tag")
    
    if (tag == "\\name") {
      out$name = flatten_rd(r)
    }
    if (tag == "\\title") {
      out$title = 
        flatten_rd(r)  |>
        fix_code_links()
    }
    if (tag == "\\usage") {
      out$usage = flatten_rd(r)
    }
    if (tag == "\\arguments") {
      out$args = list()
      for (j in 1:length(r)) {
        rr = r[[j]]
        
        if (attr(rr, "Rd_tag") == "\\item") {
          if ( length(rr) != 2) {
            warning(paste0("Failed to parse argument: ", as.character(rr)))
          }
          else {
            argname = 
              flatten_rd(rr[[1]])  |>
              fix_code_links()
            
            argdesc = 
              flatten_rd(rr[[2]])  |>
              fix_code_links()
            out$args[argname] = argdesc
          }
        }
      }
    }
    if (tag == "\\value") {
      out$value = 
        flatten_rd(r) |>
        fix_code_links()
    }
    if (tag == "\\description") {
      out$description = 
        flatten_rd(r) |>
        fix_code_links()
    }
    if (tag == "\\seealso") {
      out$seealso = 
        flatten_rd(r) |>
        fix_code_links()
    }
    
    if (tag == "\\examples") {
      out$examples = flatten_rd(r) # TODO: handle dontrun / donttest?
    }
    
    if (tag == "\\author") {
      out$author =
        flatten_rd(r) |>
        fix_code_links()
    }
    if (tag == "\\details") {
      out$details  = 
        flatten_rd(r) |>
        fix_code_links()
    }
    if (tag == "\\keyword") {
      out$keyword  = 
        flatten_rd(r) |>
        fix_code_links()
    }
    if (tag == "\\references") {
      out$references  = 
        flatten_rd(r) |>
        fix_code_links()
    }
    if (tag == "\\note") {
      out$note = 
        flatten_rd(r) |>
        fix_code_links()
    }
    # TODO: add alias?
  }
  return(out)
}




to_rmd <- function(x, outfile) {
  
  remove_pre_newlines <- function(y) {
    gsub("^\n*", "", y)
  }
  remove_trailing_newlines <- function(y) {
    gsub("\n*$", "", y)
  }
  remove_all_newlines <- function(y) {
    gsub("\n", "", y)
  }
  make_section <- function(y) {
    paste0("# ", y)
  }
  make_subsection <- function(y) {
    paste0("## ", y)
  }
  make_newparagraph <- function() {
    return("\n\n")
  }
  make_codeblock <- function(y) {
    paste0("\n```r\n", y, "\n```\n")
  }
  make_codechunk <- function(y) {
    paste0("\n```{r}\n", y, "\n```\n")
  }
  
  wrt <- function(y) {
    cat(y, sep="", file = outfile, append = TRUE)
  }
  
  
  if (file.exists(outfile)) {
    stop("File already exists")
  }
  
  
  wrt(make_section(remove_pre_newlines(x$name)))
  wrt(make_newparagraph())
  wrt(remove_pre_newlines(x$title))
  wrt(make_newparagraph())
  
  # invisibly load package 
  wrt(make_newparagraph())
  wrt("```{r include=FALSE}\nlibrary(gdalcubes)\n```")
  wrt(make_newparagraph())
  
  wrt(make_subsection("Description"))
  wrt(make_newparagraph())
  wrt(remove_pre_newlines(x$description))
  wrt(make_newparagraph())
  
  wrt(make_subsection("Usage"))
  wrt(make_newparagraph())
  wrt(make_codeblock(remove_pre_newlines(x$usage)))
  wrt(make_newparagraph())
  
  if (!is.null(x$args) && length(x$args) > 0) {
    wrt(make_subsection("Arguments"))
    wrt(make_newparagraph())
    wrt("| Argument    | Description                       |\n")
    wrt("|:------------|:----------------------------------|\n")
    for (arg in names(x$args)) {
      wrt(paste0("| ", remove_all_newlines(arg), " | ", remove_all_newlines(x$args[[arg]]), " |\n"))
    }
    wrt(make_newparagraph())
  }
  
  
  
  
  if (!is.null(x$details)) {
    wrt(make_subsection("Details"))
    wrt(make_newparagraph())
    wrt(remove_pre_newlines(x$details))
    wrt(make_newparagraph())
  }
  
  if (!is.null(x$value)) {
    wrt(make_subsection("Value"))
    wrt(make_newparagraph())
    wrt(remove_pre_newlines(x$value))
    wrt(make_newparagraph())
  }
  
  if (!is.null(x$note)) {
    wrt(make_subsection("Note"))
    wrt(make_newparagraph())
    for (i in 1:length(x$note)) {
      wrt(remove_pre_newlines(x$note))
      wrt(make_newparagraph())
    }
  }
  
  if (!is.null(x$examples)) {
    wrt(make_subsection("Examples"))
    wrt(make_newparagraph())
    x$examples |>
      remove_pre_newlines() |>
      remove_trailing_newlines() |>
      make_codechunk() |>
      wrt()
    wrt(make_newparagraph())
  }
  
  if (!is.null(x$seealso)) {
    wrt(make_subsection("See Also"))
    wrt(make_newparagraph())
    wrt(remove_pre_newlines(x$seealso))
    wrt(make_newparagraph())
  }
  
  if (!is.null(x$author)) {
    wrt(make_subsection("Author(s)"))
    wrt(make_newparagraph())
    wrt(remove_pre_newlines(x$author))
    wrt(make_newparagraph())
  }
  
  if (!is.null(x$keyword)) {
    wrt(make_subsection("Keywords"))
    wrt(make_newparagraph())
    wrt(remove_all_newlines(paste(x$keyword, collapse = " ")))
    wrt(make_newparagraph())
  }
  
  if (!is.null(x$references)) {
    wrt(make_subsection("References"))
    wrt(make_newparagraph())
    wrt(remove_pre_newlines(x$references))
    wrt(make_newparagraph())
  }
  
}

################

rdfiles = readRDS(system.file("help/paths.rds", package="gdalcubes"))
TARGET_DIR = "source/reference/ref"
if (dir.exists(TARGET_DIR)) {
  file.remove(list.files(TARGET_DIR, pattern = ".Rmd", full.names = TRUE))
} else {
  dir.create(TARGET_DIR)
}

for(i in 1:length(rdfiles)) {
  name = tools::file_path_sans_ext(basename(rdfiles[i]))
  cat(paste0("[", i , "/",length(rdfiles), "] Writing ", name, ".Rmd\n"))
  out = extract_rd_fields(rdfiles[i])
  to_rmd(out, file.path(TARGET_DIR, paste0(name,".Rmd")))
}
cat("DONE.\n")
