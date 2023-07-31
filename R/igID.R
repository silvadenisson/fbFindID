#' Extract ID page Instagram
#'
#' @param x url instragram (\code{character}).
#'
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' igID("https://www.instagram.com/denissonmatuto")
#' }

igID <- function(x) {

  if(is.character(x)){
    html <- rvest::read_html(x)

    texto <- rvest::html_element(html, "body") %>% rvest::html_text2()

    write(texto, file = "teste.html")

    id <- stringr::str_sub(texto,
                           stringr::str_locate_all(texto, "profilePage_")[[1]][1,1],
                           stringr::str_locate_all(texto, "profilePage_")[[1]][1,2] + 20) %>%
      stringr::str_remove_all("[:punct:]|[:alpha:]")

    return(id)

  } else{

    message("class character")

  }

}

