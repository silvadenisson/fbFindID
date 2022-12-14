#' Extract ID page facebook
#'
#' @param x url facebook (\code{character}).
#'
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' fbID("https://www.facebook.com/denisson.silva.9")
#' }

fbID <- function(x) {

  if(is.character(x)){
    html <- rvest::read_html(x)

    texto <- rvest::html_element(html, "body") %>% rvest::html_text2()

    id <- stringr::str_sub(texto,
                           stringr::str_locate_all(texto, "pageID|userID|entity_id")[[1]][1,1],
                           stringr::str_locate_all(texto, "pageID|userID|entity_id")[[1]][1,2] + 20) %>%
      stringr::str_remove_all("[:punct:]|[:alpha:]")

    return(id)

  } else{

    message("class character")

  }

}

