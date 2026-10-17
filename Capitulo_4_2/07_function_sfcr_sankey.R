sfcr_sankey <- function(tfm, baseline, when = c("start","end")) 
{
  if (!requireNamespace("networkD3", quietly = TRUE)) {
    stop("Packages \"networkD3\" needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  # Ensure 'baseline' is a valid sfcr_tbl object
  if (!inherits(baseline, "sfcr_tbl")) {
    stop("Please supply a valid baseline model.")
  }
  when <- match.arg(when, c("start", "end"))
  bl1 <- attr(baseline, "calls")
  bl2 <- attr(baseline, "external")$lhs
  bl3 <- attr(baseline, "matrix")
  if (when == "start") {
    m <- bl3[4:5, ]
  }
  else {
    nr <- nrow(bl3)
    m <- bl3[(nr - 1):nr, ]
    # rownames(m) <- NULL
  }
  nms <- colnames(tfm)
  colnames(tfm) <- c("name", nms[2:length(nms)])
  tfm <- tfm %>% dplyr::mutate(dplyr::across(-1, ~stringr::str_replace_all(.x, 
                                                                           "d\\((.*?)\\)", "\\(\\1 - \\1\\[-1\\]\\)"))) %>% dplyr::mutate(dplyr::across(-1, 
                                                                                                                                                        ~sfcr:::.add_time2(.x))) %>% dplyr::mutate(dplyr::across(-1, 
                                                                                                                                                                                                          ~gsub(sfcr:::.pvar(bl1$lhs), "m\\[2,'\\1'\\]", .x, perl = T))) %>% 
    dplyr::mutate(dplyr::across(-1, ~gsub(sfcr:::.pvar(bl2), "m\\[2,'\\1'\\]", 
                                          .x, perl = T))) %>% dplyr::mutate(dplyr::across(-1, 
                                                                                          ~gsub(sfcr:::.pvarlag(bl1$lhs), "m\\[1,'\\1'\\]", .x, perl = T))) %>% 
    dplyr::mutate(dplyr::across(-1, ~gsub(sfcr:::.pvarlag(bl2), 
                                          "m\\[1,'\\1'\\]", .x, perl = T))) %>% dplyr::mutate(dplyr::across(-1, 
                                                                                                            ~gsub("___", "", .x)))
  .eval_matrices <- function(x) {
    purrr::map(x, function(y) if (stringr::str_length(y) == 
                                     0) {
      NA_real_
    }
    else {
      eval(str2expression(y))
    })
  }
  tfm <- tfm %>% dplyr::mutate(dplyr::across(-1, ~as.character(.eval_matrices(.x))))
  tfm <- tfm %>% tidyr::pivot_longer(cols = -name, names_to = "sector") %>% 
    dplyr::mutate(value =  suppressWarnings(as.numeric(value)))
  tfm_plus <- dplyr::filter(tfm, value > 0)
  tfm_minus <- dplyr::filter(tfm, value < 0)
  nodes <- tibble::tibble(name = c(unique(tfm$name), unique(tfm$sector), 
                                   paste0(unique(tfm$sector), "1")), node = 0:(length(name) - 
                                                                                 1))
  links_minus <- tfm_minus %>% dplyr::mutate(value = -1 * value) %>% 
    dplyr::left_join(nodes, by = c("name")) %>% dplyr::left_join(nodes, 
                                                                 by = c(sector = "name")) %>% dplyr::rename(source = node.y, 
                                                                                                            target = node.x)
  links_plus <- tfm_plus %>% dplyr::mutate(sector = paste0(sector, 
                                                           1)) %>% dplyr::left_join(nodes, by = c("name")) %>% dplyr::left_join(nodes, 
                                                                                                                                by = c(sector = "name")) %>% dplyr::rename(source = node.x, 
                                                                                                                                                                           target = node.y)
  links <- dplyr::bind_rows(links_minus, links_plus) %>% dplyr::select(source, 
                                                                       target, value)
  nodes <- nodes %>% dplyr::mutate(name = gsub("1", "", name))
  networkD3::sankeyNetwork(Links = as.data.frame(links), Nodes = as.data.frame(nodes), 
                           Source = "source", Target = "target", Value = "value", 
                           NodeID = "name", units = "dollars", fontSize = 20)
}
