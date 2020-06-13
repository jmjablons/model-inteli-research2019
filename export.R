pubmodel %>%
  purrr::map(~tidyr::gather(., param, measure, -tag, -name)) %>%
  dplyr::bind_rows() %>%
  filter(param %!in% c("counts.function", "counts.gradient", "convergence"))%>%
  xlsx::write.xlsx2('data/model_result.xls')