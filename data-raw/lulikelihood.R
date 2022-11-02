
  library(magrittr)

# likelihood
  lulikelihood <- tibble::tribble(
    ~likelihood, ~maxVal
    , "Exceptionally unlikely", 0.01
    , "Extremely unlikely", 0.05
    , "Very unlikely", 0.1
    , "Unlikely", 1/3
    , "About as likely as not", 2/3
    , "Likely", 0.9
    , "Very likely", 0.95
    , "Extremely likely", 0.99
    , "Virtually certain", 1
    ) %>%
    dplyr::mutate(likelihood = forcats::fct_inorder(likelihood)
                  , range = cut(maxVal
                                , breaks = c(0,.$maxVal)
                                )
                  , loose = dplyr::case_when(maxVal <= 1/3 ~ "+"
                                             , maxVal > 2/3 ~ "-"
                                             , TRUE ~ "0"
                                             )
                  , very = dplyr::case_when(maxVal <= 0.1 ~ "++"
                                             , maxVal <= 1/3 ~ "+"
                                             , maxVal > 0.9 ~ "--"
                                             , maxVal > 2/3 ~ "-"
                                             , TRUE ~ "0"
                                             )
                  , extreme = dplyr::case_when(maxVal <= 0.05 ~ "+++"
                                                 , maxVal <= 0.1 ~ "++"
                                                 , maxVal <= 1/3 ~ "+"
                                                 , maxVal > 0.95 ~ "---"
                                                 , maxVal > 0.9 ~ "--"
                                                 , maxVal > 2/3 ~ "-"
                                                 , TRUE ~ "0"
                                                 )
                  , exceptional = dplyr::case_when(maxVal <= 0.01 ~ "++++"
                                                      , maxVal <= 0.05 ~ "+++"
                                                      , maxVal <= 0.1 ~ "++"
                                                      , maxVal <= 1/3 ~ "+"
                                                      , maxVal > 0.99 ~ "----"
                                                      , maxVal > 0.95 ~ "---"
                                                      , maxVal > 0.9 ~ "--"
                                                      , maxVal > 2/3 ~ "-"
                                                      , TRUE ~ "0"
                                                      )
                  ) %>%
    dplyr::mutate(dplyr::across(where(is.character)
                                , ~forcats::fct_relevel(.x
                                                        , "++++"
                                                        , "+++"
                                                        , "++"
                                                        , "+"
                                                        , "0"
                                                        , "-"
                                                        , "--"
                                                        , "---"
                                                        , "----"
                                                        )
                                )
                  )
