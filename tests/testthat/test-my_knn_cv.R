test_that("my_knn_cv produces correct output type", {
  expect_type(my_knn_cv(train = my_penguins %>%
                          dplyr::select(body_mass_g,
                                 flipper_length_mm,
                                 bill_length_mm,
                                 bill_depth_mm),
                        cl = my_penguins$species,
                        k_nn = 3,
                        k_cv = 5),
              "list")
  expect_equal(length(my_knn_cv(train = my_penguins %>%
                                  dplyr::select(body_mass_g,
                                         flipper_length_mm,
                                         bill_length_mm,
                                         bill_depth_mm),
                                cl = my_penguins$species,
                                k_nn = 3,
                                k_cv = 5)),
               2)
})
