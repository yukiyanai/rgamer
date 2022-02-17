context("Implement matching")

test1 <- matching(
  g1_prefs = list(w1 = c(1, 2),
                  w2 = c(2, 1),
                  w3 = c(1, 2)),
  g2_prefs = list(m1 = c(1, 2, 3),
                  m2 = c(2, 3, 1)))

test2 <- matching(
  g1_names = c("w1", "w2", "w3"),
  g1_prefs = list(c(1, 2),
                  c(2, 1),
                  c(1, 2)),
  g2_names = c("m1", "m2"),
  g2_prefs = list(c(1, 2, 3),
                  c(2, 3, 1)))

test3 <- matching(
  g1_names = c("Amy", "Beatrice", "Cindy"),
  g1_prefs = list(c("Dick", "Eric"),
                  c("Eric", "Dick"),
                  c("Dick", "Eric")),
  g2_names = c("Dick", "Eric"),
  g2_prefs = list(c("Amy", "Beatrice", "Cindy"),
                  c("Beatrice", "Cindy", "Amy")))

test4 <- matching(
  g1_prefs <- list(w1 = c(1, 2),
                   w2 = c(2, 1),
                   w3 = c(1, 2)),
  g2_prefs <- list(m1 = c(1, 2),
                   m2 = c(2, 3))
)

df1 <- data.frame(
  name = c("w1", "w2", "w3"),
  p1 = c(1, 2, 1),
  p2 = c(2, 1, 2)
)
df2 <- data.frame(
  name = c("m1", "m2"),
  p1 = c(1, 2),
  p2 = c(2, 3),
  p3 = c(3, 1)
)
test6 <- matching_df(df1 = df1,
                     df2 = df2)

test_that("matching implements matching", {
  expect_s3_class(test1, "matching")
  expect_s3_class(test1$data, "data.frame")
  expect_type(test1$history, "character")
  expect_type(test1$results, "character")
  expect_length(test1, 3)
  expect_s3_class(test2, "matching")
  expect_s3_class(test3, "matching")
  expect_s3_class(test4, "matching")
  expect_error(matching(
    g1_names = c("Amy", "Beatrice", "Cindy"),
    g1_prefs = list(c("Dick", "Eric"),
                    c("Eric", "Dick"),
                    c("Dick", "Yuki")),
    g2_names = c("Dick", "Eric"),
    g2_prefs = list(c("Amy", "Beatrice", "Cindy"),
                    c("Beatrice", "Cindy", "Amy"))))
})

test_that("matching using data frames", {
  expect_s3_class(test6, "matching")
  expect_s3_class(test6$data, "data.frame")
  expect_type(test6$history, "character")
  expect_type(test6$results, "character")
  expect_length(test6, 3)
})
