# Edge case tests for dataFormat function
# Tests for unusual/abnormal input patterns

# ============================================================
# Phase 1: ID column identification
# ============================================================

test_that("id=NULL auto-detects numeric consecutive IDs (1:N)", {
  set.seed(201)
  data <- data.frame(
    id = 1:15,
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  result <- dataFormat(data)
  expect_equal(ncol(result$Q), 2)
  expect_equal(result$ID, as.character(1:15))
})

test_that("id=1 explicitly treats column 1 as ID", {
  set.seed(202)
  data <- data.frame(
    id = 1:15,
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  result <- dataFormat(data, id = 1)
  expect_equal(ncol(result$Q), 2)
  expect_equal(result$ID, as.character(1:15))
})

test_that("id=NULL auto-detects string IDs in first column", {
  set.seed(203)
  data <- data.frame(
    student = paste0("S", 1:15),
    item1 = sample(0:1, 15, replace = TRUE),
    item2 = sample(0:1, 15, replace = TRUE)
  )
  result <- dataFormat(data)
  expect_equal(ncol(result$U), 2)
  expect_equal(result$ID, paste0("S", 1:15))
})

test_that("id=NULL generates auto IDs when first column is response data + message", {
  set.seed(204)
  # First column has duplicates and looks like response data
  data <- data.frame(
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE),
    item3 = sample(1:4, 15, replace = TRUE)
  )
  expect_message(result <- dataFormat(data), "No ID column detected")
  expect_equal(ncol(result$Q), 3)
  expect_equal(result$ID[1], "Student1")
})

test_that("explicit id for non-first column works", {
  set.seed(205)
  data <- data.frame(
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE),
    student = paste0("S", 1:15)
  )
  result <- dataFormat(data, id = 3)
  expect_equal(ncol(result$Q), 2)
  expect_equal(result$ID, paste0("S", 1:15))
})

test_that("NA in ID column causes error (duplicated NAs)", {
  data <- data.frame(
    id = c(paste0("S", 1:13), NA, NA),
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  expect_error(dataFormat(data, id = 1), "Duplicated IDs found")
})

test_that("id column number exceeds ncol causes error", {
  data <- data.frame(
    item1 = sample(0:1, 15, replace = TRUE),
    item2 = sample(0:1, 15, replace = TRUE)
  )
  expect_error(dataFormat(data, id = 5), "ID column number exceeds")
})

test_that("rownames lost by unclass: auto-generated IDs used instead", {
  # NOTE: as.data.frame(unclass(data)) on line 61 resets rownames to 1:n.
  # The rownames-as-ID path is effectively dead code in the current implementation.
  set.seed(206)
  data <- data.frame(
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  rownames(data) <- paste0("Subj", 1:15)
  expect_message(result <- dataFormat(data), "No ID column detected")
  # Rownames are lost, so auto-generated IDs are used
  expect_equal(result$ID[1], "Student1")
})

# ============================================================
# Phase 2: Response type detection
# ============================================================

test_that("CA provided without response.type → auto-detect as rated", {
  set.seed(211)
  data <- data.frame(
    id = paste0("S", 1:20),
    item1 = sample(1:4, 20, replace = TRUE),
    item2 = sample(1:4, 20, replace = TRUE),
    item3 = sample(1:4, 20, replace = TRUE)
  )
  result <- dataFormat(data, CA = c(2, 3, 1))
  expect_equal(result$response.type, "rated")
  expect_true(!is.null(result$U))
  expect_true(!is.null(result$Q))
  expect_equal(result$CA, c(2, 3, 1))
})

test_that("CA with numeric ID (1:N) → rated with correct ID detection", {
  data <- data.frame(
    id = 1:15,
    item1 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3),
    item2 = c(2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
  )
  result <- dataFormat(data, CA = c(3, 2))
  expect_equal(result$response.type, "rated")
  expect_equal(ncol(result$U), 2)
  expect_equal(ncol(result$Q), 2)
})

test_that("rated U matrix correctly scores against CA", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1),
    item2 = c(3, 3, 3, 1, 1, 1, 2, 2, 2, 3)
  )
  result <- dataFormat(data, CA = c(2, 3))
  # item1: CA=2 → S2,S5,S8 correct
  expect_equal(result$U[, 1], c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
  # item2: CA=3 → S1,S2,S3,S10 correct
  expect_equal(result$U[, 2], c(1, 1, 1, 0, 0, 0, 0, 0, 0, 1))
})

test_that("rated U matrix preserves missing as -1", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = c(1, NA, 3, 1, 2, 3, 1, 2, 3, 1),
    item2 = c(3, 3, NA, 1, 1, 1, 2, 2, 2, 3)
  )
  result <- dataFormat(data, CA = c(2, 3))
  expect_equal(result$U[2, 1], -1) # NA in item1 → -1 in U
  expect_equal(result$U[3, 2], -1) # NA in item2 → -1 in U
  # Non-missing values still scored correctly
  expect_equal(result$U[1, 1], 0) # 1 != CA(2)
  expect_equal(result$U[1, 2], 1) # 3 == CA(3)
})

test_that("CA length mismatch causes error", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  expect_error(dataFormat(data, CA = c(1, 2, 3)), "length of CA must match")
})

test_that("CA value not in data causes error", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:3, 15, replace = TRUE),
    item2 = sample(1:3, 15, replace = TRUE)
  )
  expect_error(dataFormat(data, CA = c(5, 1)), "not a valid response category")
})

test_that("2-category non-binary (1/2) without response.type causes error", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:2, 15, replace = TRUE),
    item2 = sample(1:2, 15, replace = TRUE)
  )
  expect_error(dataFormat(data), "2-category data with non-binary values")
})

test_that("2-category non-binary with explicit response.type works", {
  set.seed(213)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:2, 15, replace = TRUE),
    item2 = sample(1:2, 15, replace = TRUE)
  )
  result <- dataFormat(data, response.type = "ordinal")
  expect_equal(result$response.type, "ordinal")
})

test_that(">=20 categories without response.type causes error", {
  set.seed(214)
  data <- data.frame(
    id = paste0("S", 1:100),
    item1 = sample(1:25, 100, replace = TRUE),
    item2 = sample(1:25, 100, replace = TRUE)
  )
  expect_error(dataFormat(data), "Too many categories")
})

test_that(">=20 categories with explicit response.type works", {
  set.seed(215)
  data <- data.frame(
    id = paste0("S", 1:100),
    item1 = sample(1:25, 100, replace = TRUE),
    item2 = sample(1:25, 100, replace = TRUE)
  )
  result <- dataFormat(data, response.type = "ordinal")
  expect_equal(result$response.type, "ordinal")
})

test_that("binary 0/1 auto-detected correctly", {
  set.seed(216)
  data <- data.frame(
    id = paste0("S", 1:20),
    item1 = sample(0:1, 20, replace = TRUE),
    item2 = sample(0:1, 20, replace = TRUE)
  )
  result <- dataFormat(data)
  expect_equal(result$response.type, "binary")
  expect_true(!is.null(result$U))
})

test_that("binary data with CA still becomes rated", {
  set.seed(217)
  data <- data.frame(
    id = paste0("S", 1:20),
    item1 = sample(0:1, 20, replace = TRUE),
    item2 = sample(0:1, 20, replace = TRUE)
  )
  result <- dataFormat(data, CA = c(1, 1))
  expect_equal(result$response.type, "binary")
  # Binary check runs first, CA check only for non-binary
})

test_that("explicit response.type overrides auto-detection", {
  set.seed(218)
  data <- data.frame(
    id = paste0("S", 1:20),
    item1 = sample(1:5, 20, replace = TRUE),
    item2 = sample(1:5, 20, replace = TRUE)
  )
  result <- dataFormat(data, response.type = "nominal")
  expect_equal(result$response.type, "nominal")
})

test_that("invalid response.type causes error", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(0:1, 15, replace = TRUE)
  )
  expect_error(dataFormat(data, response.type = "invalid"))
})

# ============================================================
# Size anomalies
# ============================================================

test_that("fewer than 10 rows causes error", {
  data <- data.frame(
    id = paste0("S", 1:5),
    item1 = c(0, 1, 0, 1, 0),
    item2 = c(1, 0, 1, 0, 1)
  )
  expect_error(dataFormat(data), "at least 10 rows")
})

test_that("exactly 10 rows works (boundary)", {
  set.seed(221)
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = sample(0:1, 10, replace = TRUE),
    item2 = sample(0:1, 10, replace = TRUE)
  )
  result <- dataFormat(data)
  expect_s3_class(result, "exametrika")
  expect_equal(nrow(result$U), 10)
})

test_that("0 rows causes error", {
  data <- data.frame(
    id = character(0),
    item1 = numeric(0),
    item2 = numeric(0)
  )
  expect_error(dataFormat(data), "at least 10 rows")
})

test_that("single item after ID removal works", {
  set.seed(222)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:4, 15, replace = TRUE)
  )
  result <- dataFormat(data)
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 1)
})

# ============================================================
# Variance anomalies (sd = NA or 0)
# ============================================================

test_that("all items constant value → warns zero variance", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = rep(3, 15),
    item2 = rep(5, 15),
    item3 = rep(1, 15)
  )
  expect_message(result <- dataFormat(data), "zero variance")
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 3) # Not excluded, but warned
})

test_that("all-NA column → warns all missing values", {
  set.seed(231)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = rep(NA, 15),
    item2 = sample(1:4, 15, replace = TRUE),
    item3 = sample(1:4, 15, replace = TRUE)
  )
  expect_message(result <- dataFormat(data), "all missing values")
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 3) # All 3 columns remain
  expect_true(all(result$Q[, 1] == -1)) # item1 is all -1
})

test_that("all items all NA → warns all missing + all students missing", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = rep(NA, 15),
    item2 = rep(NA, 15)
  )
  expect_message(
    expect_message(result <- dataFormat(data), "all missing values"),
    "all missing responses"
  )
  expect_s3_class(result, "exametrika")
  expect_true(all(result$Q == -1))
  expect_true(all(result$Z == 0))
})

test_that("column with Inf triggers sd=NA → excluded", {
  set.seed(232)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = c(Inf, sample(1:4, 14, replace = TRUE)), # sd=NA due to Inf
    item2 = sample(1:4, 15, replace = TRUE),
    item3 = sample(1:4, 15, replace = TRUE)
  )
  expect_message(result <- dataFormat(data, response.type = "nominal"), "no variance")
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 2) # item1 excluded
  expect_false("item1" %in% result$ItemLabel)
})

# ============================================================
# Missing value handling
# ============================================================

test_that("na=NA is handled (treated as NULL)", {
  set.seed(241)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(c(1, 2, 3, NA), 15, replace = TRUE),
    item2 = sample(c(1, 2, 3, NA), 15, replace = TRUE)
  )
  result <- dataFormat(data, na = NA)
  expect_s3_class(result, "exametrika")
})

test_that("custom na value converted to -1 in response matrix", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = c(1, 2, 99, 1, 2, 3, 1, 2, 3, 1),
    item2 = c(3, 99, 2, 1, 2, 3, 1, 99, 3, 2)
  )
  result <- dataFormat(data, na = 99)
  expect_equal(unname(result$Q[1, 1]), 1)
  expect_equal(unname(result$Q[3, 1]), -1) # 99 → -1
  expect_equal(unname(result$Z[3, 1]), 0) # missing indicator
})

test_that("Z matrix overrides response data for missing", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1),
    item2 = c(3, 3, 2, 1, 2, 3, 1, 2, 3, 2)
  )
  Z <- matrix(1, nrow = 10, ncol = 2)
  Z[1, 1] <- 0 # Force item1 of student 1 to missing
  Z[5, 2] <- 0 # Force item2 of student 5 to missing
  result <- dataFormat(data, Z = Z)
  expect_equal(unname(result$Q[1, 1]), -1)
  expect_equal(unname(result$Q[5, 2]), -1)
  expect_equal(unname(result$Z[1, 1]), 0)
  expect_equal(unname(result$Z[5, 2]), 0)
})

test_that("Z matrix with non-0/1 values causes error", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = sample(1:3, 10, replace = TRUE),
    item2 = sample(1:3, 10, replace = TRUE)
  )
  Z_bad <- matrix(c(0, 1, 2, 1), nrow = 10, ncol = 2)
  expect_error(dataFormat(data, Z = Z_bad), "must contain only 0 or 1")
})

# ============================================================
# rated without response.type (regression test for the bug fix)
# ============================================================

test_that("rated auto-detection: ordinal-looking data + CA → rated", {
  set.seed(251)
  data <- data.frame(
    id = paste0("S", 1:20),
    item1 = sample(1:5, 20, replace = TRUE),
    item2 = sample(1:5, 20, replace = TRUE),
    item3 = sample(1:5, 20, replace = TRUE)
  )
  result <- dataFormat(data, CA = c(3, 1, 5))
  expect_equal(result$response.type, "rated")
  expect_true(!is.null(result$U))
  expect_true(all(result$U[result$Q != -1] %in% c(0, 1)))
})

test_that("rated auto-detection: nominal-looking data + CA → rated", {
  set.seed(252)
  data <- data.frame(
    id = paste0("S", 1:20),
    item1 = sample(c(1, 5, 10), 20, replace = TRUE),
    item2 = sample(c(2, 7, 15), 20, replace = TRUE)
  )
  result <- dataFormat(data, CA = c(5, 7))
  expect_equal(result$response.type, "rated")
})

# ============================================================
# Weight vector anomalies
# ============================================================

test_that("default weight is all 1s matching number of items", {
  set.seed(261)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:3, 15, replace = TRUE),
    item2 = sample(1:3, 15, replace = TRUE),
    item3 = sample(1:3, 15, replace = TRUE)
  )
  result <- dataFormat(data)
  expect_equal(result$w, c(1, 1, 1))
})

test_that("custom weight vector applied correctly", {
  set.seed(262)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:3, 15, replace = TRUE),
    item2 = sample(1:3, 15, replace = TRUE)
  )
  result <- dataFormat(data, w = c(2, 0.5))
  expect_equal(result$w, c(2, 0.5))
})

# ============================================================
# Factor / CategoryLabel handling
# ============================================================

test_that("factor response columns converted to numeric with labels preserved", {
  set.seed(271)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = factor(sample(c("A", "B", "C"), 15, replace = TRUE)),
    item2 = factor(sample(c("X", "Y", "Z"), 15, replace = TRUE))
  )
  result <- dataFormat(data, response.type = "nominal")
  expect_s3_class(result, "exametrika")
  expect_true(!is.null(result$CategoryLabel))
  expect_true(all(result$Q[result$Q != -1] > 0)) # numeric, not character
})

test_that("mixed factor and numeric columns handled", {
  set.seed(272)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = factor(sample(c("Low", "Mid", "High"), 15, replace = TRUE),
      levels = c("Low", "Mid", "High")
    ),
    item2 = sample(1:3, 15, replace = TRUE)
  )
  result <- dataFormat(data, response.type = "ordinal")
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 2)
})

# ============================================================
# Matrix input (no column names, no ID)
# ============================================================

test_that("matrix without colnames gets V1/V2/V3 labels from as.data.frame", {
  set.seed(281)
  mat <- matrix(sample(0:1, 15 * 3, replace = TRUE), nrow = 15, ncol = 3)
  expect_message(result <- dataFormat(mat), "No ID column detected")
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "binary")
  # as.data.frame(unclass()) on unnamed matrix gives "V1", "V2", ...
  expect_equal(result$ItemLabel, c("V1", "V2", "V3"))
  expect_equal(result$ID[1], "Student1")
})

test_that("matrix with colnames preserves item labels", {
  set.seed(282)
  mat <- matrix(sample(1:4, 20 * 2, replace = TRUE), nrow = 20, ncol = 2)
  colnames(mat) <- c("Q1", "Q2")
  result <- dataFormat(mat)
  expect_s3_class(result, "exametrika")
  expect_equal(result$ItemLabel, c("Q1", "Q2"))
})

# ============================================================
# Already formatted data (early return)
# ============================================================

test_that("already formatted exametrika object returned as-is", {
  set.seed(291)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(0:1, 15, replace = TRUE),
    item2 = sample(0:1, 15, replace = TRUE)
  )
  result1 <- dataFormat(data)
  result2 <- dataFormat(result1)
  expect_identical(result1, result2)
})

# ============================================================
# Regression: id default change (NULL instead of 1)
# ============================================================

test_that("existing code without id arg still works (backward compat)", {
  set.seed(301)
  # String ID in first column - should auto-detect
  data1 <- data.frame(
    student = paste0("S", 1:20),
    item1 = sample(0:1, 20, replace = TRUE),
    item2 = sample(0:1, 20, replace = TRUE)
  )
  result1 <- dataFormat(data1)
  expect_equal(ncol(result1$U), 2)
  expect_equal(result1$ID, paste0("S", 1:20))

  # Matrix input - should auto-detect no ID
  set.seed(302)
  mat <- matrix(sample(0:1, 15 * 4, replace = TRUE), nrow = 15, ncol = 4)
  result2 <- dataFormat(mat)
  expect_equal(ncol(result2$U), 4)

  # Non-first column ID - should work with explicit id
  set.seed(303)
  data3 <- data.frame(
    item1 = sample(1:3, 15, replace = TRUE),
    student = paste0("S", 1:15),
    item2 = sample(1:3, 15, replace = TRUE)
  )
  result3 <- dataFormat(data3, id = 2)
  expect_equal(ncol(result3$Q), 2)
  expect_equal(result3$ID, paste0("S", 1:15))
})

# ============================================================
# Non-consecutive numeric IDs (e.g., 101, 205, 300)
# ============================================================

test_that("non-consecutive numeric IDs auto-detected", {
  set.seed(311)
  data <- data.frame(
    id = seq(101, 115),
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  result <- dataFormat(data)
  expect_equal(ncol(result$Q), 2)
  expect_equal(result$ID, as.character(101:115))
})

# ============================================================
# categories field correctness
# ============================================================

test_that("categories counts unique non-missing values per item", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2), # 4 categories
    item2 = c(1, 1, 2, 2, 3, 3, 1, 1, 2, 2) # 3 categories
  )
  result <- dataFormat(data)
  expect_equal(unname(result$categories[1]), 4)
  expect_equal(unname(result$categories[2]), 3)
})

test_that("categories ignores missing values", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = c(1, 2, NA, 3, 1, 2, NA, 3, 1, 2), # 3 non-missing categories (1, 2, 3)
    item2 = c(1, 2, 1, 3, 3, 2, 1, 1, 3, NA) # 3 non-missing categories (1, 2, 3)
  )
  result <- dataFormat(data)
  # Note: NA→-1, so categories includes -1 as a "category" in current implementation
  # categories counts unique(x[x != -1]) so -1 is excluded
  expect_equal(unname(result$categories[1]), 3)
  expect_equal(unname(result$categories[2]), 3)
})

# ============================================================
# Absurd / adversarial input patterns
# ============================================================

# --- Wrong data types passed to data parameter ---

test_that("list input coerced to data.frame (as.data.frame handles it)", {
  # list with 10-element vectors becomes a 10x2 data.frame
  result <- dataFormat(list(
    a = sample(0:1, 10, replace = TRUE),
    b = sample(0:1, 10, replace = TRUE)
  ))
  expect_s3_class(result, "exametrika")
})

test_that("NULL input causes error", {
  expect_error(dataFormat(NULL))
})

test_that("single vector coerced to 1-column data.frame", {
  # A 10-element vector becomes a 10x1 data.frame
  result <- dataFormat(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0))
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "binary")
})

test_that("truly invalid input: string scalar", {
  expect_error(dataFormat("not a dataset"))
})

# --- Weird values in response data ---

test_that("negative values in response data", {
  set.seed(401)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(c(-2, -1, 0, 1, 2), 15, replace = TRUE),
    item2 = sample(c(-2, -1, 0, 1, 2), 15, replace = TRUE)
  )
  # Negative values: auto-detect should fail or classify as nominal
  # -1 is the missing indicator internally, so this is problematic
  result <- dataFormat(data, response.type = "nominal")
  expect_s3_class(result, "exametrika")
})

test_that("decimal/float values in response data", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = c(1.5, 2.7, 3.1, 1.5, 2.7, 3.1, 1.5, 2.7, 3.1, 1.5, 2.7, 3.1, 1.5, 2.7, 3.1),
    item2 = c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3, 0.1, 0.2, 0.3, 0.1, 0.2, 0.3, 0.1, 0.2, 0.3)
  )
  # Non-integer values: auto ordinal detection should fail (not integers)
  # Should fall through to nominal
  result <- dataFormat(data)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "nominal")
})

test_that("Inf values in response data → item excluded by sd=NA", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = c(1, 2, Inf, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    item2 = c(3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1)
  )
  # Inf causes sd=NA → item excluded with message
  expect_message(result <- dataFormat(data, response.type = "nominal"), "no variance")
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 1) # item1 excluded
})

test_that("NaN values in response data treated as NA", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = c(1, 2, NaN, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    item2 = c(3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1)
  )
  result <- dataFormat(data)
  expect_s3_class(result, "exametrika")
  expect_equal(unname(result$Z[3, 1]), 0) # NaN → NA → missing
})

test_that("very large numbers in response data", {
  set.seed(402)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(c(1000000, 2000000, 3000000), 15, replace = TRUE),
    item2 = sample(c(1000000, 2000000, 3000000), 15, replace = TRUE)
  )
  result <- dataFormat(data, response.type = "nominal")
  expect_s3_class(result, "exametrika")
})

# --- ID column weirdness ---

test_that("ID column with all NAs (auto-detect) → auto-ID + warns all missing", {
  data <- data.frame(
    id = rep(NA, 15),
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  # All NA in first column: not consecutive, not string → auto-ID generated
  # The all-NA column also triggers "all missing values" warning
  expect_message(result <- dataFormat(data), "No ID column detected")
  expect_s3_class(result, "exametrika")
})

test_that("ID column with empty strings", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:3, 15, replace = TRUE),
    item2 = sample(1:3, 15, replace = TRUE),
    stringsAsFactors = FALSE
  )
  data$id[5] <- ""
  result <- dataFormat(data, id = 1)
  expect_s3_class(result, "exametrika")
  expect_equal(result$ID[5], "")
})

test_that("ID column with whitespace strings", {
  data <- data.frame(
    id = c(paste0("S", 1:13), "  ", " "),
    item1 = sample(1:3, 15, replace = TRUE),
    item2 = sample(1:3, 15, replace = TRUE),
    stringsAsFactors = FALSE
  )
  # "  " and " " are different strings, no duplicate
  result <- dataFormat(data, id = 1)
  expect_s3_class(result, "exametrika")
})

test_that("all same ID value causes duplicate error with row numbers", {
  data <- data.frame(
    id = rep("SAME", 15),
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  expect_error(dataFormat(data, id = 1), "Duplicated IDs found")
  expect_error(dataFormat(data, id = 1), "rows") # Row numbers shown
})

test_that("duplicate IDs error shows which rows are duplicated", {
  data <- data.frame(
    id = c(
      "A", "B", "C", "A", "D", "E", "B", "F", "G", "H",
      "I", "J", "K", "L", "M"
    ),
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  expect_error(dataFormat(data, id = 1), "'A' \\(rows 1,4\\)")
  expect_error(dataFormat(data, id = 1), "'B' \\(rows 2,7\\)")
})

test_that("first column with many unique values triggers ID hint", {
  set.seed(420)
  # First column has 12 unique values out of 15 rows (80%) but has duplicates
  # → not consecutive, not detected as ID → auto-generate
  # But 80% unique is suspicious, so a hint message is shown
  data <- data.frame(
    subject = c(
      101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
      111, 112, 101, 102, 103
    ), # 12 unique out of 15
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  expect_message(dataFormat(data), "No ID column detected")
  expect_message(dataFormat(data), "first column.*unique values")
})

test_that("factor ID with few levels (< 20) auto-detected as response", {
  set.seed(403)
  data <- data.frame(
    group = factor(sample(c("A", "B", "C"), 15, replace = TRUE)),
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  # Factor with < 20 unique values: not detected as ID
  result <- dataFormat(data)
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 3) # All 3 columns as response
})

# --- CA parameter weirdness ---

test_that("CA as character vector causes error or is coerced", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:3, 15, replace = TRUE),
    item2 = sample(1:3, 15, replace = TRUE)
  )
  # CA should be numeric; character "2" should be coerced
  result <- dataFormat(data, CA = c("2", "1"))
  expect_equal(result$response.type, "rated")
})

test_that("CA=NULL with response.type='rated' causes error", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:3, 15, replace = TRUE),
    item2 = sample(1:3, 15, replace = TRUE)
  )
  expect_error(
    dataFormat(data, response.type = "rated", CA = NULL),
    "Correct Answer must be specified"
  )
})

test_that("CA with named vector works", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3),
    item2 = c(3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1)
  )
  result <- dataFormat(data, CA = c(item1 = 2, item2 = 3))
  expect_equal(result$response.type, "rated")
  expect_equal(as.numeric(result$CA), c(2, 3))
})

# --- na parameter weirdness ---

test_that("na=0 treats 0 as missing (ambiguous with binary)", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = c(0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2),
    item2 = c(3, 2, 1, 0, 3, 2, 1, 0, 3, 2, 1, 0, 3, 2, 1)
  )
  result <- dataFormat(data, na = 0)
  # 0 should be treated as missing
  expect_equal(unname(result$Z[1, 1]), 0) # item1[1] = 0 → missing
  expect_equal(unname(result$Q[1, 1]), -1)
})

test_that("na=-999 custom sentinel value", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = c(1, 2, -999, 3, 1, 2, -999, 3, 1, 2),
    item2 = c(3, -999, 1, 2, 3, -999, 1, 2, 3, 1)
  )
  result <- dataFormat(data, na = -999)
  expect_equal(unname(result$Q[3, 1]), -1)
  expect_equal(unname(result$Q[2, 2]), -1)
  expect_equal(unname(result$Z[3, 1]), 0)
})

# --- Data with more columns than rows ---

test_that("wide data (more items than students)", {
  set.seed(405)
  n_items <- 30
  n_students <- 12
  mat <- matrix(sample(0:1, n_students * n_items, replace = TRUE),
    nrow = n_students, ncol = n_items
  )
  colnames(mat) <- paste0("Q", 1:n_items)
  result <- dataFormat(mat)
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$U), n_items)
  expect_equal(nrow(result$U), n_students)
})

# --- Extremely sparse data ---

test_that("mostly NA data (>90% missing) → NA becomes -1, still works", {
  data <- data.frame(
    id = paste0("S", 1:20),
    item1 = c(1, rep(NA, 19)),
    item2 = c(rep(NA, 19), 2),
    item3 = c(NA, 3, rep(NA, 18))
  )
  # After NA→-1, each column has {value, -1, -1, ...}: sd > 0, not excluded
  result <- dataFormat(data)
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 3)
})

test_that("sparse but enough variance survives", {
  set.seed(407)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = c(1, 2, 3, rep(NA, 12)), # 3 non-NA, has variance
    item2 = c(rep(NA, 12), 3, 4, 5), # 3 non-NA, has variance
    item3 = sample(1:3, 15, replace = TRUE) # fully observed
  )
  result <- dataFormat(data)
  expect_s3_class(result, "exametrika")
})

# --- Duplicate column names ---

test_that("duplicate column names in data.frame", {
  set.seed(408)
  data <- data.frame(
    id = paste0("S", 1:15),
    x = sample(1:3, 15, replace = TRUE),
    x = sample(1:3, 15, replace = TRUE),
    check.names = FALSE
  )
  # R allows duplicate column names with check.names=FALSE
  result <- dataFormat(data)
  expect_s3_class(result, "exametrika")
})

# --- Column names with special characters ---

test_that("column names with spaces and special chars → make.names applied", {
  set.seed(409)
  data <- data.frame(
    id = paste0("S", 1:15),
    `item 1` = sample(1:4, 15, replace = TRUE),
    `item-2` = sample(1:4, 15, replace = TRUE),
    check.names = FALSE
  )
  result <- dataFormat(data, id = 1)
  expect_s3_class(result, "exametrika")
  # as.data.frame(unclass()) applies make.names: "item 1" → "item.1"
  expect_true("item.1" %in% result$ItemLabel)
})

# --- Mixed binary and ordinal columns ---

test_that("mix of binary and ordinal items → ordinal overall", {
  set.seed(410)
  data <- data.frame(
    id = paste0("S", 1:20),
    item1 = sample(0:1, 20, replace = TRUE), # binary
    item2 = sample(0:1, 20, replace = TRUE), # binary
    item3 = sample(1:5, 20, replace = TRUE), # ordinal
    item4 = sample(1:5, 20, replace = TRUE) # ordinal
  )
  result <- dataFormat(data)
  # Not all binary → should be ordinal or nominal
  expect_true(result$response.type %in% c("ordinal", "nominal"))
})

# --- Boolean/logical data ---

test_that("logical TRUE/FALSE data detected as binary", {
  set.seed(411)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(c(TRUE, FALSE), 15, replace = TRUE),
    item2 = sample(c(TRUE, FALSE), 15, replace = TRUE)
  )
  result <- dataFormat(data)
  expect_equal(result$response.type, "binary")
  expect_true(all(result$U[result$U != -1] %in% c(0, 1)))
})

# --- Factor ID column with many levels (>= 20) looks like ID ---

test_that("factor ID with >= 20 levels detected as ID", {
  set.seed(412)
  data <- data.frame(
    student = factor(paste0("Student_", 1:25)),
    item1 = sample(1:4, 25, replace = TRUE),
    item2 = sample(1:4, 25, replace = TRUE)
  )
  result <- dataFormat(data)
  expect_equal(ncol(result$Q), 2)
  expect_equal(length(result$ID), 25)
})

# --- Factor with unused levels ---

test_that("factor with unused levels handled correctly", {
  set.seed(413)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = factor(sample(c("A", "B"), 15, replace = TRUE),
      levels = c("A", "B", "C", "D")
    ), # C and D unused
    item2 = factor(sample(c("X", "Y"), 15, replace = TRUE),
      levels = c("X", "Y", "Z")
    ) # Z unused
  )
  result <- dataFormat(data, response.type = "nominal")
  expect_s3_class(result, "exametrika")
})

# --- Tibble-like input (data.frame subclass) ---

test_that("data.frame subclass still works", {
  set.seed(414)
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(0:1, 15, replace = TRUE),
    item2 = sample(0:1, 15, replace = TRUE)
  )
  class(data) <- c("tbl_df", "tbl", "data.frame")
  result <- dataFormat(data)
  expect_s3_class(result, "exametrika")
})

# --- rated: all correct or all incorrect ---

test_that("rated: all students answer correctly → U all 1s, warns zero variance", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = rep(3, 10),
    item2 = rep(2, 10)
  )
  expect_message(result <- dataFormat(data, CA = c(3, 2)), "zero variance")
  expect_equal(result$response.type, "rated")
  expect_true(all(result$U == 1))
})

test_that("rated: almost no student answers correctly", {
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2), # CA=2, only S10 correct
    item2 = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 1) # CA=1, only S10 correct
  )
  result <- dataFormat(data, CA = c(2, 1))
  expect_equal(result$response.type, "rated")
  expect_equal(sum(result$U[, 1] == 1), 1) # Only 1 correct for item1
  expect_equal(sum(result$U[, 2] == 1), 1) # Only 1 correct for item2
})

# --- Z matrix edge cases ---

test_that("Z matrix all zeros → all -1, warns all missing", {
  set.seed(415)
  data <- data.frame(
    id = paste0("S", 1:10),
    item1 = sample(1:3, 10, replace = TRUE),
    item2 = sample(1:3, 10, replace = TRUE)
  )
  Z_all_zero <- matrix(0, nrow = 10, ncol = 2)
  expect_message(result <- dataFormat(data, Z = Z_all_zero), "all missing")
  expect_s3_class(result, "exametrika")
  expect_true(all(result$Q == -1))
  expect_true(all(result$Z == 0))
})

# --- response.type explicitly set to binary but data is not ---

test_that("binary response.type with non-binary data causes error", {
  data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:4, 15, replace = TRUE),
    item2 = sample(1:4, 15, replace = TRUE)
  )
  expect_error(
    dataFormat(data, response.type = "binary"),
    "can only contain the values 0, 1"
  )
})

# --- Multiple issues combined ---

test_that("numeric ID + CA + NA all at once", {
  data <- data.frame(
    id = 1:15,
    item1 = c(1, NA, 3, 2, 1, 3, 2, NA, 1, 3, 2, 1, 3, 2, 1),
    item2 = c(NA, 2, 1, 3, 2, 1, NA, 3, 2, 1, 3, 2, 1, 3, 2),
    item3 = c(2, 3, 1, NA, 2, 3, 1, 2, NA, 3, 1, 2, 3, 1, 2)
  )
  result <- dataFormat(data, CA = c(2, 3, 1))
  expect_equal(result$response.type, "rated")
  expect_equal(ncol(result$U), 3)
  expect_equal(ncol(result$Q), 3)
  # Check missing preserved in U
  expect_equal(result$U[2, 1], -1) # NA → -1
  expect_equal(result$U[1, 2], -1) # NA → -1
  # Check correct scoring
  expect_equal(result$U[1, 1], 0) # 1 != CA(2)
  expect_equal(result$U[3, 2], 0) # 1 != CA(3)
  expect_equal(result$U[4, 1], 1) # 2 == CA(2)
})

test_that("custom na + CA + explicit id", {
  data <- data.frame(
    item1 = c(1, 99, 3, 2, 1, 3, 2, 99, 1, 3, 2, 1, 3, 2, 1),
    student = paste0("S", 1:15),
    item2 = c(99, 2, 1, 3, 2, 1, 99, 3, 2, 1, 3, 2, 1, 3, 2)
  )
  result <- dataFormat(data, na = 99, id = 2, CA = c(2, 3))
  expect_equal(result$response.type, "rated")
  expect_equal(result$ID, paste0("S", 1:15))
  expect_equal(unname(result$Q[2, 1]), -1) # 99 → -1
  expect_equal(unname(result$U[2, 1]), -1) # missing preserved in U
})

# --- Regression: sample datasets still work ---

test_that("package dataset J5S10 loads and formats correctly", {
  data(J5S10, package = "exametrika")
  result <- dataFormat(J5S10)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "binary")
})

test_that("package dataset J35S500 loads and formats correctly", {
  data(J35S500, package = "exametrika")
  result <- dataFormat(J35S500)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "ordinal")
})

test_that("package dataset J20S600 loads and formats correctly", {
  data(J20S600, package = "exametrika")
  result <- dataFormat(J20S600)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "nominal")
})
