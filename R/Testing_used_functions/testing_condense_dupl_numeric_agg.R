#____________________________________________________
#### testing condense_dupl_numeric_agg ####
#____________________________________________________

test_that("testing output of condense_dupl_numeric_agg",
          {
            expect_equal(condense_dupl_numeric_agg(NA), NA)
            expect_equal(condense_dupl_numeric_agg(0), 0)
            expect_equal(condense_dupl_numeric_agg(1), 1)
            expect_equal(condense_dupl_numeric_agg(c(0, 1)), 1)
            expect_equal(condense_dupl_numeric_agg(c(1, 1)), 1)
            expect_equal(condense_dupl_numeric_agg(c(2, 1)), 1.5)
            expect_equal(condense_dupl_numeric_agg(c(1, 1, 0)), 1)
            expect_equal(condense_dupl_numeric_agg(c(1, 0, 1)), 1)
            expect_equal(condense_dupl_numeric_agg(c(0, 1, 1)), 1)
            expect_equal(condense_dupl_numeric_agg(c(0, 0, 0)), 0)
            expect_equal(condense_dupl_numeric_agg(c(1, 0, 0)), 1)
            expect_equal(condense_dupl_numeric_agg(c(0, 1, 0)), 1)
            expect_equal(condense_dupl_numeric_agg(c(0, 0, 1)), 1)
            expect_equal(condense_dupl_numeric_agg(c(3, 2, 1)), 2)
            expect_equal(condense_dupl_numeric_agg(c(3, 2, 0)), 2.5)
          })