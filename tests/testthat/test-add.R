test_that("addAssignments works", {
  added_assignments <- addAssignments(a,assignment)
  
  expect_s4_class(added_assignments,'Analysis')
})
