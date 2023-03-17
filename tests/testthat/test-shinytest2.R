library(shinytest2)

test_that("{shinytest2} recording: cad_overview", {
  app <- AppDriver$new(variant = platform_variant(), name = "cad_overview", height = 841, 
      width = 1169)
  app$set_inputs(team = "Canada")
  app$expect_values()
})


test_that("{shinytest2} recording: mex_box", {
  app <- AppDriver$new(variant = platform_variant(), name = "mex_box", height = 841, 
      width = 1169)
  app$set_inputs(team = "Mexico")
  app$set_inputs(sport = "Boxing")
  app$expect_values()
})


test_that("{shinytest2} recording: swiss_winter", {
  app <- AppDriver$new(variant = platform_variant(), name = "swiss_winter", height = 841, 
      width = 1169)
  app$set_inputs(team = "Switzerland")
  app$set_inputs(season = "Winter")
  app$expect_values()
})