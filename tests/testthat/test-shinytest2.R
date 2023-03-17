library(shinytest2)

test_that("{shinytest2} recording: cad_overview", {
  app <- AppDriver$new(variant = platform_variant(), name = "cad_overview", height = 841, 
      width = 1169)
  app$set_inputs(team = "Canada")
  app$expect_screenshot()
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


test_that("{shinytest2} recording: usa_medal_tally", {
  app <- AppDriver$new(name = "usa_medal_tally", height = 841, width = 1169)
  app$set_inputs(tabs = "Medal Tally Breakdown")
  app$set_inputs(team_p2 = "United States of America")
  app$set_inputs(medal = c("Gold", "Silver"))
  app$set_inputs(medal = "Gold")
  app$expect_values()
})


test_that("{shinytest2} recording: gym_china", {
  app <- AppDriver$new(name = "gym_china", height = 841, width = 1169)
  app$set_inputs(sport = "Gymnastics")
  app$set_inputs(team = "China")
  app$expect_values()
})
