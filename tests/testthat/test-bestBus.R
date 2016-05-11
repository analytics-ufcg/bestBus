library(bestBus)

test_that('best_travel_deprecated test',{
  expect_equivalent(bestBus::best_travel_deprecated(15, 20, 25), -1)
  expect_equivalent(bestBus::best_travel_deprecated(27, 20, 25), 0)
  expect_equivalent(bestBus::best_travel_deprecated(25, 20, 17), 1)
  expect_equivalent(bestBus::best_travel_deprecated(17, 20, 17), -1)
  expect_equivalent(bestBus::best_travel_deprecated(20, 20, 17), 1)
  expect_equivalent(bestBus::best_travel_deprecated(20, 20, 27), 0)
  expect_equivalent(bestBus::best_travel_deprecated(25, 20, 20), 0)
  expect_equivalent(bestBus::best_travel_deprecated(15, 20, 20), -1)
  expect_equivalent(bestBus::best_travel_deprecated(NA, 21, 30), 0)
  expect_equivalent(bestBus::best_travel_deprecated(20, 21, NA), -1)
})

test_that('best_length_diff_deprecated test', {
  expect_equivalent(bestBus::best_length_diff_deprecated(35, 50, 40), 15)
  expect_equivalent(bestBus::best_length_diff_deprecated(35, 30, 40), 0)
  expect_equivalent(bestBus::best_length_diff_deprecated(45, 50, 40), 10)
  expect_equivalent(bestBus::best_length_diff_deprecated(NA, 50, 40), 10)
  expect_equivalent(bestBus::best_length_diff_deprecated(35, 50, NA), 15)
})

test_that('time_difference test', {
  expect_equivalent(bestBus::time_difference("02:30:30", "02:45:30"), 15)
  expect_equivalent(bestBus::time_difference("02:45:30", "02:30:30"), 15)
  expect_equivalent(bestBus::time_difference("02:30:30", "02:45:45"), 15.25)
  expect_equivalent(bestBus::time_difference("02:45:45", "02:30:30"), 15.25)
  expect_equivalent(bestBus::time_difference("11:00:00", "13:00:00"), 120)
  expect_equivalent(bestBus::time_difference("13:00:00", "11:00:00"), 120)
  expect_equivalent(bestBus::time_difference("01:00:00", "23:00:00"), 1320)
})

test_that('best_travel test', {
  expect_equivalent(bestBus::best_travel(20, 30, 40, 15, 20, 5, 30), -1)
  expect_equivalent(bestBus::best_travel(31, 30, 40, 15, 20, 5, 30), 0)
  expect_equivalent(bestBus::best_travel(31, 30, 20, 15, 20, 5, 30), 1)
  expect_equivalent(bestBus::best_travel(25, 30, 40, 15, 20, 10, 30), 0)
  expect_equivalent(bestBus::best_travel(40, 30, 25, 15, 20, 10, 30), 0)
  expect_equivalent(bestBus::best_travel(25, 30, 19, 15, 25, 10, 20), -1)
  expect_equivalent(bestBus::best_travel(19, 30, 25, 25, 15, 10, 20), 1)
  expect_equivalent(bestBus::best_travel(25, 30, 19, 25, 25, 10, 20), 0)
  expect_equivalent(bestBus::best_travel(19, 30, 25, 25, 25, 10, 20), 0)

  expect_equivalent(bestBus::best_travel(30, 30, 45, 15, 15, 0, 20), 0)
  expect_equivalent(bestBus::best_travel(45, 30, 30, 15, 15, 0, 20), 0)
  expect_equivalent(bestBus::best_travel(30, 30, 45, 25, 15, 0, 20), 0)
  expect_equivalent(bestBus::best_travel(45, 30, 30, 15, 25, 0, 20), 0)

  expect_equivalent(bestBus::best_travel(NA, 30, 15, NA, 15, 10, 20), 1)
  expect_equivalent(bestBus::best_travel(NA, 30, 15, NA, 15, 20, 20), 0)
  expect_equivalent(bestBus::best_travel(NA, 30, 15, NA, 25, 10, 20), 0)

  expect_equivalent(bestBus::best_travel(15, 30, NA, 15, NA, 10, 20), -1)
  expect_equivalent(bestBus::best_travel(15, 30, NA, 15, NA, 16, 20), 0)
  expect_equivalent(bestBus::best_travel(15, 30, NA, 25, NA, 10, 20), 0)
})

test_that('best_length_diff test', {
  expect_equivalent(bestBus::best_length_diff(25, 30, 40, -1), 5)
  expect_equivalent(bestBus::best_length_diff(40, 30, 25, 1), 5)
  expect_equivalent(bestBus::best_length_diff(35, 30, 40, 0), 0)
  expect_equivalent(bestBus::best_length_diff(25, 30, NA, -1), 5)
  expect_equivalent(bestBus::best_length_diff(NA, 30, 26, 1), 4)

})

test_that('group_minutes test', {
  expect_equivalent(bestBus::group_minutes("20:14:20", 20), "20:00:00")
  expect_equivalent(bestBus::group_minutes("20:20:00", 20), "20:20:00")
  expect_equivalent(bestBus::group_minutes("20:20:01", 20), "20:20:00")
  expect_equivalent(bestBus::group_minutes("13:19:59", 20), "13:00:00")
  expect_equivalent(bestBus::group_minutes("13:30:00", 70), "12:50:00")
})
