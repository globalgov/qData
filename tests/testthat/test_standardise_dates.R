test_that("standardise_dates() treats typical dates correctly",{
  expect_equal(standardise_dates("2010-01-01"), c("2010-01-01"))
  # expect_equal(standardise_dates("0000-00-00"), NA) #tofix
})

test_that("standardise_dates() takes three variables as input",{
  expect_equal(standardise_dates("2010", "01", "01"), lubridate::as_date("2010-01-01"))
  expect_equal(standardise_dates("2010", "1", "1"), lubridate::as_date("2010-01-01"))
  expect_error(standardise_dates("2010", "1"), "you need to pass standardise_dates")
})

# test_that("standardise_dates() treats reverse ordered dates correctly", {
#   expect_match(as.character(standardise_dates("30.1.1874")), "1874-01-30")
#   expect_match(as.character(standardise_dates("31.12.1879")), "1879-12-31")
#   expect_match(as.character(standardise_dates("6/10/2012")), "2012-10-06")
#   expect_match(as.character(standardise_dates("9.4.1939")), "1939-04-09")
#   expect_match(as.character(standardise_dates("12/5/1993")), "1993-05-12")
#   expect_match(as.character(standardise_dates("10.30.93")), "1993-10-30")
#   expect_match(as.character(standardise_dates("20/06/04")), "2004-06-20")
#   expect_match(as.character(standardise_dates("may 20, 2010")), "2010-05-20")
# })
# 
# # test_that("standardise_dates() treats incomplete dates correctly",{
# #   expect_match(as.character(min(standardise_dates("2010"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010"))), "2010-12-31")
# #   expect_match(as.character(min(standardise_dates("2010-01"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010-01"))), "2010-01-31")
# # })
# 
# # test_that("standardise_dates() treats missing month components correctly",{
# #   expect_match(as.character(max(standardise_dates("2010-00-00"))), "2010-12-31")
# #   expect_match(as.character(min(standardise_dates("2010-??-??"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010-??-??"))), "2010-12-31")
# #   expect_match(as.character(min(standardise_dates("2010-NA-NA"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010-NA-NA"))), "2010-12-31")
# #   expect_match(as.character(min(standardise_dates("2010-00-00"))), "2010-01-01")
# # })
# # 
# # test_that("standardise_dates() treats missing day components correctly",{
# #   expect_match(as.character(max(standardise_dates("2010-01-00"))), "2010-01-31")
# #   expect_match(as.character(min(standardise_dates("2010-01-??"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010-01-??"))), "2010-01-31")
# #   expect_match(as.character(min(standardise_dates("2010-01-NA"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010-01-NA"))), "2010-01-31")
# #   expect_match(as.character(min(standardise_dates("2010-01-00"))), "2010-01-01") 
# # })
# # 
# # test_that("standardise_dates() treats ranged dates correctly",{
# #   expect_match(as.character(min(standardise_dates("2010:2011"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010:2011"))), "2011-12-31")
# #   expect_match(as.character(min(standardise_dates("2010-01:03"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010-01:03"))), "2010-03-31")
# #   expect_match(as.character(min(standardise_dates("2010-01-01:03"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010-01-01:03"))), "2010-01-03")
# #   expect_match(as.character(min(standardise_dates("2010_2011"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010_2011"))), "2011-12-31")
# #   expect_match(as.character(min(standardise_dates("2010-01_03"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010-01_03"))), "2010-03-31")
# #   expect_match(as.character(min(standardise_dates("2010-01-01_03"))), "2010-01-01")
# #   expect_match(as.character(max(standardise_dates("2010-01-01_03"))), "2010-01-03")
# #   expect_match(as.character(min(standardise_dates("2010-00-00"))), "2010-01-01")
# # })
# 
# test_that("standardise_dates() treats future dates correctly",{
#   expect_warning(recent(), "is deprecated")
#   expect_match(as.character(standardise_dates("9999-12-31")), "9999-12-31")
#   expect_match(as.character(standardise_dates("2599-12-31")), "9999-12-31")
# })
# 
# test_that("standardise_dates() treats historical dates correctly",{
#   skip_on_os("linux")
#   expect_match(as.character(standardise_dates("1712-01-01")), "1712-01-01")
#   expect_match(as.character(standardise_dates("712-01-01")[[1]]), "0712-01-01")
#   expect_match(as.character(standardise_dates("0712-01-01")[[1]]), "0712-01-01")
#   #expect_equal(as.character(min(standardise_dates("712")[[1]])), "0712-01-01")
#   expect_match(as.character(standardise_dates("712-01-01")[[1]]), "0712-01-01")
#   # expect_match(as.character(min(standardise_dates("712 AD")[[1]])), "0712-01-01") 
#   # expect_match(as.character(min(standardise_dates("1712 AD")[[1]])), "1712-01-01") 
#   # expect_match(as.character(min(standardise_dates("12 AD")[[1]])), "0012-01-01") 
#   #expect_match(as.character(min(standardise_dates("12")[[1]])), "0012-01-01") 
#   # expect_match(as.character(min(standardise_dates("1712 AD")[[1]])), "1712-01-01") 
#   # expect_match(as.character(standardise_dates("712 BC")), "-712-01-01")
#   # expect_match(as.character(standardise_dates("1712 BC")), "-1712-01-01")
#   # expect_match(as.character(standardise_dates("44 BC")), "-044-01-01")
#   # expect_match(as.character(standardise_dates("-712")), "-712-01-01")
#   # expect_match(as.character(standardise_dates("-1712")), "-1712-01-01")
#   # expect_match(as.character(standardise_dates("-1712-10-10")), "-1712-10-10") #tofix
#   # expect_match(as.character(standardise_dates("-12-10-1712")), "-1712-12-10") #tofix
# })
# 
# test_that("standardise_dates() treats multiple inconsistent dates correctly",{
#   expect_equal(as.character(standardise_dates(c("2010-10-12", "2010-11-13", "2010-12-14"))), c("2010-10-12", "2010-11-13", "2010-12-14"))
#   dat3 <- data.frame(date = c("2010-10-12", "2010-11-13", "2010-12-14"))
#   expect_equal(as.character(standardise_dates(dat3$date)), c("2010-10-12", "2010-11-13", "2010-12-14"))
#   dat2 <- data.frame(date = c("2010.10.12", "2010/11/13", "2010-12-14"))
#   expect_equal(as.character(standardise_dates(dat2$date)), c("2010-10-12", "2010-11-13", "2010-12-14"))
#   dat <- data.frame(date = as.character(c("2010.10.12","2010/03/30", "10/12/93", "12-10-1993", "NA")))
#   expect_equal(as.character(standardise_dates(dat$date)), c("2010-10-12", "2010-03-30", "1993-12-10", "1993-10-12", NA)) #tofix
#   dat1 <- data.frame(date = c("12.10.2010", " ", "20/10/1993", "12-10-1993"))
#   expect_equal(as.character(standardise_dates(dat1$date)), c("2010-10-12", NA, "1993-10-20", "1993-10-12")) #tofix
# })
# 
# # Example of errors from datasets in qEnviron
# test_that("standardise_dates() treats vector of dates correctly", {
#   dates4 <- data.frame(date = as.Date(c("1351-08-01", "1353-10-20", "1403-06-27", "1407-03-10",
#                                         "1656-07-17", "NA", "1867-04-29")))
#   expect_equal(as.character(standardise_dates(dates4$date)), c("1351-08-01", "1353-10-20", "1403-06-27", "1407-03-10",
#                                                                "1656-07-17", NA, "1867-04-29"))
# })
# 
# test_that("standardise_dates() treats special dates format correctly", {
#   dates5 <- data.frame(date = c("Sep 12, 2009", "Oct 1, 2019", "Nov 13, 1998", "NA", "May 13, 2003"))
#   expect_equal(as.character(standardise_dates(dates5$date)), c("2009-09-12", "2019-10-01", "1998-11-13", NA, "2003-05-13"))
# })
# 
# # test_that("standardise_dates() treats inconsistent date format correctly", {
# #  dates6 <- data.frame(date = c("30/4/1990", "NA", "2010-12-30", "Obsolete?", "2010-00-00", "2599-01-01"))
# #  expect_equal(as.character(standardise_dates(dates6$date)), c("1990-04-30", NA, "2010-12-30", NA, "2010-01-01", "9999-12-31"))
# # }) #tofix
