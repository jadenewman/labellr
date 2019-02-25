## numeric_classify
# check data
expect_true(numeric_classify(var_name = "var", data = data.frame(var = c(1)), ">=0"))
expect_false(numeric_classify(var_name = "var", data = data.frame(var = c(-1)), ">=0"))
expect_error(numeric_classify(var_name = "var", data = data.frame(var = c(NA)), ">=0"))
expect_error(numeric_classify(var_name = "var", data = data.frame(var = c("1")), ">=0"))
# check rules
expect_error(numeric_classify(var_name = "var", data = data.frame(var = c(-1)), "=0"))

# character_classify
# character_classify(var_name = "var",data = data.frame(var = c("hi","bye"),stringsAsFactors = F),
#                                  rule = "hi")
# character_classify(var_name = "var", data = data.frame(var = c(NA),stringsAsFactors = F), "bye")
# 
# 
# # date_classify
# date_classify(var_name = "var", data = data.frame(var = c("hi","hi")), "1")



# classify