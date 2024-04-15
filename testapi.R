# Install mice from Github
# The version of the mice on the local machine should be the same as the one in the API
devtools::install_github("amices/mice@support_blocks") #version used in API

# Packages
library(jsonlite)
library(httr2)
library(mice)
library(dplyr)

##### Example Data ##### 

# Fetch from API
response_ex <- request("http://localhost:8080") |> 
  req_headers(accept = "text/json") |> 
  req_url_path("/exampledata") |> 
  req_url_query(name = "nhanes")  |>
  req_perform() |>
  resp_body_string() |>
  fromJSON()

# Validate the response
example <- response_ex$result

# Validate the element
all.equal(example,mice::nhanes) #attributes are different for row.names
identical(example,mice::nhanes) #data types are different for columns with NA

# Re-write the element
row.names(example) <- as.character(row.names(example))
example$age <- as.numeric(example$age)
example$hyp <- as.numeric(example$hyp)
example$chl <- as.numeric(example$chl)
all.equal(example,mice::nhanes) 
identical(example,mice::nhanes) 


##### Long ##### 

# Fetch from the API
response_long <- request("http://localhost:8080") |> 
  req_headers("accept" = "text/json") |> 
  req_url_path("/long") |> 
  req_url_query(payload = '{"data":"nhanes","maxit":2,"m":2,"seed":1}') |>
  req_perform() |>
  resp_body_string() |> 
  fromJSON()

# Validate the response
long <- response_long$result

# Validate the element
test_long <- mice::mice(data = nhanes, maxit = 2, m=2, seed=1)
complete_long <- complete(test_long,"long",include = TRUE)
getlong <- long %>% relocate(.imp,.id,.after=chl)
all.equal(getlong, complete_long) #there are differences in data type

# Check the elements of imputed values
all.equal(getlong$age, complete_long$age)
all.equal(getlong$bmi, complete_long$bmi)
all.equal(getlong$hyp, complete_long$hyp)
all.equal(getlong$chl, complete_long$chl)

##### Fit ##### 

# Fetch from the API
response_fit <- request("http://localhost:8080") |> 
  req_headers("accept" = "text/json") |> 
  req_url_path("/fit") |> 
  req_url_query(payload = paste0('{"data":', (toJSON(long)),', "model":["lm"], "formula":["chl ~ age + bmi"]}')) |>
  req_perform() |>
  resp_body_string() |> 
  fromJSON()

# Validate the response
fit <- response_fit$result

# Validate the element
fitted <- with(test_long,lm(chl ~ age + bmi))
test_fit <- getfit(fitted)
testfit <- as.data.frame(summary(test_fit))
all.equal(round(fit[-1],4),round(testfit[-1],4)) #there are mean relative differences for estimate, std.error, statistic, and p.value


##### Pool ##### 

# Fetch from the API
response_pool <- request("http://localhost:8080") |> 
  req_headers("accept" = "text/json") |> 
  req_url_path("/pool") |> 
  req_url_query(payload = paste0('{"data":', (toJSON(fit)),'}')) |>
  req_perform() |>
  resp_body_string() |> 
  fromJSON()

# Validate the response
pool <- response_pool$result

# Validate the element
test_pool <- pool(fitted)
getpool <- pool |> 
  subset(select = -c(std.error, statistic, p.value, conf.low, conf.high)) |>
  relocate(ubar,b,t,dfcom, .before = df)
all.equal(round(getpool[-1],2),round(test_pool$pooled[-1],2)) #there is difference in rounding
