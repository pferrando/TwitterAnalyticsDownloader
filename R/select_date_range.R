select_date_range <- function(start_date, end_date){
  ### Select date range in calendar
  ## Open calendar
  open_calendar()
  ## Select start date in calendar
  select_start_date(start_date)
  ## Select end date in calendar
  select_end_date(end_date)
  ## Confirm date range by clicking the Update button
  click_update_date_button()
}

# ------------------------------------------------------------------
open_calendar <- function(){
  ### Open the calendar
  webElem <- remDr$findElement("xpath","//div[@class='pull-right']")
  webElem$clickElement()
  Sys.sleep(1)
}

# ------------------------------------------------------------------
click_update_date_button <- function(){
  ### Click on update button to confirm date range
  webElem <- remDr$findElement("xpath","//button[@class='applyBtn btn btn-sm btn-primary']")
  webElem$clickElement()
  Sys.sleep(1)
}

# ------------------------------------------------------------------
# ------------------------------------------------------------------
select_start_date <- function(date){
  ### Select specific date in left calendar
  ## Compute number of clicks required until the right month
  # The number of clicks are the difference of months between the target date and the current date displayed in the calendar
  target_year <- format(as.Date(date),"%Y")
  target_month <- format(as.Date(date),"%m")
  current_date <- as.Date(unlist(remDr$findElement("xpath","//div[@class='calendar left']//input[@name='daterangepicker_start']")$getElementAttribute("value")), "%m/%d/%Y")
  current_year <- format(as.Date(current_date),"%Y")
  current_month <- format(as.Date(current_date),"%m")
  delta_months <- (as.numeric(target_year)-as.numeric(current_year))*12 + as.numeric(target_month)-as.numeric(current_month)
  ## Decide if we must click previous or next depending on the difference in months
  if(delta_months < 0){
    ## Click previous month until the right month
    for(i in 1:abs(delta_months)){
      click_previous_left_calendar()
    }
  }else if(delta_months > 0){
    ## Click next month until the right month
    for(i in 1:abs(delta_months)){
      click_next_left_calendar()
    }
  }
  ## Select start date
  click_date_left_calendar(date)
}

# ------------------------------------------------------------------
click_previous_left_calendar <- function(){
  ### Go to previous month for the start date
  webElem <- remDr$findElement("xpath","//div[@class='calendar left']//span[@class='Icon Icon--caretLeft Icon--tiny']")
  webElem$clickElement()
  Sys.sleep(1)
}

# ------------------------------------------------------------------
click_next_left_calendar <- function(){
  ### Go to next month for the start date
  webElem <- remDr$findElement("xpath","//div[@class='calendar left']//span[@class='Icon Icon--caretRight Icon--tiny']")
  webElem$clickElement()
  Sys.sleep(1)
}

# ------------------------------------------------------------------
click_date_left_calendar <- function(date){
  ### Pick start date
  webElem <- remDr$findElements("xpath","//div[@class='calendar left']//td[(@class='available' or @class='available in-range' or @class='available active start-date' or @class='available active end-date')]")
  webElem[[as.numeric(format(as.Date(date,format="%Y-%m-%d"), "%d"))]]$clickElement()
  Sys.sleep(1)
}

# ------------------------------------------------------------------
# ------------------------------------------------------------------
select_end_date <- function(date){
  ### Select specific date in right calendar
  ## Compute number of clicks required until the right month
  # The number of clicks are the difference of months between the target date and the current date displayed in the calendar
  target_year <- format(as.Date(date),"%Y")
  target_month <- format(as.Date(date),"%m")
  current_date <- as.Date(unlist(remDr$findElement("xpath","//div[@class='calendar right']//input[@name='daterangepicker_end']")$getElementAttribute("value")), "%m/%d/%Y")
  current_year <- format(as.Date(current_date),"%Y")
  current_month <- format(as.Date(current_date),"%m")
  delta_months <- (as.numeric(target_year)-as.numeric(current_year))*12 + as.numeric(target_month)-as.numeric(current_month)
  ## Decide if we must click previous or next depending on the difference in months
  if(delta_months < 0){
    ## Click previous month until the right month
    for(i in 1:abs(delta_months)){
      click_previous_right_calendar()
    }
  }else if(delta_months > 0){
    ## Click next month until the right month
    for(i in 1:abs(delta_months)){
      click_next_right_calendar()
    }
  }
  ## Select start date
  click_date_right_calendar(date)
}

# ------------------------------------------------------------------
click_previous_right_calendar <- function(){
  ### Go to previous month for the end date
  webElem <- remDr$findElement("xpath","//div[@class='calendar right']//span[@class='Icon Icon--caretLeft Icon--tiny']")
  webElem$clickElement()
  Sys.sleep(1)
}

# ------------------------------------------------------------------
click_next_right_calendar <- function(){
  ### Go to previous month for the end date
  webElem <- remDr$findElement("xpath","//div[@class='calendar right']//span[@class='Icon Icon--caretRight Icon--tiny']")
  webElem$clickElement()
  Sys.sleep(1)
}

# ------------------------------------------------------------------
click_date_right_calendar <- function(date){
  ### Pick end date
  webElem <- remDr$findElements("xpath","//div[@class='calendar right']//td[(@class='available' or @class='available in-range' or @class='available active start-date' or @class='available active end-date')]")
  webElem[[as.numeric(format(as.Date(date,format="%Y-%m-%d"), "%d"))]]$clickElement()
  Sys.sleep(1)
}
