df <- read.csv("C:/Users/eejin/Desktop/oop data/data/MIE.csv")

summary(df)
lapply(df,class)

setGeneric("print")
setGeneric("summary")
setGeneric("subject", function(x,...){standardGeneric("subject")})
setGeneric("visit", function(x,...){standardGeneric("visit")})
setGeneric("room", function(x,...){standardGeneric("room")})

#LongitudinalData Class, methods

setClass("LongitudinalData", slots = c(id = "numeric", visit = "numeric", room = "character", value = "numeric", timepoint = "numeric"))

setMethod("print", c(x = "LongitudinalData"),
          function(x){paste("Longitudinal dataset with", length(unique(x@id)), "subjects")})

setMethod("subject", c(x = "LongitudinalData"),
          function(x,n){new("subject_class", 
                            id = x@id[x@id == n], 
                            visit = x@visit[x@id == n],
                            room = x@room[x@id == n], 
                            value = x@value[x@id == n],
                            timepoint = x@timepoint[x@id == n])})

#Subject class, methods

setClass("subject_class", slots=c(id = "numeric", visit = "numeric", room = "character", value = "numeric", timepoint = "numeric"))

setMethod("print", c(x = "subject_class"),
          function(x){if(length(unique(x@id)) > 0)
          {cat(paste("Subject ID:",unique(x@id)))}
            else{NULL}})

setMethod("summary", c(object = "subject_class"),
          function(object){new("subject_summary", 
                               id = object@id, 
                               visit = object@visit, 
                               room = object@room, 
                               value = object@value)})

setMethod("visit", c(x = "subject_class"),
          function(x,n){new("visit_class", 
                            id = x@id[x@visit == n], 
                            visit = x@visit[x@visit == n],
                            room = x@room[x@visit == n], 
                            value = x@value[x@visit == n],
                            timepoint = x@timepoint[x@visit == n])})

# subject_summary class, methods

setClass("subject_summary", slots = c(id = "numeric", visit = "numeric", room = "character", value = "numeric"))

setMethod("print", c(x = "subject_summary"),
          function(x){cat(paste("ID:", 
                                unique(x@id)),
                          "\n")
            as.data.frame(cbind(visit=x@visit,room=x@room,value=x@value),stringsAsFactors = FALSE) %>%
              mutate(value = as.numeric(value)) %>%
              group_by(visit,room) %>%
              summarise(avg = mean(value)) %>%
              spread(room,avg)})

#visit_class class, methods

setClass("visit_class", slots = c(id = "numeric", visit = "numeric", room = "character", value = "numeric", timepoint = "numeric"))

setMethod("room", c(x = "visit_class"),
          function(x,n){new("room_class", 
                            id = x@id[x@room == n], 
                            visit = x@visit[x@room == n],
                            room = x@room[x@room == n], 
                            value = x@value[x@room == n],
                            timepoint = x@timepoint[x@room == n])})

#Room_class class, methods

setClass("room_class", slots = c(id = "numeric", visit = "numeric", room = "character", value = "numeric", timepoint = "numeric"))

setMethod("print", c(x = "room_class"),
          function(x){cat(paste("ID:", unique(x@id)),"\n")
            cat(paste("Visit:",unique(x@visit)),"\n")
            cat(paste("Room:",unique(x@room)))})

setMethod("summary", c(object = "room_class"), function(object){new("room_summary", id = object@id, value = object@value)})

#Room_summary class, methods

setClass("room_summary", slots = c(id = "numeric", value = "numeric"))

setMethod("print", c(x = "room_summary"),
          function(x){cat(paste("ID:",  unique(x@id)),"\n")
            summary(x@value)})


make_LD <- function(x) {new("LongitudinalData",
                            id = x$id,
                            visit = x$visit,
                            room = x$room, 
                            value = x$value, 
                            timepoint = x$timepoint)}


library(readr)
library(magrittr)
library(tidyr)
library(dplyr)

data <- read_csv("C:/Users/eejin/Desktop/oop data/data/MIE.csv")
x <- make_LD(data)
print(class(x))
print(x)

## Subject 10 doesn't exist
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
