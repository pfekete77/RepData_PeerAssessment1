tmp <- data.frame(actData)
tmp <- data.frame(steps=actData$steps,
                  D=parse_date_time(actData$date,"%y-%m-%d"))

tmp1 <- data.frame(x=c(1,2,3,4),
                   y=c("2012-01-01",
                       "2012-01-01",
                       "2012-01-02",
                       "2012-01-03"))
tmp2 <- group_by(tmp1,
                 y)
summarise(tmp2,
          mean(x))
tmp2

tmp1 <- data.frame(x=c(1,2,3,4),
                   y=c("a",
                       "a",
                       "b",
                       "c"))
tmp2 <- mutate(tmp1,y)
tmp2

tmp3 <- summarise(tmp1,
                  meanSteps = mean(x))
tmp3

tmp2 <- group_by(tmp1,
                 y)
