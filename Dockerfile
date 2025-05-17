# Use the rocker/shiny base image which includes R and Shiny Server
FROM rocker/shiny:latest

RUN R -e "install.packages(c('shiny', 'png', 'shinyWidgets', 'DT', 'dplyr', 'DBI', 'RSQLite'))"

RUN mkdir /home/app

COPY . /home/app

WORKDIR /home/app

RUN chmod 644 MyBooks.db

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app', host = '0.0.0.0', port = 3838)"]