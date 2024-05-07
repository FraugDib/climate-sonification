library(tidyverse)
library(audio)
library(googleLanguageR)
library(sound)

## Project Variables ----
tempo <- 240 # set the tempo - this must must be 60 x the fps of the animated line chart for them to match up
sample_rate <- 44100 # set the sample rate 

## Project Functions ----

# this fucntion is the bit that turns the frequency and duration values into a song. Might as well be magic
make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

## Get and process the data ----

get_temp_change <- function() {
  temp_change <- try(readr::read_csv("inputs/global_temp_change.csv"), silent = TRUE)
  if (class(temp_change) == "try-error") {
    stop("Error reading the temperature change file.")
  }
  temp_change <- temp_change %>%
    rename(lowess_temp = 'Lowess(5)') %>%
    mutate(max_value = max(lowess_temp)) %>%
    mutate(note = (lowess_temp/max_value)) %>%
    mutate(frequency = (note * 440)+330) %>%
    mutate(duration = 1) %>%
    mutate(geo = 'Global')
  return(temp_change)
}

temp_change <- get_temp_change()

## Make the chart ----

ggplot(data = temp_change, aes(x = Year, y = lowess_temp)) + 
  geom_line()

## Make the audio

global_temp_change_wave <-
  mapply(make_sine, temp_change$frequency, temp_change$duration) 

## Save the tune wav file
save_wave <- function(wave, filename) {
  try(save.wave(wave, filename), silent = TRUE)
}

save_wave(global_temp_change_wave, "tempoutput/temp_change_240bpm.wav")

## creds for google talk ----
## for this to work, you need a google cloud account
## https://console.cloud.google.com/home/dashboard?project=data-sonification-tts-project&authuser=2

gl_auth <- function(json_file) {
  try(gl_auth(json_file), silent = TRUE)
}

gl_auth("secret/data-sonification-tts-project-e68ab8e37964.json")

temp_change_data_min <- try(temp_change %>% 
                             slice_min(lowess_temp, n=1, with_ties = F), silent = TRUE)
if (class(temp_change_data_min) == "try-error") {
  stop("Error getting the minimum temperature change value.")
}

temp_change_data_min_wav <- try(mapply(make_sine,temp_change_data_min$frequency, 3), silent = TRUE)
if (class(temp_change_data_min_wav) == "try-error") {
  stop("Error creating the minimum temperature change wave.")
}

save_wave(temp_change_data_min_wav, "output/temp_change_min.wav")

temp_change_data_max <- try(temp_change%>% 
                             slice_max(lowess_temp, n=1, with_ties = F), silent = TRUE)
if (class(temp_change_data_max) == "try-error") {
  stop("Error getting the maximum temperature change value.")
}

temp_change_data_max_wav <- try(mapply(make_sine,temp_change_data_max$frequency, 3), silent = TRUE)
if (class(temp_change_data_max_wav) == "try-error") {
  stop("Error creating the maximum temperature change wave.")
}

save_wave(temp_change_data_max_wav, "output/temp_change_max.wav")

gl_talk <- function(text, output, name) {
  try(gl_talk(text, output=output, name=name), silent = TRUE)
}

gl_talk(paste0("Difference between average global temperature each year from 1880 to 2019 and the average global temperature from 1951 to 1980. Lowest value is a difference of ",temp_change_data_min$lowess_temp ," degrees Celsius, which sounds like this:"),output="output/intro1.wav",name="en-GB-Wavenet-A")
gl_talk(paste0("Highest value is a difference of ", temp_change_data_max$lowess_temp ," degrees Celsius, which sounds like this:"),output="output/intro2.wav",name="en-GB-Wavenet-A")

# prepare the wav files for garage band / audacity. Needs to be like this to match the beat to the covid-data
s_intro_gb <- appendSample("output/intro1.wav","output/temp_change_min.wav","output/intro2.wav","output/temp_change_max.wav")
saveSample(s_intro_gb,"output/global_temp_change_intro.wav", overwrite = T)

## Remove temporary files
file.remove("output/intro1.wav")
file.remove("output/intro2.wav")

