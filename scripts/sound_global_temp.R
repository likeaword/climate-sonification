
library(tidyverse)
library(audio)
#library(googleLanguageR)
library(sound)
library(fuzzyjoin)

## Project Variables ----
tempo <- 240 # set the tempo - this must must be 60 x the fps of the animated line chart for them to match up
sample_rate <- 44100 # set the sample rate 

## Project Functions ----

# this function is the bit that turns the frequency and duration values into a song. Might as well be magic
make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

## Get and process the data ----

temp_change <- readr::read_csv("inputs/global_temp_change.csv") %>% 
  
  #Jamie had used lowess_temp as the name for this column but I use y_value to make it a bit more re-usable
  rename(y_value = 'Lowess(5)') %>% 
  mutate(max_value = max(y_value)) %>%
  mutate(note = (y_value/max_value)) %>% # calculate the value as a percentage of the max 
  mutate(frequency = (note * 440)+330) %>% # turn the percentage into a musical note frequency
  mutate(duration = 1) %>% # set all durations to be equal to one
  mutate(geo = 'Global')



# Get a list of frequencies to fit the data to
note_ref <- readr::read_csv("inputs/note_ref.csv")
note_ref <- note_ref %>%
  rename(force_note = 'Frequency')

# Filter down to all semitones across two octaves
#note_ref <- note_ref %>%
#  filter(Octave>=3 & Octave <= 4 )

# Or filter down to a scale across two octaves 
 note_ref <- note_ref %>%
  filter(Octave>=3 & Octave <= 4 & C_Maj_Scale==TRUE)


# Count the number of steps
no_of_notes <-length(note_ref$Midi_tuning)
no_of_steps <- no_of_notes-1

# Divide the range of values across the number of steps
range_of_y_data <- max(temp_change$y_value) - min(temp_change$y_value)
each_step_of_y_data <- range_of_y_data/no_of_steps
note_ref <- note_ref[order(note_ref$Midi_tuning),]

# Assign each note a value within the range of values in the dtaset
for(i in 1:no_of_notes){
  if(i==1){
  current_y_value <- min(temp_change$y_value)  
  }
  note_ref$y_value[i] <-  current_y_value
  current_y_value <- current_y_value + each_step_of_y_data
}

#Tidy up the columns 
note_ref <-note_ref %>%
  select(c(Midi_tuning,force_note,y_value))

# Do a table join to add in a Midi value and a note frequency (force_note) for each record
# using fuzzyjoin to allow us to join to the nearest value not an exact match

temp_change <- difference_left_join(temp_change, note_ref,  max_dist=(each_step_of_y_data/2))




## Make the chart ----

ggplot(data = temp_change, aes(x = Year, y = y_value.x)) + 
  geom_line()

## Make the audio ----

global_temp_change_wave <-
  mapply(make_sine, temp_change$force_note , temp_change$duration) 

# save the tune wav file
save.wave(global_temp_change_wave,paste0("tempoutput/temp_change_240bpm_C_scale_2_octaves.wav"))

## Make the MIDI CSV ----

# how many data values
no_y_values <- length(temp_change$y_value.x)

# create empty dataframes with the correct rows and columns
midi_note_on <- data.frame(matrix(NA, nrow=no_y_values, ncol=7))
midi_note_on <- midi_note_on %>%
  rename(Track = 'X1') %>%
  rename(Time = 'X2') %>%
  rename(Event = 'X3') %>%
  rename(Channel = 'X4') %>%
  rename(Note = 'X5') %>%
  rename(Velocity = 'X6') %>%
  rename(Order = 'X7')

midi_note_on$Time <- as.double(midi_note_on$Time)


midi_note_off <- data.frame(matrix(NA, nrow=no_y_values, ncol=7))
midi_note_off <- midi_note_off %>%
  rename(Track = 'X1') %>%
  rename(Time = 'X2') %>%
  rename(Event = 'X3') %>%
  rename(Channel = 'X4') %>%
  rename(Note = 'X5') %>%
  rename(Velocity = 'X6') %>%
  rename(Order = 'X7')


# in this version each note lasts the same length of time
note_on_time <- 0
note_off_time <- 120
midi_note_on$Event <- " Note_on_c"
midi_note_off$Event <- " Note_off_c"

for(i in seq(from=1, to=no_y_values, by=1)){

  note <- temp_change$Midi_tuning[i]
  #set the notes on

  midi_note_on$Track[i] <- 2
  midi_note_on$Channel[i] <- 1
  midi_note_on$Time[i] <- note_on_time
  midi_note_on$Note[i] <- note
  midi_note_on$Velocity[i] <- 81
  midi_note_on$Order[i] <- i
  
  
  #set the notes off

  midi_note_off$Track[i] <- 2
  midi_note_off$Channel[i] <- 1
  midi_note_off$Time[i] <- note_off_time
  midi_note_off$Note[i] <- note
  midi_note_off$Velocity[i] <- 0
  midi_note_off$Order[i] <- i
  print(note_on_time)
  print(note_off_time)
  
  note_on_time <- note_on_time + 120
  note_off_time <- note_off_time + 120

}


midi_note_on_off <- rbind(midi_note_on,midi_note_off)
midi_note_on_off <- midi_note_on_off %>%
  arrange(Order,desc(Event))

#Save the csv
write_excel_csv(midi_note_on_off,file.path("tempoutput/note_on_off.csv"),col_names = FALSE)
#Jamie did some cleve stuff with text to speech but I haven't got into that


## creds for google talk ----
## for this to work, you need a google cloud account
## https://console.cloud.google.com/home/dashboard?project=data-sonification-tts-project&authuser=2

#gl_auth("secret/data-sonification-tts-project-e68ab8e37964.json")

#temp_change_data_min <- temp_change %>% 
#  slice_min(lowess_temp, n=1, with_ties = F) # get the lowest datpoint in the dataset

#temp_change_data_min_wav <- mapply(make_sine,temp_change_data_min$frequency, 3)
#save.wave(temp_change_data_min_wav, "output/temp_change_min.wav")


#temp_change_data_max <- temp_change%>% 
#  slice_max(lowess_temp, n=1, with_ties = F)

#temp_change_data_max_wav <- mapply(make_sine,temp_change_data_max$frequency, 3)
#save.wave(temp_change_data_max_wav, "output/temp_change_max.wav")


#gl_talk(paste0("Difference between average global temperature each year from 1880 to 2019 and the average global temperature from 1951 to 1980. Lowest value is a difference of ",temp_change_data_min$lowess_temp ,"degrees celsius, which sounds like this:"),output="output/intro1.wav",name="en-GB-Wavenet-A")
#gl_talk(paste0("Highest value is a difference of ", temp_change_data_max$lowess_temp ," degrees celsius, which sounds like this:"),output="output/intro2.wav",name="en-GB-Wavenet-A")

# prepare the wav files for garage band / audacity. Needs to be like this to match the beat to the covid-data
#s_intro_gb <- appendSample("output/intro1.wav","output/temp_change_min.wav","output/intro2.wav","output/temp_change_max.wav")
#saveSample(s_intro_gb,"output/global_temp_change_intro.wav", overwrite = T)



