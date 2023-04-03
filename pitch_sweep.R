

library(tuneR)

# Set the sampling rate and duration
srate <- 44100
duration <- 3

# Set the starting and ending frequencies
start_freq <- 100
end_freq <- 1000

# Generate the time vector
t <- 1/srate * (0:(duration*srate-1))

# Generate the frequency vector
freq <- start_freq + (end_freq - start_freq) * t/(duration*srate)

# Generate the sound signal
sound_signal <- sin(2*pi*freq*t)

# Play the sound
play(sound_signal, srate)



# take two ----------------------------------------------------------------

library(audio)

# Set the sampling rate and duration
srate <- 44100
duration <- 5

# Set the starting and ending frequencies
start_freq <- 100
end_freq <- 1000

# Generate the time vector
t <- 1/srate * (0:(duration*srate-1))

# Generate the frequency vector
# freq <- start_freq + (end_freq - start_freq) * t/(duration*srate)

freq <- start_freq * (end_freq/start_freq)^(t/duration)

# Generate the sound signal
sound_signal <- sin(2*pi*freq*t)


# Convert the sound signal to class Wave
sound_wave <- Wave(sound_signal, samp.rate = srate, pcm = FALSE, bit = 32)

# Save the sound wave as a WAV file
writeWave(sound_wave, "pitch_sweep.wav")



