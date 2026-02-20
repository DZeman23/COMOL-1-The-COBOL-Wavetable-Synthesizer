# COMOL-1: The COBOL Wavetable Synthesizer

This is a highly flexible and versatile wavetable synthesizer written entirely in COBOL.
In the long term, I aim to develop this project to create a program that fully emulates
the time variable control system of the Roland JD800, but with more user customizability 
in the selection and editing of waveforms.  

```text
  ______    ______   __       __   ______   __        __   
 /      \  /      \ |  \     /  \ /      \ |  \     _/  \  
|  $$$$$$\|  $$$$$$\| $$\   /  $$|  $$$$$$\| $$    |   $$  
| $$   \$$| $$  | $$| $$$\ /  $$$| $$  | $$| $$ ____\$$$$  
| $$      | $$  | $$| $$$$\  $$$$| $$  | $$| $$|    \| $$  
| $$   __ | $$  | $$| $$\$$ $$ $$| $$  | $$| $$ \$$$$$|$$  
| $$__/  \| $$__/ $$| $$ \$$$| $$| $$__/ $$| $$___  _| $$_ 
 \$$    $$ \$$    $$| $$  \$ | $$ \$$    $$| $$   \|   $$ \
  \$$$$$$   \$$$$$$  \$$      \$$  \$$$$$$  \$$$$$$ \$$$$$$
     .-----.         .-----.   .--------------. 
 100 |  |  |     100 |=====|   |      /\      | 
   - |  |  |       - |=====|   |     /..\     | 
   - |  |  |       - |=====|   |    /::::\    | 
   - |  |  |       - |=====|   |      UP      | 
   - |  |  |       - |  |  |   |______________| 
   - |  |  |       - |  |  |   |  __________  | 
  50 |  |  |      50 |  |  |   |     DOWN     | 
   - |  |  |       - |  |  |   |    \::::/    | 
   - |  |  |       - |  |  |   |     \../     | 
   - |  |  |       - |  |  |   |      \/      | 
   - |=====|       - |  |  |   '--------------' 
   - |=====|       - |  |  |                    
   - |=====|       - |  |  |                    
   0 |=====|       0 |  |  |                    
     '-----'         '-----'                    
```               
	 
## What This Program Does Overall:
(The numbers of each step will be used as an index for the Full Documentation file)

1. Generates or loads waveforms into a wavetable

2. Applies pitch shifting via variable-speed playback

3. Processes audio through Time Variant JD800 style digital biquad filters

4. Simulates analog characteristics (warmth, drift, saturation, bit reduction)

5. Shapes amplitude with Time Variant envelopes

6. Uses high-quality interpolation to prevent aliasing

7. Outputs processed audio as raw PCM data

8. Auto run and audio FX modules have been added, their readme documentation is in their subfolders
	 
## Why COBOL?

**COBOL excels at fixed-point arithmetic.** Audio DSP requires precise decimal calculations for filter coefficients, envelope slopes, and sample interpolation. COBOL's `PICTURE` clauses with explicit decimal placement (like `PIC S9(5)V9(10)`) provide predictable, reproducible math without floating-point rounding errors that plague C/C++. Furthermore, because of the high degree of precision involved in floating point languages such as C++, it is easy to make a sterile sounding instrument. In certain cases, as I will demonstrate, intentionally inducing truncation through fixed-point allows for a degree of imperfection well suited to digital-analogue simulation.

**Packed decimal (COMP-3) is perfect for wavetables.** Storing 2048 samples with 17 decimal places is an ideal job for COBOL. COBOL's packed decimal stores digits efficiently while maintaining the precision needed for high-quality audio.

**Business logic is surprisingly intuitive for handling raw audio stored in binary.** Audio processing is surprisingly similar to batch processing: read records (samples), apply transformations (filters/envelopes), write output. COBOL's procedural structure with named paragraphs creates self-documenting signal flow.

**It's an exercise in algorithm design.** Modern audio software assumes you need C++ and SIMD optimization. This project proves that algorithm design matters more than language choice. A well-structured COBOL program with proper fixed-point math can generate professional-quality audio—it just challenges our preconceptions about "appropriate" tools.

**Is COBOL the best tool for the job?** Well uh... no admittedly not. I intentionally chose an uninuitive appraoch to the task in hopes that it would force me into a "first principles" way of thinking about the task. But the end product is a surprisingly nice sounding virtual synth. I hope that the way that I have figured out   how to process binary for audio processing will open some new possibilities for poor old dusty COBOL. Sometimes the best way to deeply understand a problem domain is to approach it from a completely unexpected angle. Writing a virtual analog synthesizer in a 65-year-old business language forces you to think carefully about every calculation, every data structure, every assumption about how audio synthesis "should" work.

## Special note of thanks:
This project would not have been possible without **Curtis Roads**' exceptional book 'the computer music tutorial'.
Furthermore, the envelope design owes its form to the Roland JD800 synth, from which I derived its structure.

## External Software
**MANDATORY**: Audacity, GnuCOBOL Compiler. 
	(Audacity is needed to handle 16-bit headerless PCM files. Techinally, any software that can do this is a-okay. But Audacity is free and it is what my guide is based on.)
**OPTIONAL**: StupidSimpleSampler, Savihost 
	(these are for mapping the output to a keyboard)
	
## What is a 16-bit Headerless PCM File?

**Raw audio data with no metadata.** A headerless PCM (Pulse Code Modulation) file contains pure audio samples with no header information—no sample rate, no channel count, no file format markers. Just sequential samples from start to finish.

**16-bit means each sample uses two bytes.** Every audio sample is stored as a signed 16-bit integer (range: -32,768 to +32,767), representing the amplitude at that moment in time. This gives 65,536 possible amplitude values, providing CD-quality dynamic range.

**Little-endian byte ordering.** Each sample is stored as two bytes: the low byte first, then the high byte. For example, a sample value of 1,000 (0x03E8 in hex) is stored as `E8 03`.
**You must know the playback parameters.** Since there's no header, you will need to manually set:
- **Sample rate** (44,100 Hz)
- **Bit depth** (16-bit in this case)
- **Channels** (mono in this case)
- **Byte order** (little-endian)


# Quick-Start Guide:

## Auto-Run with Python
I suggest that you read this file first and run the program as explained here to understand how the COBOL works. Once you have tried it, you can read the README in the AutoRun folder to automate the synth.

## Audacity:
Audacity is a crucial piece of software for this project. COBOL itself does not have the capability to play back .raw (check the External Software section to get the details of what a .raw file is). Because of this built in limitation, you will need external software to play back anything you process or want to process in the synth. 

### Opening a .raw File

1. **Launch Audacity** and go to `File > Import > Raw Data...`

2. **Select your .raw file** (e.g., `Output.raw`)

3. **Configure the import settings:**
   - **Encoding:** `Signed 16-bit PCM`
   - **Byte order:** `Little-endian`
   - **Channels:** `1 Channel (Mono)`
   - **Sample rate:** `44100 Hz` 
   - **Start offset:** `0` bytes
   - **Amount to import:** `100` %

4. **Click OK** and your waveform will appear

### Exporting to .raw Format

1. **Select your audio** (or leave unselected to export everything)

2. **Go to** `File > Export > Export Audio...`

3. **Set these options:**
   - **File name:** Choose your output name (e.g., `MySound.raw`)
   - **File type:** `Other uncompressed files`
   - **Header:** `RAW (header-less)`
   - **Encoding:** `Signed 16-bit PCM`

4. **Click Save**

## Setup. 

1. Download all of the copybook files, the main code and any sample from the sample bank that you want to use. 

2. Make sure that the main code is in the same file directory as all of the copybooks. 

3. Create an empty .raw file called 'Output.raw'.

4. COBOL has **FIXED** File paths, make sure to update them before you compile. Input is the file from sample bank and output is the file you created in step 3.

5. Compile and run. Open your Output.raw file in audacity. Enjoy

6. For a preset sound please follow the instructions in the markup files found in the Presets folder. These are simple to make, although you will have to interact with audacity to get the most out of them. I will aim to include the final sound with processing in an audacity project folder, in case you want to use these samples in music without the hassle of wrangling COMOL-1

#### Interpolation Sequencer

The **Interpolation Sequencer** allows you to create custom patterns that cycle through different interpolation methods for each output sample, producing unique audio textures and "grit. for a maximum of 16 characters per string.

**How to use:**
- When prompted, enter a pattern using digits 1-3 (up to 16 characters)
- **1** = None (Nearest Neighbor) - Raw, aliased sound
- **2** = Linear - Smooth interpolation
- **3** = Sinc - Highest quality, band-limited interpolation

**Examples:**
- `2222` - Pure linear interpolation (smooth, standard quality)
- `3331` - Mostly sinc with occasional aliasing for subtle texture
- `1212` - Alternates between raw and smooth every other sample
- `321` - Cycles through all three methods (sinc → linear → none, repeat)


The pattern repeats throughout playback. Shorter patterns create rhythmic digital artifacts, while longer patterns produce evolving timbral variations. If you leave it blank, it defaults to linear (2).

# Advanced Tutorial: Making your own waveforms. 

## Creating Custom Waveforms in Audacity

### Method 1: Recording/Sampling Audio (Recommended)

#### Preparing Your Sample
1. **Import or record your source audio** in Audacity
2. **Select exactly 2048 samples:**
   - Click and drag to select a region
   - Look at the bottom toolbar: "Audio Position" shows sample count. If it shows seconds or another measurement, select the dropdown menu and then select samples.
   - Adjust selection until you have exactly 2048 samples.
   - Right click and select 'Split Clip'. Delete everything to the left and right of your sample. Move the sample to the beginning of the track.
   - **Tip:** For loopable waveforms, select a section that starts and ends at zero-crossing points (where the waveform crosses the center line) to avoid clicks

3. **Important: Normalize your selection**
   - With 2048 samples selected, go to `Effect > Normalize...`
   - Check "Normalize peak amplitude to" `-1.0 dB`
   - This ensures the waveform uses the full dynamic range without clipping

4. **Export as .raw:**
   - With selection still active, go to `File > Export > Export Selected Audio...`
   - File type: `Other uncompressed files`
   - Header: `RAW (header-less)`
   - Encoding: `Signed 16-bit PCM`
   - Save with a descriptive name

#### What Makes a Good 2048-Sample Waveform?

**Seamless looping is critical.** The 2048th sample must connect smoothly back to the 1st sample, or you'll hear clicks/pops when the wavetable loops. Look for natural loop points in sustained tones (organ, pad sounds, vocal "ah" vowels).

**Harmonic content matters.** Bright, harmonically rich sources (saw waves, square waves, distorted guitars, brass) give you more tonal possibilities. Sine waves work but sound plain. Complex timbres create interesting textures when pitch-shifted.

**Single-cycle vs. multi-cycle.** For traditional wavetable synthesis, capture ONE complete cycle of a periodic waveform. For creative textures, 2048 samples at 44.1kHz = ~46ms of audio—you can capture short percussive hits, formant transitions, or evolving pad textures.

**Avoid DC offset.** The waveform should be centered around zero. After importing, use `Effect > Normalize...` and ensure "Remove DC offset" is checked.

### Method 2: Drawing Waveforms (For Geometric Shapes)

1. **Generate a tone to get started:**
   - `Generate > Tone...`
   - Waveform: Sine (you'll draw over this)
   - Frequency: 440 Hz
   - Amplitude: 0.8
   - Duration: exactly `0.046439909` seconds (this equals 2048 samples at 44.1kHz)

2. **Switch to Draw Tool:**
   - Click the pencil icon in the toolbar (or press `F3`)
   - Zoom in VERY close: `View > Zoom > Zoom In` (Ctrl+1) repeatedly
   - You can now click and drag to reshape the waveform

3. **Drawing tips:**
   - Draw smooth curves for warm, mellow tones
   - Add sharp corners/edges for brighter, buzzier sounds
   - Make the waveform asymmetric for even-harmonic content
   - **Critical:** Ensure the end connects smoothly to the beginning (zoom to verify the loop point)

4. **Normalize and export** (same as Method 1, steps 3-4)

### Pro Tips for Quality Wavetables


**Test your loop.** After exporting, import the .raw file back into Audacity and use `Effect > Repeat...` to duplicate it 10-20 times. Play it back—if you hear clicks, your loop point isn't clean.

**Capture "movement."** Don't just grab static waveforms. Sample the attack portion of a plucked string, the formant shift in a vowel sound, or the decay tail of a bell. These evolving textures create organic, living tones.

**Bit depth matters less than you think.** 16-bit is plenty. The COBOL synth's fixed-point processing will add its own character regardless of source bit depth.

# To Do List:

1. Implement LFOs 
2. ~~Update the filter from a static to a dynamic time variant style.~~ 
3. Write a Mainframe compliant version of the code. 
4. Expand the sample bank with all the waveforms from the original JD800.
5. ~~Find a way to 'batch' generate 4 or more outputs to better match the 4 voices of the JD800.~~
6. ~~Implement basic JD800 style FX.~~ 
