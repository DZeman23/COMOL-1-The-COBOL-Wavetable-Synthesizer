# COMOL & COMOL-FX Preset: I-13 NosferatuPad

**I-13: NosferatuPad** is a massive, 90s-workstation-style gothic cinematic patch. It features a heavily driven growl, a deep phasing sub-bass, an icy bitcrushed PWM swarm, and a majestic, delayed-vibrato brass lead. 

This preset relies on stacking octaves *before* the FX processing to achieve its full, colossal scale. **Follow these instructions carefully to build the final sound.**

---

## 🛠️ Requirements
* **COMOL Synth Engine** (using `VampireHunter.ini`)
* **COMOL-FX Engine** (using `VampireFX.ini`)
* **Audacity** (or another audio editor capable of raw data manipulation and layering)

---

## 🎹 Rendering Workflow

This process involves generating the base layers, manually generating octave variations, combining them in an audio editor, and then running the combined audio through the FX engine.

### Step 1: Initial Waveform Generation
1.  Open the COMOL Configurator.
2.  Load the `VampireHunter.ini` template.
3.  Click **Generate All Waveforms** to render the initial set of RAW files (e.g., `Output1.raw` through `Output4.raw`).

### Step 2: Import and Analyze
1.  Open Audacity.
2.  Import the generated RAW files (**File > Import > Raw Data...**). *Note: Ensure your import settings match the COMOL engine output (Signed 16-bit PCM RAW, 44100 Hz).*
3.  Listen to each waveform to understand its fundamental character.

### Step 3: Generate the Octave Shifts
1.  Return to the COMOL Configurator (`VampireHunter.ini`).
2.  For each waveform, adjust the `user-octave` parameter. Shift it **up or down one octave** based on your assessment in Step 2 and personal taste.
3.  Generate the waveforms again. *Be sure to specify a different output filename (e.g., `Output1_oct.raw`) so you don't overwrite the files generated in Step 1.*

### Step 4: Layer and Normalize
1.  In Audacity, import the newly generated octave-shifted waveforms.
2.  Combine each base waveform with its corresponding octave-shifted version onto a single track (or mix them down).
3.  Select the combined audio for each layer.
4.  Normalize the peak amplitude to **-1.0 dB** (**Effect > Volume and Compression > Normalize...**).
5.  Export these four combined and normalized tracks as new RAW files (Signed 16-bit PCM).

### Step 5: Process through COMOL-FX
1.  Open the COMOL-FX Configurator.
2.  Load the `VampireFX.ini` template.
3.  Update the `in_file_path` for each preset to point to the combined and normalized RAW files you exported in Step 4.
4.  Click **Generate All Presets**.

The FX engine will now process your stacked waveforms, outputting the final Left and Right channels for **I-13: NosferatuPad**.
