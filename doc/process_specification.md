# Unified Photographic Process Configuration Specification

**Version:** 2.0
**Status:** Draft / Proposed
**Author:** Marco Minutoli
**Date:** September 2026

---

## 1. Overview and Purpose

In a hybrid analog-digital darkroom workflow, film exposure, film development, and darkroom printing form a tightly coupled sensitometric chain:
1. **In the field**, a scene's Subject Brightness Range (SBR) is metered in zones/stops.
2. **In chemical development**, the film development time is adjusted ($N, N+, N-$) so that the negative's density range ($\Delta D_{\text{neg}}$) matches the exposure scale of the intended printing paper grade.
3. **Under the enlarger**, an easel probe meters negative highlights ($L_{\min}$) and shadows ($L_{\max}$) to determine the negative contrast range and select the matching paper grade and base exposure time.

When the film calibration software (`btzs-charts`) and the darkroom timer firmware (`esp32_fstop_timer`) use differing definitions of paper exposure scale or zone placement, the system goes out of sync. For example, if the software targets an exposure scale spanning 90% of the paper curve while the timer measures between Zone 2 (75%) and Zone 9 (5%), negatives calibrated for Normal ($N=0$) contrast will be diagnosed by the timer as over-contrasty, erroneously demanding Grade 0 instead of Grade 2.

This specification establishes a **single, unified Process Configuration schema** and formal mathematical model governing both `btzs-charts` and `esp32_fstop_timer`.

---

## 2. Mathematical Model

### 2.1. Paper Emulsion Characteristic Model

Photographic paper reflection density is modeled using a 4-parameter logistic curve:

$$D(x) = D_{\min} + \frac{D_{\max} - D_{\min}}{1 + \exp\left(-\text{slope} \cdot (x - x_0)\right)}$$

where:
* $x = \log_{10}(H)$ is the relative log exposure.
* $D_{\min}$ is the paper base plus fog density (paper white).
* $D_{\max}$ is the maximum achievable reflection black density.
* $\text{slope}$ is the maximum gradient of the curve (paper contrast rate).
* $x_0$ is the log exposure at the curve midpoint (inflection point).

The **normalized paper density fraction** $y \in (0, 1)$ represents position along the net dynamic scale:

$$y = \frac{D - D_{\min}}{D_{\max} - D_{\min}}$$

The inverse logistic function computes the relative log exposure required to achieve any normalized density $y$:

$$x(y) = x_0 + \frac{1}{\text{slope}} \ln\left(\frac{y}{1 - y}\right)$$

---

### 2.2. Zone System Partitioning & Paper Log Exposure Mapping

In the Zone System, zones are defined as equal increments of exposure in
the scene (1 stop = 0.301 log exposure). When printing onto photographic
paper, the negative's optical densities modulate the exposure reaching
the paper easel:

$$\Delta \log_{10}(H_{\text{paper}}) = \Delta D_{\text{neg}}$$

Photographic paper responds non-linearly according to its sigmoidal (S-shaped)
characteristic curve. Zones on paper are therefore **equally spaced along
the paper log exposure axis ($x$)**, while their corresponding reflection
densities ($D$ or $y$) vary along the S-curve.

The printable tonal range is anchored between two physical print thresholds:
* **Textured Highlight Anchor** ($y_{\text{hl}} \approx 0.04$): Detail just
  above base plus fog ($D_{\min} + 0.04$).
* **Textured Shadow Anchor** ($y_{\text{sh}} \approx 0.90$): Deep textured
  shadow near maximum black ($0.90 \times D_{\max}$).

The paper's total usable **Log Exposure Range (LER)** is:

$$\text{LER} = x(y_{\text{sh}}) - x(y_{\text{hl}})$$

Between the shadow and highlight anchors, the paper log exposure axis is
divided into $\Delta \text{Zones}$ equal intervals:

$$\Delta x_{\text{zone}} = \frac{\text{LER}}{\Delta \text{Zones}}$$

where:
* $\Delta \text{Zones} = 9 - 2 = 7.0\text{ stops}$ for the Standard 10-Zone
  system (Zone 2 to Zone 9).
* $\Delta \text{Zones} = 8 - 2 = 6.0\text{ stops}$ for the Ansel Adams 11-Zone
  system (Zone II to Zone VIII).

The relative log exposure for any zone $k$ on paper is:

$$x(k) = x(y_{\text{sh}}) - (k - k_{\text{shadow}}) \times \Delta x_{\text{zone}}$$

The resulting print reflection density for Zone $k$ is evaluated directly
through the paper emulsion model:

$$D(k) = D_{\min} + \frac{D_{\max} - D_{\min}}{1 + \exp\left(-\text{slope} \cdot (x(k) - x_0)\right)}$$

---

### 2.3. Print Densities Across the S-Curve

Because exposure is divided linearly on the $x$-axis, the S-curve naturally
compresses contrast in the highlights (toe) and deep shadows (shoulder), while
maximizing separation in the midtones:

#### Classical 11-Zone Ansel Adams Model ($\Delta \text{Zones} = 6.0$)
* **Zone 0**: $D \approx 0.98 \times D_{\max}$ (Maximum black, no detail)
* **Zone I**: $D \approx 0.96 \times D_{\max}$ (Near black)
* **Zone II**: $D \approx 0.90 \times D_{\max}$ (**Textured Shadow Anchor**)
* **Zone III**: $D \approx 0.79 \times D_{\max}$ (Deep textured shadows)
* **Zone IV**: $D \approx 0.60 \times D_{\max}$ (Dark foliage, landscape shadow)
* **Zone V**: $D \approx 0.38 \times D_{\max}$ (Middle gray reference)
* **Zone VI**: $D \approx 0.20 \times D_{\max}$ (Light skin, textured stone)
* **Zone VII**: $D \approx 0.09 \times D_{\max}$ (Light concrete, bright surfaces)
* **Zone VIII**: $D \approx 0.04 \times D_{\max}$ (**Textured Highlight Anchor**)
* **Zone IX**: $D \approx 0.02 \times D_{\max}$ (Faint tone before paper white)
* **Zone X**: $D = D_{\min}$ (Pure paper white)

---

### 2.4. Target Zones, Paper LER, and the Zone Contrast Constant

The process configuration designates:
* $k_{\text{highlight}}$: Target zone for textured highlight detail
  (`9` in 10-zone; `8` in Adams 11-zone).
* $k_{\text{shadow}}$: Target zone for textured shadow detail
  (`2` in 10-zone; `2` in Adams 11-zone).

The normalized density fractions are:
* $y_{\text{hl}} = 0.04$ (or explicit `highlightDensityFraction`)
* $y_{\text{sh}} = 0.90$ (or explicit `shadowDensityFraction`)

The Log Exposure Range (LER) spanned between shadow and highlight is:

$$\text{LER} = x(y_{\text{sh}}) - x(y_{\text{hl}}) = \frac{1}{\text{slope}} \left[ \ln\left(\frac{y_{\text{sh}}}{1 - y_{\text{sh}}}\right) - \ln\left(\frac{y_{\text{hl}}}{1 - y_{\text{hl}}}\right) \right]$$

Combining logarithmic terms yields the **Zone Contrast Constant** $K_{\text{zone}}$:

$$K_{\text{zone}} = \ln\left( \frac{y_{\text{sh}} \cdot (1 - y_{\text{hl}})}{y_{\text{hl}} \cdot (1 - y_{\text{sh}})} \right)$$

$$\text{LER} = \frac{K_{\text{zone}}}{\text{slope}}$$

#### Standard Print Constant ($y_{\text{hl}} = 0.04, y_{\text{sh}} = 0.90$):
$$K_{\text{zone}} = \ln\left(\frac{0.90 \times 0.96}{0.04 \times 0.10}\right) = \ln(216) \approx 5.3752784$$
$$\text{LER} = \frac{5.3752784}{\text{slope}}$$

This matches standard sensitometric practice (ISO 6846 / Phil Davis BTZS)
and the darkroom timer firmware.

---

### 2.5. Film Exposure, Development, and SBR Coupling

#### Normal Scene Measuring Range (SBR)
The number of stops spanned between shadow and highlight in a "Normal" ($N=0$) scene is:

$$\Delta \text{Zones} = | k_{\text{highlight}} - k_{\text{shadow}} |$$

For $k_{\text{highlight}} = 9$ and $k_{\text{shadow}} = 2$:
$$\Delta \text{Zones} = 9 - 2 = 7.0\text{ stops}$$

In subject log exposure:
$$\Delta \log_{10}(H_{\text{subject}}) = \Delta \text{Zones} \times \log_{10}(2) \approx \Delta \text{Zones} \times 0.30103$$

#### Film Speed Point ($ID_{\min}$)
The film speed point (Zone 2 on the negative) is placed at a net optical density above film base plus fog ($D_{\text{b+f}}$):
$$D_{\text{speed}} = D_{\text{b+f}} + \Delta D_{\text{filmSpeed}}$$
where $\Delta D_{\text{filmSpeed}} = 0.10$ according to ISO 6:1993.

#### Target Average Gradient ($\bar{G}_{\text{normal}}$)
To produce a print that spans from Zone 2 to Zone 9 on Grade 2 paper, the negative's density range ($\Delta D_{\text{neg}}$) must equal the paper's LER, taking into account the flare compensation factor ($F_{\text{flare}}$):

$$\Delta D_{\text{neg}} = \text{LER}_{\text{Grade 2}} \times F_{\text{flare}}$$

$$\bar{G}_{\text{target}} = \frac{\Delta D_{\text{neg}}}{\Delta \log_{10}(H_{\text{subject}})} = \frac{\text{LER}_{\text{Grade 2}} \times F_{\text{flare}}}{\Delta \text{Zones} \times 0.30103}$$

Because both `btzs-charts` and `esp32_fstop_timer` evaluate $\text{LER}$ using identical $y_{\text{hl}}$ and $y_{\text{sh}}$, film developed using the resulting charts will meter on the enlarger easel at:
$$\Delta D_{\text{easel}} = \log_{10}\left(\frac{L_{\max}}{L_{\min}}\right) \approx \text{LER}_{\text{Grade 2}}$$
triggering an exact **Grade 2** suggestion on the timer.

---

## 3. JSON Schema Specification

The configuration file is formatted as JSON.

### 3.1. Schema Definition

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "PhotographicProcessConfiguration",
  "type": "object",
  "properties": {
    "version": {
      "type": "string",
      "enum": ["2.0"]
    },
    "name": {
      "type": "string",
      "description": "Descriptive name of this process configuration"
    },
    "zoneSystem": {
      "type": "object",
      "properties": {
        "numZones": {
          "type": "integer",
          "minimum": 3,
          "maximum": 20,
          "default": 10,
          "description": "Total number of zones (N) in the partition"
        },
        "orientation": {
          "type": "string",
          "enum": ["blackIsZone0", "whiteIsZone0"],
          "default": "blackIsZone0",
          "description": "Direction of zone indexing"
        },
        "partitionMethod": {
          "type": "string",
          "enum": ["uniformDensity"],
          "default": "uniformDensity",
          "description": "Method used to divide dynamic range"
        },
        "highlightZone": {
          "type": "number",
          "description": "Zone index representing textured highlight detail"
        },
        "shadowZone": {
          "type": "number",
          "description": "Zone index representing textured shadow detail"
        },
        "customZoneCenters": {
          "type": "object",
          "additionalProperties": { "type": "number" },
          "description": "Optional explicit overrides for specific zone centers (y in [0, 1])"
        }
      },
      "required": ["numZones", "highlightZone", "shadowZone"]
    },
    "paper": {
      "type": "object",
      "properties": {
        "targetGrade": {
          "type": "string",
          "default": "2",
          "description": "Paper grade label used as reference for Normal film contrast"
        },
        "highlightDensityFraction": {
          "type": ["number", "null"],
          "description": "Explicit normalized y fraction for highlight (overrides zone center if set)"
        },
        "shadowDensityFraction": {
          "type": ["number", "null"],
          "description": "Explicit normalized y fraction for shadow (overrides zone center if set)"
        }
      }
    },
    "film": {
      "type": "object",
      "properties": {
        "speedPointDensity": {
          "type": "number",
          "default": 0.10,
          "description": "Net density above base+fog defining the film speed point (Zone 2)"
        },
        "standardAvgGradient": {
          "type": "number",
          "default": 0.58,
          "description": "Reference average gradient anchoring rated ISO box speed"
        },
        "speedPointFactor": {
          "type": "number",
          "default": 1.0,
          "description": "Sensitometric speed point factor"
        },
        "flareCompensationFactor": {
          "type": "number",
          "default": 1.03,
          "description": "Camera and lens flare multiplier"
        },
        "normalSbrStops": {
          "type": ["number", "null"],
          "description": "Normal SBR in stops (overrides |highlightZone - shadowZone| if set)"
        }
      },
      "required": ["speedPointDensity", "standardAvgGradient", "flareCompensationFactor"]
    }
  },
  "required": ["zoneSystem", "film"]
}
```

---

## 4. Reference Configurations

### 4.1. Standard 10-Zone Configuration (`Standard_10Zone.json`)
*Matches the ESP32 timer firmware defaults.*

```json
{
  "version": "2.0",
  "name": "Standard 10-Zone BTZS Process",
  "zoneSystem": {
    "numZones": 10,
    "orientation": "blackIsZone0",
    "partitionMethod": "uniformDensity",
    "highlightZone": 9,
    "shadowZone": 2
  },
  "paper": {
    "targetGrade": "2"
  },
  "film": {
    "speedPointDensity": 0.10,
    "standardAvgGradient": 0.58,
    "speedPointFactor": 1.0,
    "flareCompensationFactor": 1.03
  }
}
```

*Calculated Parameters:*
* $y_{\text{hl}} = 1.0 - \frac{9.5}{10} = 0.05$
* $y_{\text{sh}} = 1.0 - \frac{2.5}{10} = 0.75$
* $K_{\text{zone}} = \ln(57) \approx 4.0430513$
* $\Delta \text{Zones} = 9 - 2 = 7.0\text{ stops}$

---

### 4.2. Classical 11-Zone Ansel Adams Configuration (`Adams_11Zone.json`)
*Classical Adams Zone System where Zone II is textured shadow and Zone VIII is textured highlight.*

```json
{
  "version": "2.0",
  "name": "Ansel Adams 11-Zone Process",
  "zoneSystem": {
    "numZones": 11,
    "orientation": "blackIsZone0",
    "partitionMethod": "uniformDensity",
    "highlightZone": 8,
    "shadowZone": 2
  },
  "paper": {
    "targetGrade": "2"
  },
  "film": {
    "speedPointDensity": 0.10,
    "standardAvgGradient": 0.58,
    "speedPointFactor": 1.0,
    "flareCompensationFactor": 1.03
  }
}
```

*Calculated Parameters:*
* $y_{\text{hl}} = 1.0 - \frac{8.5}{11} \approx 0.22727$
* $y_{\text{sh}} = 1.0 - \frac{2.5}{11} \approx 0.77273$
* $K_{\text{zone}} = \ln\left(\frac{0.77273 \times 0.77273}{0.22727 \times 0.22727}\right) = \ln(3.400^2) \approx 2.4475$
* $\Delta \text{Zones} = 8 - 2 = 6.0\text{ stops}$

---

## 5. Backward Compatibility (Version 1.0 Support)

Legacy configuration files lack the `zoneSystem` block:

```json
{
  "standardAvgGradient": 0.58,
  "speedPointFactor": 1.0,
  "flareCompensationFactor": 1.03,
  "zoneRange": 7.0,
  "filmSpeedPointDensity": 0.1,
  "paperSpeedPointDensity": 0.04,
  "paperIdMaxPercentage": 0.90
}
```

Parsers in both `btzs-charts` and `esp32_fstop_timer` must implement automatic migration:
1. If `zoneSystem` is missing:
   * Treat as Version 1.0.
   * Map `standardAvgGradient` $\to$ `film.standardAvgGradient`.
   * Map `speedPointFactor` $\to$ `film.speedPointFactor`.
   * Map `flareCompensationFactor` $\to$ `film.flareCompensationFactor`.
   * Map `filmSpeedPointDensity` $\to$ `film.speedPointDensity`.
   * Map `zoneRange` $\to$ `film.normalSbrStops`.
   * If `paperSpeedPointDensity` and `paperIdMaxPercentage` are present, set:
     - `paper.shadowDensityFraction = paperIdMaxPercentage`
     - When $D_{\min}$ and $D_{\max}$ are known, compute $y_{\text{hl}} = \frac{\text{paperSpeedPointDensity}}{D_{\max} - D_{\min}}$.
2. If `zoneSystem` is present:
   * Evaluate $y_{\text{hl}}$ and $y_{\text{sh}}$ from the zone partition formulas.

---

## 6. Storage, Menu Hierarchy, and Serial Upload Protocol

To ensure seamless operational symmetry between `btzs-charts` and `esp32_fstop_timer`, process configurations are stored in dedicated storage on the timer, selected via an interactive menu, and uploaded over serial via `btzs-exporter`.

### 6.1. Filesystem Layout on ESP32 (`/config/process/`)

The ESP32 flash filesystem (LittleFS) separates emulsion profiles from process configurations:
* `/profiles/`: Contains paper profile JSON files (e.g., `/profiles/AU_Pearl_D72_1_3.json`).
* `/config/process/`: Contains process configuration JSON files (e.g., `/config/process/Standard_10Zone.json`, `/config/process/Adams_11Zone.json`).

### 6.2. Timer Menu Hierarchy

Under the **Light Meter** menu, an interactive submenu manages process configurations:

```
[Light Meter]
 ├── Take Readings (Command)
 ├── Active Paper (Submenu -> list profiles from /profiles/)
 ├── Delete Paper (Submenu -> delete profile from /profiles/)
 ├── Process Config (Submenu)
 │    ├── Active Process (Submenu -> switch active config for current session; marked with *)
 │    ├── Default Process (Submenu -> select boot default config; marked with [D])
 │    ├── View Process (Screen -> display active N, orientation, zones, K_zone, SBR)
 │    └── Delete Process (Submenu -> delete config from /config/process/)
 ├── View Filters (Submenu)
 └── Cal Filters (Command)
```

#### Boot & Runtime Behavior:
1. **Boot Initialization**:
   * The timer reads the NVS preference key `def_proc` (e.g. `"/config/process/Standard_10Zone.json"`).
   * If `def_proc` exists and points to a valid file in `/config/process/`, it is parsed and activated.
   * If missing or unreadable, the timer falls back to built-in compile-time standard 10-zone defaults.
2. **Session Switching (`Active Process`)**:
   * Lists all `.json` files in `/config/process/`.
   * Selecting a file immediately activates it for metering calculations without changing the boot default.
3. **Setting Default (`Default Process`)**:
   * Selecting a file writes its path to NVS preference `def_proc`, ensuring it is loaded upon subsequent reboots.

### 6.3. Serial Upload Protocol Extension

The ESP32 serial communication handler (`handleSerialUpload`) is extended with dedicated config commands alongside the profile commands:

| Command Frame | Payload | Response | Description |
| :--- | :--- | :--- | :--- |
| `[START_UPLOAD:<name>:<size>]` | Raw bytes | `-[UPLOAD_OK]-` / `-[UPLOAD_ERR]-` | Upload paper profile to `/profiles/<name>` |
| `[START_CONFIG_UPLOAD:<name>:<size>]` | Raw bytes | `-[UPLOAD_OK]-` / `-[UPLOAD_ERR]-` | Upload process config to `/config/process/<name>` |
| `[DELETE_PROFILE:<name>]` | None | `-[DELETE_OK]-` / `-[DELETE_ERR]-` | Delete profile from `/profiles/<name>` |
| `[DELETE_CONFIG:<name>]` | None | `-[DELETE_OK]-` / `-[DELETE_ERR]-` | Delete config from `/config/process/<name>` |

Upon receiving `[START_CONFIG_UPLOAD:...]`:
1. The timer writes incoming bytes to a temporary file in `/config/process/`.
2. Validates JSON parsing and required schema fields (`zoneSystem` or legacy v1 keys).
3. If valid, confirms with `-[UPLOAD_OK]-`. If malformed, deletes the temporary file and responds `-[UPLOAD_ERR]-`.

### 6.4. `btzs-exporter` Upload Integration

The `btzs-exporter` utility provides direct command-line flags to upload process configurations to a connected timer:

```bash
# Upload a process configuration to the timer
btzs-exporter --upload-config ./data/Standard_10Zone.json --port /dev/ttyUSB0
```

---

## 7. Tool Implementation Checklist

### 7.1. `btzs-charts` Suite (Haskell)
* [ ] **`src/BtzsCharts/Types.hs`**: Define `ZoneSystemConfig`, `PaperProcessConfig`, `FilmProcessConfig`; implement dual-mode `FromJSON ProcessConfiguration`.
* [ ] **`src/BtzsCharts/PaperAnalysis.hs`**: Update `paperSpeedPoint`, `paperIdMax`, and `logExposureRange` to use resolved $y_{\text{hl}}, y_{\text{sh}}, K_{\text{zone}}$.
* [ ] **`src/BtzsCharts/FieldCharts.hs`**: Derive normal SBR directly from $|k_{\text{hl}} - k_{\text{sh}}|$.
* [ ] **`exporter/Main.hs`**: Add `--upload-config <FILE>` and `--port <PORT>` CLI options to stream process configs via serial using `START_CONFIG_UPLOAD`.
* [ ] **Unit Tests**: Add tasty-hedgehog tests verifying 10-zone, 11-zone, and legacy v1 JSON parsing.

### 7.2. `esp32_fstop_timer` (C++)
* [ ] **`include/ProcessConfig.h` / `src/ProcessConfigManager.cpp`**: Implement `ProcessConfigManager` singleton managing `/config/process/` directory, LittleFS parsing, NVS `def_proc` loading, and active zone calculations.
* [ ] **`include/PaperProfile.h`**: Update `getHighlightLogExposure`, `getShadowLogExposure`, and `getLogExposureRange` to reference active `ProcessZoneParams`.
* [ ] **`src/PaperProfileManager.cpp`**: Update `handleSerialUpload()` to support `[START_CONFIG_UPLOAD:...]` and `[DELETE_CONFIG:...]`.
* [ ] **`src/FStopTimerUI.cc` / `include/FStopTimerUI.h`**: Add `Process Config` submenus (`Active Process`, `Default Process`, `View Process`, `Delete Process`).
* [ ] **Unit Tests (`test/test_fstop_math.cpp`)**: Add tests for `ProcessConfigManager` and verify multi-zone exposure math.


