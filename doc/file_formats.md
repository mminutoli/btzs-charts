# BTZS File Format Specifications

This document specifies the JSON file formats used by the `btzs-charts` suite (`btzs-charts`, `btzs-exporter`, `btzs-uploader`, and `btzs-recorder`) to represent step tablets, film tests, paper tests, and exported paper profiles.

---

## 1. Step Tablet Definition (`StepTablet`)

A step tablet file describes the optical transmission density of each step in a calibrated transmission step wedge (such as a Stouffer T2115 21-step wedge).

### Schema

| Field | Type | Required | Description |
| :--- | :--- | :--- | :--- |
| `steptabletName` | `string` | Yes | Human-readable name/identifier of the step tablet |
| `densities` | `array` of `number` | Yes | Calibrated transmission optical densities of each step, ordered from Step 1 (thinnest/lowest density) to Step $N$ (densest) |

### Requirements
- Densities must be listed in ascending order (Step 1 to Step $N$).
- The length of `densities` determines the expected number of measurements for every curve in a test. All strips measured against this step tablet must contain exactly this number of readings.

### Example (`data/Stauffer-21steps.json`)

```json
{
  "steptabletName": "Stouffer 21steps",
  "densities": [
    0.05,
    0.17,
    0.31,
    0.48,
    0.61,
    0.78,
    0.92,
    1.07,
    1.21,
    1.37,
    1.51,
    1.65,
    1.80,
    1.94,
    2.09,
    2.24,
    2.39,
    2.54,
    2.70,
    2.85,
    3.00
  ]
}
```

---

## 2. Film Material Test (`MaterialTest` - Film)

A film test file captures the sensitometric measurements and processing parameters of a film emulsion developed across multiple development times.

### Schema

| Field | Type | Required | Description |
| :--- | :--- | :--- | :--- |
| `type` | `string` | Yes | Must be `"Film"` |
| `name` | `string` | Yes | Film emulsion name (e.g. `"Arista Ultra.EDU 100"`) |
| `developer` | `string` | Yes | Developer formula and dilution (e.g. `"D76 1+1"`) |
| `temperature` | `number` | Yes | Processing temperature in °C (e.g. `20.0`) |
| `ratedIso` | `number` | Yes | Manufacturer rated/nominal ISO box speed (e.g. `100.0`) |
| `exposureTime` | `number` | No | Exposure duration in seconds (constant across all strips) |
| `lux` | `number` | No | Illuminance in lux at the film plane during exposure (optional) |
| `measurements` | `object` | Yes | Map of development times (in minutes) to measurement series |

### Measurements Format (`measurements`)
The keys of `measurements` are development times expressed as numeric strings (e.g. `"5.5"`, `"8.0"`). Each value represents the density readings across the step tablet steps, and can be specified in either of two formats:

1. **Object with optional illuminance (recommended):**
   ```json
   "5.5": {
     "lux": 250.0,
     "densities": [1.01, 0.98, 0.92, 0.85, 0.76, 0.70, 0.63, 0.55, 0.49, 0.42, 0.33, 0.26, 0.21, 0.18, 0.15, 0.13, 0.13, 0.13, 0.13, 0.13, 0.13]
   }
   ```
2. **Plain array (legacy / uncalibrated):**
   ```json
   "5.5": [1.01, 0.98, 0.92, 0.85, 0.76, 0.70, 0.63, 0.55, 0.49, 0.42, 0.33, 0.26, 0.21, 0.18, 0.15, 0.13, 0.13, 0.13, 0.13, 0.13, 0.13]
   ```

### Requirements
- Density readings in each strip are transmission densities (measured in mode `T`).
- The array length of `densities` for every development time must match the number of steps in the corresponding `StepTablet`.

### Example (`data/AristaUltraEdu100.json`)

```json
{
  "type": "Film",
  "name": "Arista Ultra.EDU 100",
  "developer": "D76 1+1",
  "temperature": 20.0,
  "ratedIso": 100.0,
  "exposureTime": 1.0,
  "measurements": {
    "5.5": [
      1.01, 0.98, 0.92, 0.85, 0.76, 0.70, 0.63, 0.55, 0.49, 0.42, 0.33, 0.26, 0.21, 0.18, 0.15, 0.13, 0.13, 0.13, 0.13, 0.13, 0.13
    ],
    "8.0": [
      1.25, 1.21, 1.13, 1.05, 0.98, 0.89, 0.79, 0.70, 0.62, 0.53, 0.44, 0.36, 0.29, 0.23, 0.18, 0.16, 0.15, 0.14, 0.14, 0.14, 0.13
    ],
    "11.0": [
      1.44, 1.41, 1.34, 1.26, 1.18, 1.09, 1.00, 0.90, 0.80, 0.68, 0.58, 0.49, 0.45, 0.33, 0.27, 0.24, 0.21, 0.19, 0.18, 0.17, 0.17
    ],
    "16.0": [
      1.60, 1.54, 1.48, 1.42, 1.32, 1.23, 1.12, 1.00, 0.90, 0.77, 0.66, 0.56, 0.45, 0.37, 0.29, 0.23, 0.20, 0.18, 0.16, 0.16, 0.16
    ],
    "20.0": [
      1.91, 1.82, 1.80, 1.72, 1.62, 1.52, 1.39, 1.25, 1.12, 0.98, 0.84, 0.70, 0.57, 0.46, 0.40, 0.29, 0.23, 0.24, 0.18, 0.16, 0.14
    ]
  }
}
```

---

## 3. Paper Material Test (`MaterialTest` - Paper)

A paper test file captures reflection sensitometry for a photographic paper exposed under one or more contrast grades (e.g. Grade 00 through 5).

### Schema

| Field | Type | Required | Description |
| :--- | :--- | :--- | :--- |
| `type` | `string` | Yes | Must be `"Paper"` |
| `name` | `string` | Yes | Paper brand and surface finish (e.g. `"Arista Ultra.EDU Pearl"`) |
| `developer` | `string` | Yes | Paper developer formula (e.g. `"Generic Paper Dev"`) |
| `temperature` | `number` | Yes | Processing temperature in °C (e.g. `20.0`) |
| `exposureTime` | `number` | No | Exposure duration in seconds (constant across all grades, e.g. `10.0`) |
| `lux` | `number` | No | Global default illuminance in lux (optional fallback) |
| `measurements` | `object` | Yes | Map of paper grade labels to measurement series |

### Measurements Format (`measurements`)
The keys of `measurements` are paper grade labels: standard labels include `"00"`, `"0"`, `"1"`, `"2"`, `"3"`, `"4"`, `"5"` (or fractional grades like `"2.5"`).

Each value can be specified in either of two formats:

1. **Object with per-grade illuminance (recommended):**
   Because different contrast filters attenuate light differently, recording `lux` per grade captures the exact filter factor:
   ```json
   "2": {
     "lux": 45.2,
     "densities": [1.91, 1.91, 1.91, 1.91, 1.91, 1.91, 1.91, 1.91, 1.84, 1.42, 0.98, 0.68, 0.44, 0.33, 0.24, 0.18, 0.15, 0.13, 0.13, 0.13, 0.13]
   }
   ```
   - `lux`: Unattenuated incident illuminance at the easel plane (measured with the specific grade filter in place, open negative carrier, no step tablet).
   - `densities`: Reflection optical densities (measured in mode `R`), ordered from Step 1 to Step $N$.

2. **Plain array (legacy / uncalibrated):**
   ```json
   "2": [1.91, 1.91, 1.91, 1.91, 1.91, 1.91, 1.91, 1.91, 1.84, 1.42, 0.98, 0.68, 0.44, 0.33, 0.24, 0.18, 0.15, 0.13, 0.13, 0.13, 0.13]
   ```

### Requirements
- Density readings in each strip are reflection densities (measured in mode `R`).
- The array length of `densities` for every grade must match the number of steps in the corresponding `StepTablet`.

### Example (`data/AristaUltraEduPaperPerl.json`)

```json
{
  "type": "Paper",
  "name": "Arista Ultra.EDU Pearl",
  "developer": "Generic Paper Dev",
  "temperature": 20.0,
  "exposureTime": 10.0,
  "measurements": {
    "0": {
      "lux": 52.0,
      "densities": [
        1.85, 1.85, 1.85, 1.84, 1.80, 1.72, 1.58, 1.39,
        1.18, 0.95, 0.74, 0.55, 0.40, 0.29, 0.22, 0.17, 0.14, 0.13, 0.13, 0.13, 0.13
      ]
    },
    "2": {
      "lux": 45.2,
      "densities": [
        1.91, 1.91, 1.91, 1.91, 1.91, 1.91, 1.91, 1.91,
        1.84, 1.42, 0.98, 0.68, 0.44, 0.33, 0.24, 0.18, 0.15, 0.13, 0.13, 0.13, 0.13
      ]
    },
    "4": {
      "lux": 22.1,
      "densities": [
        1.95, 1.95, 1.95, 1.95, 1.95, 1.95, 1.95, 1.95,
        1.95, 1.94, 1.75, 1.20, 0.58, 0.28, 0.17, 0.14, 0.13, 0.13, 0.13, 0.13, 0.13
      ]
    }
  }
}
```

---

## 4. Exported Paper Profile (`PaperProfile`)

This file is generated by `btzs-exporter` from a `Paper` material test and uploaded to the ESP32 darkroom timer via `btzs-uploader`. It contains only the fitted mathematical logistic model parameters.

### Schema

| Field | Type | Description |
| :--- | :--- | :--- |
| `name` | `string` | Paper profile name |
| `developer` | `string` | Paper developer used |
| `temperature` | `number` | Development temperature in °C |
| `grades` | `array` of `PaperGrade` | List of fitted model parameters for each grade |

#### `PaperGrade` Object

| Field | Type | Description |
| :--- | :--- | :--- |
| `label` | `string` | Grade name (`"00"`, `"0"`, `"1"`, `"2"`, `"3"`, `"4"`, `"5"`) |
| `D_min` | `number` | Minimum net base density (base + fog) |
| `D_max` | `number` | Maximum saturated paper black density |
| `slope` | `number` | Fitted curve slope (contrast rate) |
| `x0` | `number` | Absolute calibrated log exposure at the curve midpoint: $$x_0 = \text{inflVal} + \log_{10}(\text{gradeLux} \times \text{exposureTime}) - \text{maxTabletDensity}$$ |

### Example

```json
{
  "name": "Arista Ultra.EDU Pearl",
  "developer": "Generic Paper Dev",
  "temperature": 20.0,
  "grades": [
    {
      "label": "2",
      "D_min": 0.1590,
      "D_max": 1.9333,
      "slope": 6.6704,
      "x0": 1.4844
    }
  ]
}
```
