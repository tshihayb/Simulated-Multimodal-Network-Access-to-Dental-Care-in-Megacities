# Data dictionary

All files live in [`Data/`](Data/). Coordinates are geographic (WGS84,
EPSG:4326) unless noted; the analysis reprojects to UTM zone 38N (EPSG:32638)
for distance work.

---

## `all facilities.xlsx` (sheet `Sheet1`)

Dental-facility inventory for Riyadh. **910 rows**, **10 columns** (exactly the
fields below — no contact, survey, or quality-control fields are distributed).

Of the 910 facilities, **766 provide dental services** (732 private + 34
public); the remaining 144 are non-dental facilities retained for context.
Ownership overall: 876 private, 34 public.

| Column | Type | Description | Values / code list |
|---|---|---|---|
| `id` | integer | Facility identifier (unique within the file). | — |
| `district_in_arabic` | text | District of the facility (Arabic). | — |
| `district_in_english` | text | District of the facility (English). | — |
| `new_region` | categorical | Official region assigned by Riyadh Municipality. | `North`, `East`, `Center`, `West`, `South` |
| `type_of_clinic` | categorical | Facility type (full classification). | `Dental clinic only`; `Polyclinic/Cosmetic clinic with dental`; `Polyclinic/Cosmetic clinic without dental`; `Hospital with dental`; `Hospital without dental`; `Primary care center with dental clinic`; `Specialized dental center` |
| `longitudes` | numeric | Longitude (decimal degrees, WGS84). | — |
| `latitudes` | numeric | Latitude (decimal degrees, WGS84). | — |
| `type_with_dental` | categorical | For facilities **with** dental services, the facility category. Blank when the facility has no dental services. | `Dental clinic only`; `Polyclinic/Cosmetic clinic`; `Hospital`; `Primary care center`; `Specialized dental center` |
| `type_without_dental` | categorical | For facilities **without** dental services, the facility category. Blank otherwise. | `Polyclinic/Cosmetic clinic`; `Hospital` |
| `private_or_public` | categorical | Ownership sector. | `Private`, `Public` |

> **Derived in code, not stored:** `with_dental_services` (`Yes`/`No`) is
> computed in the scripts as `type_with_dental` being non-empty.

---

## `2025_06_22_List_of_Riyadh_dsitricts_stripped_of_population.xlsx`

District reference list, **stripped of population** (GASTAT-restricted). Two
sheets; the pipeline reads the `Clean` sheet for district names and region.

- **`Clean`** (190 rows): `Number`, `District Name in Arabic`,
  `District Name in English`, `Region`, `New Region`, `name_en`, plus
  data-preparation tracking columns (`Done (Yes or No)`,
  `Need to be checked (Yes or No)`, `Empty based on google map (Yes or No)`,
  `Comments`).
- **`Sheet2`** (190 rows): a subset of the same columns.

No population column is present in either sheet.

---

## GeoJSON layers

| File | Contents | Key properties |
|---|---|---|
| `districts.geojson` | Riyadh district boundary polygons. | `region_id`, `name_ar`, `name_en` |
| `metro-stations-in-riyadh-by-metro-line-and-station-type-2024.geojson` | Riyadh Metro stations (2024). | line, station type, geometry |
| `bus-stops-in-riyadh-by-bus-route-direction-and-shelter-type-2024.geojson` | Riyadh bus stops by route, direction, shelter type (2024). | route, direction, shelter, geometry |
| `_official_district_labels.geojson` | Point labels for districts (map overlay). | label text, geometry |
| `_official_district_linework.geojson` | District boundary linework (map overlay). | geometry |

---

## `Final_Routes_Processed.csv`

Ordered bus-route segments (one row per consecutive stop pair).

| Column | Description |
|---|---|
| `Route_ID` | Route + direction identifier (e.g. `10_1`). |
| `Segment_Seq` | Segment order within the route. |
| `From_Name` / `To_Name` | Origin / destination stop names. |
| `Distance_m` | Segment length (metres). |
| `Status` | Processing status flag. |
| `From_Code` / `To_Code` | Origin / destination stop codes. |

## `riyadh_bus_routes_by_code.rds`

R serialized object: bus routes keyed by route code, consumed by the main
analysis (Sections 16–17) and produced by
`Code/Bus routes sequence and line build up.R`.
