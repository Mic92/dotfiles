# gmaps-cli

A simple command-line tool to search for places and get directions using Google
Maps API.

## Features

- Search for specific places
- Find multiple places nearby
- Get directions between locations
- Simple text output
- Secure API key management via command

## Setup

1. Get a Google Maps API key:
   - Go to [Google Cloud Console](https://console.cloud.google.com/)
   - Create a new project or select existing one
   - Enable "Places API" and "Directions API"
   - Create credentials → API key
   - Optionally restrict the key to Places API and Directions API

2. Store your API key securely (e.g., using `pass`, `rbw`, or environment
   variable)

3. Setup gmaps-cli with a command to retrieve your API key:
   ```bash
   # Using pass
   gmaps-cli setup --api-key-command "pass show google-maps-api-key"

   # Using rbw (Bitwarden)
   gmaps-cli setup --api-key-command "rbw get google-maps-api-key"

   # Using environment variable
   gmaps-cli setup --api-key-command "echo $GOOGLE_MAPS_API_KEY"
   ```

## Usage

### Search for a specific place

```bash
gmaps-cli search "Nobu Malibu"
```

Output:

```
Nobu Malibu
22706 Pacific Coast Hwy, Malibu, CA 90265, USA
34.0259216, -118.6819819
Rating: 4.4 (1234 reviews)
```

### Search for places nearby

```bash
gmaps-cli nearby "coffee" --limit 5

# Search near specific location
gmaps-cli nearby "restaurants" --location "34.0522,-118.2437" --limit 3
```

Output:

```
1. Blue Bottle Coffee
   123 Main St, Los Angeles, CA 90012, USA
   Rating: 4.5 $$

2. Starbucks
   456 Broadway, Los Angeles, CA 90013, USA
   Rating: 4.0 $$
```

### Get directions

```bash
gmaps-cli route "Los Angeles" "San Francisco"

# Different travel modes
gmaps-cli route "Times Square" "Central Park" --mode walking
gmaps-cli route "Brooklyn" "Manhattan" --mode transit
```

Output:

```
Route from Los Angeles, CA, USA to San Francisco, CA, USA
Distance: 382 mi
Duration: 5 hours 48 mins

Directions:

1. Head northwest on N Spring St toward W Temple St
   0.3 mi - 2 mins

2. Use the left 2 lanes to turn left onto US-101 N
   170 mi - 2 hours 31 mins
```

## Configuration

The configuration is stored in `~/.config/gmaps-cli/config.json`. It contains
the command used to retrieve your API key, ensuring the key itself is never
stored in plaintext.
