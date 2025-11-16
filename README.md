# Jellyfin-HS Backend

A Haskell-based media server backend inspired by Jellyfin, featuring video streaming via ffmpeg and PostgreSQL storage.

## Features

- **Video Streaming**: Stream videos with on-the-fly transcoding using ffmpeg
- **Quality Selection**: Support for multiple quality presets (Original, High, Medium, Low)
- **Media Library Management**: Automatic scanning and cataloging of video files
- **PostgreSQL Storage**: Robust database backend for media metadata
- **REST API**: Clean REST API built with Servant

## Prerequisites

- GHC (Glasgow Haskell Compiler) >= 9.4
- Cabal >= 3.0
- PostgreSQL >= 13
- ffmpeg and ffprobe

## Installation

```bash
# Install dependencies
cabal update
cabal build

# Set up PostgreSQL database
createdb jellyfin
```

## Configuration

Create a `.env` file or set environment variables:

```bash
DB_HOST=localhost
DB_PORT=5432
DB_USER=jellyfin
DB_PASSWORD=jellyfin
DB_NAME=jellyfin
SERVER_PORT=8080
MEDIA_ROOT=/path/to/your/media
```

## Running

```bash
cabal run jellyfin-hs
```

The server will start on `http://localhost:8080` (or your configured port).

## API Endpoints

### Health Check
```
GET /health
```

### Get All Media Items
```
GET /api/media
```

### Get Specific Media Item
```
GET /api/media/:id
```

### Stream Video
```
GET /api/stream/:id?quality=<original|high|medium|low>
```

### Scan Media Library
```
POST /api/scan
```

## Development

```bash
# Build
cabal build

# Run
cabal run

# Clean
cabal clean
```

## License

MIT
