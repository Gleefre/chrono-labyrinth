#!/usr/bin/env bash

# determine the directory where .sh lies
WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/bin"

cd "$WORK_DIR"
./ChronoLabyrinth
