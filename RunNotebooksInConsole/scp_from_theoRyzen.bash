#!/bin/bash

# Hardcoded remote user, host, and directory
REMOTE_USER="martin"
REMOTE_HOST="theoRyzen"
REMOTE_DIR="/home/martin/Downloads/0dFBM"

# SCP the contents of the remote folder to the current local directory
scp -r "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DIR}/*" ./

# Check if the command was successful
if [ $? -eq 0 ]; then
    echo "Files successfully copied from ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DIR} to current directory."
else
    echo "Failed to copy files from remote host."
fi