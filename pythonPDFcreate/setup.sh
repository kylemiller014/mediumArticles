#!/bin/bash

# --- One-Shot Setup Script for Mac ---

# Stop on error
set -e

echo "Creating virtual environment 'venv'..."
python3 -m venv venv

echo "Activating virtual environment..."
source venv/bin/activate

echo "Upgrading pip..."
pip install --upgrade pip

echo "Installing required packages (streamlit + fpdf)..."
pip install streamlit fpdf

echo "All set! Launching Streamlit app..."
python3 -m streamlit run streamlit_app.py
