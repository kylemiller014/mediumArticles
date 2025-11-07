#!/bin/bash
# Failsafe setup for Streamlit recipe PDF app
# Creates venv in pythonPDFcreate folder and runs everything inside it

set -e  # Stop on first error

VENV_DIR="pythonPDFcreate"

# Check for Python3
if ! command -v python3 &> /dev/null; then
    echo "❌ Python3 not found. Please install Python3."
    exit 1
fi

# Create virtual environment if missing
if [ ! -d "$VENV_DIR" ]; then
    echo "🔹 Creating virtual environment in '$VENV_DIR'..."
    python3 -m venv "$VENV_DIR"
else
    echo "🔹 Virtual environment '$VENV_DIR' already exists. Skipping creation."
fi

# Upgrade pip inside venv
echo "🔹 Upgrading pip..."
./$VENV_DIR/venv/bin/pip install --upgrade pip

# Install required packages
echo "🔹 Installing required packages (streamlit + fpdf)..."
./$VENV_DIR/venv/bin/pip install streamlit fpdf

# Run Streamlit app
if [ -f "recipeToPDF.py" ]; then
    echo "🔹 Launching Streamlit app..."
    ./$VENV_DIR/venv/bin/python -m streamlit run ./recipeToPDF.py
else
    echo "❌ recipeToPDF.py not found in project folder."
    exit 1
fi
