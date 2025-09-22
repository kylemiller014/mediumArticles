import streamlit as st
from fpdf import FPDF
import tempfile
import os

# Create recipe function

def create_recipe_pdf(title, ingredients, instructions):
    instructions = [step.replace("–", "-") for step in instructions]

    pdf = FPDF()
    pdf.add_page()
    pdf.set_font("Arial", 'B', 16)
    pdf.cell(0, 10, title, ln=True, align='C')

    pdf.set_font("Arial", 'B', 12)
    pdf.cell(0, 10, "Ingredients:", ln=True)
    pdf.set_font("Arial", '', 12)
    for item in ingredients:
        pdf.cell(0, 10, f"- {item}", ln=True)

    pdf.ln(5)
    pdf.set_font("Arial", 'B', 12)
    pdf.cell(0, 10, "Instructions:", ln=True)
    pdf.set_font("Arial", '', 12)
    for step in instructions:
        pdf.multi_cell(0, 10, step)

    # Save to temporary file
    tmp_file = tempfile.NamedTemporaryFile(delete=False, suffix=".pdf")
    pdf.output(tmp_file.name)
    return tmp_file.name

# Streamlit UI
st.set_page_config(page_title="📄 Recipe PDF Generator", layout="centered")
st.title("📄 Recipe PDF Generator")
st.caption("Enter a recipe and download a printable PDF!")

title = st.text_input("Recipe Title")

with st.expander("🧂 Ingredients"):
    ingredients_input = st.text_area("Enter one ingredient per line")

with st.expander("👨‍🍳 Instructions"):
    instructions_input = st.text_area("Enter one step per line")

if st.button("Generate PDF") and title and ingredients_input and instructions_input:
    ingredients = [line.strip() for line in ingredients_input.splitlines() if line.strip()]
    instructions = [line.strip() for line in instructions_input.splitlines() if line.strip()]

    pdf_path = create_recipe_pdf(title, ingredients, instructions)

    with open(pdf_path, "rb") as f:
        st.download_button(
            label="📥 Download Recipe PDF",
            data=f,
            file_name=f"{title.replace(' ', '_')}.pdf",
            mime="application/pdf"
        )

    os.remove(pdf_path)  # Cleanup

