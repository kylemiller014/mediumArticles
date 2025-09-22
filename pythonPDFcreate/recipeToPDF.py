import streamlit as st
from fpdf import FPDF
import tempfile
import os

# --- Create the Styled PDF ---
def create_recipe_pdf(title, ingredients, instructions):
    pdf = FPDF()
    pdf.add_page()

    # Title section
    pdf.set_font("Times", 'B', 22)
    pdf.set_text_color(34, 139, 34)  # Forest green
    pdf.cell(0, 15, title, ln=True, align='C')
    pdf.ln(5)

    # Ingredients heading
    pdf.set_fill_color(245, 245, 245)  # light gray block
    pdf.set_text_color(0, 0, 0)
    pdf.set_font("Helvetica", 'B', 16)
    pdf.cell(0, 10, "Ingredients", ln=True, fill=True)
    pdf.ln(2)

    # Ingredients list
    pdf.set_font("Helvetica", '', 12)
    for item in ingredients:
        pdf.cell(0, 8, f"- {item}", ln=True)
    pdf.ln(5)

    # Instructions heading
    pdf.set_fill_color(245, 245, 245)
    pdf.set_font("Helvetica", 'B', 16)
    pdf.cell(0, 10, "Instructions", ln=True, fill=True)
    pdf.ln(2)

    # Instructions steps
    pdf.set_font("Helvetica", '', 12)
    for idx, step in enumerate(instructions, start=1):
        pdf.multi_cell(0, 8, f"{idx}. {step}")
        pdf.ln(1)

    return pdf

# --- Streamlit UI ---
st.set_page_config(page_title="📄 Recipe PDF Generator", layout="centered")
st.title("📄 Styled Recipe PDF Generator")
st.caption("Enter your recipe details and download a polished PDF.")

title = st.text_input("Recipe Title")

with st.expander("🧂 Ingredients"):
    ingredients_input = st.text_area("Enter one ingredient per line")

with st.expander("👨‍🍳 Instructions"):
    instructions_input = st.text_area("Enter one step per line")

if st.button("Generate PDF") and title and ingredients_input and instructions_input:
    ingredients = [line.strip() for line in ingredients_input.splitlines() if line.strip()]
    instructions = [line.strip() for line in instructions_input.splitlines() if line.strip()]

    pdf_obj = create_recipe_pdf(title, ingredients, instructions)

    # Save to temp file
    tmp_file = tempfile.NamedTemporaryFile(delete=False, suffix=".pdf")
    pdf_obj.output(tmp_file.name)

    with open(tmp_file.name, "rb") as f:
        st.download_button(
            label="📥 Download Styled Recipe PDF",
            data=f,
            file_name=f"{title.replace(' ', '_')}.pdf",
            mime="application/pdf"
        )

    os.remove(tmp_file.name)  # cleanup temp file
