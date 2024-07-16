import sys
import pymupdf

merged = pymupdf.open()

for pdf_path in sys.argv[1:]:
    pdf = pymupdf.open(pdf_path)
    merged.insert_pdf(pdf)

merged.save("merged.pdf")
