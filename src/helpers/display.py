import pandas as pd
import ipywidgets as widgets
from IPython.display import display, HTML

def interactive_sentence_display(df):
    current_row = 0

    def display_content():
        output.clear_output()
        with output:
            display_html = df.iloc[current_row][['previous', 'sentence', 'next']].to_frame().to_html()
            display(HTML(display_html))

    def show_metadata(change):
        output.clear_output()
        with output:
            if change['new'] == 'Show Metadata':
                display_html = df.iloc[current_row][['previous', 'sentence', 'next', 'num', 'Titre']].to_frame().to_html()
                display(HTML(display_html))
            else:
                display_html = df.iloc[current_row][['previous', 'sentence', 'next']].to_frame().to_html()
                display(HTML(display_html))

    def prev_button_clicked(b):
        nonlocal current_row
        if current_row > 0:
            current_row -= 1
            metadata_toggle.value = 'Hide Metadata'  # Set to 'Show Metadata' when navigating to a new row
            display_content()

    def next_button_clicked(b):
        nonlocal current_row
        if current_row < len(df) - 1:
            current_row += 1
            metadata_toggle.value = 'Hide Metadata'  # Set to 'Show Metadata' when navigating to a new row
            display_content()

    output = widgets.Output()
    metadata_toggle = widgets.ToggleButtons(options=['Show Metadata', 'Hide Metadata'])
    prev_button = widgets.Button(description='Previous')
    next_button = widgets.Button(description='Next')

    metadata_toggle.observe(show_metadata, 'value')
    prev_button.on_click(prev_button_clicked)
    next_button.on_click(next_button_clicked)

    display(metadata_toggle, prev_button, next_button, output)
    show_metadata({'new': 'Hide Metadata'})  # Hide metadata by default

# Usage example:
# Replace 'samples' with your DataFrame
# interactive_dataframe_display(samples)


import pandas as pd
from reportlab.lib.pagesizes import letter
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, PageBreak, Table, TableStyle
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib import colors
from reportlab.lib.units import inch
import re

class NumberedCanvas:
    def __init__(self, canvas, doc):
        self.canvas = canvas
        self.doc = doc

    def drawPageNumber(self, page_num):
        self.canvas.saveState()
        self.canvas.setFont('Times-Roman', 10)
        page_number_text = "%d" % page_num
        self.canvas.drawCentredString(
            0.75 * inch,
            0.75 * inch,
            page_number_text,
        )
        self.canvas.restoreState()

def generate_sentences_pdf(df, pdf_filename):
    # Initialize the PDF document
    doc = SimpleDocTemplate(pdf_filename, pagesize=letter)
    story = []

    # Define styles and table formatting
    styles = getSampleStyleSheet()
    style = styles['Normal']
    style.alignment = 1  # Center-align text in cells

    table_data = []  # Data for each table (one row per page)
    num_sentences = len(df)
    
    # Define column headers
    headers = ["n° sentence", "# sentences", "n° avis", "Previous", "Sentence", "Next"]
    header_style = styles['Normal']
    header_style.textColor = colors.black
    header_paragraphs = [Paragraph(header, header_style) for header in headers]
    table_data.append(header_paragraphs)  # Add headers to the first table data list

    for i, (_, row) in enumerate(df.iterrows()):
        sentence_index = str(row.get('sentence_index', ''))
        number_sentences = str(len(df))
        num = str(row.get('num', ''))
        
        sentence = row['sentence']
        previous = row['previous']
        next_sentence = row['next']

        # Clean up and format the text
        sentence = re.sub(r'\n', ' ', sentence)
        sentence = re.sub(r'\s+', ' ', sentence)

        previous = re.sub(r'\n', ' ', previous)
        previous = re.sub(r'\s+', ' ', previous)

        next_sentence = re.sub(r'\n', ' ', next_sentence)
        next_sentence = re.sub(r'\s+', ' ', next_sentence)

        # Create Paragraph objects for the cells
        index_paragraph = Paragraph(sentence_index, style)
        number_sentences_paragraph = Paragraph(number_sentences, style)
        num_paragraph = Paragraph(num, style)
        previous_paragraph = Paragraph(previous, style)
        sentence_paragraph = Paragraph(sentence, style)
        next_paragraph = Paragraph(next_sentence, style)

        # Append row data
        table_data.append([index_paragraph, number_sentences_paragraph, num_paragraph, previous_paragraph, sentence_paragraph, next_paragraph])

        # Check if it's time to start a new page or it's the last sentence
        if (i + 1) % 3 == 0 or (i == num_sentences - 1):
            table = Table(table_data, colWidths=[0.25*inch, 0.25*inch, 0.25*inch, 2.5*inch, 2.5*inch, 2.5*inch])
            table.setStyle(TableStyle([
                ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
                ('TEXTCOLOR', (0, 0), (-1, -1), colors.black),
                ('INNERGRID', (0, 0), (-1, -1), 0.25, colors.black),
                ('BOX', (0, 0), (-1, -1), 0.25, colors.black)
            ]))

            story.append(table)
            if (i + 1) % 3 == 0 and (i != num_sentences - 1):
                story.append(PageBreak())  # Add a page break if not the last sentence

            table_data = []  # Clear the data for the next table

    # Custom canvas for page numbers
    def onLaterPages(canvas, doc):
        NumberedCanvas(canvas, doc).drawPageNumber(doc.page)

    # Build the PDF
    doc.build(story, onFirstPage=onLaterPages, onLaterPages=onLaterPages)
