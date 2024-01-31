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
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, PageBreak
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib import colors
from reportlab.platypus.tables import Table, TableStyle
from reportlab.lib.units import inch
import re

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

    for i, (_, row) in enumerate(df.iterrows()):
        sentence = row['sentence']
        previous = row['previous']
        next_sentence = row['next']

        # Clean up and format the text (remove '\n' and multiple spaces)
        sentence = re.sub(r'\n', ' ', sentence)
        sentence = re.sub(r'\s+', ' ', sentence)

        previous = re.sub(r'\n', ' ', previous)
        previous = re.sub(r'\s+', ' ', previous)

        next_sentence = re.sub(r'\n', ' ', next_sentence)
        next_sentence = re.sub(r'\s+', ' ', next_sentence)

        # Create Paragraph objects for the cells
        previous_paragraph = Paragraph(previous, style)
        sentence_paragraph = Paragraph(sentence, style)
        next_paragraph = Paragraph(next_sentence, style)

        # Calculate maximum cell height for this page
        max_cell_height = max(
            [previous_paragraph.wrap(doc.width, doc.bottomMargin)[1],  # Extract the height from the tuple
             sentence_paragraph.wrap(doc.width, doc.bottomMargin)[1],  # Extract the height from the tuple
             next_paragraph.wrap(doc.width, doc.bottomMargin)[1]]  # Extract the height from the tuple
        ) + 2 * style.leading  # Use style.leading directly

        # Create a table row with sentence and context
        table_data.append([previous_paragraph, sentence_paragraph, next_paragraph])

        # Check if it's time to start a new page or it's the last sentence
        if (i + 1) % 3 == 0 or (i == num_sentences - 1):
            # Adjust row heights for this page
            row_heights = [max_cell_height * 2.5] * len(table_data)  # Make rows three times taller
            table = Table(table_data, colWidths=[2.5*inch, 2.5*inch, 2.5*inch], rowHeights=row_heights)
            table.setStyle(TableStyle([('BACKGROUND', (0, 0), (-1, 0), colors.grey),
                                       ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
                                       ('TEXTCOLOR', (0, 0), (-1, -1), colors.black),
                                       ('INNERGRID', (0, 0), (-1, -1), 0.25, colors.black),
                                       ('BOX', (0, 0), (-1, -1), 0.25, colors.black)]))

            story.append(table)
            story.append(PageBreak())  # Add a page break

            table_data = []  # Clear the data for the next table

    # Build the PDF
    doc.build(story)