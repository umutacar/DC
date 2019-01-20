import StringIO
import os
import sys

from pdfminer.converter import TextConverter, XMLConverter
from pdfminer.pdfinterp import PDFPageInterpreter
from pdfminer.pdfinterp import PDFResourceManager
from pdfminer.pdfpage import PDFPage
from pdfminer.layout import LAParams


from PyPDF2 import PdfFileWriter, PdfFileReader

from pervasives import os_utils 

## Constants
DOT = '.'
PDF = 'pdf'
TXT = 'txt'
XML = 'xml'
PAGES = 'pages'
SLASH = '/'
WORD_MARGIN = 2.7
CHAR_MARGIN = 1.0

## Utils
def mk_file_name(dir_name, file_name, ext):
  return dir_name + SLASH + file_name + DOT + ext

## Split pages and extract text

def split_and_extract_text (inputfile):

  inputfile_name, inputfile_extension = os.path.splitext(inputfile)

  reader = PdfFileReader(open(inputfile, "rb"))

  for i in range(reader.numPages):
    page = reader.getPage(i) 
    contents = page.extractText()

    txt_file = open (mk_file_name(PAGES, inputfile_name + str(i), TXT), 'w')
    print "Writing output to", txt_file
    txt_file.write(contents)

    output = PdfFileWriter()
    output.addPage(page)
    with open(mk_file_name (PAGES, inputfile_name + str(i), PDF), "wb") as outputStream:
        output.write(outputStream)


# takes inputfile_name and splits it into pages
# e.g., for a pdf with 35 pages it produces
# inputfile_name_1.pdf  .. inputfile_name_35.pdf 
# these files are placed under ./pages subdirectory.
def split_into_pages (inputfile_name):

    inputfile = open(inputfile_name, "rb")
    reader = PdfFileReader(inputfile)

    for i in range(reader.numPages):
      page_number = i + 1
      page = reader.getPage(i) 
      output = PdfFileWriter()
      output.addPage(page)
      outfile_name = os_utils.mk_file_name_page (inputfile_name, page_number)
      outfile = open (outfile_name, "wb")
      output.write(outfile)
      print "Wrote page", page_number, "into file", outfile_name
      os_utils.mv_file_subdir (outfile_name, PAGES)

## Based on 
## https://www.blog.pythonlibrary.org/2018/05/03/exporting-data-from-pdfs-with-python/
## See page by page extraction on the same blog.
def extract_text(inputfile):
    inputfile_txt = os_utils.mk_file_name_ext (inputfile, os_utils.TEXT_EXTENSION)
    inputfile_xml = os_utils.mk_file_name_ext (inputfile, os_utils.XML_EXTENSION)

    resource_manager = PDFResourceManager()
    fake_file_handle = StringIO.StringIO()
    laparams = LAParams(word_margin=WORD_MARGIN, char_margin=CHAR_MARGIN)

    converter = TextConverter(resource_manager, fake_file_handle, laparams=laparams)
    page_interpreter = PDFPageInterpreter(resource_manager, converter)
 
    with open(inputfile, 'rb') as fh:
        for page in PDFPage.get_pages(fh, 
                                      caching=True,
                                      check_extractable=True):
            page_interpreter.process_page(page)
 
        text = fake_file_handle.getvalue()
    # close open handles
    converter.close()
    fake_file_handle.close()
 
    if text:
        outfile = inputfile_txt
        outfile_handle = open (outfile, "w")
        outfile_handle.write(text)
        print "Wrote output to", outfile

def extract_text_from_page (page):
    result_file = StringIO.StringIO()
    resource_manager = PDFResourceManager()
    laparams = LAParams(word_margin=WORD_MARGIN, char_margin=CHAR_MARGIN)
    converter = TextConverter(resource_manager, result_file, laparams=laparams)
    page_interpreter = PDFPageInterpreter(resource_manager, converter)
    page_interpreter.process_page(page)
    result_text = result_file.getvalue()
    converter.close ()
    result_file.close ()
    return result_text


def extract_text_per_page (inputfile_name): 
    inputfile_name_txt = os_utils.mk_file_name_ext (inputfile_name, os_utils.TEXT_EXTENSION)


    inputfile = open(inputfile_name, 'rb')
     
    i = 0
    for page in PDFPage.get_pages(inputfile, 
                                  caching=True,
                                  check_extractable=True):        
        i = i + 1
        page_text = extract_text_from_page (page)
        if page_text:
            outfile_name = os_utils.mk_file_name_page (inputfile_name_txt, i)
            outfile = open (outfile_name, "w")
            outfile.write(page_text)
            os_utils.mv_file_subdir (outfile_name, PAGES)
            print "Wrote output to", outfile_name

def main ():
    inputfile = sys.argv[1]
#  split_and_extract_text (inputfile)
    split_into_pages (inputfile)
    extract_text_per_page (inputfile)
  
if __name__ == '__main__':
    main ()
