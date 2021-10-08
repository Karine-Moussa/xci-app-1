# This script returns the gene start and end positions
# based upon querying a location within the gene
from genelocator import get_genelocator
import xlsxwriter

# set up
gl = get_genelocator('GRCh38', gencode_version=31, coding_only=False, auto_fetch=True)

# Example
gene = gl.at('chrX', 135421942)

# List of locations:
l = [103919547, 136909394, 118839555, 119468261, 132218621,  45851016,  23783176,
     11111137, 154428449, 119469097, 135344086, 130524256,  76172935, 149540973,
     149938473, 107206631, 119565408, 106611929, 134990937, 135520629, 135344031,
     134797154, 134797231, 134807174, 134807192,  64205743, 135032302, 135052190,
     120311535, 7148189,   8732185,  74925451, 135421942, 135098702, 138712106,
     149938547, 134550023,  73563084,  49879355,  46545492,  47836792, 104157026,
     132023216, 132023507,  46912432,  72131539,  72131900,  18674980, 103356451,
     46447188]

gene_returns = []
for location in l:
    gene = gl.at('chrX', location)
    for g in gene:
        g["pos"] = location
    for entry in gene:
        # some may have more than 1 entry
        gene_returns.append(entry)

def create_xlsx_file(file_path: str, headers: dict, items: list):
    with xlsxwriter.Workbook(file_path) as workbook:
        worksheet = workbook.add_worksheet()
        worksheet.write_row(row=0, col=0, data=headers.values())
        header_keys = list(headers.keys())
        for index, item in enumerate(items):
            row = map(lambda field_id: item.get(field_id, ''), header_keys)
            worksheet.write_row(row=index + 1, col=0, data=row)

headers = {
    'pos': 'POS',
    'chrom': 'Chromosome',
    'start': 'Start',
    'end': 'Stop',
    'ensg': 'Ensemble',
    'symbol': 'Symbol',
}

entries = gene_returns

create_xlsx_file("gene_locations_cotton14.xlsx", headers, entries)
