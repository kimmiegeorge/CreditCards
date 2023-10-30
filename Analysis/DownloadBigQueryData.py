import pandas as pd 
from google.cloud import bigquery
import sys

def downloadTable(tbl_str, output):

    print("In download")

    selectQuery = """SELECT * FROM %s""" % (tbl_str)
    bigqueryClient = bigquery.Client()
    df = bigqueryClient.query(selectQuery).to_dataframe()
    print(df.shape[0])
    df.to_csv(output)


def main():

    table = sys.argv[1]
    output_str = sys.argv[2]

    # output 
    downloadTable(table, output_str)

if __name__ == "__main__":
    main()

    