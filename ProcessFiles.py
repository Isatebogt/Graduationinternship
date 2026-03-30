import csv

import pandas
import pandas as pd
import numpy as np
import re
from IPython.display import display
from pathlib import Path
import openpyxl


def open_files(abundance, metadata):
    df = pd.read_csv(abundance, sep="\t")
    metadata_df = pd.read_excel(metadata, sheet_name= "metadata")
    return df, metadata_df


def detect_taxa_levels(taxonomy_column):
    pattern = r"^(domain|kingdom|phylum|class|order|family|genus|species)"

    taxa_levels = []

    for value in taxonomy_column:
        match = re.search(pattern, str(value))
        if match:
            taxa_levels.append(match.group(0))

    taxa_levels = list(set(taxa_levels))
    return taxa_levels


def filter_taxon(df, taxon, taxon_col=1):
    """Filter rows for a specific taxonomic level."""

    df_taxon = df[df.iloc[:, taxon_col].str.startswith(taxon, na=False)]

    pattern = rf"(?<={taxon} - ).+"

    new_data = []

    for _, row in df_taxon.iterrows():
        match = re.search(pattern, str(row.iloc[taxon_col]))

        if match:
            new_data.append(match.group(0).strip().replace(" ", "_"))
        else:
            new_data.append(None)

    df_taxon["class id"] = new_data
    # df_taxon = pd.concat([df_taxon.reset_index(drop=True),
    #                       pd.DataFrame(new_data)], axis=1)

    return df_taxon


def reshape_and_calculate(df):
    """Convert wide to long and calculate percentages per sample."""
    df_long = df.melt(
        # columns that remain the same
        id_vars=["class id", "#class"],
        # name of new column having the sample names
        var_name="sample",
        # name of new columm having the values of the sample
        value_name="value"
    )
    # make it percentages
    new_column = df_long.groupby('sample')['value'].apply(lambda x: x / x.sum() * 100)
    df_long['percentage'] = new_column.reset_index(level=0, drop=True)
    # remove the .fastaq.gz for readability and further steps.
    df_long['sample'] = df_long['sample'].str.split(".").str[0]
    return df_long


def top_n_taxa(df_long, n=10):
    """Keep top N taxa and group the rest as 'Other'."""
    top_taxa = df_long.groupby('class id')['percentage'].sum().nlargest(n).index
    # if class id in top 10, then continue with the name otherwise name it other.
    df_long['class id'] = df_long['class id'].where(df_long['class id'].isin(top_taxa), 'other')
    # Each row shows the total percentage of a taxon in a specific sample.
    df_final = df_long.groupby(['sample', 'class id'], as_index=False)['percentage'].sum()
    df_final = df_final.rename(columns={"class id": "classid"})
    df_final['classid'] = df_final['classid'].str.split(".").str[0]
    return df_final
#
#
# def save_table(df, taxon, output_dir):
#     df.to_csv(output_dir / f"{taxon}_table.csv", index=False)
#
#
#

def add_metadata(df_top, metadata_df):
    metadata_df = metadata_df.rename(columns = {"Sample-id":"sample","DESCRIPTION":"sample","GenotypeIL22":"GT","Age":"Day" })
    metadata_df['sample'] = metadata_df['sample'].str.split(".").str[0]
    metadata_df['GT'] = metadata_df['GT'].str.strip().str.upper()
    # get the only the usefull

    #df_final = pd.merge(df_top, metadata_df, on='sample', how='left')
    metadata_df["Day"] = metadata_df["Day"].str.extract(r"(\d+)")
    df_final = pd.merge(df_top, metadata_df[['sample', 'Day', 'GT']], on='sample', how='left')
    df_final = df_final.sort_values(by=['Day', 'GT'], ascending=[True, False])

    return df_final

def process_taxa(df, taxon, output_dir, metadata_df):
    print(f"Processing: {taxon}")

    df_taxon = filter_taxon(df, taxon)
    df_long = reshape_and_calculate(df_taxon)
    # n = 10 --> largest 10 to keep
    df_top = top_n_taxa(df_long, n=10)
    # save_table(df_top, taxon, output_dir)
    df_top = add_metadata(df_top, metadata_df)


    return df_top


def main():
    abundance = Path("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/inputdir/IL-22/biotaviz_clean_relative.txt")
    metadata = Path("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/inputdir/IL-22/for_canoco.xlsx")

    namedocument = input("dataname: cxcl8a or IL-22")

    output_dir = Path("C:/Users/isate/OneDrive - Wageningen University & Research/HMI/App-1/outputdir/"+ namedocument +"/check/")
    output_dir.mkdir(parents=True, exist_ok=True)

    df, metadata_df = open_files(abundance, metadata)

    taxa_levels = detect_taxa_levels(df.iloc[:, 1])

    for taxon in taxa_levels:
         df_top = process_taxa(df, taxon, output_dir, metadata_df)
         df_top = df_top.apply(lambda col: col.str.strip() if col.dtype == 'object' else col)
         df_top.to_csv(output_dir / f"{taxon}_table.csv", index=False)





main()