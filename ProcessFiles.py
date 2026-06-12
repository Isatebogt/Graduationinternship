import pandas as pd
import re
from pathlib import Path


def open_files(abundance, metadata):
    df = pd.read_csv(abundance, sep="\t")
    metadata_df = pd.read_excel(metadata, sheet_name= "metadata")
    return df, metadata_df


def detect_taxa_levels(taxonomy_column):
    """
    Return the taxonomic levels found in the taxonomy column.

    :param taxonomy_column: Series containing taxonomy strings
    :return: List containing the taxa levels found in this document
    """
    pattern = r"^(domain|kingdom|phylum|class|order|family|genus|species)"

    taxa_levels = []

    for value in taxonomy_column:
        match = re.search(pattern, str(value))
        if match:
            taxa_levels.append(match.group(0))

    taxa_levels = list(set(taxa_levels))
    return taxa_levels


def filter_taxon(df, taxon, taxon_col=1):
    """
    Filter rows for a specific taxonomic level.

    The name of each taxa extracted and _ added if needed.

    :param: df: Pandas dataframe.
    :param: taxon_col: taxonomic level to filter for.
    :param: taxon: taxon name

    :return: Filtered dataframe with the rows for the specific taxon.

    """

    df_taxon = df[df.iloc[:, taxon_col].str.startswith(taxon, na=False)]

    # Pattern to catch everything after taxon
    pattern = rf"(?<={taxon} - ).+"

    new_data = []

    for _, row in df_taxon.iterrows():
        match = re.search(pattern, str(row.iloc[taxon_col]))

        if match:
            name = match.group(0).strip()
            # Spaces are turned to underscore.
            name = re.sub(r'[^a-zA-Z0-9]+', '_', name)
            name = name.strip('_')
            new_data.append(name)
        else:
            new_data.append(None)

    df_taxon["class id"] = new_data

    return df_taxon


def reshape_and_calculate(df):
    """

    Convert wide to long and calculate percentages per sample.

    :param: df: Pandas dataframe with classid column and #class.
    :return: Pandas dataframe with percentages per sample.

    """
    df_long = df.melt(
        # columns that remain the same
        id_vars=["class id", "#class"],
        # name of new column having the sample names
        var_name="sample",
        # name of new columm having the values of the sample
        value_name="value"
    )
    # Make raw values percentages
    new_column = df_long.groupby('sample')['value'].apply(lambda x: x / x.sum() * 100)
    df_long['percentage'] = new_column.reset_index(level=0, drop=True)

    # remove the .fastaq.gz for readability and further steps, if needed.
    df_long['sample'] = df_long['sample'].str.split(".").str[0]

    # Sum percentages for the sample taxon pairs, if needed.
    df_final = df_long.groupby(['sample', 'class id'], as_index=False)['percentage'].sum()
    df_final = df_final.rename(columns={"class id": "classid"})

    return df_final


def top_n_taxa(df_long, n=10):
    """

    Keep top N taxa and group the rest as 'Other'. This is not used in the main but can be turned on.

    :param: df_long: Pandas dataframe with classid column and #class.
    :param: n: number of taxa to keep
    :return: Pandas dataframe with top N taxa and group the rest as 'Other'

    """
    top_taxa = df_long.groupby('classid')['percentage'].sum().nlargest(n).index

    # if class id in top 10, then continue with the name otherwise name it other.
    df_long['classid'] = df_long['classid'].where(df_long['classid'].isin(top_taxa), 'other')

    # Each row shows the total percentage of a taxon in a specific sample.
    df_final = df_long.groupby(['sample', 'classid'], as_index=False)['percentage'].sum()
    df_final = df_final.rename(columns={"class id": "classid"})
    return df_final


def save_table(df, taxon, output_dir):
    """
    Save to a csv file

    :param:df: dataframe to write.
    taxon:
        Taxonomic level name used to construct the output filename.
    output_dir:
        Directory that must already exist (created in :func:`main`).
    """
    df.to_csv(output_dir / f"{taxon}_table.csv", index=False)




def add_metadata(df_top, metadata_df):
    """
    Merge sample data with the metadata

    :param: df_top: Long-format abundance dataframe.
    :param: metadata_df: Raw metadata dataframe.

    :return: pd.DataFrama:  Merged and sorted dataframe
    """

    # Rename columns because of inconsistency
    metadata_df = metadata_df.rename(columns = {"Sample-id":"sample","DESCRIPTION":"sample","GenotypeIL22":"GT","Age":"Day" })
    metadata_df['sample'] = metadata_df['sample'].str.split(".").str[0]
    metadata_df['GT'] = metadata_df['GT'].str.strip().str.upper()

    #df_final = pd.merge(df_top, metadata_df, on='sample', how='left')
    metadata_df["Day"] = metadata_df["Day"].str.extract(r"(\d+)")
    df_final = pd.merge(df_top, metadata_df[['sample', 'Day', 'GT']], on='sample', how='left')
    df_final = df_final.sort_values(by=['Day', 'GT'], ascending=[True, False])

    return df_final

def process_taxa(df, taxon, output_dir, metadata_df, top_n=None):
    """
     Run the full pipeline for a single taxonomic level.

    :param df: abundance dataframe
    :param taxon: taxonomic level to process
    :param output_dir: directory where the file is saved
    :param metadata_df: dataframe
    :return: final long format dataframe with metadata
    """
    print(f"Processing: {taxon}")

    df_taxon = filter_taxon(df, taxon)
    df_final = reshape_and_calculate(df_taxon)

    if top_n is not None:
        df_final = top_n_taxa(df_final, n=top_n)

    save_table(df_final, taxon, output_dir)
    df_top = add_metadata(df_final, metadata_df)
    return df_top




def main():
    abundance = Path("path\\to\\abundancefile")
    metadata = Path("path\\to\\metadatafile(forcanocoo)")
    output_dir = Path("outputdir")
    output_dir.mkdir(parents=True, exist_ok=True)

    df, metadata_df = open_files(abundance, metadata)

    taxa_levels = detect_taxa_levels(df.iloc[:, 1])

    TOP_N = 10 # change to none if you want all taxa included

    for taxon in taxa_levels:
         df_top = process_taxa(df, taxon, output_dir, metadata_df, top_n=TOP_N)
         df_top = df_top.apply(lambda col: col.str.strip() if col.dtype == 'object' else col)
         df_top.to_csv(output_dir / f"{taxon}_table.csv", index=False)

main()
