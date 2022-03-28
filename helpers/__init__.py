import pandas as pd
import numpy as np
import seaborn as sns
import logging

PARQUET_EXTENSION = ".parquet"
CSV_EXTENSION = ".csv"
PNG_EXTENSION = ".png"
HTML_EXTENSION = ".html"
GEOJSON_EXTENSION = ".geojson"

TRIPS_COLUMNS = [
    "tripduration",
    "starttime",
    "stoptime",
    "startstationid",
    "endstationid",
    "bikeid",
    "usertype",
    "birthyear",
    "gender",
]


def get_trips(
    year: int, data_dir_rel_path="../data/", convert_dates=False
) -> pd.DataFrame:
    """
    Returns trips for a given year
    :param year: the year for which to return trips for
    :param convert_dates: whether to convert the `starttime` and `stoptime` date columns to `datetime64`
    :param data_dir_rel_path: the relative path to the data directory
    :return: dataframe of trips. NOTE: some years have mixed date types for the "starttime" and "stoptime" columns
    and for these, the type of the column will be left as string rather than datetime64
    """
    parquet_filepath = (
        data_dir_rel_path + "tripdata_parquet/NY/" + str(year) + PARQUET_EXTENSION
    )
    logging.debug(f"Reading {parquet_filepath} for year {year}...")
    trips = pd.read_parquet(
        parquet_filepath,
        columns=TRIPS_COLUMNS,
        engine="pyarrow",
    ).reset_index()
    trips.drop(trips.columns[0], axis=1, inplace=True)  # drop the dask index

    logging.debug(f"Changing dtypes")
    __convert_trips_dtypes(trips, year, convert_dates)

    logging.debug(f"Done with {year}!")

    return trips


def __convert_trips_dtypes(trips: pd.DataFrame, year: int, convert_dates=False) -> None:
    # manually change dtype of columns for trips df as long as it is possible
    # starttime and stoptime have mixed formats for some years
    trips["tripduration"] = trips["tripduration"].astype("int32")
    trips["startstationid"] = trips["startstationid"].astype("int16")
    trips["endstationid"] = trips["endstationid"].astype("int16")
    trips["bikeid"] = trips["bikeid"].astype("int32")
    trips["birthyear"] = trips["birthyear"].astype("float16").astype("int16")
    trips["gender"] = trips["gender"].astype("int8")
    trips["usertype"] = trips["usertype"].astype("category")

    if convert_dates:
        trips["starttime"] = trips["starttime"].astype("datetime64")
        trips["stoptime"] = trips["stoptime"].astype("datetime64")


def colors_from_values(values: pd.Series, palette_name: str, ascending=True):
    # https://stackoverflow.com/questions/36271302/changing-color-scale-in-seaborn-bar-plot
    # convert to indices
    values = values.sort_values(ascending=ascending).reset_index()
    indices = values.sort_values(by=values.columns[0]).index
    # use the indices to get the colors
    palette = sns.color_palette(palette_name, len(values))
    return np.array(palette).take(indices, axis=0)
