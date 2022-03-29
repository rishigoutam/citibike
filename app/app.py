import os

import dash
from dash import html
from dash import dcc
from dash.dependencies import Input, Output

import sys

sys.path.insert(0, "..")
import helpers

ASSETS_DIR = "./assets/"

# create iframes reading raw html from assets
with open(ASSETS_DIR + "allstations" + helpers.HTML_EXTENSION) as f:
    text = f.read()
allstations_iframe = html.Div(
    children=[html.Iframe(srcDoc=text, style={"height": "1067px", "width": "100%"})]
)
with open(ASSETS_DIR + "allstations-watercolor" + helpers.HTML_EXTENSION) as f:
    text = f.read()
allstations_watercolor_iframe = html.Div(
    children=[html.Iframe(srcDoc=text, style={"height": "1067px", "width": "100%"})]
)

with open(ASSETS_DIR + "topstations" + helpers.HTML_EXTENSION) as f:
    text = f.read()
topstations_toner_iframe = html.Div(
    children=[
        html.Iframe(
            srcDoc=text,
            style={"height": "1067px", "width": "100%"},
        )
    ]
)
with open(ASSETS_DIR + "topstations-watercolor" + helpers.HTML_EXTENSION) as f:
    text = f.read()
topstations_watercolor_iframe = html.Div(
    children=[
        html.Iframe(
            srcDoc=text,
            style={"height": "1067px", "width": "100%"},
        )
    ]
)

with open(ASSETS_DIR + "topstations-heatmap" + helpers.HTML_EXTENSION) as f:
    text = f.read()
heatmap_toner_iframe = html.Div(
    children=[
        html.Iframe(
            srcDoc=text,
            style={"height": "1067px", "width": "100%"},
        )
    ]
)
with open(ASSETS_DIR + "topstations-heatmap-watercolor" + helpers.HTML_EXTENSION) as f:
    text = f.read()
heatmap_watercolor_iframe = html.Div(
    children=[
        html.Iframe(
            srcDoc=text,
            style={"height": "1067px", "width": "100%"},
        )
    ]
)

# app
app = dash.Dash(__name__)
app.layout = html.Div(
    [
        html.H1("Citibike Trips - Rebalance Analysis"),
        dcc.Tabs(
            id="tabs-toplevel",
            value="tab-1",
            children=[
                dcc.Tab(label="All Stations", value="tab-1"),
                dcc.Tab(label="Top Station Pairs", value="tab-2"),
                dcc.Tab(label="Stations Heatmap)", value="tab-3"),
            ],
        ),
        html.Div(id="tabs-firstlevel"),
    ]
)


@app.callback(
    Output("tabs-firstlevel", "children"),
    Input("tabs-toplevel", "value"),
)
def render_content(tab):
    if tab == "tab-1":
        return allstations_iframe
    elif tab == "tab-2":
        return topstations_toner_iframe
    elif tab == "tab-3":
        return heatmap_watercolor_iframe


app.run_server(debug=False, use_reloader=False)

#%%
