import os

import dash
from dash import html
from dash import dcc
from dash.dependencies import Input, Output
import dash_bootstrap_components as dbc

import sys

sys.path.insert(0, "..")
import helpers

ASSETS_DIR = "./assets/"

# styles
iframe_style = {"height": "600px", "width": "100%"}

# create iframes reading raw html from asset
with open(ASSETS_DIR + "allstations" + helpers.HTML_EXTENSION) as f:
    text = f.read()
allstations_iframe = html.Div(children=[html.Iframe(srcDoc=text, style=iframe_style)])
with open(ASSETS_DIR + "allstations-watercolor" + helpers.HTML_EXTENSION) as f:
    text = f.read()
allstations_watercolor_iframe = html.Div(
    children=[html.Iframe(srcDoc=text, style=iframe_style)]
)

with open(ASSETS_DIR + "topstations" + helpers.HTML_EXTENSION) as f:
    text = f.read()
topstations_toner_iframe = html.Div(
    children=[html.Iframe(srcDoc=text, style=iframe_style)]
)
with open(ASSETS_DIR + "topstations-watercolor" + helpers.HTML_EXTENSION) as f:
    text = f.read()
topstations_watercolor_iframe = html.Div(
    children=[html.Iframe(srcDoc=text, style=iframe_style)]
)

with open(ASSETS_DIR + "topstations-heatmap" + helpers.HTML_EXTENSION) as f:
    text = f.read()
heatmap_toner_iframe = html.Div(children=[html.Iframe(srcDoc=text, style=iframe_style)])
with open(ASSETS_DIR + "topstations-heatmap-watercolor" + helpers.HTML_EXTENSION) as f:
    text = f.read()
heatmap_watercolor_iframe = html.Div(
    children=[html.Iframe(srcDoc=text, style=iframe_style)]
)

# app
app = dash.Dash(__name__, external_stylesheets=[dbc.themes.SOLAR])
app.layout = dbc.Container(
    [
        dcc.Store(id="store"),
        html.Div(id="blank-output"),
        dbc.Tabs(
            [
                dbc.Tab(label="Station Distribution", tab_id="tab-1"),
                dbc.Tab(label="Station Information", tab_id="tab-2"),
                dbc.Tab(label="Top Rebalance Routes", tab_id="tab-3"),
            ],
            id="tabs",
            active_tab="tab-1",
        ),
        html.Div(id="tab-content"),
    ]
)


@app.callback(
    Output("tab-content", "children"),
    Input("tabs", "active_tab"),
)
def render_tab_content(active_tab):
    if active_tab:
        if active_tab == "tab-1":
            return heatmap_watercolor_iframe
        elif active_tab == "tab-2":
            return allstations_iframe
        elif active_tab == "tab-3":
            return topstations_toner_iframe
    else:
        return "No tab selected"


# change title
app.clientside_callback(
    """
    function(active_tab) {
        if (active_tab === 'tab-1') {
            document.title = 'Citibike Station Heatmap'
        } else if (active_tab === 'tab-2') {
            document.title = 'Station Information'
        } else if (active_tab === 'tab-3') {
            document.title = 'Top Routes by Rebalance Counts'
        }
    }
    """,
    Output("blank-output", "children"),
    Input("tabs", "active_tab"),
)

app.run_server(debug=True, use_reloader=False, port=8051)

# %%
