import os

import dash
from dash import html
from dash import dcc
from dash.dependencies import Input, Output

import sys

sys.path.insert(0, "..")
import helpers

ASSETS_DIR = "./assets/"

# styles
tab_style = {"line-height": "3vh"}
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
app = dash.Dash(__name__)
app.layout = html.Div(
    [
        html.Div(id="blank-output"),
        dcc.Tabs(
            id="tabs-toplevel",
            value="tab-1",
            children=[
                dcc.Tab(label="Station Distribution", value="tab-1", style=tab_style),
                dcc.Tab(label="Station Information", value="tab-3", style=tab_style),
                dcc.Tab(label="Top Rebalance Routes", value="tab-2", style=tab_style),
            ],
        ),
        html.Div(id="tabs-toplevel-children"),
    ]
)


@app.callback(
    Output("tabs-toplevel-children", "children"),
    Input("tabs-toplevel", "value"),
)
def render_content(tab):
    if tab == "tab-1":
        return heatmap_watercolor_iframe
    elif tab == "tab-2":
        return topstations_toner_iframe
    elif tab == "tab-3":
        return allstations_iframe


# change title
app.clientside_callback(
    """
    function(tab_value) {
        if (tab_value === 'tab-1') {
            document.title = 'Citibike Station Heatmap'
        } else if (tab_value === 'tab-2') {
            document.title = 'Station Information'
        } else if (tab_value === 'tab-3') {
            document.title = 'Top Routes by Rebalance Counts'
        }
    }
    """,
    Output("blank-output", "children"),
    Input("tabs-toplevel", "value"),
)

app.run_server(debug=False, use_reloader=False, port=8051)

# %%
