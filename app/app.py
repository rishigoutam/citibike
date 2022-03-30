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
# fixed sidebar on left. NOTE: hacky CSS widths
SIDEBAR_STYLE = {
    "position": "fixed",
    "top": 0,
    "left": 0,
    "bottom": 0,
    "width": "220px",
    "padding": "1rem .5rem",
    "background-color": "#f8f9fa",
}
# styles for the main content to position it to right of sidebar
CONTENT_STYLE = {
    "margin-left": "110px",
}

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

# app sidebar
sidebar = html.Div(
    [
        html.H3("Citibike", className="display-4"),
        html.Hr(),
        html.P(
            "Improving operations through trip demand prediction and dock station rebalancing analysis",
            className="lead",
        ),
        dbc.Nav(
            [
                dbc.NavLink("Home", href="/", active="exact"),
                dbc.NavLink("Demand for trips", href="/page-1", active="exact"),
                dbc.NavLink("Rebalance analysis", href="/page-2", active="exact"),
            ],
            vertical=True,
            pills=True,
        ),
    ],
    style=SIDEBAR_STYLE,
)

# app content
content = dbc.Container(id="page-content", style=CONTENT_STYLE)
maps_content = dbc.Container(
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
    ],
)
# app layout
app.layout = dbc.Container([dcc.Location(id="url"), sidebar, content])


# tab callback
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


# document title callback
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


# sidebar navigation callback
@app.callback(Output("page-content", "children"), [Input("url", "pathname")])
def render_page_content(pathname):
    if pathname == "/":
        return html.P("Coming soon!")
    elif pathname == "/page-1":
        return html.P("Coming soon!")
    elif pathname == "/page-2":
        return maps_content
    # If the user tries to reach a different page, return a 404 message
    return dbc.Jumbotron(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognised..."),
        ]
    )


app.run_server(debug=True, use_reloader=False, port=8052)

# %%
