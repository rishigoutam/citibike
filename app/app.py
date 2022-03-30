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
title_style = {"padding-left": "1.5em"}
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

# create home page
home_page = dbc.Container([html.P("Coming soon!")])


# create slides page
with open(ASSETS_DIR + "google-slides" + helpers.HTML_EXTENSION) as f:
    text = f.read()
google_slides_iframe = html.Div(children=[html.Iframe(srcDoc=text, style=iframe_style)])
slides_page = dbc.Container(
    [
        html.H1("NYC Data Science Academy Capstone", style=title_style),
        html.H3(
            "Predicting Citi Bike Trip Demand and Analysis of Station Re-balancing",
            style=title_style,
        ),
        google_slides_iframe,
        html.H4(
            "Rishi Goutam, James Goudreault, Srikar Pamidi, March 31 2022",
            style=title_style,
        ),
    ]
)


# create maps page
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

# app side navbar with router links
sidebar = html.Div(
    [
        html.H3("Citi Bike", className="display-4"),
        html.Hr(),
        html.P(
            "Improving operations through trip demand prediction and dock station rebalancing analysis"
        ),
        dbc.Nav(
            [
                dbc.NavLink("About Citi Bike", href="/", active="exact"),
                dbc.NavLink("Presentation", href="/slides", active="exact"),
                dbc.NavLink("Interactive analysis", href="/maps", active="exact"),
                dbc.NavLink("Future Development", href="/wip", active="exact"),
                dbc.NavLink("Contact", href="/contact", active="exact"),
            ],
            vertical=True,
            pills=True,
        ),
    ],
    style=SIDEBAR_STYLE,
)

# app content
content = dbc.Container(id="page-content", style=CONTENT_STYLE)
maps_page = dbc.Container(
    [
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
            document.title = 'Citi Bike Station Heatmap'
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
        return home_page
    elif pathname == "/slides":
        return slides_page
        # return html.P("Coming soon!")
    elif pathname == "/maps":
        return maps_page
    elif pathname == "/wip":
        return home_page
    elif pathname == "/contact":
        return home_page
    else:
        # If the user tries to reach a different page, return a 404 message
        return dbc.Alert(
            [
                html.H1("404: Not found", className="text-danger"),
                html.Hr(),
                html.P(f"The pathname {pathname} was not recognised..."),
            ]
        )


app.run_server(debug=True, use_reloader=False, port=8054)

# %%
