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
iframe_style = {"height": "700px", "width": "100%"}
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


def spacer():
    return html.H4("")


# create home page
home_page = dbc.Container(
    dbc.Col(
        [
            spacer(),
            spacer(),
            spacer(),
            html.H2("Citi Bike - Predicting and Inducing Demand"),
            # dbc.Row([
            #     html.Div(html.Img(src="./assets/demographics/pie-gender.png"), style={"overflow": "hidden", "height": "220px"}),
            #     html.Div(html.Img(src="./assets/demographics/pie-usertype.png"), style={"overflow": "hidden", "height": "220px"}),
            # ], style={"display": "flex", "flex-direction": "col"}),
            html.H4("Trip demand is influenced by seasonality and weather"),
            html.P(
                "Citi bike trips show several patterns. Demand varies depending on time (of year, day of week, time of day), temperature, and precipitation. Predicting these variations is important as Citi Bike needs to have enough bikes out to meet demand for better customer experience. Also, in leaner months, it needs to take bikes in for storage to avoid wear and tear on bikes."
            ),
            html.H4("Rebalancing induces demand by increasing supply"),
            html.P(
                "By bringing bikes to where they are needed, there is a virtuous cycle of demand generation, leading to increasingly satisfied customers. Through our exploratory data analysis, we shall see how Citi Bike uses rebalancing techniques such as paid employees as well as volunteers to rebalance bikes."
            ),
        ],
        style={"width": "800px"},
    )
)

# create slides page
with open(ASSETS_DIR + "google-slides" + helpers.HTML_EXTENSION) as f:
    text = f.read()
google_slides_iframe = html.Div(children=[html.Iframe(srcDoc=text, style=iframe_style)])
slides_page = dbc.Container(
    dbc.Col(
        [
            html.H1("NYC Data Science Academy Capstone"),
            html.H3(
                "Predicting Citi Bike Trip Demand and Analysis of Station Re-balancing",
            ),
            google_slides_iframe,
            html.H4(
                "Rishi Goutam, James Goudreault, Srikar Pamidi, March 31 2022",
            ),
        ]
    )
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

maps_page = dbc.Container(
    [
        html.Div(id="blank-output"),
        spacer(),
        spacer(),
        spacer(),
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

# create wip page
wip_page = dbc.Container(
    dbc.Col(
        [
            spacer(),
            html.H3("Work in Progress..."),
            spacer(),
            spacer(),
            spacer(),
            spacer(),
            dbc.Label("Team Checklist"),
            spacer(),
            dbc.Checklist(
                options=[
                    {"label": "Convert CSV to parquet", "value": 1},
                    {"label": "Conduct exploratory data analysis", "value": 2},
                    {
                        "label": "Create SARIMA model for trip demand predictions based on seasons",
                        "value": 3,
                    },
                    {
                        "label": "Create LSTM model for seasonal + weather-based trip predictions",
                        "value": 4,
                    },
                    {
                        "label": "Create LSTM model for dock station capacity prediction",
                        "value": 5,
                    },
                    {"label": "Read real-time Citi Bike GBFS feed", "value": 6},
                    {"label": "Host this Dash application", "value": 7},
                ],
                value=[1, 2, 3, 4],
                id="checklist-input",
            ),
            spacer(),
            spacer(),
            dbc.Progress(value=80, animated=True, striped=True),
        ]
    )
)


# create contact page
def create_card(
    filename: str,
    fullname: str,
    description: str,
    github_url: str,
    linkedin_url: str,
    email_address: str,
):
    return dbc.Card(
        [
            dbc.CardImg(src=ASSETS_DIR + filename + helpers.PNG_EXTENSION, top=True),
            dbc.CardBody(
                [
                    html.H4(fullname, className="card-title"),
                    html.P(
                        description,
                        className="card-text",
                    ),
                    dbc.ButtonGroup(
                        [
                            dbc.Button("GitHub", color="primary", href=github_url),
                            dbc.Button("LinkedIn", color="primary", href=linkedin_url),
                            dbc.Button(
                                "Email", color="primary", href="mailto:" + email_address
                            ),
                        ]
                    ),
                ]
            ),
        ],
        style={"width": "18rem"},
    )


card_rishi = create_card(
    filename="rishi",
    fullname="Rishi Goutam",
    description="I am a data scientist with a computer science and business background. I love creating maps!",
    github_url="github.com/rishigoutam/",
    linkedin_url="linkedin.com/in/rishigoutam/,",
    email_address="rishi@goutam.org",
)
card_james = create_card(
    filename="james",
    fullname="James Goudreault",
    description="I have a background in engineering and operations. I like climbing big mountains!",
    github_url="github.com/gitjgoud/",
    linkedin_url="linkedin.com/in/james-goudreault/",
    email_address="",
)
card_srikar = create_card(
    filename="srikar",
    fullname="Srikar Pamidimukkala",
    description="I am a mathematician, student, lifelong learner. I love diving deep into ML algorithms and time series.",
    github_url="github.com/SacredFisher/",
    linkedin_url="linkedin.com/in/srikar-pamidi/",
    email_address="",
)
contact_page = dbc.Container(
    [
        dbc.Col(
            [
                spacer(),
                spacer(),
                spacer(),
                dbc.Row(html.H3("The Team"), align="end"),
                dbc.Row(
                    html.H4("Please feel free to reach out or view our portfolios"),
                    align="end",
                ),
                spacer(),
                dbc.Row(
                    [dbc.Col(card_rishi), dbc.Col(card_james), dbc.Col(card_srikar)],
                    align="start",
                ),
            ]
        )
    ]
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

# app layout
content = dbc.Container(id="page-content", style=CONTENT_STYLE)
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
        return wip_page
    elif pathname == "/contact":
        return contact_page
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
