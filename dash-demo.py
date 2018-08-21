import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.graph_objs as go
import pandas as pd
import dash_reusable_components as drc


PCheatInit = 0.3
UInit = 0.25
PhaseSize = 450

fakemode = True
fakedata = pd.read_csv("sample.csv")

app = dash.Dash(__name__)
app.layout = html.Div([
    # Banner display
    html.Div([
        html.H2(
            'Disobedience as a Mechanism of Change',
            id='title'
        ),
    ],
             className="banner"
    ),
    # Body
    html.Div(className="container", children=[
        html.Div(className='row', children=[
            html.Div(className='five columns', children=[
                drc.Card([
                    drc.NamedInlineCheckboxes(
                        name="Options",
                        short="config-options",
                        options=[
                            {'label': 'Adaptive Rulers', 'value': 'adapt_rul'},
                            {'label': 'Adaptive Agents', 'value': 'adapt_ag'},
                        ],
                        vals=['adapt_rul']
                    ),
                    drc.NamedSlider(
                        name="Allocation Unfairness",
                        id='slider-y',
                        marks={i/10: i/10 for i in range(10)},
                        min=0,
                        max=1,
                        value=UInit,
                        step=0.01,
                        updatemode='drag',
                        vertical=False
                    ),
                    drc.NamedSlider(
                        name="Non-Compliance",
                        id='slider-x',
                        marks={i/10: i/10 for i in range(10)},
                        min=0,
                        max=1,
                        value=PCheatInit,
                        step=0.01,
                        updatemode='drag'
                    ),
                ]),


                dcc.Graph(id='phase-plot',
                          config={'displayModeBar': False}),

            ]),

            html.Div(
                className='seven columns',
                style={'float': 'right'},
                children=[
                    dcc.Graph(
                        id='timeline',
                    ),
                    dcc.Interval(id='timeline-update',
                                 interval=1000, n_intervals=0),
                    # Hidden Div storing JSON-serialized data
                    html.Div(id='timeline-data', style={'display': 'none'}),
                    html.Button(
                        'Run Simulation',
                        id='button-run-operation',
                        style={'margin-right': '10px', 'margin-top': '5px'}
                    ),
                ]
            )
        ])
    ])
])

if fakemode:  # here is one I made earlier...
    @app.callback(
        Output('timeline-data', 'children'),
        [Input('timeline-update', 'n_intervals')])
    def gen_timeline_data(interval):
        selection = fakedata[:5*interval]
        return selection.to_json(orient='split')

    @app.callback(
        Output('timeline-update', 'n_intervals'),
        [Input('button-run-operation', 'n_clicks')])
    def restart_simulation(interval):
        # resets the tineline
        return 0


def col2trace(xdata, ydata, title):
    return go.Scatter(x=xdata, y=ydata, name=title)


@app.callback(
    Output('timeline', 'figure'),
    [Input('timeline-data', 'children')])
def update_timeline(timeline_json):
    # TODO: check if erlang will send this same orientation
    timeline_df = pd.read_json(timeline_json, orient='split')
    tsteps = timeline_df.index+1

    timeline_keys = [
        ("alloc_unfairness", "Alloc Unfairness (U)"),
        ("inequality", "Inequality"),
        ("people_noncompliance", "Disobedience"),
        ("satisf_clique", "Rulers' Satisfaction"),
        ("satisf_people", "People's Satisfaction")
    ]

    traces = [col2trace(tsteps, timeline_df[tk[0]], tk[1])
              for tk in timeline_keys]

    return go.Figure(
        data=traces,
        layout={'title': 'Timeline',
                'xaxis': {'title': 'turn'}}
    )


@app.callback(
    Output('phase-plot', 'figure'),
    [Input('slider-x', 'value'),
     Input('slider-y', 'value')])
def update_figure(PCheat, U):
    return {
        'data': [
            go.Scatter(
                x=[PCheat],
                y=[U],
                mode='markers',
                marker=dict(
                    size=20
                )
            )
        ],
        'layout': go.Layout(
            title='Exploratory Space',
            showlegend=False,
            width=PhaseSize,
            height=PhaseSize,
            xaxis=dict(
                title="Disobedience",
                range=[0, 1],
                fixedrange=True,
                showline=True,
                showgrid=True,
                mirror='ticks',
                linecolor='#636363',
                linewidth=6
            ),
            yaxis=dict(
                title="Allocation Unfairness",
                range=[0, 1],
                fixedrange=True,
                showline=True,
                showgrid=True,
                mirror='ticks',
                linecolor='#636363',
                linewidth=6
            )
        )
    }


external_css = [
    "https://cdnjs.cloudflare.com/ajax/libs/normalize/7.0.0/normalize.min.css",  # Normalize the CSS
    "https://fonts.googleapis.com/css?family=Open+Sans|Roboto"  # Fonts
    "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
    "https://cdn.rawgit.com/xhlulu/0acba79000a3fd1e6f552ed82edb8a64/raw/dash_template.css",
    "https://rawgit.com/plotly/dash-live-model-training/master/custom_styles.css"
]

for css in external_css:
    app.css.append_css({"external_url": css})


if __name__ == '__main__':
    app.run_server(debug=True)

# Open questions / TODOs:
# - Interface:
#    - center exploratory space
#    - movable exploratory space
#    - live changes / reset simulation / pause button
#    - presets configurations
#    - timeline updates exploratory space
#    - some statistics (metrics) on the side?
# - How to communicate from Erlang to program?
# - Write python function that calls erlang program and reads json


# Python - Erlang communication
# 1 - python call erlang as command line argument
# 2 - erlang outputs json to ... (file? database? stdouot captured by python?)
# 3 - python reads/receive erlang's output and feed it to graph
# 4 - implement reset function, etc..
