import os
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output, State
import plotly.graph_objs as go
import pandas as pd
import dash_reusable_components as drc


PCheatInit = 0.3
UInit = 0.25
PhaseSize = 450

fakemode = False
# here is one I made earlier...
sim_path = "data/livedata.csv" if not fakemode else "data/sample.csv"

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
                    html.H4(
                        'Setup',
                        id='card-title'
                    ),
                    drc.NamedInlineCheckboxes(
                        name="Features",
                        short="config-options",
                        options=[
                            {'label': 'Forgiveness', 'value': 'forg'},
                            {'label': 'Rulers\' Corruption',
                             'value': 'adapt_rul'},  # FIXME: 3 states
                            {'label': 'Reformation', 'value': 'reform'},
                        ],
                        vals=['forg', 'adapt_rul', 'reform']
                    ),
                    drc.NamedSlider(
                        name="Initial Allocation Unfairness",
                        id='slider-u',
                        marks={i/10: i/10 for i in range(10)},
                        min=0,
                        max=1,
                        value=UInit,
                        step=0.01,
                        updatemode='drag',
                        vertical=False
                    ),
                    drc.NamedSlider(
                        name="Initial Non-Compliance",
                        id='slider-pcheat',
                        marks={i/10: i/10 for i in range(10)},
                        min=0.01,
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
                        id='button-run',
                        style={'margin-right': '10px', 'margin-top': '5px'}
                    ),
                    # html.Button(
                    #     'Pause',
                    #     id='button-pause',
                    #     style={'margin-top': '5px'}
                    # )
                ]
            )
        ])
    ])
])


@app.callback(
    Output('timeline-update', 'n_intervals'),
    [Input('timeline-data', 'children')])
def restart_simulation(_):
    # resets the timeline counter
    return 0


@app.callback(
    Output('timeline-data', 'children'),
    [Input('button-run', 'n_clicks')],
    [State('slider-u', 'value'),
     State('slider-pcheat', 'value'),
     State('check-config-options', 'values')])
def gen_timeline_data(_, u, disob, features):
    # starts new simulation and resets the timeline
    if not fakemode:
        # U (alloc unfairness) - [0,1]
        # Disob - [0,1]
        # Forg - {off, general, [oppresion]}
        # Reform - {on, off}
        # AdaptRul - {off, fixed, adaptive} TODO: dropdown?
        forg = 'general' if 'forg' in features else 'oppression'
        reform = 'on' if 'reform' in features else 'off'
        adapt_rul = 'fixed' if 'adapt_rul' in features else 'off'

        # FIXME: in future dont need to always compile (move .beam?)
        sim_cmd = ("erl -compile pardon -compile aux -compile dataio && "
                   "erl -noshell -s pardon main {:.2f} {:.2f} {} "
                   "{} {} political -s init stop").format(u, disob, forg,
                                                          reform, adapt_rul)
        print("Starting new execution...")
        print(sim_cmd)
        os.system(sim_cmd)
        print("Execution finished")

    try:
        data = pd.read_csv(sim_path)
    except FileNotFoundError as error:
        print(error)
        data = None
    return data.to_json(orient='split')


def col2trace(xdata, ydata, title):
    return go.Scatter(x=xdata, y=ydata, name=title)


@app.callback(
    Output('timeline', 'figure'),
    [Input('timeline-update', 'n_intervals')],
    [State('timeline-data', 'children')])
def update_timeline(interval, timeline_json):
    timeline_df = pd.read_json(timeline_json, orient='split')[:5*interval]
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


def fig_phaseplot(PCheat, U):
    return {
        'data': [
            go.Scatter(
                x=[U],
                y=[PCheat],
                mode='markers',
                marker=dict(
                    size=30
                )
            )
        ],
        'layout': go.Layout(
            title='Exploratory Space',
            showlegend=False,
            width=PhaseSize,
            height=PhaseSize,
            yaxis=dict(
                title="Disobedience",
                range=[0, 1],
                fixedrange=True,
                showline=True,
                showgrid=True,
                mirror='ticks',
                linecolor='#636363',
                linewidth=6
            ),
            xaxis=dict(
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


@app.callback(
    Output('phase-plot', 'figure'),
    [Input('timeline-update', 'n_intervals')],
    [State('timeline-data', 'children')])
def update_phaseplot(interval, timeline_json):
    timeline_data = pd.read_json(timeline_json, orient='split')
    last_timeline_row = timeline_data.iloc[5*interval]
    return fig_phaseplot(last_timeline_row["people_noncompliance"],
                         last_timeline_row["alloc_unfairness"])


# @app.callback(
#     Output('phase-plot', 'figure'),
#     [Input('slider-pcheat', 'value'),
#      Input('slider-u', 'value')])
# def set_phaseplot(PCheat, U):
#     return fig_phaseplot(PCheat, U)


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
