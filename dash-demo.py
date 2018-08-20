import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.graph_objs as go
import numpy as np
import dash_reusable_components as drc

app = dash.Dash()

PCheatInit = 0.1
UInit = 0.1
PhaseSize = 450

# Timeline (temp)
N = 100

random_x = np.arange(1, N+1)
random_y0 = np.random.randn(N)+10
random_y1 = np.random.randn(N)+5
random_y2 = np.random.randn(N)+15

# Create traces
trace0 = go.Scatter(
    x=random_x,
    y=random_y0,
    name='Disobedience'
)
trace1 = go.Scatter(
    x=random_x,
    y=random_y1,
    name='Satisfaction'
)
trace2 = go.Scatter(
    x=random_x,
    y=random_y2,
    name='Inequality'
)
timedata = [trace0, trace1, trace2]

# layout:
# options (checkboxes)
# phase shift graph
# timeline
# some statistics (metrics) on the side?
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
                        marks={i/10: '{}'.format(i/10) for i in range(10)},
                        min=0,
                        max=1,
                        value=UInit,
                        step=0.1,
                        updatemode='drag',
                        vertical=False
                    ),
                    drc.NamedSlider(
                        name="Non-Compliance",
                        id='slider-x',
                        marks={i/10: '{}'.format(i/10) for i in range(10)},
                        min=0,
                        max=1,
                        value=PCheatInit,
                        step=0.1,
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


@app.callback(Output('timeline', 'figure'),
              [Input('timeline-update', 'n_intervals')])
def gen_timeline(interval):
    # FIXME: gambi
    subtraces = [{'x': t['x'][0:interval],
                  'y': t['y'][0:interval],
                  'name': t['name']} for t in timedata]
    return go.Figure(
        data=subtraces,
        layout={'title': 'Timeline',
                'xaxis': {'title': 'step'}}
    )


@app.callback(Output('timeline-update', 'n_intervals'),
              [Input('button-run-operation', 'n_clicks')])
def gen_timeline(interval):
    # resets the tineline
    return 0


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
    # Normalize the CSS
    "https://cdnjs.cloudflare.com/ajax/libs/normalize/7.0.0/normalize.min.css",
    # Fonts
    "https://fonts.googleapis.com/css?family=Open+Sans|Roboto"
    "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
    # For production
    "https://cdn.rawgit.com/xhlulu/0acba79000a3fd1e6f552ed82edb8a64/raw/dash_template.css",
    # Custom CSS
    "https://cdn.rawgit.com/xhlulu/dash-image-processing/1d2ec55e/custom_styles.css",
]

for css in external_css:
    app.css.append_css({"external_url": css})

if __name__ == '__main__':
    app.run_server()

# Open questions / TODOs:
# - How to make an interactive phase plot?
# - read more documentation of graph
# - How to communicate from Erlang to program?
# - Write python function that calls erlang program and reads json


# Python - Erlang communication
# 1 - python call erlang as command line argument
# 2 - erlang outputs json to ... (file? database? stdouot captured by python?)
# 3 - python reads/receive erlang's output and feed it to graph
#       - how to do incremental graph? Maybe by erasing current partial and
#       plotting all the data all the time?
# 4 - implement reset function, etc..
