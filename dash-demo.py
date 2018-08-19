import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.graph_objs as go
import numpy as np

app = dash.Dash()

PCheatInit = 0.1
UInit = 0.1
PhaseSize = 500

# Timeline (temp)
N = 100

random_x = np.linspace(0, 1, N)
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
    dcc.Checklist(
        id="config-options",
        options=[
            {'label': 'Adaptive Rulers', 'value': 'adapt_rul'},
            {'label': 'Adaptive Agents', 'value': 'adapt_ag'},
        ],
        values=['adapt_rul']
    ),
    html.Div(
        style={'margin': '5em'},
        children=[
            dcc.Graph(id='phase-plot'),
            html.Div(
                dcc.Slider(
                    id='slider-y',
                    marks={i/10: '{}'.format(i/10) for i in range(10)},
                    min=0,
                    max=1,
                    value=UInit,
                    step=0.1,
                    updatemode='drag',
                    vertical=False
                ),
                style={'margin': '2em'}
            ),
            html.Div(
                dcc.Slider(
                    id='slider-x',
                    marks={i/10: '{}'.format(i/10) for i in range(10)},
                    min=0,
                    max=1,
                    value=PCheatInit,
                    step=0.1,
                    updatemode='drag'
                ),
                style={'margin': '2em'}
            ),
        ]
    ),
    dcc.Graph(
        id='timeline',
        figure=go.Figure(
            data=timedata,
            layout={
                'title': 'Timeline'
            }
        )
    ),
    # dcc.Input(id='my-id', value='initial value', type='text'),
    # dcc.Input(id='my-id2', value='ble', type='text'),
    html.Div(id='my-div')
])


@app.callback(
    Output(component_id='my-div', component_property='children'),
    [Input(component_id='slider-x', component_property='value')]
)
def update_output_div(input_value):
    return 'You\'ve entered "{}"'.format(input_value)


@app.callback(
    dash.dependencies.Output('phase-plot', 'figure'),
    [dash.dependencies.Input('slider-x', 'value'),
     dash.dependencies.Input('slider-y', 'value')])
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


app.css.append_css(
    {"external_url": "https://codepen.io/chriddyp/pen/bWLwgP.css"})

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
#       - how to do incremental graph? Maybe by erasing current partial and plotting all the data all the time?
# 4 - implement reset function, etc..
