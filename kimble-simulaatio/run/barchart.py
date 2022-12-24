import bar_chart_race as bcr
import pandas as pd 

df = pd.read_table("voittajat.txt",delim_whitespace=True)
df.loc[-1] = ["Pelaaja1","Pelaaja2","Pelaaja3","Pelaaja4"]  # adding a row
df.index = df.index + 1  # shifting index
df.sort_index(inplace=True) 
# df = bcr.load_dataset('covid19_tutorial')
print(df)
bcr.bar_chart_race(
    df=df,
    filename='kimblekisa.mp4',
    orientation='h',
    sort='desc',
    n_bars=4,
    steps_per_period=20,
    interpolate_period=True,
    fixed_order=True,
    label_bars=True,
    bar_size=.95,
    period_length=300,
    cmap='dark12',
    scale='linear',
    writer=None,
    fig=None,
    bar_kwargs={'alpha': .7},
    filter_column_colors=False)