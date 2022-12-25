import bar_chart_race as bcr
import pandas as pd 
import matplotlib
#cmap = matplotlib.colors.ListedColormap(matplotlib.cm.get_cmap("Set1").colors[0:3] + (matplotlib.cm.get_cmap("Set1").colors[4]))
alkuperäiset = list(matplotlib.cm.get_cmap("Set1").colors[0:3])
uudet = matplotlib.cm.get_cmap("Set1").colors[4]
alkuperäiset.append(uudet)
cmap = tuple(alkuperäiset)
df = pd.read_table("voittajat.txt",delim_whitespace=True)
df.sort_index(inplace=True) 
bcr.bar_chart_race(
    df=df,
    filename='kimblekisa.mp4',
    orientation='h',
    # fixed_order= ["pelaaja1,","pelaaja2", "pelaaja3", "pelaaja4"],
    steps_per_period=20,
    interpolate_period=True,
    fixed_order=True,
    label_bars=True,
    bar_size=.95,
    period_length=300,
    cmap=cmap,
    scale='linear',
    writer=None,
    fig=None,
    bar_kwargs={'alpha': .7},
    filter_column_colors=False)
