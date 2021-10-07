import pandas as pd
windows=[7200,14400,28800,57600,115200]
etype=['','_serial']

for t in etype:
    all_logs=[]

    for window in windows:
        df = pd.read_csv("log{t}{window}.csv".format(t=t, window=window),delimiter=',',header=0)
        df=df.drop(columns=['query time'])
        avg_std=df.mean().append(df.std(),ignore_index=True)
        all_logs.append(avg_std)

    newcols=['avg '+x for x in df.columns]
    newcols=newcols+['std '+x for x in df.columns]

    df=pd.DataFrame(all_logs)
    df.columns=newcols
    df.index=windows
    df.to_csv("log{t}_all.csv".format(t=t),sep=',')
