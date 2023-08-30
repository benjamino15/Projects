import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import geopandas as gpd
import matplotlib.colors as colors

df = pd.read_excel('CaseB/Final_with food price.xlsx', header=1)
df = df[df["reference_label"] == "Sep-Dec"]

df17 = df[df["reference_year"] == 2017]
df18 = df[df["reference_year"] == 2018]
df19 = df[df["reference_year"] == 2019]
df20 = df[df["reference_year"] == 2020]


chad = gpd.read_file('chad_maps/gadm36_TCD_1.shp')

means17 = df17.groupby(['NAME_1'])['phase_class'].mean()
chad17 = pd.merge(chad, means17, on='NAME_1')

means18 = df18.groupby(['NAME_1'])['phase_class'].mean()
chad18 = pd.merge(chad, means18, on='NAME_1')

means19 = df19.groupby(['NAME_1'])['phase_class'].mean()
chad19 = pd.merge(chad, means19, on='NAME_1')

means20 = df20.groupby(['NAME_1'])['phase_class'].mean()
chad20 = pd.merge(chad, means20, on='NAME_1')

chad17.plot(column='phase_class', legend=True)
plt.show()
######################


def get_proper_names(data_chad: pd.DataFrame, results: pd.DataFrame, colname_1, colname_2):
    new_admin1 = results[colname_2].dropna().unique()
    old_admin1 = data_chad[colname_1].dropna().unique()

    names_correct, indices1, indices2 = np.intersect1d(new_admin1, old_admin1, return_indices=True)

    # new_admin_correct = new_admin1[indices1]
    # old_admin_correct = old_admin1[indices2]
    #
    new_admin_false = np.delete(new_admin1, indices1)
    old_admin_false = np.delete(old_admin1, indices2)

    dict_encoding = {key: key for key in names_correct}

    mapping = {'Barh-El-Gazel': 'Barh el Ghazel',
               'Ennedi-Est': 'Ennedi Est',
               'Guera': 'Guéra',
               'Mayo Kebbi Est': 'Mayo-Kebbi Est',
               'Ouaddai': 'Ouaddaï',
               'Tandjile': 'Tandjilé',
               "N'Djamena": "Ville de N'Djamena"}

    dict_encoding.update(mapping)

    # for i in range(len(mapping_old)):
    #     dict_encoding[mapping_old[i]] = mapping_new[i]

    return dict_encoding


def main():
    df = pd.read_excel('CaseB/Final_with food price.xlsx', header=1)

    df17 = df[df["reference_year"] == 2017]
    df18 = df[df["reference_year"] == 2018]
    df19 = df[df["reference_year"] == 2019]
    df20 = df[df["reference_year"] == 2020]

    df = df[df["reference_label"] == "Sep-Dec"]

    chad = gpd.read_file('chad_maps/gadm36_TCD_1.shp')
    results_2020 = pd.read_csv('results2020.csv')
    results_2021 = pd.read_csv('results2021.csv')

    col_chad = 'NAME_1'
    col_results = '2'

    dict_encoding_2020 = get_proper_names(chad, results_2020, colname_1=col_chad, colname_2=col_results)
    results_2020[col_results] = results_2020[col_results].map(dict_encoding_2020)
    results_2020.columns = ['Unnamed: 0', '1', col_chad, '3', '4']

    dict_encoding_2021 = get_proper_names(chad, results_2021, colname_1=col_chad, colname_2=col_results)
    results_2021[col_results] = results_2021[col_results].map(dict_encoding_2021)
    results_2021.columns = ['Unnamed: 0', '1', col_chad, '3', '4']

    # From this point on they have the correct names!

    df17.groupby('phase_class').count()

    chad.plot()
    plt.show()
##########################################################




    df = pd.read_excel('CaseB/Final_with food price.xlsx', header=1)
    df21Jan = df[df["reference_year"] == 2021]
    df21Sep = df[df["reference_year"] == 2021]
    df21Jan = df21Jan[df21Jan["reference_label"] == "Jan-May"]
    df21Sep = df21Sep[df21Sep["reference_label"] == "Sep-Dec"]




    # chad.iloc[:, :4]
    # change the name of the second column in df to NAME_

    colors_list = ['#ADD8E6', '#0000FF', '#00008B']
    colors_list_plot4 = ['#ADD8E6', '#0000FF']
    cmap = colors.ListedColormap(colors_list)
    cmap_plot4 = colors.ListedColormap(colors_list_plot4)

    # merge the two dataframes based on column 'A'
    chad2020 = pd.merge(chad, results_2020, on='NAME_1')
    chad2021 = pd.merge(chad, results_2021, on='NAME_1')

    # plot de map of the regions using phase class variable
    chad2020[chad2020["4"] == "Jan-May"].plot(column='1', legend=True, categorical=True, cmap=cmap)
    chad2021[chad2021["4"] == "Jan-May"].plot(column='1', legend=True, categorical=True, cmap=cmap)
    

    df21Jan = pd.merge(chad, df21Jan, on='NAME_1')
    df21Sep = pd.merge(chad, df21Sep, on='NAME_1')
    df21Jan.plot(column='phase_class', legend=True, categorical=True, cmap=cmap)
    plt.show()
     



    fig, axs = plt.subplots(2, 2, figsize=(12, 10))

    # plots
    df21Jan.plot(column='phase_class', legend=True, categorical=True, cmap=cmap, ax=axs[0,0])
    axs[0, 0].set_title("Actual Jan-May 2021")
    axs[0, 0].set_ylabel("Longitude")

    chad2020[chad2020["4"] == "Jan-May"].plot(column='1', legend=True, categorical=True, cmap=cmap, ax=axs[0,1])
    axs[0, 1].set_title("Predicted Jan-May 2021")
    axs[0, 1].set_ylabel("Longitude")

    df21Sep.plot(column='phase_class', legend=True, categorical=True, cmap=cmap, ax=axs[1,0])
    axs[1, 0].set_title("Actual Sep-Dec 2021")
    axs[1, 0].set_ylabel("Longitude")
    axs[1, 0].set_xlabel("Latitude")

    chad2020[chad2020["4"] == "Sep-Dec"].plot(column='1', legend=True, categorical=True, cmap=cmap_plot4, ax=axs[1,1])
    axs[1, 1].set_title("Predicted Sep-Dec 2021")
    axs[1, 1].set_ylabel("Longitude")
    axs[1, 1].set_xlabel("Latitude")

    # Adjust the spacing between the subplots
    plt.subplots_adjust(wspace=0.1, hspace=0.2)

    plt.show()







    chad2021[chad2021["4"] == "Jan-May"].plot(column='1', legend=True, categorical=True, cmap=cmap_plot4)
    chad2021[chad2021["4"] == "Sep-Dec"].plot(column='1', legend=True, categorical=True, cmap=cmap)

    plt.show()

    fig, (ax1, ax2) = plt.subplots(ncols=2, figsize=(10, 4))

    # Plot the first subplot
    chad2021[chad2021["4"] == "Jan-May"].plot(column='1', legend=True, categorical=True, cmap=cmap, ax=ax1)
    ax1.set_title("Prediction Jan-May 2022")
    ax1.set_ylabel("Longitude")
    ax1.set_xlabel("Latitude")

    # Plot the second subplot
    chad2021[chad2021["4"] == "Sep-Dec"].plot(column='1', legend=True, categorical=True, cmap=cmap, ax=ax2)
    ax2.set_title("Prediction Sep-Dec 2022")
    ax1.set_ylabel("Longitude")
    ax1.set_xlabel("Latitude")
    

    plt.show()


    chad2021[chad2021["4"] == "Prediction Jan-May 202"].plot(column='1', legend=True, categorical=True, cmap=cmap)
    plt.show()
     


if __name__ == "__main__":
    main()